open! Core
open! Async

module Connection_source : sig
  type 'a t =
    | Web of 'a
    | Plain_tcp
  [@@deriving sexp_of]
end

module Connection_initiated_from : sig
  type t =
    | Websocket_request of Cohttp_async.Request.t
    | Tcp
  [@@deriving sexp_of]
end

type http_handler =
  body:Cohttp_async.Body.t
  -> Socket.Address.Inet.t
  -> Cohttp_async.Request.t
  -> Cohttp_async.Server.response Deferred.t

type raw_http_handler =
  body:Cohttp_async.Body.t
  -> Socket.Address.Inet.t
  -> Cohttp_async.Request.t
  -> Cohttp_async.Server.response_action Deferred.t

type should_process_request =
  Socket.Address.Inet.t
  -> (Cohttp.Header.t * [ `is_websocket_request of bool ]) Connection_source.t
  -> unit Deferred.Or_error.t

type 'l tcp_server = (Socket.Address.Inet.t, 'l) Tcp.Server.t Deferred.t
type 'l ws_server = (Socket.Address.Inet.t, 'l) Cohttp_async.Server.t Deferred.t

(** There are a few parameters common to the functions below:

    - [http_handler] describes how to handle non-websocket HTTP requests. Defaults to
      always returning code 501, (for servers that are only serving web sockets and no
      other resources via HTTP)

    - [should_process_request] allows the user to deny access for a given request, before
      handling any RPCs, or serving web requests from a client

    - [on_handshake_error] defaults to [`Ignore]. *)

(** This returns a http handler that can be added into an existing cohttp server *)
val handler
  :  ?description:Info.t
  -> implementations:'connection_state Async.Rpc.Implementations.t
  -> initial_connection_state:
       ('connection
        -> Connection_initiated_from.t
        -> Socket.Address.Inet.t
        -> Async.Rpc.Connection.t
        -> 'connection_state)
  -> ?http_handler:('connection -> http_handler) (** [http_handler] *)
  -> ?handshake_timeout:Time_float.Span.t
  -> ?heartbeat_config:Async.Rpc.Connection.Heartbeat_config.t
  -> ?heartbeat_timeout_style:Async.Rpc.Connection.Heartbeat_timeout_style.t
  -> ?should_process_request:should_process_request
  -> ?on_handshake_error:
       [ `Ignore | `Raise | `Call of Socket.Address.Inet.t -> Exn.t -> unit ]
  -> 'connection
     (** The ['connection] argument allows a wrapper for [handler] to pass extra
         information to the [initial_connection_state] and [http_handler] callbacks. *)
  -> raw_http_handler

(** Serves both HTTP/Websockets and regular RPCs via TCP server. *)
val serve_with_tcp_server
  :  where_to_listen_for_tcp:(Socket.Address.Inet.t, 'l) Tcp.Where_to_listen.t
  -> ?max_message_size:int
  -> ?make_transport:Async.Rpc.Connection.transport_maker
  -> where_to_listen:(Socket.Address.Inet.t, 'l) Tcp.Where_to_listen.t
  -> implementations:'s Async.Rpc.Implementations.t
  -> initial_connection_state:
       (unit
        -> Connection_initiated_from.t
        -> Socket.Address.Inet.t
        -> Async.Rpc.Connection.t
        -> 's)
  -> ?http_handler:(unit -> http_handler)
  -> ?handshake_timeout:Time_float.Span.t
  -> ?heartbeat_config:Async.Rpc.Connection.Heartbeat_config.t
  -> ?heartbeat_timeout_style:Async.Rpc.Connection.Heartbeat_timeout_style.t
  -> ?should_process_request:should_process_request
  -> ?on_handshake_error:
       [ `Ignore | `Raise | `Call of Socket.Address.Inet.t -> Exn.t -> unit ]
  -> ?on_handler_error:
       [ `Raise | `Ignore | `Call of Socket.Address.Inet.t -> exn -> unit ]
  -> ?mode:Conduit_async.server
  -> ?backlog:int
  -> ?max_connections:int
  -> unit
  -> 'l tcp_server * 'l ws_server

(** Serves HTTP/Websockets only *)
val serve
  :  where_to_listen:(Socket.Address.Inet.t, 'l) Tcp.Where_to_listen.t
  -> implementations:'s Async.Rpc.Implementations.t
  -> initial_connection_state:
       (unit
        -> Connection_initiated_from.t
        -> Socket.Address.Inet.t
        -> Async.Rpc.Connection.t
        -> 's)
  -> ?http_handler:(unit -> http_handler)
  -> ?handshake_timeout:Time_float.Span.t
  -> ?heartbeat_config:Async.Rpc.Connection.Heartbeat_config.t
  -> ?heartbeat_timeout_style:Async.Rpc.Connection.Heartbeat_timeout_style.t
  -> ?should_process_request:should_process_request
  -> ?on_handshake_error:
       [ `Ignore | `Raise | `Call of Socket.Address.Inet.t -> Exn.t -> unit ]
  -> ?on_handler_error:
       [ `Raise | `Ignore | `Call of Socket.Address.Inet.t -> exn -> unit ]
  -> ?mode:Conduit_async.server
  -> ?backlog:int
  -> ?max_connections:int
  -> unit
  -> 'l ws_server

(** Connect to a Websockets RPC server at the given URI. *)
val client
  :  ?headers:Cohttp.Header.t
  -> ?handshake_timeout:Time_ns.Span.t
  -> ?heartbeat_config:Async.Rpc.Connection.Heartbeat_config.t
  -> ?heartbeat_timeout_style:Async.Rpc.Connection.Heartbeat_timeout_style.t
  -> Uri.t
  -> Async.Rpc.Connection.t Deferred.Or_error.t

module Transport : sig
  (** These functions are similar to the [handler] and [server] function in the parent
      module. The difference is that the caller of these functions is expected to
      construct (and also close) their own [Rpc.Connection.t] from the given transport
      (which will itself be closed after the callback returns).

      This module is analogous to [Async.Rpc.Transport.Tcp]. *)

  type callback =
    Socket.Address.Inet.t
    -> Cohttp.Request.t
    -> Async_rpc_kernel.Async_rpc_kernel_private.Transport.t
    -> unit Deferred.t

  val handler
    :  ?http_handler:('connection -> http_handler) (** [http_handler] *)
    -> ?should_process_request:should_process_request
    -> callback
    -> 'connection
    -> raw_http_handler

  val serve
    :  where_to_listen:(Socket.Address.Inet.t, 'l) Tcp.Where_to_listen.t
    -> ?http_handler:(unit -> http_handler)
    -> ?should_process_request:should_process_request
    -> ?on_handler_error:
         [ `Raise | `Ignore | `Call of Socket.Address.Inet.t -> exn -> unit ]
    -> ?mode:Conduit_async.server
    -> ?backlog:int
    -> ?max_connections:int
    -> callback
    -> 'l ws_server
end
