open Core
open Async

module Connection_source = struct
  type 'a t =
    | Web of 'a
    | Plain_tcp
  [@@deriving sexp_of]
end

module Connection_initiated_from = struct
  type t =
    | Websocket_request of Cohttp.Request.t
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

let default_http_handler _ ~body:_ _ _ = Cohttp_async.Server.respond (`Code 501)
let always_provide_rpc_shapes = true

let handler_common
  ?should_process_request
  ?(http_handler = default_http_handler)
  extra_info
  f
  =
  let should_process_request =
    Option.map
      should_process_request
      ~f:(fun should_process_request inet header ~is_websocket_request ->
        should_process_request
          inet
          (Connection_source.Web (header, `is_websocket_request is_websocket_request)))
  in
  Cohttp_async_websocket.Server.(
    create
      ~opcode:`Binary
      ~non_ws_request:(http_handler extra_info)
      ?should_process_request
      (fun ~inet ~subprotocol request ->
         return
           (On_connection.create (fun websocket ->
              let transport = Websocket.transport websocket in
              let%bind () = f ~inet ~subprotocol request transport in
              Rpc.Transport.close transport))))
;;

let handler
  ?(description = Info.of_string "HTTP (WS) server")
  ~implementations
  ~initial_connection_state
  ?http_handler
  ?handshake_timeout
  ?heartbeat_config
  ?heartbeat_timeout_style
  ?should_process_request
  ?(on_handshake_error = `Ignore)
  extra_info
  =
  let ws_handler ~inet ~subprotocol:(_ : string option) request transport =
    let connection_state =
      initial_connection_state
        extra_info
        (Connection_initiated_from.Websocket_request request)
        inet
    in
    let%bind connection =
      Async_rpc_kernel.Rpc.Connection.create
        ?handshake_timeout:
          (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
        ?heartbeat_config
        ?heartbeat_timeout_style
        ~implementations
        ~description
        ~connection_state
        ~provide_rpc_shapes:always_provide_rpc_shapes
        transport
    in
    match connection with
    | Ok connection -> Rpc.Connection.close_finished connection
    | Error handshake_error ->
      (match on_handshake_error with
       | `Ignore -> ()
       | `Raise -> raise handshake_error
       | `Call func -> func inet handshake_error);
      return ()
  in
  handler_common ?should_process_request ?http_handler extra_info ws_handler
;;

let serve
  ~where_to_listen
  ~implementations
  ~initial_connection_state
  ?http_handler
  ?handshake_timeout
  ?heartbeat_config
  ?heartbeat_timeout_style
  ?should_process_request
  ?on_handshake_error
  ?(on_handler_error = `Ignore)
  ?mode
  ?backlog
  ?max_connections
  ()
  =
  let description =
    let info =
      match mode with
      | None | Some `TCP -> "HTTP (WS) server"
      | Some (`OpenSSL _) | Some (`OpenSSL_with_trust_chain _) -> "HTTPS (WSS) server"
    in
    Info.of_string info
  in
  let handler =
    handler
      ~description
      ~implementations
      ~initial_connection_state
      ?http_handler
      ?handshake_timeout
      ?heartbeat_config
      ?heartbeat_timeout_style
      ?should_process_request
      ?on_handshake_error
      ()
  in
  Cohttp_async.Server.create_expert
    ?max_connections
    ?backlog
    ~on_handler_error
    ?mode
    where_to_listen
    handler
;;

let serve_with_tcp_server
  ~where_to_listen_for_tcp
  ?max_message_size
  ?make_transport
  ~where_to_listen
  ~implementations
  ~initial_connection_state
  ?http_handler
  ?handshake_timeout
  ?heartbeat_config
  ?heartbeat_timeout_style
  ?should_process_request
  ?on_handshake_error
  ?on_handler_error
  ?mode
  ?backlog
  ?max_connections
  ()
  =
  let ws_server =
    serve
      ()
      ~implementations
      ~initial_connection_state
      ~where_to_listen
      ?http_handler
      ?handshake_timeout
      ?heartbeat_config
      ?heartbeat_timeout_style
      ?should_process_request
      ?on_handshake_error
      ?on_handler_error
      ?mode
      ?backlog
      ?max_connections
  in
  let initial_connection_state addr conn =
    initial_connection_state () Connection_initiated_from.Tcp addr conn
  in
  let auth_opt_to_tcp_auth auth_opt =
    Option.map auth_opt ~f:(fun should_process_request inet ->
      let%bind result = should_process_request inet Connection_source.Plain_tcp in
      Or_error.is_ok result |> Deferred.return)
  in
  let tcp_server =
    Rpc.Connection.serve
      ()
      ~implementations
      ~initial_connection_state
      ~where_to_listen:where_to_listen_for_tcp
      ~provide_rpc_shapes:always_provide_rpc_shapes
      ?max_connections
      ?backlog
      ?max_message_size
      ?make_transport
      ?handshake_timeout
      ?heartbeat_config
      ?heartbeat_timeout_style
      ?auth:(auth_opt_to_tcp_auth should_process_request)
  in
  tcp_server, ws_server
;;

let connection_create
  ?handshake_timeout
  ?heartbeat_config
  ?heartbeat_timeout_style
  transport
  =
  Async_rpc_kernel.Rpc.Connection.create
    ~connection_state:(fun _ -> ())
    ~provide_rpc_shapes:always_provide_rpc_shapes
      (* No-op right now since there are no client implementations but we want to send rpc
         shapes in the future if there are. *)
    ?handshake_timeout
    ?heartbeat_config
    ?heartbeat_timeout_style
    transport
  >>| Or_error.of_exn_result
;;

let client ?headers ?handshake_timeout ?heartbeat_config ?heartbeat_timeout_style uri =
  let open Deferred.Or_error.Let_syntax in
  let%bind _resp, websocket = Cohttp_async_websocket.Client.create ?headers uri in
  let transport = Websocket.transport websocket in
  connection_create
    ?handshake_timeout
    ?heartbeat_config
    ?heartbeat_timeout_style
    transport
;;

module Transport = struct
  type callback =
    Socket.Address.Inet.t
    -> Cohttp.Request.t
    -> Async_rpc_kernel.Async_rpc_kernel_private.Transport.t
    -> unit Deferred.t

  let handler ?http_handler ?should_process_request callback extra_info =
    let ws_handler ~inet ~subprotocol:(_ : string option) request transport =
      callback inet request transport
    in
    handler_common ?http_handler ?should_process_request extra_info ws_handler
  ;;

  let serve
    ~where_to_listen
    ?http_handler
    ?should_process_request
    ?(on_handler_error = `Ignore)
    ?mode
    ?backlog
    ?max_connections
    callback
    =
    let handler = handler ?http_handler ?should_process_request callback () in
    Cohttp_async.Server.create_expert
      ?max_connections
      ?backlog
      ~on_handler_error
      ?mode
      where_to_listen
      handler
  ;;
end
