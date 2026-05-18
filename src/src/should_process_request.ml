open! Core
open! Async

type t =
  Socket.Address.Inet.t
  -> (Cohttp.Header.t * [ `is_websocket_request of bool ]) Connection_source.t
  -> unit Deferred.Or_error.t
