open! Core
open! Async
open! Import

val rpc : (unit, unit) Rpc.Rpc.t
val implementations : unit Rpc.Implementations.t
val send_websocket_request : port:int -> unit Or_error.t Deferred.t
val do_not_perform_global_logging : unit -> unit
