open! Core
open! Async
open! Import

module Query_and_response = struct
  type t = unit [@@deriving bin_io, sexp]
end

let rpc : (Query_and_response.t, Query_and_response.t) Rpc.Rpc.t =
  Rpc.Rpc.create
    ~name:"test"
    ~version:0
    ~bin_query:Query_and_response.bin_t
    ~bin_response:Query_and_response.bin_t
    ~include_in_error_count:Only_on_exn
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Raise
    ~implementations:
      [ Rpc.Rpc.implement rpc (fun () () ->
          print_s [%message "got query"];
          Deferred.unit)
      ]
    ~on_exception:Log_on_background_exn
;;

let send_websocket_request ~port =
  let%bind (_ : Cohttp_async.Response.t), websocket =
    let uri = Uri.make ~host:"localhost" ~port () in
    Cohttp_async_websocket.Client.create uri |> Deferred.Or_error.ok_exn
  in
  let transport = Websocket.transport websocket in
  Async_rpc_kernel.Rpc.Connection.with_close
    ~connection_state:(const ())
    transport
    ~dispatch_queries:(fun conn ->
      Deferred.Or_error.ignore_m (Rpc.Rpc.dispatch rpc conn ()))
    ~on_handshake_error:`Raise
;;

let do_not_perform_global_logging () = Log.Global.set_output []
