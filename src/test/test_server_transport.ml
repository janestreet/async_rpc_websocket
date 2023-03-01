open! Core
open! Async
open! Import
open Test_server_utils

let%expect_test "transport server" =
  do_not_perform_global_logging ();
  let%bind web_server =
    Rpc_websocket.Rpc.Transport.serve
      ~on_handler_error:`Raise
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      (fun _inet _request transport ->
         let%bind connection =
           Async_rpc_kernel.Rpc.Connection.create
             ~implementations
             ~connection_state:(fun (_ : Rpc.Connection.t) -> ())
             transport
         in
         match connection with
         | Ok connection -> Rpc.Connection.close_finished connection
         | Error handshake_error -> raise handshake_error)
  in
  let web_port = Cohttp_async.Server.listening_on web_server in
  let%bind () = send_websocket_request ~port:web_port >>| ok_exn in
  [%expect {| "got query" |}];
  return ()
;;
