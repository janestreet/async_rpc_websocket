open Core
open Async
open Cohttp_async

let rpc =
  Rpc.Rpc.create
    ~name:"test"
    ~version:0
    ~bin_query:bin_string
    ~bin_response:bin_string
    ~include_in_error_count:Only_on_exn
;;

let implementation : unit Rpc.Implementation.t = Rpc.Rpc.implement' rpc (const Fn.id)

let send_websocket_request port query =
  let%bind (_ : Response.t), websocket =
    let uri = Uri.make ~host:"localhost" ~port () in
    Cohttp_async_websocket.Client.create uri |> Deferred.Or_error.ok_exn
  in
  let transport = Websocket.transport websocket in
  Async_rpc_kernel.Rpc.Connection.with_close
    ~connection_state:(const ())
    transport
    ~dispatch_queries:(fun conn ->
      match query with
      | Some query -> Deferred.Or_error.ignore_m (Rpc.Rpc.dispatch rpc conn query)
      | None -> Deferred.Or_error.return ())
    ~on_handshake_error:`Raise
;;

let create_server implementations =
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations
      ~on_unknown_rpc:`Raise
      ~on_exception:Log_on_background_exn
  in
  Rpc_websocket.Rpc.serve
    ()
    ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    ~implementations
    ~initial_connection_state:(fun _ _ _ _ -> ())
;;

let%bench_fun "establish TCP connection" =
  let port =
    Thread_safe.block_on_async_exn (fun () ->
      let%map server =
        Tcp.Server.create
          ~on_handler_error:`Raise
          Tcp.Where_to_listen.of_port_chosen_by_os
          (fun _ _ _ -> return ())
      in
      Tcp.Server.listening_on server)
  in
  let where_to_connect =
    Host_and_port.create ~host:"localhost" ~port |> Tcp.Where_to_connect.of_host_and_port
  in
  fun () ->
    Thread_safe.block_on_async_exn (fun () ->
      Tcp.with_connection where_to_connect (fun _ _ writer -> Writer.close writer))
;;

let%bench_fun "establish RPC websocket connection" =
  let port =
    Thread_safe.block_on_async_exn (fun () ->
      let%map server = create_server [] in
      Cohttp_async.Server.listening_on server)
  in
  Log.Global.set_output [];
  fun () ->
    Thread_safe.block_on_async_exn (fun () ->
      send_websocket_request port None |> Deferred.Or_error.ok_exn)
;;

let%bench_fun "send RPC" =
  let port =
    Thread_safe.block_on_async_exn (fun () ->
      let%map server = create_server [ implementation ] in
      Cohttp_async.Server.listening_on server)
  in
  Log.Global.set_output [];
  fun () ->
    Thread_safe.block_on_async_exn (fun () ->
      send_websocket_request port (Some "hello there") |> Deferred.Or_error.ok_exn)
;;
