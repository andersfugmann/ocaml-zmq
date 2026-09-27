open OUnit

let recv_during_eintr recv send =
  let interrupted = ref false in
  let previous_handler =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> interrupted := true))
  in
  let sender =
    Thread.create
      (fun () ->
         Thread.delay 0.2;
         send ())
      ()
  in
  ignore
    (Unix.setitimer Unix.ITIMER_REAL
       { Unix.it_interval = 0.0; it_value = 0.05 });
  let result =
    Fun.protect recv ~finally:(fun () ->
      Thread.join sender;
      ignore
        (Unix.setitimer Unix.ITIMER_REAL
           { Unix.it_interval = 0.0; it_value = 0.0 });
      Sys.set_signal Sys.sigalrm previous_handler)
  in
  assert_bool "receive was not interrupted" !interrupted;
  result

let test = bracket
    (fun () ->
       let ctx = Zmq.Context.create () in
       let receiver = Zmq.Socket.create ctx Zmq.Socket.pair in
       let sender = Zmq.Socket.create ctx Zmq.Socket.pair in
       Zmq.Socket.bind receiver "inproc://eintr";
       Zmq.Socket.connect sender "inproc://eintr";
       (ctx, receiver, sender)
    )
    (fun (_, receiver, sender) ->
       let message =
         recv_during_eintr
           (fun () -> Zmq.Socket.recv receiver)
           (fun () -> Zmq.Socket.send sender "test")
       in
       assert_equal "test" message
    )
    (fun (ctx, receiver, sender) ->
       Zmq.Socket.close sender;
       Zmq.Socket.close receiver;
       Zmq.Context.terminate ctx
    )
