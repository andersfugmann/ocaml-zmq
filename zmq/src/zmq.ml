(* Copyright (c) 2011 Pedro Borges and contributors *)

(** Module Exceptions *)

type error =
| EFSM
| ENOCOMPATPROTO
| ETERM
| EMTHREAD
| EUNKNOWN

exception ZMQ_exception of error * string

external version : unit -> int * int * int = "caml_zmq_version"

let rec retry_on_intr1 f a = try f a with | Unix.Unix_error (Unix.EINTR, _, _) -> retry_on_intr1 f a
let rec retry_on_intr2 f a b = try f a b with | Unix.Unix_error (Unix.EINTR, _, _) -> retry_on_intr2 f a b
let rec retry_on_intr3 f a b c = try f a b c with | Unix.Unix_error (Unix.EINTR, _, _) -> retry_on_intr3 f a b c
let rec retry_on_intr4: ('a -> 'b -> 'c -> 'd -> 'ret) -> 'a -> 'b -> 'c -> 'd -> 'ret = fun f a b c d ->
  try f a b c d with | Unix.Unix_error (Unix.EINTR, _, _) -> retry_on_intr4 f a b c d


module Context = struct
  type t

  external create_native : unit -> t = "caml_zmq_new"
  let create = retry_on_intr1 create_native

  external terminate_native : t -> unit = "caml_zmq_term"
  let terminate = retry_on_intr1 terminate_native

  type int_option =
  | ZMQ_IO_THREADS
  | ZMQ_MAX_SOCKETS
  | ZMQ_IPV6

  external set_int_option_native : t -> int_option -> int -> unit = "caml_zmq_ctx_set_int_option"
  let set_int_option = retry_on_intr3 set_int_option_native

  external get_int_option_native : t -> int_option -> int = "caml_zmq_ctx_get_int_option"
  let get_int_option = retry_on_intr2 get_int_option_native

  let get_io_threads ctx =
    get_int_option ctx ZMQ_IO_THREADS

  let set_io_threads ctx =
    set_int_option ctx ZMQ_IO_THREADS

  let get_max_sockets ctx =
    get_int_option ctx ZMQ_MAX_SOCKETS

  let set_max_sockets ctx =
    set_int_option ctx ZMQ_MAX_SOCKETS

  let get_ipv6 ctx =
    (get_int_option ctx ZMQ_IPV6) == 1

  let set_ipv6 ctx has_ipv6 =
    set_int_option ctx ZMQ_IPV6 (if has_ipv6 then 1 else 0)

end

module Msg = struct
  open Bigarray

  type t
  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  external init_data_native : bigstring -> int -> int -> t = "caml_zmq_msg_init_data"

  let init_data ?(offset = 0) ?length buf =
    let length =
      let max_possible = Array1.dim buf - offset in
      match length with
      | Some l -> min l max_possible
      | None -> max_possible
    in
    retry_on_intr3 init_data_native buf offset length

  external size_native : t -> int = "caml_zmq_msg_size"
  let size = retry_on_intr1 size_native

  external unsafe_data_native : t -> bigstring = "caml_zmq_msg_data"
  let unsafe_data = retry_on_intr1 unsafe_data_native

  let copy_data msg =
    let data = unsafe_data msg in
    let copy = Array1.create char c_layout (Array1.dim data) in
    Array1.blit data copy;
    copy

  external close_native : t -> unit = "caml_zmq_msg_close"
  let close = retry_on_intr1 close_native

  external gets_native : t -> string -> string = "caml_zmq_msg_gets"
  let gets = retry_on_intr2 gets_native
end

module Socket = struct

  type + 'a t


  (** This is an int so we know which socket we
    * are building inside the external functions *)

  type 'a kind = int

  let pair   = 0
  let pub    = 1
  let sub    = 2
  let req    = 3
  let rep    = 4
  let dealer = 5
  let router = 6
  let pull   = 7
  let push   = 8
  let xpub   = 9
  let xsub   = 10
  let stream = 11

  (** Creation and Destruction *)
  external create_native : Context.t -> 'a kind -> 'a t = "caml_zmq_socket"
  let create = retry_on_intr2 create_native

  external close_native : 'a t -> unit = "caml_zmq_close"
  let close socket = retry_on_intr1 close_native socket


  (** Wiring *)
  external connect_native : 'a t -> string -> unit = "caml_zmq_connect"
  let connect socket = retry_on_intr2 connect_native socket

  external disconnect_native : 'a t -> string -> unit = "caml_zmq_disconnect"
  let disconnect socket = retry_on_intr2 disconnect_native socket

  external bind_native : 'a t -> string -> unit = "caml_zmq_bind"
  let bind socket = retry_on_intr2 bind_native socket

  external unbind_native : 'a t -> string -> unit = "caml_zmq_unbind"
  let unbind socket = retry_on_intr2 unbind_native socket


  (** Send and Receive *)
  external recv_native : 'a t -> bool -> string = "caml_zmq_recv"
  let recv ?(block = true) socket = retry_on_intr2 recv_native socket block

  external send_native : 'a t -> string -> bool -> bool -> unit = "caml_zmq_send"
  let send ?(block = true) ?(more = false) socket message =
    retry_on_intr4 send_native socket message block more

  external recv_msg_native : 'a t -> bool -> Msg.t = "caml_zmq_recv_msg"
  let recv_msg ?(block = true) socket =
    retry_on_intr2 recv_msg_native socket block

  external send_msg_native : 'a t -> Msg.t -> bool -> bool -> unit = "caml_zmq_send_msg"
  let send_msg ?(block = true) ?(more = false) socket message =
    retry_on_intr4 send_msg_native socket message block more

  type int64_option =
  | ZMQ_AFFINITY
  | ZMQ_MAXMSGSIZE

  external set_int64_option_native : 'a t -> int64_option -> int -> unit = "caml_zmq_set_int64_option"
  let set_int64_option socket = retry_on_intr3 set_int64_option_native socket

  external get_int64_option_native : 'a t -> int64_option -> int = "caml_zmq_get_int64_option"
  let get_int64_option socket = retry_on_intr2 get_int64_option_native socket


  type string_option =
  | ZMQ_IDENTITY
  | ZMQ_SUBSCRIBE
  | ZMQ_UNSUBSCRIBE
  | ZMQ_LAST_ENDPOINT
  | ZMQ_TCP_ACCEPT_FILTER
  | ZMQ_PLAIN_USERNAME
  | ZMQ_PLAIN_PASSWORD
  | ZMQ_CURVE_PUBLICKEY
  | ZMQ_CURVE_SECRETKEY
  | ZMQ_CURVE_SERVERKEY
  | ZMQ_ZAP_DOMAIN

  external set_string_option_native : 'a t -> string_option -> string -> unit = "caml_zmq_set_string_option"
  let set_string_option socket = retry_on_intr3 set_string_option_native socket

  external get_string_option_native : 'a t -> string_option -> int -> string = "caml_zmq_get_string_option"
  let get_string_option socket = retry_on_intr3 get_string_option_native socket


  [@@@warning "-37"]
  type int_option =
  | ZMQ_RATE
  | ZMQ_RECOVERY_IVL
  | ZMQ_SNDBUF
  | ZMQ_RCVBUF
  | ZMQ_RCVMORE
  | ZMQ_EVENTS
  | ZMQ_TYPE
  | ZMQ_LINGER
  | ZMQ_RECONNECT_IVL
  | ZMQ_BACKLOG
  | ZMQ_RECONNECT_IVL_MAX
  | ZMQ_SNDHWM
  | ZMQ_RCVHWM
  | ZMQ_MULTICAST_HOPS
  | ZMQ_RCVTIMEO
  | ZMQ_SNDTIMEO
  | ZMQ_IPV6
  | ZMQ_ROUTER_MANDATORY
  | ZMQ_TCP_KEEPALIVE
  | ZMQ_TCP_KEEPALIVE_CNT
  | ZMQ_TCP_KEEPALIVE_IDLE
  | ZMQ_TCP_KEEPALIVE_INTVL
  | ZMQ_IMMEDIATE
  | ZMQ_XPUB_VERBOSE
  | ZMQ_MECHANISM
  | ZMQ_PLAIN_SERVER
  | ZMQ_CURVE_SERVER
  | ZMQ_PROBE_ROUTER
  | ZMQ_REQ_CORRELATE
  | ZMQ_REQ_RELAXED
  | ZMQ_CONFLATE
  | ZMQ_STREAM_NOTIFY
  [@@@warning "+37"]

  external set_int_option_native : 'a t -> int_option -> int -> unit = "caml_zmq_set_int_option"
  let set_int_option socket = retry_on_intr3 set_int_option_native socket

  external get_int_option_native : 'a t -> int_option -> int = "caml_zmq_get_int_option"
  let get_int_option socket = retry_on_intr2 get_int_option_native socket

  let validate_string_length min max str msg =
    match String.length str with
    | n when n < min -> invalid_arg msg
    | n when n > max -> invalid_arg msg
    | _ -> ()

  let set_max_message_size socket size =
    set_int64_option socket ZMQ_MAXMSGSIZE size

  let get_max_message_size socket =
    get_int64_option socket ZMQ_MAXMSGSIZE

  let set_affinity socket size =
    set_int64_option socket ZMQ_AFFINITY size

  let get_affinity socket =
    get_int64_option socket ZMQ_AFFINITY

  let set_identity socket identity =
    validate_string_length 1 255 identity "set_identity";
    set_string_option socket ZMQ_IDENTITY identity

  let maximal_buffer_length = 255
  let curve_z85_buffer_length = 41

  let get_identity socket =
    get_string_option socket ZMQ_IDENTITY maximal_buffer_length

  let subscribe socket topic =
    set_string_option socket ZMQ_SUBSCRIBE topic

  let unsubscribe socket topic =
    set_string_option socket ZMQ_UNSUBSCRIBE topic

  let get_last_endpoint socket =
    get_string_option socket ZMQ_LAST_ENDPOINT maximal_buffer_length

  let set_tcp_accept_filter socket filter =
    set_string_option socket ZMQ_TCP_ACCEPT_FILTER filter

  let set_rate socket rate =
    set_int_option socket ZMQ_RATE rate

  let get_rate socket =
    get_int_option socket ZMQ_RATE

  let set_recovery_interval socket interval =
    set_int_option socket ZMQ_RECOVERY_IVL interval

  let get_recovery_interval socket =
    get_int_option socket ZMQ_RECOVERY_IVL

  let set_send_buffer_size socket size =
    set_int_option socket ZMQ_SNDBUF size

  let get_send_buffer_size socket =
    get_int_option socket ZMQ_SNDBUF

  let set_receive_buffer_size socket size =
    set_int_option socket ZMQ_RCVBUF size

  let get_receive_buffer_size socket =
    get_int_option socket ZMQ_RCVBUF

  let has_more socket =
    get_int_option socket ZMQ_RCVMORE != 0

  let set_linger_period socket period =
    set_int_option socket ZMQ_LINGER period

  let get_linger_period socket =
    get_int_option socket ZMQ_LINGER

  let set_reconnect_interval socket interval =
    set_int_option socket ZMQ_RECONNECT_IVL interval

  let get_reconnect_interval socket =
    get_int_option socket ZMQ_RECONNECT_IVL

  let set_connection_backlog socket backlog =
    set_int_option socket ZMQ_BACKLOG backlog

  let get_connection_backlog socket =
    get_int_option socket ZMQ_BACKLOG

  let set_reconnect_interval_max socket interval =
    set_int_option socket ZMQ_RECONNECT_IVL_MAX interval

  let get_reconnect_interval_max socket =
    get_int_option socket ZMQ_RECONNECT_IVL_MAX

  let set_send_high_water_mark socket mark =
    set_int_option socket ZMQ_SNDHWM mark

  let get_send_high_water_mark socket =
    get_int_option socket ZMQ_SNDHWM

  let set_receive_high_water_mark socket mark =
    set_int_option socket ZMQ_RCVHWM mark

  let get_receive_high_water_mark socket =
    get_int_option socket ZMQ_RCVHWM

  let set_multicast_hops socket hops =
    set_int_option socket ZMQ_MULTICAST_HOPS hops

  let get_multicast_hops socket =
    get_int_option socket ZMQ_MULTICAST_HOPS

  let set_receive_timeout socket timeout =
    set_int_option socket ZMQ_RCVTIMEO timeout

  let get_receive_timeout socket =
    get_int_option socket ZMQ_RCVTIMEO

  let set_send_timeout socket timeout =
    set_int_option socket ZMQ_SNDTIMEO timeout

  let get_send_timeout socket =
    get_int_option socket ZMQ_SNDTIMEO

  let set_ipv6 socket flag =
    let value = match flag with true -> 1 | false -> 0 in
    set_int_option socket ZMQ_IPV6 value

  let get_ipv6 socket =
    match get_int_option socket ZMQ_IPV6 with
    | 0 -> false
    | _ -> true

  let set_router_mandatory socket flag =
    let value = match flag with true -> 1 | false -> 0 in
    set_int_option socket ZMQ_ROUTER_MANDATORY value

  let get_router_mandatory socket =
    match get_int_option socket ZMQ_ROUTER_MANDATORY with
    | 0 -> false
    | _ -> true

  let set_tcp_keepalive socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value false -> 0
      | `Value true -> 1
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE value

  let get_tcp_keepalive socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE with
    | -1 -> `Default
    | 0 -> `Value false
    | _ -> `Value true

  let set_tcp_keepalive_idle socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value n when n <= 0 -> invalid_arg "set_tcp_keepalive_idle"
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_IDLE value

  let get_tcp_keepalive_idle socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_IDLE with
    | -1 -> `Default
    | n when n <= 0 -> assert false
    | n -> `Value n

  let set_tcp_keepalive_interval socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value n when n <= 0 -> invalid_arg "set_tcp_keepalive_interval"
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_INTVL value

  let get_tcp_keepalive_interval socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_INTVL with
    | -1 -> `Default
    | n when n <= 0 -> assert false
    | n -> `Value n

  let set_tcp_keepalive_count socket flag =
    let value = match flag with
      | `Default -> -1
      | `Value n when n <= 0 -> invalid_arg "set_tcp_keepalive_count"
      | `Value n -> n
    in
    set_int_option socket ZMQ_TCP_KEEPALIVE_CNT value

  let get_tcp_keepalive_count socket =
    match get_int_option socket ZMQ_TCP_KEEPALIVE_CNT with
    | -1 -> `Default
    | n when n <= 0 -> assert false
    | n -> `Value n

  let set_immediate socket flag =
    let value = match flag with
      | true -> 1
      | false -> 0
    in
    set_int_option socket ZMQ_IMMEDIATE value

  let get_immediate socket =
    match get_int_option socket ZMQ_IMMEDIATE with
    | 0 -> false
    | _ -> true

  let set_xpub_verbose socket flag =
    let value = match flag with
      | true -> 1
      | false -> 0
    in
    set_int_option socket ZMQ_XPUB_VERBOSE value

  let set_probe_router socket flag =
    set_int_option socket ZMQ_PROBE_ROUTER (if flag then 1 else 0)

  let set_req_correlate socket flag =
    set_int_option socket ZMQ_REQ_CORRELATE (if flag then 1 else 0)

  let set_req_relaxed socket flag =
    set_int_option socket ZMQ_REQ_RELAXED (if flag then 1 else 0)

  let set_plain_server socket flag =
    set_int_option socket ZMQ_PLAIN_SERVER (if flag then 1 else 0)

  let set_curve_server socket flag =
    set_int_option socket ZMQ_CURVE_SERVER (if flag then 1 else 0)

  let set_plain_username socket =
    set_string_option socket ZMQ_PLAIN_USERNAME

  let get_plain_username socket =
    get_string_option socket ZMQ_PLAIN_USERNAME maximal_buffer_length

  let set_plain_password socket =
    set_string_option socket ZMQ_PLAIN_PASSWORD

  let get_plain_password socket =
    get_string_option socket ZMQ_PLAIN_PASSWORD maximal_buffer_length

  let validate_curve_key_length str msg =
    match String.length str with
    | 32 | 40 -> ()
    | _ -> invalid_arg msg

  let get_curve_publickey socket =
    get_string_option socket ZMQ_CURVE_PUBLICKEY curve_z85_buffer_length

  let set_curve_publickey socket str =
    validate_curve_key_length str "set_curve_publickey";
    set_string_option socket ZMQ_CURVE_PUBLICKEY str

  let get_curve_secretkey socket =
    get_string_option socket ZMQ_CURVE_SECRETKEY curve_z85_buffer_length

  let set_curve_secretkey socket str =
    validate_curve_key_length str "set_curve_secretkey";
    set_string_option socket ZMQ_CURVE_SECRETKEY str

  let get_curve_serverkey socket =
    get_string_option socket ZMQ_CURVE_SERVERKEY curve_z85_buffer_length

  let set_curve_serverkey socket str =
    validate_curve_key_length str "set_curve_serverkey";
    set_string_option socket ZMQ_CURVE_SERVERKEY str

  let get_mechanism socket =
    match get_int_option socket ZMQ_MECHANISM with
    | 0 -> `Null
    | 1 -> `Plain
    | 2 -> `Curve
    | _ -> assert false

  let set_zap_domain socket =
    set_string_option socket ZMQ_ZAP_DOMAIN

  let get_zap_domain socket =
    get_string_option socket ZMQ_ZAP_DOMAIN maximal_buffer_length

  let set_conflate socket flag =
    set_int_option socket ZMQ_CONFLATE (if flag then 1 else 0)

  let set_stream_notify socket stream_notify_flag =
    set_int_option socket ZMQ_STREAM_NOTIFY (if stream_notify_flag then 1 else 0)

  external get_fd_native : 'a t -> Unix.file_descr = "caml_zmq_get_fd"
  let get_fd socket = retry_on_intr1 get_fd_native socket

  type event = No_event | Poll_in | Poll_out | Poll_in_out | Poll_error

  external events_native : 'a t -> event = "caml_zmq_get_events"
  let events socket = retry_on_intr1 events_native socket

  (** Wrap recv all.
      The ZMQ documentation states that multipart messages are received atomicly. So
      reading following message parts must not block.
      This function will read set [~block:true] for following message parts to ensure that
      ZMQ will not return EAGAIN while receiving a multipart message.
  *)
  let recv_all_wrapper (f : ?block:bool -> 'a t -> 'b) =
    let rec loop acc ?block socket =
      let acc = f ?block socket :: acc in
      match has_more socket with
      | true -> loop acc ~block:true socket
      | false -> List.rev acc
    in
    fun ?block socket -> loop [] ?block socket

  (** ZMQ documentation says that sending multipart messages should be atomic.
      So, we assume that once the socket can send, all message-parts can be sent
      Therefore, all subsequent message parts are sent with [~block:true] to avoid the function raising EAGAIN,
      as its not possible to handle that gracefully - and may put the socket in a broken state.
  *)
  let send_all_wrapper (f : ?block:bool -> ?more:bool -> _ t -> _ -> unit) =
    (* Once the first message part is sent all remaining message parts can
       be sent without blocking. *)
    let rec send_all_inner_loop ?block socket =
      function
      | [] -> ()
      | hd :: [] ->
        f ?block ~more:false socket hd
      | hd :: tl ->
        f ?block ~more:true socket hd;
        send_all_inner_loop ~block:true socket tl
    in
    send_all_inner_loop

  let recv_all ?block socket =
    recv_all_wrapper recv ?block socket

  let send_all ?block socket message =
    send_all_wrapper send ?block socket message

  let recv_msg_all ?block socket =
    recv_all_wrapper recv_msg ?block socket

  let send_msg_all ?block socket message =
    send_all_wrapper send_msg ?block socket message
end

module Proxy = struct
  external zmq_proxy2_native : 'a Socket.t -> 'b Socket.t -> unit = "caml_zmq_proxy2"
  let zmq_proxy2 socket = retry_on_intr2 zmq_proxy2_native socket

  external zmq_proxy3_native : 'a Socket.t -> 'b Socket.t -> 'c Socket.t -> unit = "caml_zmq_proxy3"
  let zmq_proxy3 socket = retry_on_intr3 zmq_proxy3_native socket


  let create ?capture frontend backend =
    match capture with
    | Some capture -> zmq_proxy3 frontend backend capture
    | None -> zmq_proxy2 frontend backend

end

module Poll = struct

  type t

  type poll_event = In | Out | In_out
  type 'a poll_mask = ('a Socket.t * poll_event)

  let mask_in_out t =
    (t:>
       [`Pair|`Pub|`Sub|`Req|`Rep|`Dealer|`Router|`Pull|`Push|`Xsub|`Xpub|`Stream]
         Socket.t
    ), In_out

  let mask_in t =
    (t:>
       [`Pair|`Pub|`Sub|`Req|`Rep|`Dealer|`Router|`Pull|`Push|`Xsub|`Xpub|`Stream]
         Socket.t
    ), In

  let mask_out t =
    (t:>
       [`Pair|`Pub|`Sub|`Req|`Rep|`Dealer|`Router|`Pull|`Push|`Xsub|`Xpub|`Stream]
         Socket.t
    ), Out

  external mask_of_native : 'a poll_mask array -> t = "caml_zmq_poll_of_pollitem_array"
  let mask_of mask = retry_on_intr1 mask_of_native mask

  external of_masks_native : 'a poll_mask array -> t = "caml_zmq_poll_of_pollitem_array"
  let of_masks mask = retry_on_intr1 of_masks_native mask

  external poll_native: t -> int -> poll_event option array = "caml_zmq_poll"
  let poll ?(timeout = -1) items =
    poll_native items timeout

end

module Monitor = struct
  type t = string

  type address = string
  type error_no = int
  type error_text = string

  type event =
  | Connected of address * Unix.file_descr
  | Connect_delayed of address
  | Connect_retried of address * int (*interval*)
  | Listening of address * Unix.file_descr
  | Bind_failed of address * error_no * error_text
  | Accepted of address * Unix.file_descr
  | Accept_failed of address * error_no * error_text
  | Closed of address * Unix.file_descr
  | Close_failed of address * error_no * error_text
  | Disconnected of address * Unix.file_descr
  | Monitor_stopped of address
  | Handshake_failed_no_detail of address
  | Handshake_succeeded of address
  | Handshake_failed_protocol of address * int
  | Handshake_failed_auth of address * int

  external socket_monitor_native : 'a Socket.t -> string -> unit = "caml_zmq_socket_monitor"
  let socket_monitor socket = retry_on_intr2 socket_monitor_native socket

  let create socket =
    (* Construct an anonymous inproc channel name *)
    let socket_id = Hashtbl.hash (Socket.get_fd socket) in
    let address = Printf.sprintf "inproc://_socket_monitor-%d-%x.%x"
      (Unix.getpid ())
      socket_id
      (Random.bits ())
    in
    socket_monitor socket address;
    address

  let connect ctx t =
    let s = Socket.create ctx Socket.pair in
    Socket.connect s t;
    s

  external decode_monitor_event_native : string -> string -> event = "caml_decode_monitor_event"
  let decode_monitor_event = retry_on_intr2 decode_monitor_event_native

  let recv ?block socket =
    match Socket.recv_all ?block socket with
    | [event; addr] -> decode_monitor_event event addr
    | _ -> assert false

  let get_peer_address fd =
    try
      let sockaddr = Unix.getpeername fd in
      let domain = match Unix.domain_of_sockaddr sockaddr with
        | Unix.PF_UNIX -> "unix"
        | Unix.PF_INET -> "tcp"
        | Unix.PF_INET6 -> "tcp6"
      in
      match sockaddr with
      | Unix.ADDR_UNIX s -> Printf.sprintf "%s://%s" domain s;
      | Unix.ADDR_INET (addr, port) -> Printf.sprintf "%s://%s:%d" domain (Unix.string_of_inet_addr addr) port
    with
    | Unix.Unix_error _ -> "unknown"

  let internal_string_of_event push_address pop_address = function
    | Connected (addr, fd) -> Printf.sprintf "Connect: %s. peer %s" addr (push_address fd)
    | Connect_delayed addr -> Printf.sprintf "Connect delayed: %s" addr
    | Connect_retried (addr, interval) -> Printf.sprintf "Connect retried: %s - %d" addr interval
    | Listening (addr, fd) -> Printf.sprintf "Listening: %s - peer %s" addr (push_address fd)
    | Bind_failed (addr, error_no, error_text) -> Printf.sprintf "Bind failed: %s. %d:%s" addr error_no error_text
    | Accepted (addr, fd) -> Printf.sprintf "Accepted: %s. peer %s" addr (push_address fd)
    | Accept_failed (addr, error_no, error_text) -> Printf.sprintf "Accept failed: %s. %d:%s" addr error_no error_text
    | Closed (addr, fd) -> Printf.sprintf "Closed: %s. peer %s" addr (pop_address fd)
    | Close_failed (addr, error_no, error_text) -> Printf.sprintf "Close failed: %s. %d:%s" addr error_no error_text
    | Disconnected (addr, fd) -> Printf.sprintf "Disconnect: %s. peer %s" addr (pop_address fd)
    | Monitor_stopped addr -> Printf.sprintf "Monitor_stopped: %s" addr
    | Handshake_failed_no_detail addr -> Printf.sprintf "Handshake_failed_no_detail: %s" addr
    | Handshake_succeeded addr -> Printf.sprintf "Handshake_succeeded: %s" addr
    | Handshake_failed_protocol (addr, code) -> Printf.sprintf "Handshake_failed_protocol: %s - %d" addr code
    | Handshake_failed_auth (addr, code) -> Printf.sprintf "Handshake_failed_auth: %s - %d" addr code

  let string_of_event event = internal_string_of_event get_peer_address get_peer_address event

  let mk_string_of_event () =
    let state = ref [] in

    let pop_address fd =
      let rec pop acc = function
        | [] -> (get_peer_address fd, acc)
        | (fd', address) :: xs when fd' = fd -> (address, acc @ xs)
        | x :: xs -> pop (x :: acc) xs
      in
      let (address, new_state) = pop [] !state in
      state := new_state;
      address
    in

    let push_address fd =
      let address = get_peer_address fd in
      state := (fd, address) :: !state;
      address
    in
    internal_string_of_event push_address pop_address

end

module Z85 = struct
  external encode_native : string -> string = "caml_z85_encode"
  let encode = retry_on_intr1 encode_native

  external decode_native : string -> string = "caml_z85_decode"
  let decode = retry_on_intr1 decode_native

end

module Curve = struct
  external keypair_native : unit -> string * string = "caml_curve_keypair"
  let keypair = retry_on_intr1 keypair_native
end

(* The following code is called by fail.c *)

[@@@warning "-37"]
type internal_error =
(* zmq.h defines the following Unix error codes if they are not already defined
 * by the system headers *)
| I_ENOTSUP
| I_EPROTONOSUPPORT
| I_ENOBUFS
| I_ENETDOWN
| I_EADDRINUSE
| I_EADDRNOTAVAIL
| I_ECONNREFUSED
| I_EINPROGRESS
| I_ENOTSOCK
| I_EMSGSIZE
| I_EAFNOSUPPORT
| I_ENETUNREACH
| I_ECONNABORTED
| I_ECONNRESET
| I_ENOTCONN
| I_ETIMEDOUT
| I_EHOSTUNREACH
| I_ENETRESET
(* The following error codes are ZMQ-specific *)
| I_EFSM
| I_ENOCOMPATPROTO
| I_ETERM
| I_EMTHREAD
| I_EUNKNOWN
[@@@warning "+37"]

(* All Unix-type errors are mapped to their corresponding constructor in
 * Unix -- except I_ENOTSUP, which is mapped to EOPNOTSUPP ("Operation not
 * supported on socket") since there is no Unix.ENOTSUP.
 * ZMQ-specific errors are mapped to the constructors of Zmq.error. *)
let zmq_raise e str func_name =
  let exn = match e with
  | I_ENOTSUP         -> Unix.(Unix_error (EOPNOTSUPP     , func_name, ""))
  | I_EPROTONOSUPPORT -> Unix.(Unix_error (EPROTONOSUPPORT, func_name, ""))
  | I_ENOBUFS         -> Unix.(Unix_error (ENOBUFS        , func_name, ""))
  | I_ENETDOWN        -> Unix.(Unix_error (ENETDOWN       , func_name, ""))
  | I_EADDRINUSE      -> Unix.(Unix_error (EADDRINUSE     , func_name, ""))
  | I_EADDRNOTAVAIL   -> Unix.(Unix_error (EADDRNOTAVAIL  , func_name, ""))
  | I_ECONNREFUSED    -> Unix.(Unix_error (ECONNREFUSED   , func_name, ""))
  | I_EINPROGRESS     -> Unix.(Unix_error (EINPROGRESS    , func_name, ""))
  | I_ENOTSOCK        -> Unix.(Unix_error (ENOTSOCK       , func_name, ""))
  | I_EMSGSIZE        -> Unix.(Unix_error (EMSGSIZE       , func_name, ""))
  | I_EAFNOSUPPORT    -> Unix.(Unix_error (EAFNOSUPPORT   , func_name, ""))
  | I_ENETUNREACH     -> Unix.(Unix_error (ENETUNREACH    , func_name, ""))
  | I_ECONNABORTED    -> Unix.(Unix_error (ECONNABORTED   , func_name, ""))
  | I_ECONNRESET      -> Unix.(Unix_error (ECONNRESET     , func_name, ""))
  | I_ENOTCONN        -> Unix.(Unix_error (ENOTCONN       , func_name, ""))
  | I_ETIMEDOUT       -> Unix.(Unix_error (ETIMEDOUT      , func_name, ""))
  | I_EHOSTUNREACH    -> Unix.(Unix_error (EHOSTUNREACH   , func_name, ""))
  | I_ENETRESET       -> Unix.(Unix_error (ENETRESET      , func_name, ""))
  | I_EFSM            -> ZMQ_exception (EFSM          , str)
  | I_ENOCOMPATPROTO  -> ZMQ_exception (ENOCOMPATPROTO, str)
  | I_ETERM           -> ZMQ_exception (ETERM         , str)
  | I_EMTHREAD        -> ZMQ_exception (EMTHREAD      , str)
  | I_EUNKNOWN        -> ZMQ_exception (EUNKNOWN      , str)
  in

  raise exn


let () = Callback.register "Zmq.zmq_raise" zmq_raise
