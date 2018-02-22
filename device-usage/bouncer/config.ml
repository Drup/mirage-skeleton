open Mirage

(** Keys *)

let dest_ip =
  let doc = Key.Arg.info ~doc:"Destination HTTP IP address." ["ip"] in
  Key.(create "dest_ip" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let dest_port =
  let doc = Key.Arg.info ~doc:"Destination HTTP port." ["port"] in
  Key.(create "dest_port" Arg.(opt int 80 doc))

let local_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." ["listen"] in
  Key.(create "local_port" Arg.(opt int 8080 doc))

let stack = generic_stackv4 default_network

(** Go! *)

let main =
  let keys =
    [ Key.abstract dest_ip; Key.abstract dest_port ; Key.abstract local_port ]
  in
  foreign ~keys "Unikernel.Main" (console @-> stackv4 @-> job)

let () = register "bouncer" [ main $ default_console $ stack ]
