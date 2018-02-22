open Lwt.Infix
open Mirage_types_lwt

let src = Logs.Src.create "bouncer" ~doc:"TCP Bouncer"
module Log = (val Logs.src_log src : Logs.LOG)

module Main
    (Clock: PCLOCK)
    (Stack: STACKV4)
= struct

  module Logs_reporter = Mirage_logs.Make(Clock)

  module TCP = Stack.TCPV4

  let info peer port pp s = Log.info
      (fun f -> f "HTTP [%s:%d] %a" (Ipaddr.V4.to_string peer) port pp s)
  let error peer port pp s = Log.err
      (fun f -> f "HTTP [%s:%d] error: %a" (Ipaddr.V4.to_string peer) port pp s)

  type rd_wr_result = Stop | Continue

  let rec read_write peer port flow1 flow2 =
    let info = info peer port Fmt.string in
    let error pp x = error peer port pp x in
    info "read_write" ;

    let redirect f1 f2 =
      TCP.read f1 >>= function
      | Ok (`Data buf) -> (
          let buflen = Cstruct.len buf in
          info ("read "^(string_of_int buflen)^" bytes\nbuf:\n"
                ^(Cstruct.to_string buf)) ;
          if      buflen = 0 then Lwt.return Continue
          else if buflen < 0 then Lwt.return Stop
          else (* buflen > 0 *) (
            TCP.write f2 buf
            >>= function
            | Ok () ->
              info ("wrote "^(string_of_int buflen)^" bytes") ;
              Lwt.return Continue
            | Error e -> error TCP.pp_write_error e ; Lwt.return Stop
          )
        )
      | Ok `Eof -> info "read eof." ; Lwt.return Stop
      | Error e -> error TCP.pp_error e ; Lwt.return Stop
    in

    Lwt.catch
      (fun () -> redirect flow1 flow2 <?> redirect flow2 flow1)
      (fun exn ->
        info ("rdwr: fail: "^(Printexc.to_string exn)) ;
        Lwt.return Stop
      )
    >>= function
    | Stop -> Lwt.return_unit
    | Continue -> read_write peer port flow1 flow2

  let accept connect_distant flow =
    let peer, port = TCP.dst flow in
    connect_distant () >>= fun outbound ->
    read_write peer port flow outbound
  
  let start clock stack =
    Logs_reporter.(create clock |> run) @@ fun () ->

    let dest = Key_gen.dest_ip () in
    let dest_port = Key_gen.dest_port () in
    Log.info (fun f ->
        f "forwarding to %a:%d/TCP" Ipaddr.V4.pp_hum dest dest_port) ;

    let connect_distant () =
      TCP.create_connection (Stack.tcpv4 stack) (dest, dest_port) >>= function
      | Error _ ->
        Fmt.failwith "outbound connection! [%a]:%d"
          Ipaddr.V4.pp_hum dest dest_port
      | Ok flow -> Lwt.return flow
    in
    
    let port = Key_gen.local_port () in
    Stack.listen_tcpv4 stack ~port (accept connect_distant) ;
    Log.info (fun f ->
        f "listening on port %d/TCP" port) ;

    Stack.listen stack

end
