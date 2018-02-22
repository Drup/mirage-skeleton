open Lwt.Infix
open Mirage_types_lwt

module Main (C: CONSOLE) (Stack : STACKV4) = struct

  module TCP = Stack.TCPV4
  
  let log c peer port fmt =
    Fmt.kstrf (C.log c) ("HTTP [%a:%d] " ^^ fmt ^^ "@.")
      Ipaddr.V4.pp_hum peer port

  type rd_wr_result = Stop | Continue

  let rec read_write c outflow inflow =
    let peer, port = TCP.dst inflow in 
    let log fmt = log c peer port fmt in
    let redirect f1 f2 =
      TCP.read f1 >>= function
      | Ok (`Data buf) -> (
          let buflen = Cstruct.len buf in
          log "read %d bytes@.buf:@.%s@." buflen (Cstruct.to_string buf)
          >>= fun () ->
          if      buflen = 0 then Lwt.return Continue
          else if buflen < 0 then Lwt.return Stop
          else (* buflen > 0 *) (
            TCP.write f2 buf >>= function
            | Ok () ->
              log "wrote %d bytes" buflen    >>= fun () -> Lwt.return Continue
            | Error e ->
              log "error: %a" TCP.pp_write_error e >>= fun () -> Lwt.return Stop
          )
        )
      | Ok `Eof -> log "read eof." >>= fun () -> Lwt.return Stop
      | Error e -> log "error: %a" TCP.pp_error e >>= fun () ->  Lwt.return Stop
    in

    (redirect outflow inflow <?> redirect inflow outflow) >>= function
    | Stop -> Lwt.return_unit
    | Continue -> read_write c outflow inflow

  
  let start c stack =
    let dest = Key_gen.dest_ip () in
    let dest_port = Key_gen.dest_port () in
    let port = Key_gen.local_port () in

    begin
      TCP.create_connection (Stack.tcpv4 stack) (dest, dest_port) >>= function
      | Error _ ->
        Fmt.failwith "outbound connection! [%a]:%d"
          Ipaddr.V4.pp_hum dest dest_port
      | Ok flow -> Lwt.return flow
    end >>= fun outbound ->
    
    Stack.listen_tcpv4 stack ~port (read_write c outbound) ;
    Stack.listen stack

end
