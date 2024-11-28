open Cohttp_eio
open Cohttp

let authenticator =
  match Ca_certs.authenticator () with
  | Ok x -> x
  | Error (`Msg m) ->
      Fmt.failwith "Failed to create system store X509 authenticator: %s" m

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs_threaded.enable ();
  Logs.Src.set_level Cohttp_eio.src (Some Debug)

let https ~authenticator =
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Error (`Msg msg) -> failwith ("tls configuration problem: " ^ msg)
    | Ok tls_config -> tls_config
  in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let headers = Header.add_authorization 
  (Header.init ()) 
  (`Basic (Sys.getenv "USERNAME", Sys.getenv "PASSWORD"));;

(* TODO: *)
(* 1. unable to resolve IP address for hostname *)
(* 2. need to disable ssl validation *)
let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client = Client.make ~https:(Some (https ~authenticator)) env#net in
  (* let client = Client.make ~https:(None) env#net in *)
  Eio.Switch.run @@ fun sw ->
  let resp, body =
    Client.get 
      ~sw 
      (* ~headers *)
      client 
      (Uri.of_string "https://lin-jchen-01.int.cpacket.com/api/epg_fr/known_udp_protocols/")
  in
  if Http.Status.compare resp.status `OK = 0 then
    print_string @@ Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
  else Fmt.epr "Unexpected HTTP status: %a" Http.Status.pp resp.status
