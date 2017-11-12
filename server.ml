open Unix
open Core

let get_host host port =
  let options = [AI_FAMILY (PF_INET); AI_SOCKTYPE (SOCK_STREAM)] in
  let myaddr = getaddrinfo host port options in
  match myaddr with
  | address :: _ -> Some address
  | [] -> None

let socket_not_empty descriptor buffer =
  let bytes_received = recv descriptor buffer 0 (Bytes.length buffer) [MSG_PEEK] in
  bytes_received <> 0

let create_socket callback fail_callback =
  let port = "4520" in
  let available_host = get_host "" port in
  match available_host with
  | Some {ai_family; ai_socktype; ai_protocol; ai_addr; _} -> (
      let server_socket = socket ai_family ai_socktype ai_protocol in
      bind server_socket ai_addr;
      listen server_socket 2;
      while true do
        let (descriptor, _) = accept server_socket in
        let buffer = ref (Bytes.create 10) in
        let bytes_received = ref 0 in
        while socket_not_empty descriptor !(buffer) do
          buffer := Bytes.create (Bytes.length !(buffer) + 10);
          Printf.printf "Receiving: %i length: " !(bytes_received) ;
          bytes_received := recv descriptor !(buffer) 0 (Bytes.length !(buffer)) [];
        done;
        close descriptor;
        callback (!(bytes_received), !(buffer));
      done
    )
  | None -> fail_callback ();
