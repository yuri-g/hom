open Unix
open Core
open Core.Bytes


let () =
  (Server.create_socket (fun (size, buffer) -> Printf.printf "Received: %i bytes:\n%s\n%!" size buffer)
     (fun () -> Printf.printf "Failed to start server\n%!" )
  )
