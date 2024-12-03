open Base

module Read_input = struct
  let read_input_file (file_name : string) : string =
    try
      (* read the input file and return a single string *)
      let input_channel = Stdlib.open_in file_name in
      let buffer_size = 1024 in
      let buffer = Bytes.create buffer_size in
      let result = Buffer.create 1024 in
      let rec read_chunks () =
        match Stdlib.input input_channel buffer 0 buffer_size with
        | 0 ->
            Stdlib.close_in input_channel;
            Buffer.contents result
        | n ->
            let buf_str = Bytes.to_string buffer in
            Buffer.add_substring result buf_str ~pos:0 ~len:n;
            read_chunks ()
      in
      read_chunks ()
    with
    | Sys_error msg ->
        Stdlib.Printf.eprintf "Error reading file %s, msg: %s\n" file_name msg;
        ""
    | ex ->
        Stdlib.Printf.eprintf "Unexpected error reading file %s, msg: %s\n"
          file_name (Exn.to_string ex);
        ""
end
