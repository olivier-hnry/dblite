open Libdb.Base
open Libdb.Reader

let file_name = ref ""

let anon_fun s = match !file_name with
  | "" -> file_name := s
  | _ -> failwith "Too many arguments : all files must be in the same file."

let usage_msg = "db-lite <file>"

let () = Arg.parse [] anon_fun usage_msg; if !file_name = "" then failwith "A file name must be provided."

let prog_s = let ic = open_in !file_name
  in let s = try
    In_channel.fold_lines (fun a b -> a^"\n"^b) "" ic
  with e ->
    close_in_noerr ic;
    raise e
  in close_in ic; s

let prog_main, context = setup prog_s

let () = exec_p prog_main 0 context

