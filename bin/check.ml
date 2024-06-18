let check file cmd =
  let cin = open_in file in
  let provided_input = input_line cin in
  let expected_output = input_line cin in
  close_in cin;
  let cmd_plus_arg = Format.sprintf "%s %s" cmd provided_input in
  let pin = Unix.open_process_in cmd_plus_arg in
  let actual_output = input_line pin in
  close_in pin;
  String.equal expected_output actual_output

let () = assert (check Sys.argv.(1) Sys.argv.(2))
