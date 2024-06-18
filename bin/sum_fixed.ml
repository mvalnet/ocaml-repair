let sum l =
  let a = Array.init l (fun n -> n) in
  let rec sum n = if n >= l then n else a.(n) + sum (n + 1) in
  sum 0

let () = Printf.printf "%d\n" (sum (int_of_string Sys.argv.(1)))
