let fact n =
  let rec aux n = if n < 0 then 0 else n * aux (n - 1) in
  aux n

let () = Printf.printf "%d\n" (fact (int_of_string Sys.argv.(1)))
