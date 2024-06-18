(** Ocaml-repair **)

open Path
open Typedtree
open Cmt_format
open Untypeast
open Str

exception Not_implemented

(* ______ COMMAND SETUP ______ *)
let usage_msg =
  "repair <file> -c \"<command>\" -err <test> [-o <output>]  -reg [<test1>] ..."

let input_file = ref ""
let command = ref ""
let output_file = ref ""
let test = ref ""
let err = ref ""
let tests = ref []
let anon_fun filename = input_file := filename


let spec_list =
  [
    ("-c", Arg.Set_string command, "Set command");
    ("-o", Arg.Set_string output_file, "Set output file/folder");
    ("-reg", Arg.Rest_all (fun str_l -> tests := str_l), "Set regression files");
    ("-err", Arg.Set_string err, "Set buggy file");
  ]

let () = Arg.parse spec_list anon_fun usage_msg


let default_iteration =
  [
    "replace-greater";
  ]

let update_single name str =
  let oc = open_out name in
  let parse_tree = Pprintast.string_of_structure (untype_structure str) in
  output_string oc parse_tree;
  flush oc;
  close_out oc

type 'a step_result =
  | Repaired of 'a (* New (smaller) states that produces an error *)
  | Unrelevant_change
  (* This change removes the error, but other changes might be possible *)
  | No_possible_reparation

  type ('a, 'b) repairer = ('a, 'b) Repairer.t =  {
    repairer_name : string;
    repairer_func : (unit -> bool) -> 'a -> 'a;
  }


let repair_basic (state : 'a) (f : 'a -> pos:int -> 'a step_result)
    : 'a * bool =
  let rec aux (state : 'a) (pos : int) =
    match f state ~pos with
    | Repaired nstate -> nstate, true
    | Unrelevant_change -> aux state (pos + 1)
    | No_possible_reparation -> (state, false)
  in
  aux state 0

  let repair_at minimize cur_file ~pos =
    let r = ref (-1) in
    let nfile =
      minimize.repairer_func
        (fun () ->
          incr r; pos = !r)
       cur_file
    in
    (nfile, pos >= !r)

    let check_one_file file cmd =
      Format.printf "trying to open %s@." file ; 
      let cin = open_in file in
      Format.printf "opened@." ; 
      let provided_input = input_line cin in
      let expected_output = input_line cin in
      close_in cin;
      let cmd_plus_arg = Format.sprintf "%s %s" cmd provided_input in
      let pin = Unix.open_process_in cmd_plus_arg in
      let actual_output = try input_line pin with End_of_file -> "-1" in
      close_in pin;
      String.equal expected_output actual_output

    let oracle cmd files =
      let rec aux = function
      | [] -> true
      | h :: t -> if check_one_file h cmd then aux t else false
    in
    aux files

  let step_minimizer (c:string) repairer name cur_file  ~pos  =
    Format.eprintf "Trying %s: pos=%d " repairer.repairer_name pos;
    let new_file, changed = repair_at repairer cur_file ~pos in
    let r =
      if changed then (
        Format.printf "name is %s @." name ; 
        update_single name new_file;
        if oracle c (!err :: !tests) then (Repaired new_file)
        else Unrelevant_change)
      else No_possible_reparation
    in
    let () =
      match r with
      | Repaired _ -> Format.eprintf "Repaired.@."
      | Unrelevant_change -> Format.eprintf "Unrelevant_change.@."
      | No_possible_reparation -> Format.eprintf "No possible reparation.@."
    in
    r


let extract_cmt = function
      | Implementation type_struct -> type_struct
      | Partial_implementation _ | Packed _ | Interface _ | Partial_interface _ ->
          raise Not_implemented

let main () =
  (* PARSING COMMAND AND READING FILES*)
  let input = !input_file in
  let cmt_command = "ocamlc -bin-annot "^ input in
  let _ = Sys.command cmt_command in
  let cmt_infos = read_cmt (String.sub input 0 (String.length input - 3) ^ ".cmt") in
  let input_struct = ref (extract_cmt cmt_infos.cmt_annots) in

   (* APPLY HEURISITCS *)
    let output_file =
      if !output_file = "" then
        String.sub input 0 (String.length input - 3) ^ "_repair.ml"
      else !output_file
    in

    let c = !command in
    update_single output_file !input_struct;
    Format.printf "output_file is : %s" output_file;

    let repaired = ref false in
    while not !repaired do
      let a, b = repair_basic !input_struct (step_minimizer c Repairer.repair_strict_to_large output_file) in
      input_struct := a;
      repaired := b;
    done;
    ()

let _ = main ()
