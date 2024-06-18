(** Ocaml-repair **)

open Path
open Typedtree
open Cmt_format
open Untypeast
open Str

exception Not_implemented

(* ______ COMMAND SETUP ______ *)
let usage_msg =
  "repair <file.ml> -c \"<command>\" -reg <file.txt> -err <file.txt> [-o <output>]"

let input_file = ref ""
let command = ref ""
let output_file = ref ""
let file = ref ""
let err = ref ""
let tests = ref []
let anon_fun _ = ()

let spec_list =
  [
    ("-c", Arg.Set_string command, "Set command");
    ("-o", Arg.Set_string output_file, "Set output file/folder");
    ("-reg", Arg.Set_string file, "Set regression files");
    ("-e", Arg.Set_string err, "Set buggy file");
  ]

let () = Arg.parse spec_list anon_fun usage_msg

(* let all_euristics =
  List.fold_left
    (fun minimizers m -> Smap.add m.minimizer_name m minimizers)
    Smap.empty
    [
      
    ] *)

let default_iteration =
  [
    "replace-greater";
  ]
(* 
let minimizers_to_run =
  List.map
    (fun name ->
      try Smap.find name all_minimizers
      with Not_found ->
        Format.eprintf "Minimizer %S not found@." name;
        exit 1)
    (if !arg_minimizers = "" then default_iteration
    else String.split_on_char ',' !arg_minimizers)
 *)


let update_single name str =
  let oc = open_out name in
  let parse_tree = Pprintast.string_of_structure (untype_structure str) in
  output_string oc parse_tree;
  flush oc;
  close_out oc

(* ______ ONE FILE MINIMIZATION ______ *)

(** [one_file_minimize c map file] minimizes [file] in the file set [map]
  regarding to the command [c] *)
(* let one_file_minimize c structure file minimizer : structure * bool
    =
      apply_repair false file minimizer c *)

type 'a step_result =
  | Repaired of 'a (* New (smaller) states that produces an error *)
  | Unrelevant_change
  (* This change removes the error, but other changes might be possible *)
  | No_possible_reparation

  type ('a, 'b) repairer = {
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

  let step_minimizer (c:string) repairer name cur_file  ~pos  =
    Format.eprintf "Trying %s: pos=%d " repairer.repairer_name pos;
    let new_file, changed = repair_at repairer cur_file ~pos in
    let r =
      if changed then (
        update_single name new_file;
        if oracle c then (Repaired new_file)
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
  let cmt_command = !command^" --bin-annot -stop-after-typing "^ input in
  let _ = Sys.command cmt_command in 
  let cmt_infos = read_cmt (String.sub input 0 (String.length input - 3) ^ ".cmt") in
  let input_struct = ref (extract_cmt cmt_infos.cmt_annots) in 

   (* APPLY HEURISITCS *)
    let output_file =
      if !output_file = "" then
        String.sub input 0 (String.length input - 3) ^ "_repair.ml"
      else !output_file
    in

    let c = !command ^ " " ^ output_file in
    update_single output_file !input_struct;

    let repaired = ref false in
    while not !repaired do
      let a, b = repair_basic !input_struct (step_minimizer c repairer output_file) in
      input_struct := a;
      repaired := b;
    done;
    ()

let _ = main ()