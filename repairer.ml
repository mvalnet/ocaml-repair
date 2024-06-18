type ('a, 'b) t = {
  repairer_name : string;
  repairer_func : (unit -> bool) -> 'a -> 'a;
}
let strict_to_large should_apply =
  let mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          match e.exp_desc with
          | Typedtree.Texp_ident (li, loc, val_desc) -> (
              match li with
              | Path.Pdot (p, s) ->
(*                   Format.printf "%s @." s ;
 *)                  if String.equal s ">" then(
(*                     Format.printf "should_apply : %b @." (should_apply ()) ; 
 *)                    if should_apply () then
                      {
                        e with
                        exp_desc =  (
                          Typedtree.Texp_ident
                            (Path.Pdot (p, ">="), {loc with txt = Lident ">=" }, val_desc));
                      }
                    else Tast_mapper.default.expr mapper e)
                  else Tast_mapper.default.expr mapper e
              | _ -> Tast_mapper.default.expr mapper e)
          | _ -> Tast_mapper.default.expr mapper e);
    }
  in
  mapper.structure mapper

let repair_strict_to_large =
  { repairer_name = "strict_to_large"; repairer_func = strict_to_large }
