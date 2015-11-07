(* OASIS_START *)
(* DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e) *)
(* OASIS_STOP *)
open Ocamlbuild_plugin

let doc_intro = "index.text"

let () =
  dispatch
    (function hook ->
      dispatch_default hook ;
      match hook with
        | After_rules ->
            dep ["ocaml"; "doc"; "extension:html"] & [doc_intro] ;
            flag ["ocaml"; "doc"; "extension:html"] & S[A"-intro"; P doc_intro];
        | _ -> ()
    )
