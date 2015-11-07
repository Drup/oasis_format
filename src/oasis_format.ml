(*
 * Copyright (c) 2015 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)



(** Some sedlexing stuff. *)

module S = Sedlexing
module SU = Sedlexing.Utf8

(** The state of the parser, a stream and a position. *)
type lexbuf = {
  stream : S.lexbuf ;
  mutable pos : Lexing.position ;
}

(** Initialize with the null position. *)
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

(** Register a new line in the lexer's position. *)
let new_line lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
       pos_lnum = lcp.pos_lnum + 1;
       pos_bol = lcp.pos_cnum;
    }

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = S.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

(** [ParseError (file, line, col, token)] *)
exception ParseError of (string * int * int * string)

let raise_ParseError lexbuf =
  let { pos } = lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  let tok = SU.lexeme lexbuf.stream in
  raise @@ ParseError (pos.pos_fname, line, col, tok)

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, colunm %i, token %s"
    (file_to_string file)
    line cnum tok

(** Indentation *)
(* indentation is stored by levels, one tab is one level. *)

let is_whitespace = function
  (* | 0x09..0x0d *)
  | i when 0x09 <= i && i <= 0x0d -> true
  | 0x20 | 0x85 | 0xa0 | 0x1680
  | 0x2000 | 0x200a | 0x2028 | 0x2029
  | 0x202f | 0x205f | 0x3000 -> true
  | _ -> false

let count_indent a =
  let l = Array.length a in
  let rec f i count =
    if i >= 0 && is_whitespace a.(i) then f (i-1) (count+1)
    else count
  in
  f (l-1) 0

(* This doesn't warn the user in case of weird indentations. *)
let compute_indent ~indent r =
  let open Oasis_parser in
  let rec f n l = match l with
    | [] ->
      r := [n] ;
      [INDENT]
    | h :: t ->
      if indent = h then []
      else if indent > h then begin
        r := (indent - h) :: l ;
        [INDENT]
      end
      else begin
        r := t ;
        DEDENT :: f n t
      end
  in
  f indent !r

(* newline, according to
   http://www.unicode.org/standard/reports/tr13/tr13-5.html
*)
let char_newline =
  [%sedlex.regexp? 0x0a..0x0d | 0x085 | 0x2028 | 0x2029 ]
let newline =
  [%sedlex.regexp? 0x0d,0x0a | char_newline ]

let pop r = match !r with
  | [] -> None
  | h::t -> r := t ; Some h

let lex_indent lexbuf f =
  (* indentation levels *)
  let levels = ref [] in
  (* previous tokens, if multiple were returned *)
  let temp = ref [] in
  let buf = lexbuf.stream in
  let rec aux () =
    match pop temp with
    | Some h -> h
    | None -> begin match%sedlex buf with
        | Star (Star (Sub (white_space, char_newline)), newline),
          newline, Star (Sub (white_space, char_newline)) ->
          let s = S.lexeme buf in
          let indent = count_indent s in
          begin match compute_indent ~indent levels with
            | [] -> aux ()
            | h :: t -> temp := t ; h
          end
        | _ -> f lexbuf
      end
  in
  aux

(** The Lexing. *)
let rec lex lexbuf =
  let open Oasis_parser in
  let buf = lexbuf.stream in
  match%sedlex buf with
  | "+:" -> PLUSCOLON
  | "$:" -> DOLLARCOLON
  | ":" -> COLON
  | "if" -> IF
  | "else" -> ELSE

  | '"', Star (Compl '"'), '"' -> STRING (SU.lexeme buf)

  (* Comment *)
  | "#", Star (Compl '\n'), "\n" -> lex lexbuf

  (* Expression *)
  | "!" -> NOT
  | "&&" -> AND
  | "||" -> OR
  | "(" -> LPAREN
  | ")" -> RPAREN

  (* Idents *)
  | ('a'..'z'|'A'..'Z'), Star ('a'..'z' | 'A'..'Z' | '0'..'9') ->
    IDENT (SU.lexeme buf)

  | eof -> EOF

  | _ -> raise_ParseError lexbuf

let parse lexbuf =
  let f = lex_indent lexbuf lex in
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = f () in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised
      Oasis_parser.main
  in
  try
    parser lexer
  with
    | Oasis_parser.Error
    | S.MalFormed
    | S.InvalidCodepoint _
      -> raise_ParseError lexbuf
