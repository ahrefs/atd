(*
   This module complements the Yojson.Safe lexer and was extracted from the
   original source file 'read.mll'.
   See longer note in the mli.
*)
{
  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  (* see description in common.mli *)
  type lexer_state = Yojson.Safe.lexer_state = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }

  let custom_error descr v (lexbuf : Lexing.lexbuf) =
    let offs = lexbuf.lex_abs_pos - 1 in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol - 1 in
    let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol) in
    let file_line =
      match v.fname with
          None -> "Line"
        | Some s ->
            Printf.sprintf "File %s, line" s
    in
    let bytes =
      if pos1 = pos2 then
        Printf.sprintf "byte %i" (pos1+1)
      else
        Printf.sprintf "bytes %i-%i" (pos1+1) (pos2+1)
    in
    let msg = Printf.sprintf "%s %i, %s:\n%s" file_line v.lnum bytes descr in
    Yojson.json_error msg

  let long_error descr v lexbuf =
    let junk = Lexing.lexeme lexbuf in
    let buf_size = 32 in
    let buf = Buffer.create buf_size in
    let () =
      Yojson_lexer_utils.read_junk_without_positions buf buf_size lexbuf in
    let extra_junk = Buffer.contents buf in
    custom_error
      (Printf.sprintf "%s '%s%s'" descr junk extra_junk)
      v lexbuf

  type variant_kind = [ `Square_bracket | `Double_quote ]
}

rule start_any_variant v = parse
  | '"'      { Buffer.clear v.buf;
               `Double_quote }
  | '['      { `Square_bracket }
  | _        { long_error "Expected '\"' or '[' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }

and start_any_tuple v = parse
  | '['      { true }
  | _        { long_error "Expected '[' but found" v lexbuf }
  | eof      { custom_error "Unexpected end of input" v lexbuf }
