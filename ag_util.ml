module Biniou =
struct
  type 'a reader = Bi_inbuf.t -> 'a
  type 'a writer = Bi_outbuf.t -> 'a -> unit  

  let from_channel ?len ?shrlen read ic =
    let ib = Bi_inbuf.from_channel ?len ?shrlen ic in
    read ib

  let from_file ?len ?shrlen read fname =
    let ic = open_in_bin fname in
    try
      let x = from_channel ?len ?shrlen read ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  let to_channel ?len ?shrlen write oc x =
    let ob = Bi_outbuf.create_channel_writer ?len ?shrlen oc in
    write ob x;
    Bi_outbuf.flush_channel_writer ob

  let to_file ?len ?shrlen write fname x =
    let oc = open_out_bin fname in
    try
      to_channel ?len ?shrlen write oc x;
      close_out oc
    with e ->
      close_out_noerr oc;
      raise e
end

module Json =
struct
  type 'a reader = Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
  type 'a writer = Bi_outbuf.t -> 'a -> unit

  let from_channel ?buf ?fname ?lnum read ic =
    let lexbuf = Lexing.from_channel ic in
    let ls = Yojson.Safe.init_lexer ?buf ?fname ?lnum () in
    read ls lexbuf

  let from_file ?buf ?lnum read fname =
    let ic = open_in_bin fname in
    try
      let x = from_channel ?buf ~fname ?lnum read ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  let to_channel ?len write oc x = Biniou.to_channel ?len ~shrlen:0 write oc x
  let to_file ?len write fname x = Biniou.to_file ?len ~shrlen:0 write fname x
end
