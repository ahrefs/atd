(** Various convenience functions (file/channel IOs) *)

module Biniou :
sig
  type 'a reader = Bi_inbuf.t -> 'a
    (** Type of a [read_] function as produced by [atdgen -biniou]. *)

  type 'a writer = Bi_outbuf.t -> 'a -> unit  
    (** Type of a [write_] function as produced by [atdgen -biniou]. *)

  val from_channel :
    ?len:int ->
    ?shrlen:int ->
    'a reader -> in_channel -> 'a
    (** Read a biniou value from a channel.
        @param len     input buffer length.
        @param shrlen  initial length of the table used to store shared values.
    *)

  val to_channel :
    ?len:int ->
    ?shrlen:int -> 
    'a writer -> out_channel -> 'a -> unit
    (** Write a biniou value to a channel.
        @param len     output buffer length.
        @param shrlen  initial length of the table used to store shared values.
    *)

  val from_file :
    ?len:int ->
    ?shrlen:int ->
    'a reader -> string -> 'a
    (** Read a biniou value from a file.
        @param len     input buffer length.
        @param shrlen  initial length of the table used to store shared values.
    *)

  val to_file :
    ?len:int ->
    ?shrlen:int -> 
    'a writer -> string -> 'a -> unit
    (** Write a biniou value to a file.
        @param len     output buffer length.
        @param shrlen  initial length of the table used to store shared values.
    *)
end

module Json :
sig
  type 'a reader = Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
    (** Type of a [read_] function as produced by [atdgen -json].

        In versions of yojson greater than 1.0.1,
        type [Yojson.Safe.lexer_state] is equivalent to
        [Yojson.lexer_state], [Yojson.Basic.lexer_state] and
        [Yojson.Raw.lexer_state]. *)

  type 'a writer = Bi_outbuf.t -> 'a -> unit
    (** Type of a [write_] function as produced by [atdgen -json]. *)

  val from_channel :
    ?buf:Bi_outbuf.t ->
    ?fname:string ->
    ?lnum:int ->
    'a reader -> in_channel -> 'a
    (** Read a JSON value from a channel.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param fname   input file name to be used in error messages.
                       It does not have to be the name of a real file,
                       it can be something like ["<stdin>"].
        @param lnum    Line number to start from. Default: 1.
    *)

  val to_channel :
    ?len:int ->
    'a writer -> out_channel -> 'a -> unit
    (** Write a JSON value to a channel.
        @param len     output buffer length.
    *)

  val from_file :
    ?buf:Bi_outbuf.t ->
    ?lnum:int ->
    'a reader -> string -> 'a
    (** Read a JSON value from a channel.
        @param buf     buffer used to accumulate string data
                       during the lexing phase.
        @param lnum    Line number to start from. Default: 1.
    *)

  val to_file :
    ?len:int ->
    'a writer -> string -> 'a -> unit
    (** Write a JSON value to a file.
        @param len     output buffer length.
    *)
end
