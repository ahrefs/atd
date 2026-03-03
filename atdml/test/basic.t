Generate OCaml code from a basic ATD file.

  $ cat > hello.atd << 'EOF'
  > type color = [
  >   | Red
  >   | Green <json name="green">
  >   | Blue
  > ]
  > EOF
  $ atdml hello.atd
  $ cat hello.mli
  (* Auto-generated from "hello.atd". *)
  
  type color =
    | Red
    | Green
    | Blue
  
  val color_of_yojson : Yojson.Safe.t -> color
  val yojson_of_color : color -> Yojson.Safe.t
  val color_of_string : string -> color
  val string_of_color : color -> string
  val color_of_channel : in_channel -> color
  val color_of_file : string -> color
  



