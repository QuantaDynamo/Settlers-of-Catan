exception Empty
exception Invalid

type command = Start | Quit | Roll

val parse_string : string -> command
(** [parse_string]  *)
