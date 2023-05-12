exception Empty
exception Invalid

type command =
  | Start
  | Quit
  | Roll
  | Empty
  | Invalid
  | Settle
  | BuildRoad

val parse_string : string -> command
(** [parse_string] parses a player's string input into a command. It
    converts all strings to lowercase to assist with usability *)
