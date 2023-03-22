open Gamestate

exception Empty
exception Invalid

type command = Start | Quit | Roll | Empty | Invalid

let parse_string str =
  match String.lowercase_ascii str with
  | "start" -> Start
  | "quit" -> Quit
  | "roll" -> Roll
  | "" -> Empty
  | _ -> Invalid
