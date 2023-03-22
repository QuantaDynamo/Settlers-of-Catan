open Gamestate

exception Empty
exception Invalid

type command = Start | Quit | Roll | Empty | Invalid | Settle

let parse_string str =
  match String.lowercase_ascii str with
  | "start" -> Start
  | "quit" -> Quit
  | "roll" -> Roll
  | "settle" -> Settle
  | "" -> Empty
  | _ -> Invalid
