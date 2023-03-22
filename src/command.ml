open Gamestate

exception Empty
exception Invalid

type command = Start | Quit | Roll

let parse_string str =
  match String.lowercase_ascii str with
  | "start" -> Start
  | "quit" -> Quit
  | "roll" -> Roll
  | "" -> raise Empty
  | _ -> raise Invalid
