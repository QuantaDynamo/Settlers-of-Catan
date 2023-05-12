open Gamestate

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

let parse_string str =
  match String.lowercase_ascii str with
  | "start" -> Start
  | "quit" -> Quit
  | "roll" -> Roll
  | "settle" -> Settle
  | "build road" -> BuildRoad
  | "" -> Empty
  | _ -> Invalid
