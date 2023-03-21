open Player
open Board
open Gamestate

exception Empty
exception Invalid

type command = Start | Quit | Roll

let command_messages = function
  | Start -> "Hello! Welcome to Settlers of Caml-tan!"
  | Roll ->
      let p, q = roll_dice () in
      "You've rolled a" ^ string_of_int (p + q) ^ "and have moved to that tile."
  | Quit -> "Thank you for playing Settlers of Caml-tan! See you next time!"

let new_cmd cmd =
  match cmd with
  | Start -> command_messages Start
  | Roll -> command_messages Roll
  | Quit -> command_messages Quit

let parse_string str =
  match String.lowercase_ascii str with
  | "start" -> new_cmd Start
  | "quit" -> new_cmd Quit
  | "roll" -> new_cmd Roll
  | "" -> raise Empty
  | _ -> raise Invalid
