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
  | PlayCard
  | Rob
  | Trade
  | EndTurn
  | CheckResources
  | CheckSettlements
  | CheckRoads
  | CheckScore
  | CheckCards

let parse_string str =
  match String.lowercase_ascii str with
  | "start" -> Start
  | "quit" -> Quit
  | "roll" -> Roll
  | "settle" -> Settle
  | "build road" -> BuildRoad
  | "play card" -> PlayCard
  | "rob" -> Rob
  | "trade" -> Trade
  | "end turn" -> EndTurn
  | "check resources" -> CheckResources
  | "check settlements" -> CheckSettlements
  | "check roads" -> CheckRoads
  | "check score" -> CheckScore
  | "check cards" -> CheckCards
  | "" -> Empty
  | _ -> Invalid
