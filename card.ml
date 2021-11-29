(**Module for Card*)
open Format
type suit = HEARTS | CLUBS | SPADES | DIAMONDS
(* 1= ACE ... 11= Jack, 12= Queen, 13 = King *)
type rank = int

type card = {
  rank : rank;
  suit: suit;
}

(**[get_suit_string s] is the string representation of the suit [s]
   Requires: [s] is of type suit*)
let get_suit_string suit =
  match suit with
  | HEARTS -> "<3"
  | CLUBS -> "-3"
  | SPADES -> "-<"
  | DIAMONDS -> "<>"

(**[get_rank_string rank] is the string representation of the rank [rank] 
   Requires: [rank] is an int from 1-13*)
let get_rank_string rank =
  if rank > 1 && rank < 11 then string_of_int rank else
    match rank with
    | 1 -> "A"
    | 11 -> "J"
    | 12 -> "Q"
    | _ -> "K"


let print_card card =
  let suit_string = get_suit_string card.suit in
  let rank_string = get_rank_string card.rank in
  print_endline " -------";
  print_endline ("|" ^ suit_string ^ "     |");
  print_endline "|       |";
  if String.length rank_string = 2 then 
    print_endline ("|  " ^ rank_string ^ "   |")
  else print_endline ("|   " ^ rank_string ^ "   |");
  print_endline "|       |";
  print_endline ("|     " ^ suit_string ^ "|");
  print_endline " -------"

(**[print_string_list card num] is the string list representation of the card 
   [card] 
   Requires: [c]ard is a card and [num] is an int*)
let print_string_list card num =
  let suit_string = get_suit_string card.suit in
  let rank_string = get_rank_string card.rank in
  [" ------- "; "|" ^ suit_string ^ "     |"; "|       |"; 
   if String.length rank_string = 2 then "|  " ^ rank_string ^ "   |"
   else "|   " ^ rank_string ^ "   |";
   "|       |"; "|     " ^ suit_string ^ "|"; " ------- ";]

(**[print_string_list_num card num] is the string list representation of the 
   card [card] with [num] formatted in front before the card [card]
   Requires: [card] is a card and [num] is an int*)
let print_string_list_num card num =
  let suit_string = get_suit_string card.suit in
  let rank_string = get_rank_string card.rank in
  let top_line = if num < 10 then ". ------- " else ".------- " in 
  let left_line = 
    if String.length rank_string = 2 then "  |  " ^ rank_string ^ "   |"
    else "  |   " ^ rank_string ^ "   |" in 
  [ string_of_int num ^ top_line; 
    "  |" ^ suit_string ^ "     |"; 
    "  |       |"; 
    left_line;
    "  |       |"; 
    "  |     " ^ suit_string ^ "|"; 
    "   ------- "
  ]

(**[new_result card_string result res num] is the string list [res] of 
   combining all of the cards to be printed in a row. 
   Requires [card_string] is a string list
            [result] is a string list
            [res] is a string list
            [num] is an int*)
let rec new_result card_string result res num=
  match (card_string, result) with
  | ([], []) -> if num mod 2 = 0 then List.rev res else res
  | (h_c::t_c, h_r::t_r) -> new_result t_c t_r ((h_c^"  "^h_r)::res) (num + 1)
  | (h_c::t_c, _) -> new_result t_c result (h_c::res) (num + 1)
  | (_,_) -> res

(**[make_prints c result f num] is the general prints of all [c] applied with
   function [f]
   Requires: [result] is a string list
            [f] is a function that is of type 'a->int->string list
            [num] is an int*)
let rec make_prints card result f num = 
  match card with 
  | [] -> result
  | h :: t -> 
    let single_card = f h num in
    let new_result = new_result single_card result [] 1 in 
    make_prints t new_result f (num - 1)

(**[print_all c_list num] prints all of [c_list]
   Requires: [c_list] is a string list and [num] is an int*)
let rec print_all c_list num =
  match c_list with
  | [] -> ()
  | h::t -> print_endline h; num + 1 |> print_all t 

let print_all_cards c_deck =
  let to_print = make_prints c_deck [] print_string_list 1 in
  print_all to_print 1

let print_all_cards_nums c_deck num =
  let to_print = make_prints c_deck [] print_string_list_num (num + 1) in
  print_all to_print 1