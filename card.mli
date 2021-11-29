(** *)

type suit = HEARTS | CLUBS | SPADES | DIAMONDS
(* 1= ACE ... 11= Jack, 12= Queen, 13 = King *)
type rank = int
type card = {
  rank : rank;
  suit: suit;
}

(** [print_card card] prints [card] in the console*)
val print_card : card -> unit

(** [print_all_cards cards] prints all cards in [cards]
    Requires [cards] is a card list *)
val print_all_cards: card list -> unit

(**[print_all_cards_nums c_deck num] prints all cards in [c_deck] with num 
   from 1 to [num + 1] in front of them
   Requires [c_deck] is a card list and [num] is a int*)
val print_all_cards_nums: card list -> int -> unit