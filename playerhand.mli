(**Playerhand mli*)
open Card

type deck = Card.card list

(**[is_winning_hand deck] is True if [deck]represents a winning 
   hand, otherwise is False. A winning hand is constituted by one of the 
   following combinations: 
   - 4 consecutive in rank cards of the same suit, 3 consecutive cards of other
   - 3 cards of same rank, 4 cards of another rank 
   - 3 cards of same rank, 4 cards consecutive in rank, same suit
   - 4 cards consecutive in rank, same suit, 3 cards same rank
*)
val is_winning_hand : deck -> bool

(**[is_winning_hand deck] is True if [deck] represents a winning 
   hand, otherwise is False. A winning hand is constituted by one of the 
   following combinations: 
   - 3 consecutive in rank cards of the same suit, 3 consecutive cards of other
   - 3 cards of same rank, 3 cards of another rank 
   - 3 cards of same rank, 3 cards consecutive in rank, same suit
   - 3 cards consecutive in rank, same suit, 3 cards same rank
*)
val is_winning_hand_6 : deck -> bool

(** [check_same_rank deck] is True if all cards in list [deck] are all the same
    rank and False otherwise. *)
val check_same_rank : deck -> bool

(** [check_two_consecutive_groups deck] is True if [deck] contains two groups of 
    consecutive values all of the same suit. These two groups may or may not be 
    consecutive in regard to eachother.  *)
val check_two_consecutive_groups_same_suit : deck -> bool

(** [check_three_consecutive_groups_same_suit deck] is True if [deck] contains 
    three groups of consecutive values all of the same suit. These two groups 
    may or may not be consecutive in regard to eachother.  *)
val check_three_consecutive_groups_same_suit : deck -> bool

(** [check_two_consecutive_groups deck] is True if [deck] contains two groups of 
    consecutive values either both same suit or two different suits. These two 
    groups may or may not be  consecutive in regard to eachother.  *)
val check_two_consecutive_groups : deck -> bool 

(** [check_three_consecutive_groups deck] is True if [deck] contains three 
    groups of consecutive values either both same suit or two different suits. 
    These two groups may or may not be  consecutive in regard to eachother.  *)
val check_three_consecutive_groups : deck -> bool

(** [check_consecutive cards] is True if [cards] represent consecutive rank in 
    the same suit, else is False. 
    Requires: [cards] is nonempty *)
val check_consecutive : deck -> bool

(** [remove_card card deck] is the card list [deck] with element [card] removed. 
    Requires: [card] is element of [deck] *)
val remove_card: card -> deck -> card list