(**Deck mli*)
open Card

type deck = Card.card list 

(** [shuffle deck] is a list containing the elements of [deck] in a different, 
    random order
    Requires: [deck] is a deck *)
val shuffle : deck -> deck

(** [pop deck] is a tuple of the first element of [deck] and [deck] with its 
    first element removed 
    Requires: [deck] is a deck*)
val pop : deck -> (Card.card * Card.card list)

(** [push card deck] is [deck] with [card] added to its front 
    Requires: [card] is a card and [deck] is a deck*)
val push : Card.card -> deck -> deck

(** [peek deck] prints the first card in deck [deck] 
    Requires: [deck] is a deck*)
val peek : deck -> unit

(** [is_empty deck] is True if the deck is empty and False if there is at least
    one card element.
    Requires: [deck] is a deck *)
val is_empty : deck -> bool

(** [hands deck num_players num_cards] is tuple where the first element is 
    the remaining [deck] after dealing [num_cards] cards 
    (removing [num_cards] elements) each to [num_players] players and 
    the second element is a double list of each player's initial hand 
    Requires: [deck] is a deck and [num_players] is an int*)
val hands : deck -> int -> int -> (Card.card list * Card.card list list)