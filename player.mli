(**player mli*)
open Deckstack

(* the player's hand, consisting of 7 cards (for now) *)
type hand = Deckstack.deck

(* number of games won *)
type score = int 