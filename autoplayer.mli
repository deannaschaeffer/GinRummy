(* autoplayer mli *)
open Deckstack
open Card

(**[pick_easy] is an Int Option randomly chosen between 1-2 using
   random seeding *)
val pick_easy : int option

(**[discard_easy num] is an Int Option randomly chosen between 1-[num] using
   random seeding 
   Requires: [num] is the number of cards in players' hands*)
val discard_easy : int -> int option

(**[pick_medium top hand] is an Int Option chosen between 1-2 
   2 is picked if [top] on the discard pile pairs with any card
   in the autoplayer's [hand]
   Requires: [top] is a Card
            [hand] is a Card list *)
val pick_med : Card.card -> Deckstack.deck -> int option

(**[discard_med hand flip] is an Int Option that is the card that has no pairs 
   in [hand] or in the event that all cards in [hand] pair is the card
   in the first pair
   Requires: [hand] and [flip] are Card lists*)
val discard_med : Deckstack.deck -> Deckstack.deck -> int option

(** [pick_hard top hand num] is an int option that corresponds to the pile
    to pick from based on the top of the discard pile [top] and the hand [hand]
    which contains [num] cards 
    Requires: [top] is a card, [hand] is a card list, [num] is an int *)
val pick_hard : Card.card -> Deckstack.deck -> int -> int option

(* hard level autoplayer chooses which card to discard from its hand *)
(** [discard_hard hand num] is an int option that corresponds to the card
    to discard from the hand [hand] based on the contents of its [num] cards
    Requires: [hand] is a card list, [num] is an int *)
val discard_hard : Deckstack.deck -> int -> int option 


