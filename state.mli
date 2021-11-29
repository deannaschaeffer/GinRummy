open Gui

type t
type player

type result = Some of string | None

(**[intialize_game names] is the a state record representing a new game with 
   one player for each player_name in [names]*) 
val intialize_game: string list -> int -> bool -> theme -> t 

(**[discard st card] is the updated version of state [st] once the 
   current player according to [state] has discarded [card] in state 
   Requires [st] is a state and [card] is a Card*) 
val discard : t -> Card.card -> t

(**[draw_from_dicard st] is the updated version of state [st] once the 
   current player according to [st] has drawn one card from the discard pile
   Requires [st] is a state. *) 
val draw_from_discard: t -> t

(**[draw_from_stockpile st] is the updated version of state [st] once the 
   current player according to [st] has drawn one card from the stockpile 
   Requires [st] is a state. *) 
val draw_from_stockpile: t -> t

(**[reset_deck state] is the updated version of state [state] once the discard 
   deck has been reshuffled and emptied and the stockpile is refilled. *) 
val reset_deck: t -> t

(**[player_turn st] is the updated version of state [st] once one turn has
   been executed by the current player according to [st]
   Requires [st] is a state*) 
val player_turn: t -> int -> t
(*check is winner, if it is  *)
(*change who's turn it is, mod when change*)

(**[get_winner st] is the winner of the game represented by [state], if there
   is no winner, then None
   Requires [st] is a state*) 
val get_winner: t -> result

(**[reset_game st num_cards] is the state of the game once the deck has been 
   reshuffled and the players hands have been dealt with [num_cards] each and 
   the discard and stockpile have been reset. State now has one additional 
   round added to it so the game can be played again 
   Requires [st] is a state and [num_cards] is an int*)
val reset_game: t -> int -> t 

(**[get_rounds st] is the number of rounds that have been played
   Requires: [st] is a state*)
val get_rounds: t -> int

(**[print_results st] prints how many wins each player has 
   Requires: [st] is a state*)
val print_results: t -> unit

val is_gui: t -> bool

val get_player_wins: t -> (string*string) list