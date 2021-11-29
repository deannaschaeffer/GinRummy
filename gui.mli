open Deckstack
open Card
open Graphics

type deck_type = STOCK | DISCARD

type theme = NATURE | DARK | CORNELL

(** [display_hand deck] draws a player's hand represented by [deck] on the 
    top of a screen for game play *)
val display_hand : deck -> unit

(** [draw_card x y card] draws a card whose bottom left corner is at coordinates 
    ([x],[y]) and whose data is represented by [card].*)
val draw_card : int -> int -> card -> unit

(** [cover_discard ()] erases the discard pile only *)
val cover_discard: theme -> unit 

(** [select_card_from_hand num_cards] returns the 1-indexed index of the card 
    in the current players hand that the user has clicked on to discard where 
    [num_cards] is the total number of cards in the hand. 
    Requires: [num_cards] is 8 or 11 *)
val select_card_from_hand : int -> int 

(** [choose_pile ()] is the pile that the user has clicked on to draw from: 
    either [DRAW] or [STOCK] *)
val choose_pile : unit -> deck_type

(** [print_player_gui s] directs player named [s] through their turn. *)
val print_player_gui: string -> theme -> string -> unit

(** [show_rules theme] displays the rules on the rule page according to theme:
    [theme]. *)
val show_rules : theme -> unit

(** [game_setup ()] is a tuple including a list of player names and the number 
    of cards to play with after the user has made their edits on the setup 
    screen and pressed start. *)
val game_setup : theme -> (string list * int)

(** [open_screen theme] shows the initial home screen with options to see the 
    rules or start game according to theme: [theme]. *)
val open_screen : theme -> unit

(** [game_screen ()] initializes the games screen once the game is started. *)
val game_screen : theme -> unit 

(** [clicked_rules st] is true if state [st] indicates that the user has clicked
    the rules button and false otherwise. *)
val clicked_rules : Graphics.status -> bool

(** [clicked_dark st] is true if state [st] indicates that the user has clicked
    the dark mode button and false otherwise. *) 
val clicked_dark : Graphics.status -> bool

(** [clicked_nature st] is true if state [st] indicates that the user has 
    clicked the nature mode button and false otherwise. *) 
val clicked_nature : Graphics.status -> bool

(** [clicked_cornell st] is true if state [st] indicates that the user has 
    clicked the cornell mode button and false otherwise. *) 
val clicked_cornell : Graphics.status -> bool

(** [clicked_game st] is true if state [st] indicates that the user has clicked
    the game button and false otherwise. *)
val clicked_game : Graphics.status -> bool

(** [cover_hand_cards theme] erases the cards in the hand, regardless of number 
    of cards according to theme: [theme]. *)
val cover_hand_cards: theme -> unit

(** [print_gui_winner name] notifies that player with name [name] is the winner 
    when the game has finished. *)
val print_gui_winner: string -> theme  -> string

(** [print_gui_results name] notifies the results of the game where [name] is 
    the name of the winner. *)
val print_gui_results: (string*string) list -> theme -> unit

(** [auto_player_select theme] displays the auto player's move in the gui 
    according to theme [theme]. *)
val auto_player_select: theme -> string

(** [print_auto_message theme] displays a message related to what the auto 
    player chose to do on their turn in theme [theme].  *)
val print_auto_message: theme -> int -> unit

(** [cover_auto_message theme] erases the message demonstrating what the auto
    player did on their turn in theme [theme].  *)
val cover_auto_message: theme -> unit

(** [make_sound ()] causes the window to make a sound .  *)
val make_sound: unit -> unit