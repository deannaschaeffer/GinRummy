(**Module for Graphics*)
open Deckstack
open Graphics
open Card

type deck_type = STOCK | DISCARD

type theme = NATURE | DARK | CORNELL

let rules_list_version = [
  "OPTIONS: The game can be played with 1 - 4 players. If ";
  "you choose the option for 1 player, you will be playing ";
  "against an autoplayer. You can choose the level of the ";
  "autoplayer: easy, medium, or hard. Also, the game";
  "can be played with player hand sizes of either 7 or 10 ";
  "";
  "SETUP: A hand of the specified size (7 or 10 depending";
  "on game option) is dealt to each player initially. One ";
  "card is flipped from the draw pile to the discard pile ";
  "face up and all cards remaining cards in the deck are ";
  "placed face down in the draw pile. The first player who";
  "entered their name in game setup starts.";
  "";
  "ON YOUR TURN: On a player's turn, they have the choice ";
  "between picking up the top card in the discard pile ";
  "(visible to them) or the top card in the draw pile";
  "(invisible to them). Then they must select a card in ";
  "their current hand (including the card they just drew) ";
  "to discard. This card gets placed face up as the new";
  "top card in the discard pile. The turn is now over.";
  "";
  "HOW TO WIN: A player has won when all 7 or 10 cards in ";
  "their hand can be split up into valid groups. A valid ";
  "group is defined as a group of either three or four cards";
  "that meets ONE of the following two criteria:";
  "1) all cards are of the same suit in consecutive order ";
  "(i.e. a 5 of hearts, 6 of hearts, 7 or hearts, and 8 of ";
  "hearts)";
  "OR ";
  "2) all cards are of the same rank (i.e. a 6 of hearts, 6 ";
  "of clubs, and 6 of diamonds)";
  "";
  "NOTE ON VALUES: An ace is treated as a 1 and NOT a 14. ";
  "Additionally, a Jack is treated as value 11, a Queen is ";
  "treated as value 12, and a King is treated as value 13.";
  "For example, a 10 of spades, J of spades, Q of spades, ";
  "and K of spades is a valid hand, but a J of spades, Q ";
  "of spades, K of spades, and A of spades is not. You ";
  "do not need to press anything when you think you have ";
  "a win - the game will automatically recognize your hand ";
  "as a winner! ";
  "";
  "Good luck and have fun!"]

(* Page Constants ****************************************************)
let light_blue = (rgb 201 237 255)

let forest_green = (rgb 0 80 27)

let gray = rgb 211 211 211

let dark_gray = rgb 100 100 100 

let lime = rgb 199 234 70

let x_hand = 110

let y_hand = 450

let card_spacing = 90

let card_width = 75

let card_height = 120

let discard_x = 200

let discard_y = 100

let stock_x = 400

let stock_y = 100

let title_height () = size_y () - 100

let number_players_height () = size_y () - 150

let players_selection_height () = size_y () - 225

let add_player_height () = size_y () - 350

let rules_height () = size_y () - 150

let entry_height = 25

let entry_width = 200

let color_a theme = 
  match theme with 
  | NATURE -> light_blue
  | DARK -> black
  | CORNELL -> gray

let color_b theme = 
  match theme with 
  | NATURE -> forest_green
  | DARK -> red
  | CORNELL -> red

let color_c theme = 
  match theme with 
  | NATURE -> (rgb 158 247 188);
  | DARK -> lime
  | CORNELL -> dark_gray

let clicking x y width height st = 
  st.mouse_x > x && st.mouse_x < (x+width) && 
  st.mouse_y > y && st.mouse_y < (y+height) 

let submit_height () = size_y () - 400

(** [clear_window color] draws a blank screen with color [color]. *)
let clear_window color = 
  let fg = foreground in
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color fg

(* [draw_heart x y scale] draws a heart approximately centered at coordinates 
   ([x],[y]) with size determined by [scale] *)
let draw_heart x y scale = 
  fill_arc (x - (scale*2)) (y - (scale*1)) (scale*2) (scale*2) 0 180; (* *)
  fill_arc (x + (scale*2)) (y - (scale*1)) (scale*2) (scale*2) 0 180;
  Array.of_list 
    [(x,y-(scale*6));(x + (scale*4),y-(scale*1));(x-(scale*4),y-(scale*1))] 
  |> fill_poly 


(* [draw_flip_heart x y scale] draws an upside down heart approximately 
   centered at coordinates ([x],[y]) with size determined by [scale] *)
let draw_flip_heart x y scale = 
  fill_arc (x - (scale*2)) (y - (scale*1)) (scale*2) (scale*2) 180 360;
  fill_arc (x + (scale*2)) (y - (scale*1)) (scale*2) (scale*2) 180 360;
  Array.of_list 
    [(x,y+(scale*5));(x + (scale*4),y-(scale*1));(x-(scale*4),y-(scale*1))] 
  |> fill_poly 


(* [draw_diamond x y scale] draws a diamond approximately centered at 
   coordinates ([x],[y]) with size determined by [scale] *)
let draw_diamond x y scale = 
  Array.of_list 
    [(x,y-(scale*4));(x + (scale*2),y-(scale*1));
     (x, y + (scale*2));(x-(scale*2),y-(scale*1))] 
  |> fill_poly 

(* [draw_club x y scale] draws a club approximately centered at coordinates 
   ([x],[y]) with size determined by [scale] *)
let draw_club x y scale = 
  fill_circle (x-(scale*3)) y (scale*4);
  fill_circle x (y + (scale*6)) (scale*4);
  fill_circle (x+(scale*3)) y (scale*4);
  fill_rect (x-(scale*1)) (y-(scale*7)) (scale*2) (scale*5)

(* [draw_space x y scale] draws a spade approximately centered at coordinates 
   ([x],[y]) with size determined by [scale] *)
let draw_spade x y scale =
  fill_arc (x - (scale*4)) (y - (scale*2)) (scale*4) (scale*4) 180 360;
  fill_arc (x + (scale*4)) (y - (scale*2)) (scale*4) (scale*4) 180 360;
  Array.of_list 
    [(x,y+(scale*9));(x + (scale*8),y-(scale*2));(x-(scale*8),y-(scale*2))] 
  |> fill_poly;
  fill_rect (x-(scale*1)) (y-(scale*8)) (scale*2) (scale*5)

(* [draw_diamond x y scale] draws a diamond approximately centered at 
   coordinates ([x],[y]) with size determined by [scale] *)
let draw_flipped_spade x y scale =
  fill_arc (x - (scale*4)) (y - (scale*2)) (scale*4) (scale*4) 0 180;
  fill_arc (x + (scale*4)) (y - (scale*2)) (scale*4) (scale*4) 0 180;
  Array.of_list 
    [(x,y-(scale*13));(x + (scale*8),y-(scale*2));(x-(scale*8),y-(scale*2))] 
  |> fill_poly;
  fill_rect (x-(scale*1)) (y-scale) (scale*2) (scale*5)

(* [draw_flipped_club x y scale] draws an upside down club approximately 
   centered at coordinates ([x],[y]) with size determined by [scale] *)
let draw_flipped_club x y scale = 
  fill_circle (x-3*scale) y (scale*4);
  fill_circle x (y - (scale*6)) (scale*4);
  fill_circle (x+(scale*3)) y (scale*4);
  fill_rect (x-(scale*1)) (y+(scale*2)) (scale*2) (scale*5)

let check_remaining width cards_across select_card_from_hand num_cards
    top_row max_cards_in_row =
  let prev_cards = width/card_spacing in 
  if prev_cards >= cards_across then select_card_from_hand num_cards 
  else 
    let overflow = width mod card_spacing in 
    if overflow > card_width then select_card_from_hand num_cards else 
      prev_cards + 1 + (if top_row then 0 else max_cards_in_row) 

(* [select_card_from_hand num_cards] returns the 1-indexed index of the card 
   in the current players hand that the user has clicked on to discard where 
   [num_cards] is the total number of cards in the hand. 
   Requires: [num_cards] is 8 or 11 *)
let rec select_card_from_hand num_cards = 
  let start_x = if num_cards > 8 then (x_hand - 50) else x_hand in 
  let st = wait_next_event [Button_down] in
  let curr_x = st.mouse_x in 
  let curr_y = st.mouse_y in 
  if (curr_y) < (y_hand - 10 - card_height) || (y_hand + card_height) < curr_y 
     || (curr_y < y_hand && curr_y > y_hand - 10 )then 
    select_card_from_hand num_cards 
  else
    let () = sound 40 4000 in
    let top_row = curr_y >= y_hand in 
    let max_cards_in_row = if num_cards > 8 then 5 else 4 in 
    let cards_across = 
      if top_row then max_cards_in_row else num_cards - max_cards_in_row in 
    let width = curr_x - start_x in 
    if width < 0 then select_card_from_hand num_cards 
    else check_remaining width cards_across select_card_from_hand num_cards
        top_row max_cards_in_row


(* [choose_pile ()] is the pile that the user has clicked on to draw from: 
   either [DRAW] or [STOCK] *)
let rec choose_pile () = 
  let st = wait_next_event [Button_down] in
  let curr_x = st.mouse_x in 
  let curr_y = st.mouse_y in 
  let discard = curr_x > discard_x && curr_x < (discard_x + card_width) && 
                curr_y > discard_y && curr_y < (discard_y + card_height) in 
  if discard then let () = sound 40 4000 in DISCARD 
  else
    let stock = curr_x > stock_x && curr_x < (stock_x + card_width) && 
                curr_y > stock_y && curr_y < (stock_y + card_height) in 
    if stock then let () = sound 40 4000 in STOCK else choose_pile ()

(* [draw_symbols x y suit] draws the icons for a card whose bottom left corner
   is at coordinates ([x],[y]) and whose suit is [suit]. *)
let draw_symbols x y suit = 
  match suit with 
  | HEARTS -> 
    draw_flip_heart (x + 64) (y + 26) 2;
    draw_heart (x + 11) (y + 98) 2
  | DIAMONDS ->
    draw_diamond (x + 64) (y + 35) 3;
    draw_diamond (x + 11) (y + 95) 3
  | SPADES ->
    draw_flipped_spade (x + 64) (y + 33) 1;
    draw_spade (x + 11) (y + 93) 1
  | CLUBS -> 
    draw_flipped_club (x + 64) (y + 32) 1;
    draw_club (x + 11) (y + 90) 1

(* [draw_card x y card] draws a card whose bottom left corner is at coordinates 
   ([x],[y]) and whose data is represented by [card].*)
let draw_card x y card = 
  set_color white;
  fill_rect x y card_width card_height;
  set_color 
    (if card.suit = HEARTS || card.suit = DIAMONDS then red else black);
  moveto (x + 58) (y + 6);
  let formatted_rank = 
    match card.rank with 
    |1 -> "A" | 11 -> "J" |12 -> "Q" |13 -> "K" | _ -> 
      string_of_int card.rank in 
  draw_string formatted_rank;
  moveto (x + 8) (y + 101);
  draw_string formatted_rank;
  draw_symbols x y card.suit

(* [discard card] draws a new card on the game's discard pile whose data is 
   represented by [card]. *)
let discard card = 
  draw_card discard_x discard_y card

(* [cover_discard ()] erases the discard pile only *)
let cover_discard theme =
  set_color (color_a theme);
  fill_rect discard_x 70 card_width card_height

(* [cover_hand_cards () erases the cards in the hand, regardless of number of
   cards *)
let cover_hand_cards theme =
  set_color (color_a theme);
  fill_rect 110 300 (card_width * 4 + card_spacing * 4) (card_height * 2 + 6)

(* [display_hand deck] draws a player's hand represented by [deck] on the 
   top of a screen for game play *)
let display_hand deck =  
  let start = if List.length deck > 8 then (x_hand - 50) else x_hand in 
  let rec process_next x y switch count = function 
    | [] -> ()
    | h::t -> 
      draw_card x y h;
      if count = switch then 
        process_next start (y - card_height - 10) switch (count + 1) t else
        process_next (x + card_spacing) y switch (count + 1) t in 
  if List.length deck > 8 then 
    process_next start (y_hand) 4 0 deck else 
    process_next start y_hand 3 0 deck 

(* [display_text_block x y text] writes a list of strings [text] on the screen 
   where each string is on its own line and the top line has bottom left 
   coordinates at ([x], [y]) *)
let rec display_text_block x y text =
  match text with 
  | [] -> ()
  | h::t -> 
    moveto x y; draw_string h ; display_text_block x (y-20) t 

(* [slice_list lst curr start] is [lst] with its ([start] - [curr]) leading 
   elements removed *)
let rec slice_list lst curr start =
  match lst with 
  | [] -> []
  | h::t -> if curr = start then h::t else slice_list t (curr + 1) start

(* [check_scroll top_row] processes feedback from the user that scrolls the 
   rules page up and down as well as registers the back button, returning the 
   user to the main menu. [top_row] is the 0 indexed row in the rules that 
   should be displayed at the top of the screen. *)
let rec check_scroll theme top_row = 
  let st = wait_next_event [Button_down] in
  let row = 
    if clicking 390 (size_y () - 83) 20 15 st then 
      if top_row = 0 then top_row else top_row - 1 else 
    if clicking 390 (size_y () - 105) 20 15 st then top_row + 1 else 
      top_row in 
  if clicking 50 (size_y () - 50) 48 20 st then () else 
    ( set_color (color_a theme);
      fill_rect 0 0 (size_x ()) (title_height () - 5);
      set_color (color_b theme);
      display_text_block 20 (rules_height ()) 
        (slice_list rules_list_version 0 row);
      check_scroll theme row )

(* [draw_arrows x y scale] draws scroll arrows centered at ([x],[y]) with size
   according to [scale] *)
let draw_arrows x y scale =
  Array.of_list 
    [(x - (scale * 2), y - (scale * 1)); 
     ( x, y - (scale * 4)); (x + (scale * 2), y - (scale * 1))] 
  |> fill_poly;
  Array.of_list 
    [(x - (scale * 2),(y + 2) - (scale * 1)); 
     (x, (y + 2) + (scale * 2)); (x + (scale * 2), (y + 2) - (scale * 1))] 
  |> fill_poly

(* [show_rules ()] displays the rules on the rule page. *)
let show_rules theme = 
  set_color (color_a theme);
  fill_rect 0 0 (size_x ()) (size_y ());
  moveto 200 (title_height ());
  set_color (color_b theme);
  draw_string "Gin Rummy Rules";
  draw_rect 50 (size_y () - 50) 48 20;
  draw_arrows 400 (size_y () - 85) 5;
  display_text_block 20 (rules_height ()) rules_list_version;
  moveto 52 (size_y () - 48);
  draw_string "Back";
  check_scroll theme 0 

(* [erase_names height count] erases the player names on the setup screen where 
   [count] is the number of names to erase and [height] is the height of the 
   bottom of the top player name  *)
let rec erase_names height count theme = 
  set_color (color_a theme);
  if count < 4 then (
    fill_rect 200 height entry_width entry_height;
    erase_names (height-40) (count+1) theme
  )
  else ()

(* [draw_names names height selected count] draws player names [names] in a 
   vertical column for the setup page where [height] is the height of the 
   bottom of the next name to draw, [selected] is the index of the current 
   selected name (0 indexed) and [count] is the total number of names. *)
let rec draw_names names height selected count = 
  match names with 
  | [] -> () 
  | h::t -> 
    set_color white;
    fill_rect 200 height entry_width entry_height;
    set_color black;
    if selected = count then draw_rect 200 height entry_width entry_height 
    else ();
    moveto 205 (height + 5);
    draw_string h;
    draw_names t (height - 40) selected (count + 1)

(* [draw_number_buttons height selected_num] draws the buttons that allow a 
   player to select the number of cards the game is to be played with where 
   [height] is the height of the bottom of the buttons and [selected_num] is 
   number representing the current selection. *)
let draw_number_buttons height selected_num theme = 
  set_color (color_b theme);
  if selected_num = 7 then (
    draw_rect 300 height 20 20; 
    set_color (color_a theme);
    draw_rect 350 height 30 20
  )
  else (
    draw_rect 350 height 30 20; 
    set_color (color_a theme);
    draw_rect 300 height 20 20
  );
  set_color (color_b theme);
  moveto 305 height;
  draw_string "7";
  moveto 355 height;
  draw_string "10"

(* [check_selected_names st prev_selected index height list] is [index] if state
   [st] indicates that the player name was clicked at coordinates (200,[height] 
   and otherwise is either the index of another name in [list] or is 
   [prev_selected] if none were clicked.  *)
let rec check_selected_names st prev_selected index height list = 
  match list with 
  | [] -> prev_selected
  | h::t -> 
    if clicking 200 height entry_width entry_height st
    then index else 
      check_selected_names st prev_selected (index+1) (height - 40) t

(* [check_selected_num st prev_selected height] is 7 if the 7 button is clicked
   according to [st], 10 if the 10 is clicked, or [prev_selected] otherwise. 
   [height] is the height of the selection buttons. *)
let rec check_selected_num st prev_selected height = 
  if clicking 300 height 20 20 st then let () = sound 40 4000 in 7  else 
  if clicking 350 height 20 20 st then let () = sound 40 4000 in 10 else  
    prev_selected

(* [delete_last_char str] is [str] with its last char sliced off. *)
let delete_last_char str = 
  if String.length str = 0 then str else
    String.sub str 0 ((String.length str) - 1)

(* [is_letter key] is true if character [key] is a letter, either capital or 
   lowercase and false otherwise.   *)
let is_letter key = 
  (key >= 'a' && key <= 'z') || (key >= 'A' && key <= 'Z') || 
  (key >= '0' && key <= '9') 

let new_string key str = 
  if key = '\b' then
    let () = sound 40 4000 in  delete_last_char str else 
  if is_letter key then let () = sound 40 4000 in (str^(Char.escaped key))
  else str

(* [add_key st selected list count] is the [list] with element at index 
   [selected] (0 indexed) edited to have character [key] appended on its end if 
   [key] is a letter or number or edited to have its last letter removed if it 
   is nonempty.  *)
let rec add_key key selected list count = 
  match list with 
  | [] -> []
  | h::t -> 
    if count = selected then 
      let new_string = new_string key h in new_string::t 
    else h::(add_key key selected t (count+1))

(* [check_submit st height] is true if the submit button is being clicked 
   according to state [st] and false otherwise. [height] is the height of the 
   submit button.  *)
let check_submit st height =
  clicking 400 height 70 25 st

(* [check_new_names st names] is list [names] updated according to actions the 
   user takes to either add or delete a name indicated in state [st] *)
let check_new_names st names = 
  let new_name = "player" ^ string_of_int (List.length names + 1) in 
  if clicking 50 (add_player_height ()) 110 25 st && 
     List.length names < 4 then 
    let () = sound 40 4000 in names @ [new_name] 
  else if clicking 50 (add_player_height () - 40) 140 25 st && 
          (List.length names) > 1 then let () = sound 40 4000 in 
    match List.rev names with 
    | h::t -> List.rev t 
    | _ -> failwith "impossible"
  else names

(* [non_empty names] is true if all names in the list [names] are nonempty 
   strings*)
let rec non_empty names = 
  match names with 
  | [] -> true
  | h::t -> String.length h > 0 && non_empty t

(* [refresh_setup_page_graphics error names selected_name selected_number] 
   refreshes the setup screen by erasing necessary elements and then drawing 
   error [error], player names according to [names], a black outline around
   the name indexed by [selected_name], and a selection box around the number
   of card box with number [selection_number]. *)
let refresh_setup_page_graphics 
    error names selected_name selected_number theme =
  moveto 200 250;
  set_color (color_a theme);
  fill_rect 0 250 (size_x ()) 30; (** erase old error *)
  set_color black;
  draw_string error;
  erase_names (players_selection_height ()) 0 theme;
  draw_names names (players_selection_height ()) selected_name 0;
  draw_number_buttons (number_players_height ()) selected_number theme

(* [selection_loop names selected_name selected_number error] is the tuple 
   including a list of player names and the number of cards to play with after
   user feedback on the setup screen acts on the list of names, [names], 
   currently selected name, [selected_name], and currently selected number of 
   cards, [selected_number] where [error] is an error message to display 
   ("" if no error). *)
let rec selection_loop names selected_name selected_number error theme = 
  refresh_setup_page_graphics error names selected_name selected_number theme;
  let st = wait_next_event [Button_down;Key_pressed] in
  if st.button then 
    let new_selected_name = (check_selected_names st selected_name 0 
                               (players_selection_height ()) names) in 
    let new_selected_num = (check_selected_num st selected_number 
                              (number_players_height ())) in 
    let new_names = check_new_names st names in 
    if check_submit st (submit_height ()) then 
      let () = sound 40 4000 in
      if non_empty new_names then (new_names, selected_number) 
      else 
        selection_loop new_names new_selected_name new_selected_num  
          "No empty names!" theme
    else 
      selection_loop new_names new_selected_name new_selected_num "" theme
  else 
    selection_loop (add_key st.key selected_name names 0) selected_name 
      selected_number "" theme

(* [game_setup ()] is a tuple including a list of player names and the number 
   of cards to play with after the user has made their edits on the setup 
   screen and pressed start. *)
let game_setup theme =
  set_color (color_a theme); fill_rect 0 0 (size_x ()) (size_y ());
  moveto 200 (title_height ()); set_color (color_b theme);
  draw_string "Settings"; 
  moveto 50 (number_players_height ());
  draw_string "Number of Players"; 
  moveto 100 (players_selection_height ());
  draw_string "Players"; 
  moveto 55 (add_player_height () + 5);
  draw_rect 50 (add_player_height ()) 110 25; 
  draw_string "Add Player";
  moveto 55 (add_player_height ()-35); 
  draw_rect 50 (add_player_height ()-40) 140 25; 
  draw_string "Remove Player";
  set_color (color_b theme); 
  draw_rect 400 (submit_height ()) 70 25;
  moveto 405 (submit_height () + 5); 
  draw_string "Start";
  selection_loop ["player1"] 0 7 "" theme

(* [clicked_rules st] is true if state [st] indicates that the user has clicked
   the rules button and false otherwise. *)
let clicked_rules st =
  clicking 190 (title_height () - 110) 70 30 st

(* [clicked_dark]  *)
let clicked_dark st =
  clicking 130 (title_height () - 170) 92 20 st

let clicked_nature st =
  clicking 230 (title_height () - 170) 112 20 st

let clicked_cornell st = 
  clicking 350 (title_height () - 170) 122 20 st 

(* [clicked_game st] is true if state [st] indicates that the user has clicked
   the game button and false otherwise. *)
let clicked_game st =
  clicking 290 (title_height () - 110) 120 30 st

let draw_theme_buttons theme = 
  set_color (color_b theme); 
  moveto 132 (title_height () - 168);
  draw_string "Dark Mode"; 
  draw_rect 130 (title_height () - 170) 92 20;
  moveto 232 (title_height () - 168); 
  draw_string "Nature Mode";
  draw_rect 230 (title_height () - 170) 112 20; 
  moveto 352 (title_height () - 168);
  draw_string "Cornell Mode"; 
  draw_rect 350 (title_height () - 170) 122 20

(* [open_screen ()] shows the initial home screen with options to see the 
   rules or start game. *)
let open_screen theme = 
  clear_window (color_a theme); 
  moveto 200 (title_height ());
  set_color (color_b theme); 
  set_window_title "Gin Rummy Game";
  set_font "-adobe-courier-medium-r-normal--0-0-0-0-m-0-iso8859-2";
  draw_string "Welcome to Gin Rummy!"; 
  moveto 200 200;
  set_color (color_c theme); 
  fill_rect 190 (title_height () - 110) 70 30;
  moveto 200 (title_height () - 100); 
  set_color (color_b theme);
  draw_string "Rules"; 
  moveto 300 (title_height () - 100); 
  set_color (color_c theme); 
  fill_rect 290 (title_height () - 110) 120 30;
  set_color (color_b theme); 
  draw_string "Start Game";
  draw_theme_buttons theme

(* [print_player_gui name] directs player named [name] through their turn. *)
let print_player_gui name theme message =
  set_color (color_a theme) ;
  fill_rect discard_x (discard_y + card_height + 50) (size_x()) 100;
  set_color (color_b theme); 
  moveto (discard_x) (discard_y + card_height + 50);
  draw_string ("It is your turn " ^ name); 
  set_color (color_a theme);
  fill_rect (discard_x - card_width) 
    (discard_y + card_height + 25) (size_x()) 20;
  moveto (discard_x - card_width) (discard_y + card_height + 25);
  set_color (color_b theme); 
  draw_string message

let check_press st = 
  if clicking 200 (title_height () - 210) 100 30 st then
    let () = sound 40 4000 in "play again"
  else if clicking 400 (title_height () - 210) 60 30 st then 
    let () = sound 40 4000 in "quit"
  else ""


let rec selection_loop_end_game () = 
  let st = wait_next_event [Button_down;Key_pressed] in
  if st.button then 
    let click = check_press st in
    if click = "" then selection_loop_end_game () else click
  else selection_loop_end_game ()


(* [game_screen ()] initializes the games screen once the game is started. *)
let game_screen theme =
  set_color (color_a theme);
  fill_rect 0 0 (size_x ()) (size_y ());
  moveto 200 600;
  set_color (color_b theme);
  draw_string "Game Play";

  set_color (color_c theme);
  fill_rect 400 70 75 120;
  set_color white;
  draw_rect 410 80 55 100


let win_message name = 
  match name with 
  | "hard autoplayer" | "medium autoplayer" | "easy autoplayer" -> 
    draw_string("You lost! Autoplayer won :(")
  | _ -> draw_string ("The winner is: " ^ name ^ ". Congratulations!")

(* [print_gui_winner name] notifies that player with name [name] is the winner 
   when the game has finished.  *)
let print_gui_winner name theme =
  set_color (color_a theme); 
  fill_rect 0 0 (size_x ()) (size_y ());
  moveto 200 (title_height ()); 
  set_color (color_b theme);
  set_font "-adobe-courier-medium-r-normal--0-0-0-0-m-0-iso8859-2";
  let () = win_message name in
  moveto 400 (title_height () - 200); 
  set_color (color_c theme);
  fill_rect 400 (title_height () - 210) 60 30; 
  set_color (color_b theme);
  draw_string "Quit"; moveto 200 (title_height () - 200); 
  set_color (color_c theme); 
  fill_rect 200 (title_height () - 210) 100 30;
  set_color (color_b theme); 
  draw_string "Play Again";
  let navigation = selection_loop_end_game () in 
  let () = game_screen theme in navigation

let rec print_player_wins players y =
  match players with
  | [] -> draw_string ""
  | h::t -> 
    moveto 200 y; draw_string ((fst h)^" won: "^(snd h)^" games");
    print_player_wins t (y - 20)

let play_final_song () =
  while true do 
    sound 40 4000;
    sound 50 4000; 
    sound 60 4000;
    sound 70 4000
  done

let print_gui_results players theme =
  set_color (color_a theme); fill_rect 0 0 (size_x ()) (size_y ());
  moveto 250 600; set_color (color_b theme);
  draw_string "Results";
  print_player_wins players 550;
  play_final_song ()

let print_auto_message theme choice =
  moveto 100 670; set_color (color_b theme);
  draw_string "Autoplayer has taken their turn. Your turn again!";
  moveto 170 650;
  if choice = 2 then
    draw_string "Autoplayer drew from discard pile"
  else 
    draw_string "Autoplayer drew from stock pile"

let cover_auto_message theme =
  set_color (color_a theme);
  fill_rect 100 (size_y() - 100) (size_x ()) 40

let check_press st = 
  if clicking 190 (title_height () - 210) 60 30 st then
    let () = sound 40 4000 in "easy autoplayer"
  else if clicking 285 (title_height () - 210) 80 30 st then
    let () = sound 40 4000 in "medium autoplayer"
  else if clicking 400 (title_height () - 210) 60 30 st then   
    let () = sound 40 4000 in "hard autoplayer"
  else ""


let rec selection_loop_end_game () = 
  let st = wait_next_event [Button_down;Key_pressed] in
  if st.button then 
    let click = check_press st in
    if click = "" then selection_loop_end_game () else click
  else selection_loop_end_game ()


let auto_player_select theme =
  set_color (color_a theme); 
  fill_rect 0 0 (size_x ()) (size_y ());
  moveto 100 (title_height ()); 
  set_color (color_b theme);
  set_font "-adobe-courier-medium-r-normal--0-0-0-0-m-0-iso8859-2";
  draw_string ("Choose a level of difficulty for the autoplayer");
  moveto 295 (title_height () - 200);  
  set_color (color_c theme);
  fill_rect 285 (title_height () - 210) 80 30; set_color (color_b theme);
  draw_string "Medium"; moveto 200 (title_height () - 200); 
  set_color (color_c theme); 
  fill_rect 190 (title_height () - 210) 60 30;
  set_color (color_b theme); draw_string "Easy";
  moveto 410 (title_height () - 200);  
  set_color (color_c theme);
  fill_rect 400 (title_height () - 210) 60 30; 
  set_color (color_b theme); draw_string "Hard";
  let x = selection_loop_end_game () in 
  let () = game_screen theme in x

let make_sound () =
  sound 40 4000;
