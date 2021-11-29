open State
open ANSITerminal
open Graphics 
open Gui
open Card 

(** [play_game f] starts the adventure in file [f]. *)
exception Invalid 

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* let rec play_turn state num =
   let next_state = player_turn state in 
   match get_winner (next_state num) with 
   | None -> play_turn (next_state num) num(*sophie changed*)
   | Some winner -> print_endline ("Congrats, " ^ winner ^ ", you've won!") *)

let process_winner winner play_turn next_state num theme =
  "Congrats, " ^ winner ^ ", you've won!" |> print_endline ;
  let play_again state num_cards =
    print_endline"Do you want to play again? Type YES to play again";
    let x = read_line() in 
    if x = "YES" then
      let new_game = reset_game state num_cards in
      "ROUND "^string_of_int (get_rounds new_game) |> print_endline ;
      play_turn new_game num_cards
    else
      let () = print_endline "GAME OVER!"; in  
      print_results state; exit 0; in
  play_again next_state num theme

(**[play_turn state num] is the game play of continuous player turns for a game
   with [num] cards until there is a winner. 
   The game is maintained and played through [state]. Once there is a winner
   the user is prompted if they want to play another round
   Requires: [state] is a state
          [num] is an int*)
let rec play_turn state num theme =
  let next_state = player_turn state num in 
  match get_winner next_state with 
  | None -> play_turn next_state num theme
  | Some winner -> begin 
      if is_gui next_state |> not then 
        process_winner winner play_turn next_state num theme
      else
        let play_again = print_gui_winner winner theme in 
        if play_again = "play again" then
          let new_game = reset_game next_state num in
          play_turn new_game num theme
        else 
          print_gui_results (get_player_wins next_state) theme
    end


(**[autoplayer_level num] determines which level of autoplayer the
   user chose (1 =easy, 2=medium, 3=false). In the event that the player 
   chooses a number out of range the function calls itself
   Requires: [num] is an int*)
let rec autoplayer_level num =
  let msg = 
    "What level of autoplayer do want? (1 = easy, 2 = medium, 3 = hard)" in 
  if num = 1 then print_endline msg;
  match read_int() with 
  | 1 -> "easy autoplayer"
  | 2 -> "medium autoplayer" 
  | 3 -> "hard autoplayer"
  | _ -> print_endline "Enter a valid option"; autoplayer_level 1

let get_names_input_helper action num acc count=
  match read_line () with
  | exception End_of_file -> []
  | name -> 
    if name = "" || name = "easy autoplayer" || name = "medium autoplayer" 
       ||name = "hard autoplayer"
    then let () = print_endline"Invalid name. Try again" in
      action num acc count else 
      action (num - 1) (name::acc) (count + 1)

(**[get_names num] prompts the user to enter [num] number of player names
   and creates a string list of those names
   Requires: [num] is an int*)
let get_names num = 
  let rec action num acc count = 
    if num > 0 then 
      let () = print_endline 
          ("Enter the name of player number " ^ string_of_int count ^ ".") in
      get_names_input_helper action num acc count
    else if num = 0 && List.length acc = 1 then
      let level = autoplayer_level 1 in level :: acc
    else acc in 
  action num [] 1

let init_game num num_cards gui = 
  let names = get_names num in 
  let state = intialize_game names num_cards gui NATURE in 
  play_turn state num_cards NATURE

(**[get_ans message] prints [message] and then reads in user selection returing
   the int the user enters
   Requires: [message] is a string *)
let rec get_ans message = 
  print_endline message; 
  match read_int_opt() with 
  | Some x -> 
    if x <> 7 && x <> 10 then 
      get_ans "Invalid number of cards. Please enter either 7 or 10." 
    else x
  | None -> get_ans "Invalid number of cards. Please enter either 7 or 10."


(**[rules] is the string representation of the rules of Gin Rummy *)
let rules = "
  OPTIONS: The game can be played with 1 - 4 players. If you choose the option 
  for 1 player, you will be playing against an autoplayer. You can choose the 
  level of the autoplayer: easy, medium, or hard.
  Additionally, the game can be played with player hand sizes of either 7 or 10. 

  SETUP: A hand of the specified size (7 or 10 depending on game option) is 
  dealt to each player initially. One card is flipped from the draw pile to the 
  discard pile face up and all cards remaining cards in the deck are placed face
  down in the draw pile. The first player who entered their name in game setup 
  starts.

  ON YOUR TURN: On a player's turn, they have the choice between picking up the 
  top card in the discard pile (visible to them) or the top card in the draw 
  pile (invisible to them). Then they must select a card in their current hand 
  (including the card they just drew) to discard. This card gets placed face up 
  as the new top card in the discard pile. The turn is now over.

  HOW TO WIN: A player has won when all 7 or 10 cards in their hand can be split 
  up into valid groups. A valid group is defined as a group of either three or 
  four cards that meets ONE of the following two criteria:
   1) all cards are of the same suit in consecutive order (i.e. a 5 of hearts, 6
   of hearts, 7 or hearts, and 8 of hearts) OR 
   2) all cards are of the same rank (i.e. a 6 of hearts, 6 of clubs, and 6 of 
   diamonds)

  NOTE ON VALUES: An ace is treated as a 1 and NOT a 14. Additionally, a Jack is 
  treated as value 11, a Queen is treated as value 12, and a King is treated as 
  value 13. For example, a 10 of spades, J of spades, Q of spades, and K of 
  spades is a valid hand, but a J of spades, Q of spades, K of spades, and A of 
  spades is not. You do not need to press anything when you think you have a win 
  - the game will automatically recognize your hand as a winner! 

  Good luck and have fun! 
  "
(**[rules_question] is the string representation asking whether the user
   knows the rules *)
let rules_question = 
  "Do you know the rules? Enter YES if you do. Enter anything else if you don't"

(** [main ()] prompts for the game to play, then starts it. *)
let clear_window color = 
  let fg = foreground in
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color fg

let tuple_first = function
  | a,b -> a

let tuple_second = function
  | a,b -> b

let new_theme st theme = 
  if (clicked_dark st) then 
    let () = make_sound () in open_screen DARK; DARK 
  else if (clicked_nature st) then 
    let () = make_sound () in open_screen NATURE; NATURE 
  else if (clicked_cornell st) then 
    let () = make_sound () in open_screen CORNELL; CORNELL
  else theme

let rec select_screen theme =
  let st = wait_next_event [Button_down; Key_pressed] in
  if clicked_rules st then (
    make_sound (); show_rules theme; open_screen theme; select_screen theme
  )
  else if clicked_game st then (
    make_sound ();
    let info = game_setup theme in 
    let names = tuple_first info in 
    let names_final = if List.length names = 1 then 
        List.rev (auto_player_select theme::names) else names in
    let num_cards = tuple_second info in
    game_screen theme;
    let state = intialize_game (List.rev names_final) num_cards true theme in 
    play_turn state num_cards theme
  )
  else select_screen (new_theme st theme) 

let start_gui () = 
  open_graph " 600x750"; 
  open_screen NATURE;
  select_screen NATURE;
  while true do () done

(** [main ()] prompts for the game to play, then starts it. *)
let start_cli () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Gin Rummy.\n");
  print_endline(rules_question);
  let x = read_line() in if x <> "YES" then print_endline rules else 
    print_endline "Skipping Rules";
  let ans = get_ans "Would you like to play with 10 or 7-card hands?" in
  let invalid_num_players = 
    "Invalid number of players. Please enter a whole number between 1 and 4." in 
  let rec get_numbers message = 
    print_endline message; 
    match read_int_opt() with 
    | Some x -> 
      if x > 4 || x < 1 then get_numbers invalid_num_players
      else init_game x ans false
    | None -> 
      get_numbers "Invalid. Please enter a whole number between 1 and 4." in 
  get_numbers "Enter number of players:" 

let main () =
  let version_question = 
    "Would you like to play the CLI? Type YES to play in the command line
   or anything else to play the GUI" in 
  print_endline version_question;
  let response = read_line() in if response = "YES" then start_cli () 
  else start_gui ()

(* Execute the game engine. *)
let () = main ()
