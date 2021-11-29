(**Module for State*)
open Playerhand
open Deckstack
open Card
open ANSITerminal
open Autoplayer
open Gui 
open Graphics


type player = {
  hand : Playerhand.deck;
  name : string;
  wins: int;
}

type result = Some of string | None

type t = {
  player_turn : player;
  discard_deck : Deckstack.deck;
  stockpile: Deckstack.deck;
  players: player list;
  winner : result;(* player's name  *)
  rounds: int;
  gui : bool; (* true if gui, false otherwise*)
  theme : theme;
}

let pp_card card = string_of_int card.rank

let pp_player player = player.name

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t' in 
    loop 0 "" lst in 
  "[" ^ pp_elts lst ^ "]"

let rec get_cards suit rank res =
  match rank with
  | 0 -> res
  | _ -> get_cards suit (rank - 1) ({rank = rank ; suit = suit} :: res)

let standard_deck =
  (get_cards HEARTS 13 [])@(get_cards SPADES 13 [])@
  (get_cards DIAMONDS 13 [])@(get_cards CLUBS 13 [])

let rec make_play names decks result = 
  match (names, decks) with
  | (h_n::t_n, h_d::t_d) -> 
    make_play t_n t_d ({hand = h_d; name = h_n; wins = 0}::result)
  | (_,_) -> result


let rec check_init_hands players =
  match players with 
  | [] -> ()
  | h :: t -> 
    if is_winning_hand h.hand then 
      let () = h.name ^ " is the winner. Lucky Draw!"|> print_endline in exit 0 
    else check_init_hands t

let intialize_game (names: string list) (num_cards: int) g theme =
  let stock = shuffle standard_deck in
  let decks = hands stock (List.length names) num_cards in
  let remain_deck = fst decks in
  let popped_deck = pop remain_deck in
  let discard = push (fst popped_deck) [] in
  let new_stock = snd popped_deck in
  let players_init = make_play names (snd decks) [] in
  let () = check_init_hands players_init in
  {
    player_turn = List.nth players_init 0;
    discard_deck = discard;
    stockpile = new_stock;
    players = players_init;
    winner = None;
    rounds = 1;
    gui = g;
    theme = theme;
  }


let rec edit_players_next_round decks players res =
  match players, decks with
  | (h::t, h_d::t_d) -> 
    {h with hand = h_d}::res |> edit_players_next_round t_d t 
  |(_,_) -> List.rev res


let reset_game state num_cards =
  let stock = shuffle standard_deck in
  let decks = hands stock (List.length state.players) (num_cards) in
  let remain_deck = fst decks in
  let popped_deck = pop remain_deck in
  let dis = push (fst popped_deck) [] in
  let new_stock = snd popped_deck in
  let players_new = edit_players_next_round (snd decks) state.players [] in
  {
    player_turn = List.nth players_new 0;
    discard_deck = dis;
    stockpile = new_stock;
    players = players_new;
    winner = None;
    rounds = state.rounds + 1;
    gui = state.gui;
    theme = state.theme
  }

let rec edit_players players person new_hand win_num =
  let edit_function player = 
    if player.name = person.name then 
      { player with hand = new_hand; wins = win_num }
    else player in 
  List.map edit_function players

let rec get_new_player players name =
  match players with
  | [] -> raise (Failure "Not found")
  | h::t -> if h.name = name then h else get_new_player t name

let discard st card =
  let new_player_hand = remove_card card st.player_turn.hand in
  let new_players = edit_players st.players st.player_turn new_player_hand 
      st.player_turn.wins in
  {
    st with discard_deck = push card st.discard_deck; 
            players = new_players;
            player_turn = get_new_player new_players st.player_turn.name
  }

(**[draw_helper st deck popped_deck] edits the new_player hand and adds the card
   they drew to their hand *)
let draw_helper st deck popped_deck=
  let new_player_hand = (fst popped_deck)::st.player_turn.hand in
  edit_players st.players st.player_turn new_player_hand st.player_turn.wins

let draw_from_discard st =
  let popped_deck = pop st.discard_deck in
  let new_players = draw_helper st st.discard_deck popped_deck in
  {st with discard_deck = snd popped_deck; 
           players = new_players; player_turn = get_new_player new_players
                                      st.player_turn.name} 

let draw_from_stockpile st =
  let popped_deck = pop st.stockpile in
  let new_players = draw_helper st st.stockpile popped_deck in
  {st with stockpile = snd popped_deck; 
           players = new_players; player_turn = get_new_player new_players
                                      st.player_turn.name } 

let reset_deck st =
  if is_empty st.stockpile then
    let new_stock = shuffle st.discard_deck in
    let popped_deck = pop new_stock in
    let new_discard = push (fst popped_deck) [] in
    {st with discard_deck = new_discard; stockpile = snd popped_deck}
  else st

let not_move = "That's not a possible move. Do you want to quit? 
Type YES to quit or anything else to keep playing"

let begin_turn_cli st deck = 
  if st.player_turn.name <> "easy autoplayer" && 
     st.player_turn.name <> "medium autoplayer" && 
     st.player_turn.name <> "hard autoplayer" then
    print_endline "Your cards currently are: ";
  print_all_cards st.player_turn.hand; 
  print_endline "What would you like to do?\n
                1. Draw from stockpile\n
                2. Take the top card on the discard pile\n
                The top card on the discard pile is: \n";
  peek deck; 
  print_endline "\nEnter the number of your choice: "

let cli_decision state deck num = 
  match state.player_turn.name with
  | "easy autoplayer" -> pick_easy
  | "medium autoplayer" -> 
    let card = pop deck |> fst in 
    pick_med card state.player_turn.hand
  | "hard autoplayer" -> 
    let card = pop deck |> fst in 
    pick_hard card state.player_turn.hand num
  | _ -> read_int_opt()

let player_draw_cli st deck num player_draw = 
  let () = begin_turn_cli st deck in 
  let decision = cli_decision st deck num in 
  match decision with
  | Some 1 -> draw_from_stockpile st
  | Some 2 -> draw_from_discard st
  | _ ->
    print_endline not_move;
    let x = read_line() in 
    if x = "YES" then let () = print_endline("GAME OVER. NO WINNER") in exit 0  
    else erase Screen; player_draw deck st num

let player_decision state deck num = 
  match state.player_turn.name with
  | "easy autoplayer" -> pick_easy
  | "medium autoplayer" -> 
    let card = pop deck |> fst in 
    pick_med card state.player_turn.hand
  | "hard autoplayer" -> 
    let card = pop deck |> fst in 
    pick_hard card state.player_turn.hand num
  | _ -> Some 3

let player_draw_gui state deck num = 
  match player_decision state deck num with
  | Some 1 -> 
    let () = print_auto_message state.theme 1 in 
    draw_from_stockpile state
  | Some 2 -> 
    let () = print_auto_message state.theme 2 in 
    draw_from_discard state
  | _ -> begin
      draw_card 200 70 (List.nth state.discard_deck 0);
      let () = display_hand state.player_turn.hand  in
      let d_type = choose_pile () in
      cover_discard state.theme; cover_auto_message state.theme;
      if d_type =  DISCARD then draw_from_discard state 
      else draw_from_stockpile state
    end

let rec player_draw deck st num =
  if not st.gui then player_draw_cli st deck num player_draw 
  else player_draw_gui st deck num 

let player_turn_decision st num = 
  match st.player_turn.name with 
  | "easy autoplayer" -> discard_easy num
  | "medium autoplayer" -> discard_med st.player_turn.hand st.player_turn.hand
  | "hard autoplayer" -> discard_hard st.player_turn.hand num
  | _ -> read_int_opt()

let cli_player_cards cards num st player_cards = 
  print_endline "Your Cards are: ";
  print_all_cards cards; 
  print_endline "What card would you like to discard? ";
  print_all_cards_nums cards num;
  print_endline "Enter the number of your choice: ";
  let decision = player_turn_decision st num in
  match decision with
  | None -> 
    erase Screen; print_endline "Invalid move. Try again.";
    player_cards cards st num
  | Some x -> 
    print_int x; 
    if num = 7 && x > 8 || num = 10 && x > 11 || x < 1 then 
      let () = erase Screen; print_endline "Invalid move. Try again."; in 
      player_cards cards st num
    else List.nth (List.rev cards) (x-1) |> discard st 

let gui_player_cards st num cards player_cards = 
  if st.player_turn.name = "easy autoplayer" || 
     st.player_turn.name = "medium autoplayer" || 
     st.player_turn.name = "hard autoplayer" then
    let decision = player_turn_decision st num  in
    match decision with
    | None -> 
      erase Screen; print_endline "Invalid move. Try again." ;
      player_cards cards st num
    | Some x -> List.nth (List.rev cards) (x-1) |> discard st 
  else
    let () = print_player_gui st.player_turn.name st.theme 
        "Discard a card from your hand" in
    let () = Gui.display_hand st.player_turn.hand in
    let decision = select_card_from_hand (num + 1) in 
    cover_hand_cards st.theme;
    List.nth cards (decision -1) |> discard st 

(**[player_cards cards st num] displays the [cards] and uses [st] to
   execute the player's turn based on the choices they enter
   Requires: [st] is state and [cards] is a card list *)
let rec player_cards cards st num =
  if not st.gui then cli_player_cards cards num st player_cards
  else gui_player_cards st num cards player_cards

(**[find_new_index_player p players ind player_count] is the index int of where 
   the next player's turn index is from where [p] is in [players] and then wraps 
   around if the next index is out of range*)
let rec find_new_index_player p players ind player_count =
  match players with 
  | [] -> raise (Failure "Not Found")
  | h :: t -> 
    if h = p then (ind + 1) mod player_count
    else find_new_index_player p t (ind + 1) player_count 

let turn_announcement st = 
  if not st.gui then 
    "\n"^st.player_turn.name ^ " it is your turn! \n" |> print_endline
  else print_player_gui st.player_turn.name st.theme 
      "Choose from the discard pile or stock pile"

let rec print_results_helper players =
  match players with
  | [] -> print_endline "";
  | h::t -> 
    h.name ^" won: "^(string_of_int h.wins)^" games " |> print_endline; 
    print_results_helper t

let rec print_results st =
  print_endline "The results are: ";
  print_results_helper st.players

let player_turn st num =
  let () = turn_announcement st in
  let check_reset_st = reset_deck st in
  let new_st = player_draw check_reset_st.discard_deck check_reset_st num in
  let make_move_state = player_cards new_st.player_turn.hand new_st num in
  let determine_winner = is_winning_hand make_move_state.player_turn.hand in
  let player_count = List.length make_move_state.players in 
  let ind = find_new_index_player make_move_state.player_turn 
      make_move_state.players 0 player_count in 
  if not determine_winner then 
    {make_move_state with player_turn = List.nth make_move_state.players ind}
  else 
    let new_player = make_move_state.player_turn in 
    let win_number = new_player.wins + 1 in 
    let new_players = edit_players make_move_state.players new_player 
        new_player.hand win_number in
    { make_move_state with players = new_players; 
                           winner = Some new_player.name; 
                           player_turn = new_player}


let get_winner st =
  st.winner

let get_rounds st =
  st.rounds

let rec player_wins players res =
  match players with
  | [] -> List.rev res
  | h::t -> 
    (h.name, string_of_int h.wins)::res |> player_wins t 

let get_player_wins st =
  player_wins st.players []

let is_gui st =
  st.gui