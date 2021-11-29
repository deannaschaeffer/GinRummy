open OUnit2
open Card
open Deckstack
open Playerhand
open State
open Autoplayer

(********************************************************************
   Test Plan: 

   For our project, we created seven different modules: Card, Gui, State, Main, 
   Deckstack, Autoplayer, and Playerhand. We manually tested the first four 
   modules listed, and used OUnit and manual tests for the remaining modules. 
   Since many modules were best tested manually, we decided not to use bisect. 

   The Card module involves printing cards in the terminal, so the players’ 
   hands are visible to them, and the best way to test this is to see what the 
   cards look like when printed in the terminal, so the correctness of this 
   module is best shown with manual testing. 

   The Gui module contains all of the functionality of the GUI, which involves 
   everything the player can see and do when they choose the GUI option, 
   including buttons for different game options, colors, cards, and helpful 
   text. Since all of these components are visual, correctness of this module 
   is shown by testing manually. 

   The State module involves updating the information associated with the game 
   and each player when a new move or step occurs. Since this module consists 
   of changes as the game is played, the best way to test it is to manually 
   play the game and observe that it updates correctly at each move, which it 
   does. 

   The Main module involves connecting all of the components and communicating 
   with the player in order to play the game. Clearly, the only way to test 
   this module is to manually play the game.

   The Deckstack module involves basic functions of the deck including 
   removing a card, adding a card, shuffling, and dealing hands to a certain 
   number of players. All of these functions are best tested using OUnit with 
   manual testing used to double check correctness. The OUnit tests for this 
   module involved both black box and glass box testing because it was 
   important to ensure the correct output of these functions given a deck or 
   list of cards simply based on the specifications since that is what will 
   occur when a player plays the game. However, It was also somewhat important 
   to understand and test the specific ways in which the program shuffles or 
   deals hands to players using glass box testing. 

   The Autoplayer module involves all of the logic behind the various levels of 
   the autoplayer and how it chooses which pile to pick from and which card to 
   discard on each turn. The easy level involved choosing randomly, so it was 
   tested manually. The medium and hard levels involved some more complicated 
   logic, so OUnit tests were used to ensure their correctness. Glass box 
   testing was mainly used because the ways in which the medium and hard 
   autoplayer made decisions each turn was based on their implementations, so 
   we used tests to ensure that our specific logic was working correctly in all 
   situations. Manual testing was also used to confirm that the autoplayer was 
   integrated correctly and that it was operating correctly while the game was 
   being played.

   The Playerhand module focuses on determining whether a player’s hand is a 
   winning hand or not given the rules of the game. The functions used in this 
   module involve complicated logic that was best tested using OUnit tests. 
   Some of the OUnit tests were used to test the helper functions that looked 
   at subcomponents of a winning hand, and these involved more glass box 
   testing. The main is_winning_hand function focused more on black box testing 
   because the implementation is less important for determining a winning hand. 
   If the game is going to work correctly, a winning hand must be found 
   correctly no matter the implementation. Manual testing was also used to 
   confirm that a winning hand was correctly identified in a real game. 

 ********************************************************************)

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)


let c1 = {rank = 1; suit = HEARTS}
let c2 = {rank = 2; suit = CLUBS}
let c3 = {rank = 8; suit = HEARTS}
let d = [c1]

let deck_a = [
  {rank = 9; suit = DIAMONDS};
  {rank = 10; suit = DIAMONDS};
  {rank = 11; suit = DIAMONDS};
  {rank = 2; suit = DIAMONDS};
  {rank = 3; suit = DIAMONDS};
  {rank = 4; suit = DIAMONDS};
  {rank = 5; suit = DIAMONDS};
  {rank = 9; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 11; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
  {rank = 9; suit = SPADES};
  {rank = 3; suit = CLUBS};
  {rank = 10; suit = SPADES};
  {rank = 4; suit = CLUBS};
  {rank = 7; suit = HEARTS};
  {rank = 5; suit = CLUBS};
  {rank = 6; suit = SPADES};
  {rank = 2; suit = CLUBS};
]

let hand1 = [
  {rank = 5; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 5; suit = DIAMONDS};
  {rank = 3; suit = DIAMONDS};
  {rank = 11; suit = DIAMONDS};
  {rank = 9; suit = DIAMONDS};
]

let hand2 = [
  {rank = 6; suit = HEARTS};
  {rank = 11; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 4; suit = DIAMONDS};
  {rank = 2; suit = DIAMONDS};
  {rank = 10; suit = DIAMONDS};
]

let remains = [
  {rank = 9; suit = SPADES};
  {rank = 3; suit = CLUBS};
  {rank = 10; suit = SPADES};
  {rank = 4; suit = CLUBS};
  {rank = 7; suit = HEARTS};
  {rank = 5; suit = CLUBS};
  {rank = 6; suit = SPADES};
  {rank = 2; suit = CLUBS};
]

let hand3 = [
  {rank = 7; suit = HEARTS};
  {rank = 3; suit = CLUBS};
  {rank = 5; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 5; suit = DIAMONDS};
  {rank = 2; suit = DIAMONDS};
  {rank = 9; suit = DIAMONDS};
]

let hand4 = [
  {rank = 5; suit = CLUBS};
  {rank = 10; suit = SPADES};
  {rank = 6; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 3; suit = DIAMONDS};
  {rank = 10; suit = DIAMONDS};
]

let hand5 = [
  {rank = 6; suit = SPADES};
  {rank = 4; suit = CLUBS};
  {rank = 9; suit = SPADES};
  {rank = 11; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 4; suit = DIAMONDS};
  {rank = 11; suit = DIAMONDS};
]

let deckstack_pop_test 
    (name : string) 
    (deck: Deckstack.deck)
    (input: Card.card) 
    (expected_output : Card.card) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (fst (pop (push input deck))))

let deck_compare a b =
  if a.rank > b.rank then 1 else if a.rank < b.rank then -1
  else if a.suit = HEARTS then 1 else if b.suit = HEARTS then -1
  else if a.suit = SPADES then 1 else if b.suit = SPADES then -1
  else if a.suit = CLUBS then 1 else if b.suit = CLUBS then -1 else 0

let deckstack_shuffle_test 
    (name : string) 
    (deck: Deckstack.deck)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (List.sort deck_compare (shuffle deck)
         = List.sort deck_compare deck))

let deckstack_shuffle2_test 
    (name : string) 
    (deck: Deckstack.deck)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (shuffle deck <>  deck))

let deckstack_empty_test 
    (name : string) 
    (deck: Deckstack.deck)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (is_empty deck))

let rec printlst l = 
  match l with
  | h :: t -> string_of_int h.rank ^ ";" ^ printlst t
  | [] -> ""

let rec printlsts ll = 
  match ll with
  | h :: t ->  "[" ^ printlst h ^ "]" ^ ";" ^ printlsts t
  | [] -> ""

let deckstack_intitialhands_test 
    (name : string) 
    (deck : Deckstack.deck)
    (num_players : int)
    (num_cards : int)
    (expected_output : Card.card list * Card.card list list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (hands deck num_players num_cards) )

let deckstack_tests =
  [
    deckstack_pop_test "push and pop" d c2 c2; 
    deckstack_shuffle_test "shuffle-same deck" deck_a true;
    deckstack_shuffle_test "shuffle-same deck 2" hand1 true;
    deckstack_shuffle2_test "shuffle changes order" deck_a true;
    deckstack_shuffle2_test "shuffle changes order 2" hand2 true;
    deckstack_shuffle2_test "shuffle changes order 3" hand1 true;
    deckstack_empty_test "empty" deck_a false;
    deckstack_intitialhands_test "initial hands dealt correclty-2 players" 
      deck_a 2 7 (remains, [hand1; hand2]);
    deckstack_intitialhands_test "initial hands dealt correctly-3 players" 
      deck_a 3 7 ([c2], [hand3; hand4; hand5]);
  ]

let hand6 = [
  {rank = 6; suit = SPADES};
  {rank = 4; suit = CLUBS};
  {rank = 7; suit = SPADES};
  {rank = 11; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 4; suit = DIAMONDS};
  {rank = 11; suit = DIAMONDS};
  {rank = 8; suit = CLUBS};
]

let hand7 = [
  {rank = 9; suit = CLUBS};
  {rank = 12; suit = DIAMONDS};
  {rank = 9; suit = DIAMONDS};
  {rank = 4; suit = SPADES};
  {rank = 11; suit = DIAMONDS};
  {rank = 8; suit = DIAMONDS};
  {rank = 3; suit = HEARTS};
  {rank = 8; suit = SPADES};
]

let hand8 = [
  {rank = 9; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 11; suit = CLUBS};
  {rank = 2; suit = DIAMONDS};
  {rank = 12; suit = CLUBS};
  {rank = 9; suit = DIAMONDS};
  {rank = 9; suit = SPADES};
]

let hand9 = [
  {rank = 1; suit = DIAMONDS};
  {rank = 9; suit = CLUBS};
  {rank = 12; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 11; suit = CLUBS};
  {rank = 2; suit = DIAMONDS};
  {rank = 9; suit = DIAMONDS};
  {rank = 9; suit = SPADES};
]

let hand10 = [
  {rank = 4; suit = CLUBS};
  {rank = 1; suit = DIAMONDS};
  {rank = 9; suit = HEARTS};
  {rank = 11; suit = CLUBS};
  {rank = 2; suit = DIAMONDS};
  {rank = 12; suit = CLUBS};
  {rank = 9; suit = DIAMONDS};
  {rank = 9; suit = SPADES};
]

let hand11 = [
  {rank = 4; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 11; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 9; suit = DIAMONDS};
  {rank = 10; suit = SPADES};
]

let hand12 = [
  {rank = 5; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 11; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 9; suit = DIAMONDS};
  {rank = 10; suit = SPADES};
]

let hand13 = [
  {rank = 9; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 6; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 5; suit = SPADES};
  {rank = 2; suit = CLUBS};
  {rank = 9; suit = DIAMONDS};
]

let hand14 = [
  {rank = 9; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 10; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 10; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 10; suit = SPADES};
  {rank = 9; suit = DIAMONDS};
]

let hand15 = [
  {rank = 5; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 4; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 10; suit = SPADES};
  {rank = 2; suit = CLUBS};
  {rank = 9; suit = DIAMONDS};
]

let hand16 = [
  {rank = 5; suit = CLUBS};
  {rank = 9; suit = DIAMONDS};
  {rank = 1; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 7; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 9; suit = SPADES};
  {rank = 2; suit = CLUBS};
]

let hand17 = [
  {rank = 5; suit = CLUBS};
  {rank = 11; suit = DIAMONDS};
  {rank = 4; suit = HEARTS};
  {rank = 1; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 7; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 2; suit = CLUBS};
]

let hand18 = [
  {rank = 9; suit = CLUBS};
  {rank = 10; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 10; suit = CLUBS};
  {rank = 13; suit = CLUBS};
  {rank = 10; suit = SPADES};
  {rank = 13; suit = HEARTS};
  {rank = 9; suit = DIAMONDS};
]

let hand19 = [
  {rank = 5; suit = HEARTS};
  {rank = 8; suit = DIAMONDS};
  {rank = 7; suit = HEARTS};
  {rank = 6; suit = HEARTS};
  {rank = 8; suit = SPADES};
  {rank = 1; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 2; suit = CLUBS};
]

let hand20 = [
  {rank = 5; suit = CLUBS};
  {rank = 11; suit = DIAMONDS};
  {rank = 4; suit = HEARTS};
  {rank = 1; suit = HEARTS};
  {rank = 10; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 7; suit = CLUBS};
]

let hand21 = [
  {rank = 5; suit = CLUBS};
  {rank = 3; suit = DIAMONDS};
  {rank = 10; suit = DIAMONDS};
  {rank = 8; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 7; suit = CLUBS};
]

let hand22 = 
  [
    {rank = 4; suit = SPADES};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = SPADES};
    {rank = 2; suit = HEARTS};
    {rank = 6; suit = SPADES};
    {rank = 2; suit = DIAMONDS};
    {rank = 4; suit = CLUBS};
  ]

let c4 = {rank = 3; suit = SPADES}

let hand23 = 
  [
    {rank = 4; suit = SPADES};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = SPADES};
    {rank = 2; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 2; suit = DIAMONDS};
    {rank = 2; suit = CLUBS};
  ]

let hand24 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = SPADES};
    {rank = 2; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 2; suit = DIAMONDS};
    {rank = 2; suit = CLUBS};
  ]

let c5 = {rank = 8; suit = SPADES}

let hand25 = 
  [
    {rank = 4; suit = SPADES};
    {rank = 3; suit = SPADES};
    {rank = 5; suit = SPADES};
    {rank = 2; suit = HEARTS};
    {rank = 6; suit = SPADES};
    {rank = 7; suit = SPADES};
    {rank = 4; suit = CLUBS};
  ]

let c6 = {rank = 1; suit = DIAMONDS}

let hand26 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = SPADES};
    {rank = 2; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 2; suit = DIAMONDS};
  ]

let c7 = {rank = 6; suit = DIAMONDS}

let hand27 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 10; suit = HEARTS};
  ]

let hand28 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 2; suit = CLUBS};
  ]

let hand29 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 10; suit = HEARTS};
  ]

let hand30 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 2; suit = CLUBS};
    {rank = 2; suit = DIAMONDS};
    {rank = 8; suit = CLUBS};
  ]

let hand31 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 2; suit = CLUBS};
    {rank = 1; suit = DIAMONDS};
    {rank = 8; suit = CLUBS};
  ]

let hand32 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 6; suit = CLUBS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 2; suit = CLUBS};
    {rank = 2; suit = DIAMONDS};
  ]

let hand33 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 6; suit = CLUBS};
    {rank = 1; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 2; suit = CLUBS};
  ]

let hand34 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 7; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 6; suit = DIAMONDS};
    {rank = 8; suit = SPADES};
    {rank = 2; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 3; suit = DIAMONDS};
    {rank = 2; suit = CLUBS};
  ]

let hand35 = 
  [
    {rank = 3; suit = DIAMONDS};
    {rank = 7; suit = HEARTS};
    {rank = 8; suit = HEARTS};
    {rank = 6; suit = DIAMONDS};
    {rank = 9; suit = HEARTS};
    {rank = 2; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = CLUBS};
    {rank = 2; suit = HEARTS};
    {rank = 4; suit = DIAMONDS};
  ]

let hand36 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 2; suit = CLUBS};
    {rank = 8; suit = CLUBS};
  ]

let c8 = {rank = 4; suit = DIAMONDS}

let hand37 = 
  [
    {rank = 8; suit = DIAMONDS};
    {rank = 2; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 2; suit = CLUBS};
    {rank = 8; suit = CLUBS};
  ]

let c9 = {rank = 6; suit = SPADES}

let hand38 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 9; suit = SPADES};
    {rank = 5; suit = DIAMONDS};
    {rank = 12; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 6; suit = DIAMONDS};
    {rank = 2; suit = CLUBS};
    {rank = 7; suit = SPADES};
  ]

let c10 = {rank = 6; suit = HEARTS}

let hand39 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 9; suit = SPADES};
    {rank = 6; suit = CLUBS};
    {rank = 12; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 6; suit = DIAMONDS};
    {rank = 2; suit = CLUBS};
    {rank = 7; suit = SPADES};
  ]

let c11 = {rank = 10; suit = CLUBS}

let hand40 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 6; suit = DIAMONDS};
    {rank = 8; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 5; suit = DIAMONDS};
    {rank = 8; suit = CLUBS};
  ]

let c12 = {rank = 8; suit = DIAMONDS}

let hand41 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 6; suit = DIAMONDS};
    {rank = 12; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 5; suit = DIAMONDS};
    {rank = 8; suit = CLUBS};
  ]

let hand42 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 13; suit = DIAMONDS};
    {rank = 12; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 8; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 5; suit = DIAMONDS};
    {rank = 8; suit = CLUBS};
  ]

let hand43 = 
  [
    {rank = 4; suit = DIAMONDS};
    {rank = 13; suit = DIAMONDS};
    {rank = 12; suit = DIAMONDS};
    {rank = 2; suit = HEARTS};
    {rank = 10; suit = HEARTS};
    {rank = 7; suit = SPADES};
    {rank = 3; suit = DIAMONDS};
    {rank = 8; suit = HEARTS};
    {rank = 5; suit = DIAMONDS};
    {rank = 7; suit = DIAMONDS};
  ]

let autoplayer_pickmed_test 
    (name : string) 
    (top : Card.card)
    (stock : Deckstack.deck)  
    (expected_output : int option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pick_med top stock) )

let autoplayer_discardmed_test 
    (name : string) 
    (hand : Deckstack.deck)
    (hand_remains : Deckstack.deck)  
    (expected_output : int option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (discard_med hand hand_remains))

let print_int_opt (num : int option) =
  match num with
  | None -> "none"
  | Some x -> string_of_int x

let autoplayer_discardhard_test 
    (name : string) 
    (hand : Deckstack.deck)
    (num : int)  
    (expected_output : int option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output(discard_hard hand num)~printer:print_int_opt)

let autoplayer_pickhard_test 
    (name : string) 
    (top : Card.card)
    (hand : Deckstack.deck)
    (num : int)  
    (expected_output : int option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output(pick_hard top hand num)
        ~printer:print_int_opt)

let autoplayer_tests = 
  [
    autoplayer_pickmed_test "pick from discard-same rank" c2 hand3 (Some 2);
    autoplayer_pickmed_test "pick from stock" c1 hand3 (Some 1);
    autoplayer_pickmed_test "pick from discard-consec suit" c3 hand3 (Some 2);
    autoplayer_discardmed_test "discard from hand 6" hand6 hand6 (Some 1);
    autoplayer_discardmed_test "discard 1st w/o pair" remains remains (Some 2);
    autoplayer_discardmed_test "discard from hand 7" hand7 hand7 (Some 2);
    autoplayer_discardhard_test "winning 7" hand8 7 (Some 4);
    autoplayer_discardhard_test "group of 4" hand9 7 (Some 3);
    autoplayer_discardhard_test "group of 3" hand10 7 (Some 8);
    autoplayer_discardhard_test "consec of 4" hand11 7 (Some 1);
    autoplayer_discardhard_test "consec of 3" hand12 7 (Some 1);
    autoplayer_discardhard_test "consec of 3 and group of 3" hand13 7 (Some 3);
    autoplayer_discardhard_test "only pairs" hand7 7 (Some 2);
    autoplayer_discardhard_test "2 groups of 3" hand14 7 (Some 3);
    autoplayer_discardhard_test "6 in a row" hand15 7 (Some 1);
    autoplayer_discardhard_test "2 groups of 3 + pair" hand18 7 (Some 2);
    autoplayer_discardhard_test "1 consec of 3" hand20 7 (Some 3);
    autoplayer_discardhard_test "1 consec of 3 w/ pairs" hand21 7 (Some 6);
    autoplayer_discardhard_test "2 consecs of 3-same suit" hand17 7 (Some 6);
    autoplayer_discardhard_test "2 consecs of 3-diff suit" hand19 7 (Some 4);
    autoplayer_discardhard_test "2 consecs of 3-same suit 2" hand16 7 (Some 2);
    autoplayer_pickhard_test "becomes winning hand" c2 hand22 7 (Some 2);
    autoplayer_pickhard_test "becomes winning hand 2" c4 hand23 7 (Some 2);
    autoplayer_pickhard_test "group of 4-top matches" c3 hand24 7 (Some 2);
    autoplayer_pickhard_test "group of 4-top doesn't match remains" 
      c1 hand24 7 (Some 1);
    autoplayer_pickhard_test "consec of >4-top matches remains" 
      c5 hand25 7 (Some 2);
    autoplayer_pickhard_test "becomes group of 4" c2 hand26 7 (Some 2);
    autoplayer_pickhard_test "becomes consec of 4, part of group" 
      c6 hand26 7 (Some 2);
    autoplayer_pickhard_test "becomes consec of 4" c7 hand27 7 (Some 2);
    autoplayer_pickhard_test "2 groups of 3-top doesn't match" 
      c6 hand28 7 (Some 1);
    autoplayer_pickhard_test "makes group of 3" c2 hand29 7 (Some 2);
    autoplayer_discardhard_test "winning 10" hand30 10 (Some 7);
    autoplayer_discardhard_test "3 threes" hand31 10 (Some 2);
    autoplayer_discardhard_test "1 four, 1 three" hand32 10 (Some 6);
    autoplayer_discardhard_test "2 threes" hand33 10 (Some 5);
    autoplayer_discardhard_test "2 fours" hand34 10 (Some 7);
    autoplayer_discardhard_test "2 fours, 1 three-winning" hand35 10 (Some 2);
    autoplayer_pickhard_test "becomes winning" c7 hand36 10 (Some 2);
    autoplayer_pickhard_test "3 threes, no match" c1 hand36 10 (Some 1);
    autoplayer_pickhard_test "group of 4 & 3" c8 hand37 10 (Some 2);
    autoplayer_pickhard_test "consec of 4 & 3" c9 hand38 10 (Some 1);
    autoplayer_pickhard_test "2 threes, don't become 4" c10 hand39 10 (Some 2);
    autoplayer_pickhard_test "2 fours" c11 hand40 10 (Some 2);
    autoplayer_pickhard_test "becomes 2nd group of 4" c12 hand41 10 (Some 1);
    autoplayer_pickhard_test "becomes group of 4" c12 hand42 10 (Some 2);
    autoplayer_pickhard_test "becomes consec of 4" c7 hand42 10 (Some 2);
    autoplayer_pickhard_test "becomes consec of 5" c7 hand43 10 (Some 2);
  ]

let check_consecutive_cards_test
    (name : string) 
    (cards : Card.card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_consecutive cards)
    )
let check_same_rank_test
    (name : string) 
    (cards : Card.card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_same_rank cards)
    )

let check_two_consecutive_groups_same_suit_test
    (name : string) 
    (cards : Card.card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output(check_two_consecutive_groups_same_suit cards)
    )

let check_three_consecutive_groups_same_suit_test
    (name : string) 
    (cards : Card.card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (check_three_consecutive_groups_same_suit cards)
    )

let check_two_consecutive_groups_test
    (name : string) 
    (cards : Card.card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_two_consecutive_groups cards)
    )

let check_three_consecutive_groups_test
    (name : string) 
    (cards : Card.card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_three_consecutive_groups cards)
    )

let is_winning_hand_6_test
    (name : string) 
    (cards : Card.card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_winning_hand_6 cards)
    )

let is_winning_hand_test
    (name : string) 
    (cards : Card.card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_winning_hand cards)
    )

let d1 = [
  {rank = 3; suit = HEARTS};
  {rank = 4; suit = DIAMONDS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = DIAMONDS};
]
let d2 = [
  {rank = 3; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
]
let d3 = [
  {rank = 3; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
]
let d4 = [
  {rank = 9; suit = DIAMONDS};
  {rank = 10; suit = DIAMONDS};
  {rank = 11; suit = DIAMONDS};
  {rank = 12; suit = DIAMONDS};
]
let d5 = [
  {rank = 9; suit = DIAMONDS};
  {rank = 9; suit = HEARTS};
  {rank = 9; suit = CLUBS};
  {rank = 9; suit = SPADES};
]
let d6 = [
  {rank = 9; suit = DIAMONDS};
  {rank = 10; suit = DIAMONDS};
  {rank = 11; suit = DIAMONDS};
  {rank = 2; suit = DIAMONDS};
  {rank = 3; suit = DIAMONDS};
  {rank = 4; suit = DIAMONDS};
  {rank = 5; suit = DIAMONDS};
]
let d7 = [
  {rank = 9; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 11; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
]
let d8 = [
  {rank = 9; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 7; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
]

let d9 = [
  {rank = 11; suit = HEARTS};
  {rank = 13; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 12; suit = HEARTS};
  {rank = 7; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
]
let d10 = [
  {rank = 8; suit = HEARTS};
  {rank = 11; suit = DIAMONDS};
  {rank = 10; suit = DIAMONDS};
  {rank = 12; suit = DIAMONDS};
  {rank = 7; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
]
let d11 = [
  {rank = 4; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
  {rank = 7; suit = HEARTS};
  {rank = 8; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 10; suit = HEARTS};
]
let d12 = [
  {rank = 4; suit = HEARTS};
  {rank = 5; suit = DIAMONDS};
  {rank = 6; suit = HEARTS};
  {rank = 7; suit = HEARTS};
  {rank = 8; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 10; suit = HEARTS};
]
let d13 = [
  {rank = 0; suit = SPADES};
  {rank = 3; suit = SPADES};
  {rank = 11; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 2; suit = SPADES};
  {rank = 9; suit = CLUBS};
  {rank = 1; suit = SPADES};
]
let d14 = [
  {rank = 0; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 2; suit = SPADES};
  {rank = 7; suit = CLUBS};
  {rank = 8; suit = SPADES};
  {rank = 9; suit = SPADES};
  {rank = 10; suit = SPADES};
]
let d15 = [
  {rank = 9; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 11; suit = CLUBS};
  {rank = 12; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 9; suit = HEARTS};
]
let d16 = [
  {rank = 9; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 7; suit = CLUBS};
  {rank = 8; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 9; suit = HEARTS};
]
let d17 = [
  {rank = 9; suit = CLUBS};
  {rank = 12; suit = SPADES};
  {rank = 9; suit = HEARTS};
  {rank = 12; suit = DIAMONDS};
  {rank = 12; suit = CLUBS};
  {rank = 9; suit = SPADES};
  {rank = 9; suit = DIAMONDS};
]
let d18 = [
  {rank = 8; suit = CLUBS};
  {rank = 12; suit = SPADES};
  {rank = 9; suit = HEARTS};
  {rank = 12; suit = DIAMONDS};
  {rank = 12; suit = CLUBS};
  {rank = 9; suit = SPADES};
  {rank = 9; suit = DIAMONDS};
]
let d19 = [
  {rank = 8; suit = CLUBS};
  {rank = 7; suit = CLUBS};
  {rank = 9; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 5; suit = CLUBS};
  {rank = 9; suit = SPADES};
  {rank = 9; suit = DIAMONDS};
]
let d20 = [
  {rank = 1; suit = HEARTS};
  {rank = 2; suit = CLUBS};
  {rank = 2; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 3; suit = CLUBS};
  {rank = 3; suit = SPADES};
  {rank = 3; suit = DIAMONDS};
]
let d21 = [
  {rank = 4; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 5; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 7; suit = CLUBS};
]
let d22 = [
  {rank = 4; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 5; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 9; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 7; suit = CLUBS};
  {rank = 8; suit = CLUBS};
]
let d23 = [
  {rank = 5; suit = CLUBS};
  {rank = 1; suit = CLUBS};
  {rank = 11; suit = CLUBS};
  {rank = 3; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 9; suit = CLUBS};
  {rank = 6; suit = CLUBS};
  {rank = 7; suit = CLUBS};
  {rank = 12; suit = CLUBS};
]
let d24 = [
  {rank = 5; suit = HEARTS};
  {rank = 1; suit = HEARTS};
  {rank = 11; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 2; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 6; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 12; suit = HEARTS};
]
let d25 = [
  {rank = 5; suit = SPADES};
  {rank = 1; suit = SPADES};
  {rank = 11; suit = SPADES};
  {rank = 3; suit = SPADES};
  {rank = 10; suit = SPADES};
  {rank = 2; suit = SPADES};
  {rank = 9; suit = SPADES};
  {rank = 6; suit = SPADES};
  {rank = 7; suit = SPADES};
  {rank = 8; suit = SPADES};
]
let d26 = [
  {rank = 5; suit = SPADES};
  {rank = 1; suit = SPADES};
  {rank = 11; suit = SPADES};
  {rank = 3; suit = SPADES};
  {rank = 12; suit = SPADES};
  {rank = 2; suit = SPADES};
  {rank = 9; suit = SPADES};
  {rank = 6; suit = SPADES};
  {rank = 7; suit = SPADES};
  {rank = 8; suit = SPADES};
]
let d27 = [
  {rank = 11; suit = SPADES};
  {rank = 10; suit = SPADES};
  {rank = 9; suit = SPADES};
  {rank = 5; suit = SPADES};
  {rank = 7; suit = SPADES};
  {rank = 4; suit = SPADES};
  {rank = 8; suit = SPADES};
  {rank = 3; suit = SPADES};
  {rank = 2; suit = SPADES};
  {rank = 1; suit = SPADES};
]
let d28 = [
  {rank = 11; suit = SPADES};
  {rank = 10; suit = SPADES};
  {rank = 9; suit = SPADES};
  {rank = 5; suit = SPADES};
  {rank = 7; suit = SPADES};
  {rank = 4; suit = SPADES};
  {rank = 8; suit = SPADES};
  {rank = 3; suit = SPADES};
  {rank = 2; suit = SPADES};
  {rank = 1; suit = SPADES};
]
let d29 = [
  {rank = 11; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 9; suit = CLUBS};
  {rank = 5; suit = HEARTS};
  {rank = 7; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 6; suit = HEARTS};
  {rank = 3; suit = SPADES};
  {rank = 2; suit = SPADES};
  {rank = 1; suit = SPADES};
]
let d30 = [
  {rank = 10; suit = CLUBS};
  {rank = 8; suit = HEARTS};
  {rank = 7; suit = HEARTS};
  {rank = 4; suit = SPADES};
  {rank = 11; suit = CLUBS};
  {rank = 6; suit = HEARTS};
  {rank = 3; suit = SPADES};
  {rank = 12; suit = CLUBS};
  {rank = 2; suit = SPADES};
  {rank = 1; suit = SPADES};
]
let d31 = [
  {rank = 10; suit = HEARTS};
  {rank = 8; suit = HEARTS};
  {rank = 7; suit = CLUBS};
  {rank = 4; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 6; suit = CLUBS};
  {rank = 3; suit = HEARTS};
  {rank = 5; suit = CLUBS};
  {rank = 2; suit = HEARTS};
  {rank = 11; suit = HEARTS};
]
let d32 = [
  {rank = 10; suit = CLUBS};
  {rank = 8; suit = HEARTS};
  {rank = 7; suit = CLUBS};
  {rank = 4; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 6; suit = CLUBS};
  {rank = 3; suit = HEARTS};
  {rank = 5; suit = CLUBS};
  {rank = 2; suit = HEARTS};
  {rank = 11; suit = HEARTS};
]
let d33 = [
  {rank = 10; suit = CLUBS};
  {rank = 8; suit = CLUBS};
  {rank = 7; suit = CLUBS};
  {rank = 4; suit = HEARTS};
  {rank = 9; suit = CLUBS};
  {rank = 6; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 5; suit = CLUBS};
  {rank = 2; suit = HEARTS};
  {rank = 11; suit = CLUBS};
]
let d34 = [
  {rank = 7; suit = HEARTS};
  {rank = 9; suit = SPADES};
  {rank = 3; suit = CLUBS};
  {rank = 10; suit = SPADES};
  {rank = 13; suit = SPADES};
  {rank = 2; suit = CLUBS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
  {rank = 11; suit = SPADES};
  {rank = 1; suit = CLUBS};
]
let d35 = [
  {rank = 10; suit = SPADES};
  {rank = 10; suit = HEARTS};
  {rank = 10; suit = DIAMONDS};
  {rank = 5; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 3; suit = HEARTS};

]
let d36 = [
  {rank = 3; suit = SPADES};
  {rank = 10; suit = SPADES};
  {rank = 10; suit = HEARTS};
  {rank = 3; suit = CLUBS};
  {rank = 10; suit = DIAMONDS};
  {rank = 3; suit = HEARTS};
]
let d37 = [
  {rank = 11; suit = DIAMONDS};
  {rank = 5; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 9; suit = DIAMONDS};
  {rank = 3; suit = HEARTS};
  {rank = 10; suit = DIAMONDS};
]
let d38 = [
  {rank = 5; suit = SPADES};
  {rank = 5; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 5; suit = DIAMONDS};
  {rank = 3; suit = HEARTS};
  {rank = 5; suit = CLUBS};
]
let d39 = [
  {rank = 3; suit = SPADES};
  {rank = 3; suit = HEARTS};
  {rank = 3; suit = CLUBS};
  {rank = 2; suit = CLUBS};
  {rank = 4; suit = CLUBS};
  {rank = 5; suit = CLUBS};
]
let d40 = [
  {rank = 4; suit = DIAMONDS};
  {rank = 4; suit = SPADES};
  {rank = 4; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 11; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
]
let d41 = [
  {rank = 9; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 7; suit = DIAMONDS};
  {rank = 11; suit = HEARTS};
  {rank = 5; suit = DIAMONDS};
  {rank = 6; suit = DIAMONDS};
  {rank = 8; suit = DIAMONDS};
  {rank = 1; suit = HEARTS};
  {rank = 2; suit = HEARTS};
]
let d42 = [
  {rank = 8; suit = HEARTS};
  {rank = 11; suit = DIAMONDS};
  {rank = 3; suit = SPADES};
  {rank = 10; suit = DIAMONDS};
  {rank = 3; suit = HEARTS};
  {rank = 12; suit = DIAMONDS};
  {rank = 7; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
  {rank = 3; suit = DIAMONDS};
]
let d43 = [
  {rank = 8; suit = CLUBS};
  {rank = 8; suit = DIAMONDS};
  {rank = 8; suit = SPADES};
  {rank = 9; suit = DIAMONDS};
  {rank = 9; suit = CLUBS};
  {rank = 9; suit = SPADES};
  {rank = 10; suit = HEARTS};
  {rank = 10; suit = DIAMONDS};
  {rank = 10; suit = SPADES};
  {rank = 10; suit = CLUBS};
]
let d44 = [
  {rank = 8; suit = CLUBS};
  {rank = 8; suit = DIAMONDS};
  {rank = 8; suit = SPADES};
  {rank = 9; suit = DIAMONDS};
  {rank = 9; suit = CLUBS};
  {rank = 9; suit = SPADES};
  {rank = 10; suit = HEARTS};
  {rank = 10; suit = DIAMONDS};
  {rank = 10; suit = SPADES};
  {rank = 11; suit = CLUBS};
]
let d45 = [
  {rank = 4; suit = HEARTS};
  {rank = 5; suit = HEARTS};
  {rank = 6; suit = HEARTS};
  {rank = 7; suit = HEARTS};
  {rank = 8; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 10; suit = HEARTS};
  {rank = 10; suit = CLUBS};
  {rank = 10; suit = SPADES};
  {rank = 10; suit = DIAMONDS};
]
let d46 = [
  {rank = 9; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 11; suit = CLUBS};
  {rank = 12; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 8; suit = HEARTS};
  {rank = 8; suit = SPADES};
  {rank = 8; suit = DIAMONDS};
]
let d47 = [
  {rank = 9; suit = CLUBS};
  {rank = 10; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 7; suit = CLUBS};
  {rank = 8; suit = CLUBS};
  {rank = 9; suit = HEARTS};
  {rank = 9; suit = HEARTS};
  {rank = 11; suit = DIAMONDS};
  {rank = 11; suit = CLUBS};
  {rank = 11; suit = HEARTS};
]
let d48 = [
  {rank = 9; suit = CLUBS};
  {rank = 12; suit = SPADES};
  {rank = 9; suit = HEARTS};
  {rank = 12; suit = DIAMONDS};
  {rank = 12; suit = CLUBS};
  {rank = 9; suit = SPADES};
  {rank = 9; suit = DIAMONDS};
  {rank = 11; suit = CLUBS};
  {rank = 11; suit = HEARTS};
  {rank = 11; suit = DIAMONDS};
]
let d49 = [
  {rank = 1; suit = DIAMONDS};
  {rank = 2; suit = DIAMONDS};
  {rank = 4; suit = DIAMONDS};
  {rank = 3; suit = DIAMONDS};
  {rank = 3; suit = CLUBS};
  {rank = 3; suit = SPADES};
  {rank = 8; suit = DIAMONDS};
  {rank = 9; suit = CLUBS};
  {rank = 10; suit = HEARTS};
  {rank = 11; suit = SPADES};
]
let d50 = [
  {rank = 1; suit = DIAMONDS};
  {rank = 2; suit = DIAMONDS};
  {rank = 4; suit = CLUBS};
  {rank = 3; suit = DIAMONDS};
  {rank = 3; suit = SPADES};
  {rank = 3; suit = HEARTS};
  {rank = 4; suit = SPADES};
  {rank = 4; suit = HEARTS};
  {rank = 4; suit = DIAMONDS};
  {rank = 1; suit = SPADES};
]
let d51 = [
  {rank = 1; suit = HEARTS};
  {rank = 5; suit = DIAMONDS};
  {rank = 5; suit = CLUBS};
  {rank = 5; suit = HEARTS};
  {rank = 3; suit = HEARTS};
  {rank = 4; suit = HEARTS};
  {rank = 4; suit = SPADES};
  {rank = 2; suit = HEARTS};
  {rank = 4; suit = DIAMONDS};
  {rank = 5; suit = SPADES};
]

let playerhand_tests =
  [
    check_consecutive_cards_test "consecutive, not same suit" d1 false;
    check_consecutive_cards_test "consecutive, same suit" d2 true;
    check_consecutive_cards_test "nonconsecutive, same suit" d3 false;
    check_consecutive_cards_test "consecutive, same suit" d4 true;
    check_same_rank_test "same suit, diff rank" d4 false;
    check_same_rank_test "same suit, same rank" d5 true;
    check_same_rank_test "diff suit, same rank" d5 true;
    check_two_consecutive_groups_same_suit_test "valid 3 then 4" d6 true;
    check_two_consecutive_groups_same_suit_test "valid 4 then 3, out of order"
      d7 true;
    check_two_consecutive_groups_same_suit_test "2 in order, 5 in order" 
      d8 false;
    check_two_consecutive_groups_same_suit_test "valid" d9 true;
    check_three_consecutive_groups_same_suit_test "valid in order " d22 true;
    check_three_consecutive_groups_same_suit_test "valid all broken" d23 true;
    check_three_consecutive_groups_same_suit_test 
      "lower two together, upper separate" d24 true;
    check_three_consecutive_groups_same_suit_test 
      "lower separate, upper two together" d25 true;
    check_three_consecutive_groups_same_suit_test "invalid" d26 false;
    check_three_consecutive_groups_same_suit_test "invalid" d27 false;
    check_two_consecutive_groups_test "valid" d10 true;
    check_two_consecutive_groups_test "valid" d11 true;
    check_two_consecutive_groups_test "invalid" d12 false;
    check_two_consecutive_groups_test "valid" d13 true;
    check_two_consecutive_groups_test "six cards" d21 true;
    check_three_consecutive_groups_test "all same suit" d22 true;
    check_three_consecutive_groups_test "all same suit 2" d23 true;
    check_three_consecutive_groups_test "all same suit 3" d24 true;
    check_three_consecutive_groups_test "all same suit 3" d25 true;
    check_three_consecutive_groups_test "all same suit false" d26 false;
    check_three_consecutive_groups_test "all same suit false 2" d27 false;
    check_three_consecutive_groups_test "three different suits some together"
      d29 true;
    check_three_consecutive_groups_test "three different suits all separate"
      d30 true; 
    check_three_consecutive_groups_test "two suits all together" d31 true;
    check_three_consecutive_groups_test "one suit is off" d32 false;
    check_three_consecutive_groups_test "all consecutive 5 and 5" d33 false;
    check_three_consecutive_groups_test "4 groups all split" d34 false;
    check_three_consecutive_groups_test "3 groups, two share suit" d41 true;
    is_winning_hand_6_test "diff type groups" d35 true;
    is_winning_hand_6_test "same type groups rank " d36 true;
    is_winning_hand_6_test "same type groups incremental" d37 true;
    is_winning_hand_6_test "diff types of groups, overlapping num" d38 true;
    is_winning_hand_6_test "close but one gap" d39 false;
    is_winning_hand_test "winning" d11 true;
    is_winning_hand_test "winning" d10 true;
    is_winning_hand_test "winning" d7 true;
    is_winning_hand_test "not winning" d12 false;
    is_winning_hand_test "not winning" d14 false;
    is_winning_hand_test "winning" d15 true;
    is_winning_hand_test "winning" d16 true;
    is_winning_hand_test "winning" d17 true;
    is_winning_hand_test "not winning" d18 false;
    is_winning_hand_test "not winning" d19 true;
    is_winning_hand_test "not winning" d20 false;
    is_winning_hand_test "winning consec 1" d23 true;
    is_winning_hand_test "winning consec 2" d24 true;
    is_winning_hand_test "winning consec 3" d25 true;
    is_winning_hand_test "winning consec 4" d29 true;
    is_winning_hand_test "winning consec 5" d30 true;
    is_winning_hand_test "valid" d40 true;
    is_winning_hand_test "valid" d41 true;
    is_winning_hand_test "straightforward mixed" d42 true;
    is_winning_hand_test "consecutive same rank groups" d43 true;
    is_winning_hand_test "slightly off consecutive same rank groups" d44 false;
    is_winning_hand_test "6 consecutive and 4 same rank at top" d45 true;
    is_winning_hand_test "random test" d46 true;
    is_winning_hand_test "random test with overlap" d47 true;
    is_winning_hand_test "random consecutive multiples" d48 true;
    is_winning_hand_test "twisted up" d49 false;
    is_winning_hand_test "overlapping, close but not" d50 false;
    is_winning_hand_test "consecutive four, one needed for run" d51 true;
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    deckstack_tests;
    playerhand_tests;
    autoplayer_tests;
  ]

let _ = run_test_tt_main suite
