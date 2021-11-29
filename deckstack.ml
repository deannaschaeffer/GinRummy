(**Deckstack module*)
open Card

type deck = Card.card list 

let pop = function 
  | [] -> failwith "Empty"
  | h :: t -> (h, t)

let push card deck = 
  card :: deck

let peek deck = 
  match deck with 
  | [] -> failwith "Empty"
  | h :: t -> print_card h

let is_empty deck = 
  deck = []

let shuffle deck = 
  Random.self_init();
  let map_function x = (Random.bits(), x) in 
  let rand_assign = List.map map_function deck in 
  let sorted = List.sort compare rand_assign in 
  List.map (fun (z, y) -> y) sorted

(**[create n lsts] is a list of length n empty lists *)
let rec create n lsts =
  match n with 
  | 1 -> lsts
  | x -> create (n - 1) ([] :: lsts)

(**[find_min lsts min min_lst] is the list in [lsts] that
   is the minimum length *)
let rec find_min lsts min min_lst =
  match lsts with 
  | [] -> min_lst
  | h :: t -> 
    if List.length h < min then find_min t (List.length h) h 
    else find_min t min min_lst

(**[prepend el lsts min num] is the list with element [el] prepended to the 
   list of min length [min] in [lsts] up until all of the elements in [lsts]
   have a length of [num] *)
let rec prepend el lsts min num = 
  match lsts with 
  | h :: t -> 
    if h = min && List.length h < num then (el :: min) :: t 
    else h :: (prepend el t min num)
  | [] -> lsts 

(**[split d lsts num_players deck_size num_cards] is the tuple where 
   the first element is the remaining [deck] after dealing [num_cards] cards 
    (removing [num_cards] elements) each to [num_players] players and 
    the second element is a double list of each player's initial hand *)
let rec split deck lsts num_players deck_size num_cards =
  let min = find_min lsts max_int [] in
  match deck with
  | h :: t -> 
    if deck_size - (num_cards * num_players) - 1 < List.length t  
    then split t (prepend h lsts min num_cards) num_players deck_size num_cards
    else (deck, lsts)
  | [] -> ([], [])

let hands deck num_players num_cards =
  let lst = [] in
  let lsts = create num_players [lst] in
  split deck lsts num_players (List.length deck) num_cards


