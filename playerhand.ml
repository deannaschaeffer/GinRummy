(**Playerhand module*)
open Card

type deck = Card.card list 

(** [pp_card card] is the string representation of a card record.  *)
let pp_card card = 
  (string_of_int card.rank)

(** [pp_int_tuple tuple] is the string representation of a tuple.  *)
let pp_int_tuple = function 
  | ( a, b) -> "(" ^ string_of_int a ^ ", " ^string_of_int b ^ ")"

(** [rank_compare a b] is a compare of two element's ranks.  *)
let rank_compare a b =  
  if a.rank > b.rank then 1 else if a.rank = b.rank then 0 else -1 

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

(** [empty] is an empty card with invalid rank*)
let empty = {
  rank = -1;
  suit = HEARTS
}

(** [group_of_same deck] is an assocative list mapping rank to the number of 
    cards of that rank in [deck].
    Required: Cards in [deck] must be ordered by rank (least to greatest) *)
let group_of_same deck =
  let rec count_deck counter prev = function 
    | [] -> if counter > 1 then (prev.rank, (counter + 1))::[] else []
    | h::t -> 
      if h.rank = prev.rank then count_deck (counter + 1) prev t else
      if counter > 1 then (prev.rank, (counter + 1))::(count_deck 0 h t) else
        count_deck 0 h t in 
  let count = count_deck 0 empty deck in count

(** [check_consecutive cards] is True if [cards] represent consecutive rank in 
    the same suit, else is False. 
    Requires: [cards] is nonempty *)
let check_consecutive cards = 
  let sorted = List.sort rank_compare cards in 
  let rec check prev = function
    | [] -> true
    | h::t -> 
      if prev.rank = -1 || (prev.rank + 1 = h.rank && prev.suit = h.suit)
      then check h t else false in 
  check empty sorted

(** [check_same_rank deck] is True if all cards in list [deck] are all the same
    rank and False otherwise. *)
let check_same_rank cards = 
  let rec check prev = function
    | [] -> true
    | h::t ->
      if prev.rank = -1 || prev.rank = h.rank then check h t else false in 
  check empty cards 

(** [check_two_consecutive_groups deck] is True if [deck] contains two groups of 
    consecutive values all of the same suit. These two groups may or may not be 
    consecutive in regard to eachother.  j
    Requires: all cards in [deck] are same suit *)
let check_two_consecutive_groups_same_suit deck = 
  let sorted = List.sort rank_compare deck in 
  let rec check prev index snd = function
    | [] -> true 
    | h::t -> 
      if index = 0 then check h (index + 1) snd t else 
      if prev.rank + 1 <> h.rank then 
        if index = 3 then check h (index + 1) true t else
        if index = 4 && not snd then check h (index + 1) true t else
          false 
      else check h (index + 1) snd t
  in check empty 0 false sorted 

let rec sort_by_suit hearts diamonds spades clubs = function 
  | [] -> [hearts;diamonds;spades;clubs]
  | h::t -> 
    match h.suit with 
    | HEARTS -> sort_by_suit (h::hearts) diamonds spades clubs t 
    | DIAMONDS -> sort_by_suit hearts (h::diamonds) spades clubs t 
    | SPADES -> sort_by_suit hearts diamonds (h::spades) clubs t 
    | CLUBS -> sort_by_suit hearts diamonds spades (h::clubs) t

(**tested  *)
let check_three_consecutive_groups_same_suit deck = 
  let sorted = List.sort rank_compare deck in 
  let rec check prev curr = function
    | [] -> curr::[] 
    | h::t -> 
      match prev with 
      | None -> check (Some h) [h] t
      | Some p -> 
        if p.rank + 1 <> h.rank then  curr ::(check (Some h) [h] t) else
          check (Some h) (h::curr) t  
  in 
  let groups = check None [] sorted in 
  let rec check_groups  = function
    | [] -> true
    | h::t -> 
      let num = List.length h in 
      (num = 3 || num = 4 || num = 6 || num = 7 || num = 10) && check_groups t
  in 
  (List.flatten groups |> List.length) = 10 && check_groups groups 

(** tested! *)
let check_three_consecutive_groups deck =
  let buckets = sort_by_suit [] [] [] [] deck in 
  let rec count_groups count = function 
    | [] ->  count 
    | h::t -> 
      let l = List.length h in 
      if l <> 3 && l<>4 && l<>7 && l<>6 && l<>10 then count_groups count t else
      if l = 10 then 
        if check_three_consecutive_groups_same_suit h then 3 else -1 
      else 
      if l = 7  || l = 6 then 
        let filter_function card = card.suit <> (List.hd h).suit in 
        let others = List.filter filter_function deck in
        if check_two_consecutive_groups_same_suit h && check_consecutive others 
        then 3 else -1 
      else
      if check_consecutive h then count_groups (count + 1) t 
      else -1 
  in 
  count_groups 0 buckets = 3 

(** [check_two_consecutive_groups deck] is True if [deck] contains two groups 
    of consecutive values either both same suit or two different suits. These 
    two groups may or may not be  consecutive in regard to eachother.  *)
let check_two_consecutive_groups deck = 
  let total = List.length deck in 
  let buckets = sort_by_suit [] [] [] [] deck in 
  let rec count_groups count = function 
    | [] ->  count 
    | h::t -> 
      let l = List.length h in 
      if l <> 3 && l<>4 && l<>total then count_groups count t else
      if l = total then 
        if check_two_consecutive_groups_same_suit h then 2 else -1 
      else
      if check_consecutive h then count_groups (count + 1) t 
      else -1 
  in 
  count_groups 0 buckets = 2 

(**tested *)
let is_winning_hand_6 deck = 
  let count_key = group_of_same (List.sort rank_compare deck) in 
  let rec process_key = function
    | [] -> check_two_consecutive_groups deck 
    | ( a, b)::t -> 
      let filter_function card = card.rank <> a in 
      let others = List.filter filter_function deck in 
      if b = 3 then 
        check_same_rank others || check_consecutive others 
      else if b = 4 then 
        let first_card  = List.nth others 0 in 
        let card = {rank = a; suit = first_card.suit} in 
        card::others |> check_consecutive
      else 
        process_key t
  in 
  process_key count_key

(**[is_winning_hand deck] is True if the deck of cards represents a winning 
   hand, otherwise is False. A winning hand is constituted by one of the 
   following combinations: 
   - 4 consecutive in rank cards of the same suit, 3 consecutive cards of 
     another 
   - 3 cards of same rank, 4 cards of another rank 
   - 3 cards of same rank, 4 cards consecutive in rank, same suit
   - 4 cards consecutive in rank, same suit, 3 cards same rank
*)
let is_winning_hand_7 deck = 
  let count_key = group_of_same (List.sort rank_compare deck) in 
  let rec process_key = function
    | [] -> check_two_consecutive_groups deck 
    | ( a, b)::t -> 
      if b < 3 then process_key t else 
        let filter_function card = card.rank <> a in 
        let others = List.filter filter_function deck in
        if check_same_rank others || check_consecutive others then true else
          let first_card  = (List.nth others 0) in 
          let card = {rank = a; suit = first_card.suit} in 
          if b = 4 && (card::others |> check_consecutive ) then true else
            process_key t in 
  process_key count_key

let two_cards_with_first_two_suits rank cards = 
  let rec pair first rank = function 
    | [] -> begin 
        match first with 
        | None -> failwith "error in two_cards_with_first_two_suits"
        | Some suit -> {suit = suit;rank = rank},{suit = suit;rank = rank}
      end 
    | h::t -> 
      match first with 
      | None -> pair (Some h.suit) rank t 
      | Some suit -> 
        if suit <> h.suit then 
          {suit = suit; rank = rank},{suit = h.suit; rank = rank} 
        else pair first rank t 
  in 
  pair None rank cards 

let is_winning_hand_10 deck = 
  (* let () = print_endline (pp_list pp_card deck) in  *)
  let count_key = List.sort rank_compare deck |> group_of_same in
  let rec process_key = function 
    | [] -> check_three_consecutive_groups deck 
    | (a,b) ::t -> 
      if b < 3 then process_key t else 
        let filter_function card = card.rank <> a in 
        let others = List.filter filter_function deck in 
        if b = 3 then is_winning_hand_7 others || process_key t else
        if b = 4 then 
          match two_cards_with_first_two_suits a others with 
          | a,b -> 
            is_winning_hand_7 (a::others) || is_winning_hand_7 (b::others)
            || is_winning_hand_6 others 
        else 
          is_winning_hand_6 others || process_key t in 
  process_key count_key 

let is_winning_hand deck = 
  if List.length deck = 7 then is_winning_hand_7 deck else
  if List.length deck = 10 then is_winning_hand_10 deck else
    failwith "wrong number of cards in deck"

let rec remove_card_helper deck card res =
  match deck with
  | [] -> failwith "Card is not in hand"
  | h::t -> 
    if h = card then t @ res else remove_card_helper t card (h::res)

(** [remove_card card deck] is the card list [deck] with element [card] removed. 
    Requires: [card] is element of [deck] *)
let remove_card card deck =
  remove_card_helper deck card []