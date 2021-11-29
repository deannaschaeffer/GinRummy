(**Module for Autoplayer*)
open Deckstack
open Card
open Playerhand

let global_rand = Random.self_init();;

let pick_easy =
  let pile = Random.int 2 in
  print_endline(string_of_int pile);
  Some (pile + 1)

let discard_easy num =
  let pile = Random.int num in
  print_endline(string_of_int pile);
  Some (pile + 1)

(** [check_pairs card remain_hand] is the boolean value of whether or not the 
    card [card] has the same rank or is consecutive in suit to any of the 
    cards in the list [remain_hand] *)
let rec check_pairs card remain_hand =
  match remain_hand with 
  | [] -> false
  | h :: t -> 
    if card.rank = h.rank || 
       (card.suit = h.suit && abs (card.rank - h.rank) = 1) then true 
    else check_pairs card t

let pick_med top hand = 
  if check_pairs top hand then Some 2 else Some 1

let discard_med hand flip =
  let remain = List.rev flip in 
  let rec disc hand remain = 
    match remain with 
    | [] -> Some 1
    | h :: t -> 
      if check_pairs h (List.filter (fun x -> x <> h) hand) then disc hand t 
      else Some (List.length hand - List.length t) in 
  disc hand remain

(** [rank_compare a b] is a compare of two element's ranks.  *)
let rank_compare a b =  
  if a.rank > b.rank then 1 else if a.rank = b.rank then 0 else -1 

(** [suit_compare a b] is a compare of two element's suits. the order from
    least to greatest is hearts, diamonds, clubs, spades. If two cards have the
    same suit, it will compare their ranks *)
let suit_compare a b = 
  match a.suit, b.suit, a.rank, b.rank with 
  | a_suit, b_suit, a_rank, b_rank when a_suit = b_suit && a_rank > b_rank -> 1
  | a_suit, b_suit, a_rank, b_rank when a_suit = b_suit && a_rank < b_rank -> 
    -1
  | a_suit, b_suit, a_rank, b_rank when a_suit = b_suit && a_rank = b_rank -> 0
  | HEARTS, _, _, _ -> -1
  | _, HEARTS, _, _ -> 1
  | DIAMONDS, _, _, _ -> -1
  | _, DIAMONDS, _, _ -> 1
  | CLUBS, _, _, _ -> -1
  | _ -> 1
(* if a.suit = b.suit && a.rank > b.rank then 1
   else if a.suit = b.suit && a.rank < b.rank then -1
   else if a.suit = b.suit && a.rank = b.rank then 0
   else if a.suit = HEARTS then -1
   else if b.suit = HEARTS then 1
   else if a.suit = DIAMONDS then -1
   else if b.suit = DIAMONDS then 1
   else if a.suit = CLUBS then -1
   else 1 *)

(** [create_map hand remains lst] is a mapping of the cards from the list
    [hand] to their corresponding numbers shown to the player on the screen *)
let rec create_map hand remains lst =
  match remains with
  | [] -> lst
  | h :: t -> create_map hand t ((h, List.length hand - List.length t) :: lst)

(** [winning hand remains] checks a list [hand] of one too many cards to 
    determine whether a winning hand exists in some combination of them and
    is the boolean result of that paired with the remaining card not involved 
    in the winning hand *)
let rec winning hand remains =
  match remains with
  | [] -> (false, List.hd hand)
  | h :: t -> 
    if is_winning_hand (List.filter (fun x -> x <> h) hand) 
    then (true, h) else winning hand t

(** [empty] is an empty card with invalid rank*)
let empty = {
  rank = -1;
  suit = HEARTS
}

(** [groups hand] is the greatest number of same rank cards in the list [hand]
    paired with the remaining hand after cards of that rank are removed *)
let groups hand =
  let sorted = List.sort rank_compare hand in
  let rec group_help remains prev count rank = 
    match remains with 
    | [] -> (count, List.filter (fun x -> x.rank <> rank) hand)
    | h :: t -> 
      if prev.rank = h.rank then group_help t h (count + 1) h.rank
      else if count < 3 then group_help t h 1 h.rank 
      else (count, List.filter (fun x -> x.rank <> rank) hand) in 
  group_help sorted empty 1 (-1)

(** [consec hand] is the greatest number of consecutive same suit cards in the
    list [hand] paired with the remaining hand after consecuitve cards of 
    that suit are removed *)
let rec consec hand =
  let sorted = List.sort suit_compare hand in
  let rec consec_help remains prev count suit rank =
    match remains with 
    | [] -> 
      let filter_function card = 
        (card.suit <> suit) || (card.rank > rank ) || 
        (card.rank + count < rank) in 
      (count, List.filter filter_function hand)
    | h :: t -> 
      if prev.suit = h.suit && prev.rank + 1 = h.rank && count <= 3 then
        consec_help t h (count + 1) h.suit h.rank
      else if count < 3 then consec_help t h 1 h.suit h.rank
      else 
        let filter_function card = 
          (card.suit <> suit) || (card.rank > rank )|| 
          (card.rank + count < rank) in 
        (count, List.filter filter_function hand) in 
  consec_help sorted empty 1 HEARTS (-1)

(** [check_pairs_help hand remains] is the first card in the list [hand]
    that is not the same rank or consecutive in suit to any other card in
    [hand]. If no such card exists, it is the first card in [hand] *)
let rec check_pairs_help hand remains =
  match remains with 
  | [] -> List.hd hand
  | h :: t -> 
    if check_pairs h (List.filter (fun x -> x <> h) hand) 
    then check_pairs_help hand t else h

(** [three_help hand] is the first card in the remaining hand after a group
    or consec of 3 was found in the list [hand]. If no group/consec of 3 was 
    found, it is the first card in the list without a pair *)
let three_help hand =
  if fst (groups hand) = 3 then 
    groups hand |> snd |> List.hd 
  else if fst (consec hand) = 3 then
    consec hand |> snd |> List.hd 
  else check_pairs_help hand hand

(** [discard_hard_7 hand] is the int option representing the card to be 
    discarded from the 7-card hand [hand] *)
let discard_hard_7 hand =
  let map = create_map hand hand [] in 
  let card = winning hand hand |> snd in 
  if fst (winning hand hand) then List.assoc_opt card map
  else if fst (groups hand) = 4 then let lst = snd (groups hand) in
    List.assoc_opt (check_pairs_help lst lst) map
  else if fst (consec hand) >= 4 then let lst2 = snd (consec hand) in 
    List.assoc_opt (check_pairs_help lst2 lst2) map
  else if fst (groups hand) = 3 then 
    let card = groups hand |> snd |> three_help in 
    List.assoc_opt card map
  else if fst (consec hand) = 3 then 
    let card = consec hand |> snd |> three_help in
    List.assoc_opt card map
  else List.assoc_opt (check_pairs_help hand hand) map

(** [three_help_10 hand] is the remaining hand when a group or consec of >= 3
    was found in the list [hand]. If no group/consec of >=3 found, 
    it is just [hand] *)
let three_help_10 hand =
  if fst (groups hand) >= 3 then 
    snd (groups hand)
  else if fst (consec hand) >= 3 then
    snd (consec hand)
  else hand

(** [discard_hard_10 hand] is the int option representing the card to be 
    discarded from the 10-card hand [hand] *)
let discard_hard_10 hand =
  let map = create_map hand hand [] in 
  let card = winning hand hand |> snd in 
  if fst (winning hand hand) then List.assoc_opt card map
  else if fst (groups hand) >= 3 then let remains = snd (groups hand) in
    let cards = three_help_10 remains |> three_help_10 in
    List.assoc_opt (check_pairs_help cards cards) map 
  else if fst (consec hand) >= 3 then let remains = snd (consec hand) in
    let cards = three_help_10 remains |> three_help_10 in
    List.assoc_opt (check_pairs_help cards cards) map 
  else List.assoc_opt (check_pairs_help hand hand) map

let discard_hard hand num =
  let new_hand = List.rev hand in
  if num = 7 then discard_hard_7 new_hand else discard_hard_10 new_hand

(** [pick_help hand] checks if the list [hand] contains a group or consec of
    >= 3 *)
let pick_help hand =
  fst (groups hand) >= 3 || fst (consec hand) >= 3 

(** [pick_three_help top remains] is the int option indicating which pile to 
    pick from depending on whether or not there is a group/consec of 3+ in the 
    list [remains] and if adding the card [top] to [remains] produces a
    group/consec of 3+ *)
let pick_three_help top remains =
  if pick_help remains then Some 1
  else if pick_help (top :: remains) then Some 2 
  else pick_med top remains

(** [consec_help top hand addition num] is the int option indicating which 
    pile to pick from depending on whether the list [addition] contains a 
    consec of 3+ *)
let consec_help top hand addition num =
  let remains2 = snd (consec hand) in
  if fst (consec addition) >= 4 then Some 2
  else begin
    if num = 7 then pick_three_help top remains2
    else pick_three_help top (three_help_10 remains2)
  end

(** [pick_hard_7 top hand] is the int option representing the pile to pick
    from based on the cards in the 7-card hand [hand] given the top card of
    the discard pile [top] *)
let pick_hard_7 top hand =
  let addition = top :: hand in
  if fst (winning addition addition) then Some 2
  else if fst (groups hand) = 4 then groups hand |> snd |> pick_med top 
  else if fst (consec hand) >= 4 then consec hand |> snd |> pick_med top 
  else if fst (groups hand) = 3 then let remains1 = snd (groups hand) in begin
      if fst (groups addition) = 4 then Some 2
      else if fst (consec hand) = 3 then consec_help top hand addition 7
      else pick_three_help top remains1
    end
  else if fst (consec hand) = 3 then consec_help top hand addition 7
  else if fst (groups addition) >= 3 || fst (consec addition) >= 3 then Some 2
  else pick_med top hand

(** [pick_hard_10 top hand] is the int option representing the pile to pick
    from based on the cards in the 10-card hand [hand] given the top card of
    the discard pile [top] *)
let pick_hard_10 top hand =
  let addition = top :: hand in
  if fst (winning addition addition) then Some 2
  else if fst (groups hand) = 4 then snd (groups hand) |> three_help_10 
                                     |> pick_med top
  else if fst (consec hand) >= 4 then snd (consec hand) |> three_help_10 
                                      |> pick_med top
  else if fst (groups hand) = 3 then let remains = snd (groups hand) in begin
      if fst (groups addition) = 4 then Some 2
      else if fst (consec hand) = 3 then consec_help top hand addition 10
      else pick_three_help top (three_help_10 remains)
    end
  else if fst (consec hand) = 3 then consec_help top hand addition 10
  else if fst (groups addition) >= 3 || fst (consec addition) >= 3 then Some 2
  else pick_med top hand

let pick_hard top hand num =
  if num = 7 then pick_hard_7 top hand else pick_hard_10 top hand

