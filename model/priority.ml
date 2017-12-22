(* @meta[imandra_ignore] on @end *)
open Prelude;;
open Price;;
open Order;;
(* @meta[imandra_ignore] off @end *)

(*

Orders are ranked by price/display/time priority.
Note that, in some sense, the order's "price" depends
on the clearing price of the auction because of the way
that Discretionary and Primary peg orders behave.

For example, consider the following two buy orders:

  10.10 (NBO)
  10.09
  10.08 (Mid)
  10.07         MOC order
  10.06 (NBB)   DPeg order

The MOC is "resting" at a higher price than the DPeg order.
However, if the auction takes place at mid (10.08) then the
MOC order will not participate, while the DPeg order can
exercise discretion upto the mid point and participate in
the auction match. Therefore, in some sense, whether the
DPeg order has priority is dependent on the clearing price.

Orders exercising discretion are ranked behind all other
orders in the auction match. To capture this behaviour we
rank orders by their "priority price" as given below:

*)

(* These values should be ordered by:
   forall x.      Discretionary <= x
   forall p, p'.  p < p' ==> LimitPrice p <= LimitPrice p'
   forall x.      x <= MarketPrice   *)
type priority_price =
  | MarketPrice
  | LimitPrice of price
  | Discretionary
;;

(* Returns true iff a pegged order needs to exericse discretion
   in order to reach the clearing price *)
let peg_exercises_discretion(side, peg, limit, mdata, clearing_price) =
  let resting_price = peg_resting_price(side, peg, limit, mdata) in
  let disc_price = peg_disc_price(side, peg, limit, mdata) in
  not(is_as_aggressive_as(side, resting_price, clearing_price))
  && is_as_aggressive_as(side, disc_price, clearing_price)
;;

(* Computes the priority price for an order. If clearing_price = None
   then it simply computes the order's resting price *)
let priority_effective_price(side, order, mdata, clearing_price) =
  match order.order_type with
  | Market -> MarketPrice
  | AuctLimit (p, _)
  | ContLimit (p, Displayed) -> LimitPrice p
  | ContLimit (p, NonDisplayed) ->
    (* Non-displayed orders on the continuous book are subject to midpoint price constraint *)
    LimitPrice (less_aggressive_of(side, p, midpoint (mdata.nbb, mdata.nbo)))
  | Pegged (Midpoint, None) ->
    LimitPrice (midpoint (mdata.nbb, mdata.nbo))
  | Pegged (Midpoint, Some limit) ->
    LimitPrice (less_aggressive_of(side, limit, midpoint (mdata.nbb, mdata.nbo)))
  | Pegged (peg, limit) ->
    match clearing_price with
    | Some clearing_price ->
      if peg_exercises_discretion(side, peg, limit, mdata, clearing_price) then
        Discretionary
      else
        let resting_price = peg_resting_price(side, peg, limit, mdata) in
        LimitPrice resting_price
    | _ ->
      let resting_price = peg_resting_price(side, peg, limit, mdata) in
      LimitPrice resting_price
;;

(* Compare two orders based on price *)
let price_geq(side, o1, o2, mdata, clearing_price) =
  let p1 = priority_effective_price(side, o1, mdata, clearing_price) in
  let p2 = priority_effective_price(side, o2, mdata, clearing_price) in
  match p1, p2 with
  | MarketPrice, _ -> true
  | LimitPrice p1, LimitPrice p2 -> is_as_aggressive_as(side, p1, p2)
  | LimitPrice p1, Discretionary -> true
  | Discretionary, Discretionary -> true
  | _ -> false
;;

(* Compare two orders based on their display attribute *)
let display_geq(o1, o2) =
  is_displayed(o1) || not(is_displayed(o2))
;;

(* Does an order have priority over another? *)
let has_priority(side, o1, o2, mdata, clearing_price) = 
  if price_geq(side, o1, o2, mdata, clearing_price) then
    if price_geq(side, o2, o1, mdata, clearing_price) then
      (* Equal prices *)
      if display_geq(o1, o2) then
        if display_geq(o2, o1) then
          (* Equal display *)
          o1.timestamp < o2.timestamp
        else true
      else false
    else true
  else false
;;