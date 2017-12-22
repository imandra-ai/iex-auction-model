(* @meta[imandra_ignore] on @end *)
open Prelude;;
open Price;;
open Order;;
(* @meta[imandra_ignore] off @end *)

(* Take the less aggressive of the price and the NBBO *)
let clip_to_nbbo(side, price, mdata) =
  match side with
  | Buy -> min(price, mdata.nbb)
  | Sell -> max(price, mdata.nbo)
;;


(* This calculates the effective price of an order for the purpose
   of determining the clearing price. Note that non-displayed orders
   originating from the Continuous Book (including pegged orders)
   which are resting inside the spread are priced at the NBBO.
*)
let price_for_clearing_price_calc(side, order, collar, mdata) =
  match order.order_type with
  | Market -> collar
  | AuctLimit (limit, _)
  | ContLimit (limit, Displayed) ->
    less_aggressive_of(side, limit, collar)
  | ContLimit (limit, NonDisplayed) ->
    clip_to_nbbo(side, limit, mdata)
  | Pegged (peg, limit) ->
    let resting_price = peg_resting_price(side, peg, limit, mdata) in
    clip_to_nbbo(side, resting_price, mdata)
;;


(* Code to sort a list of orders by the price computed above.
   We sort orders using an insertion sort since this is easier
   to reason about. Note that for a specifiction this is more
   important than efficiency.
*)
let rec insert_by_price(side, to_insert, orders, collar, mdata) =
  match orders with
  | [] -> [to_insert]
  | order :: orders ->
    if price_for_clearing_price_calc(side, to_insert, collar, mdata) 
       < price_for_clearing_price_calc(side, order, collar, mdata)  then
      to_insert :: order :: orders
    else
      order :: insert_by_price(side, to_insert, orders, collar, mdata)
;;
let rec sort_by_price(side, orders, collar, mdata) =
  match orders with
  | [] -> []
  | order :: orders ->
    let sorted = sort_by_price(side, orders, collar, mdata) in
    insert_by_price(side, order, sorted, collar, mdata)
;;


(* Tests whether an order can trade at a specific price *)
let can_trade_at(side, order, price, mdata) =
  match order.order_type with
  | Market -> true
  | AuctLimit (limit, _)
  | ContLimit (limit, Displayed) ->
    is_as_aggressive_as(side, limit, price)
  | ContLimit (limit, NonDisplayed) ->
    let effective_price = clip_to_nbbo(side, limit, mdata) in
    is_as_aggressive_as(side, effective_price, price)
  | Pegged (peg, limit) ->
    let resting_price = peg_resting_price(side, peg, limit, mdata) in
    let effective_price = clip_to_nbbo(side, resting_price, mdata) in
    is_as_aggressive_as(side, effective_price, price)
;;


(* Computes the volume available to trade on one side of
   the order book at a specific price.
*)
let rec volume_at_price (side, price, orders, mdata) =
  match orders with
  | [] -> 0
  | order :: orders ->
    let rest_volume = volume_at_price(side, price, orders, mdata) in
    if can_trade_at(side, order, price, mdata) then
      order.quantity + rest_volume
    else
      rest_volume
;;


(* Computes the volume available to trade at a specific price. *)
let rec volume_traded(price, buys, sells, mdata) =
  let buy_volume = volume_at_price(Buy, price, buys, mdata) in
  let sell_volume = volume_at_price(Sell, price, sells, mdata) in
  min(buy_volume, sell_volume)
;;


(* Computes the number of excess shares on the buy side at a
   specific price. Returns a negative value if there is a sell
   imbalance at this price.
*)
let rec buy_excess_at_price(price, buys, sells, mdata) =
  let buy_volume = volume_at_price(Buy, price, buys, mdata) in
  let sell_volume = volume_at_price(Sell, price, sells, mdata) in
  buy_volume - sell_volume
;;


(* We now want to compute the price which maximises the volume
   traded. Note that there may be more than one such price. In
   this case these prices form a continuous range of price. We
   compute the start and end of this range using the functions
   below.

   Note that the IEX Specification also states that the price
   cannot be more aggressive than the first unexecuted order.
   This means that there cannot be an excess on the relevent
   side at the next most aggressive price level. For example,
   if $10.02 maximised volume, but there was a positive buy
   excess at $10.03 then that would mean that there is an
   unexecuted buy order at $10.03, and therefore we cannot
   consider $10.02 as a possible clearing price.
*)

(* @meta[measure : max_volume_lowest_price]
    let measure_max_volume_lowest_price(current, limit, buys, sells, mdata) =
      limit - current
   @end
*)
let rec max_volume_lowest_price(current, limit, buys, sells, mdata) =
  if limit <= current then
    limit
  else
    let volume = volume_traded(current, buys, sells, mdata) in
    let next = current + half_tick_size in
    let best = max_volume_lowest_price(next, limit, buys, sells, mdata) in
    let best_volume = volume_traded(best, buys, sells, mdata) in
    if volume < best_volume then
      best
    else if volume > best_volume then
      current
    else (* volume = best_volume *)
      let best_excess = buy_excess_at_price(best, buys, sells, mdata) in
      if best_excess > 0 then
        (* Unexectued buy at best price level *)
        best
      else
        current
;;

(* @meta[measure : max_volume_highest_price]
    let measure_max_volume_highest_price(limit, current, buys, sells, mdata) =
      current - limit
   @end
*)
let rec max_volume_highest_price(current, limit, buys, sells, mdata) =
  if limit <= current then
    limit
  else
    let volume = volume_traded(current, buys, sells, mdata) in
    let next = current + half_tick_size in
    let best = max_volume_highest_price(next, limit, buys, sells, mdata) in
    let best_volume = volume_traded(best, buys, sells, mdata) in
    if volume < best_volume then
      best
    else if volume > best_volume then
      current
    else (* volume = best_volume *)
      let excess = buy_excess_at_price(current, buys, sells, mdata) in
      if excess < 0 then
        (* Unexectued sell at this price level *)
        current
      else
        best
;;


(* Functions which calculate the auction collar *)
let collar_lower(mdata) =
  let mid = midpoint (mdata.nbb, mdata.nbo) in
  let ten_per = mid / 10 in
  let offset = max(ten_per, cents(50)) in
  mid - offset
;;
let collar_upper(mdata) =
  let mid = midpoint (mdata.nbb, mdata.nbo) in
  let ten_per = mid / 10 in
  let offset = max(ten_per, cents(50)) in
  mid + offset
;;


(* The function which calcualtes the clearing price
   for the auction. It computes the range of prices
   which maximise the volume traded (subject to the
   unexecuted order condition) and then from this
   range it selects the price closest to mid.
*)
let calc_clearing_price(buys, sells, mdata) =
  let mid = midpoint (mdata.nbb, mdata.nbo) in
  let lower =
    max_volume_lowest_price(collar_lower(mdata),
                            collar_upper(mdata),
                            buys, sells, mdata)
  in
  let upper =
    max_volume_highest_price(collar_lower(mdata),
                             collar_upper(mdata),
                             buys, sells, mdata)
  in
  let clearing_price =
    if mid < lower then
      lower
    else if upper < mid then
      upper
    else
      mid
  in
  if volume_traded(clearing_price, buys, sells, mdata) = 0 then
    None
  else
    Some clearing_price
;;