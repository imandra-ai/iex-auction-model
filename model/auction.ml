(* @meta[imandra_ignore] on @end *)
open Prelude;;
open Price;;
open Order;;
open Priority;;
open Fill;;
open Clearing_price;;
(* @meta[imandra_ignore] off @end *)

(* Test whether an order is eligable to particiapte in an auction
   taking place at the specified clearing price.
*)
let eligable_for_auction(side, order, mdata, clearing_price) = 
  match priority_effective_price(side, order, mdata, Some clearing_price) with
  | MarketPrice | Discretionary -> true
  | LimitPrice p -> is_as_aggressive_as(side, p, clearing_price)
;;

(* Insert an order into the correct position in a sorted list of orders. *)
let rec insert_order(side, order, orders, mdata, clearing_price) =
  match orders with
  | [] -> [ order ]
  | order2 :: orders ->
    if has_priority(side, order, order2, mdata, Some clearing_price) then
      order :: order2 :: orders
    else
      order2 :: insert_order (side, order, orders, mdata, clearing_price)
;;

(* Before we match orders we first filter out any orders which are not
   eligable to particpate in the auction, and we sort the remaining orders
   according to the priority ordering. We sort orders using insertion sort
   since this is easier to reason about. Note that the aim of the model is
   to be simple to understand and to reason about, rather than to be
   maximally efficient.
*)
let rec sort_and_filter_orders (side, orders, mdata, clearing_price) = 
  match orders with
  | [] -> []
  | order :: orders ->
    let orders = sort_and_filter_orders (side, orders, mdata, clearing_price) in
    if eligable_for_auction(side, order, mdata, clearing_price) then
      insert_order (side, order, orders, mdata, clearing_price)
    else orders
;;

(* We now define the main auction loop. This takes a list of buys and
   a list of sells, sorted in priority order, and repeatedly selects
   the highest priority buy and sell and trades them with each other.
*)

(* In order to reason soundly about functions, Imandra needs to know
   that they always terminate. Usually it is able to see this by
   itself, but in this case it requires a hint that it should consider
   the sum of the lengths of the two lists. Since this decreases every
   time we make a recursive call we know that eventually this loop
   must terminate.

   @meta[measure : auction_loop]
    let measure_auction_loop(buys, sells) =
      List.length buys + List.length sells
   @end
*)
let rec auction_loop(buys, sells) =
  match buys, sells with
  | [], _ -> []
  | _, [] -> []
  | buy :: buys, sell :: sells ->
    let fill = create_fill(buy, sell) in
    let fills =
      if buy.quantity < sell.quantity then
        let sell =
          { sell with quantity = sell.quantity - fill.fill_qty }
        in
        auction_loop(buys, sell :: sells)
      else if buy.quantity = sell.quantity then
        auction_loop(buys, sells)
      else
        let buy =
          { buy with quantity = buy.quantity - fill.fill_qty }
        in
        auction_loop(buy :: buys, sells)
    in
    fill :: fills
;;

(* Selects the lowest priority short order from a list of orders. *)
let rec lowest_priority_short (orders, mdata) =
  match orders with
  | [] -> None
  | order :: orders ->
    if order.short then
      match lowest_priority_short(orders, mdata) with
      | None -> Some order
      | Some alt_order ->
        if has_priority(Sell, order, alt_order, mdata, None) then
          Some alt_order
        else
          Some order
    else lowest_priority_short(orders, mdata)
;;

(* Removes the first occurance of an order from a list of orders *)
let rec remove_order(to_remove, orders) =
  match orders with
  | [] -> []
  | order :: orders ->
    if order = to_remove then
      orders
    else
      order :: remove_order(to_remove, orders)
;;

(* Imandra needs some help to see why the function that
   we're about to define (conduct_auction) will terminate.
   Intuitively, this is because the length of `sells'
   decreases at every recursive call. We prove a rewrite
   rule which makes this precice. We also disable some
   functions because we know that expanding their definitions
   will not be relevant to our goal. *)
let _ = Reflect.eval(":disable has_priority");;
let _ = Reflect.eval("
  theorem[rw] remove_length(lps, sells, mdata) =
    lowest_priority_short(sells, mdata) = Some lps
      ==> List.length(remove_order(lps, sells)) = List.length(sells) - 1
  ;;
");;
let _ = Reflect.eval(
    ":disable auction_loop "
    ^ "sort_and_filter_orders calc_clearing_price"
  );;

type auction_result = {
  fills : fill list;
  clearing_price : price;
};;

(* @meta[measure : conduct_auction]
    let measure_conduct_auction(buys, sells, mdata) =
      List.length sells
   @end
*)
let rec conduct_auction(buys, sells, mdata) =
  match calc_clearing_price(buys, sells, mdata) with
  | None -> None
  | Some clearing_price ->
    let lps = lowest_priority_short(sells, mdata) in
    let exclude_shorts =
      mdata.short_circuit_breaker && clearing_price <= mdata.nbb
    in
    match exclude_shorts, lps with
    | true, Some lps ->
      let sells = remove_order(lps, sells) in
      conduct_auction(buys, sells, mdata)
    | _ ->
      let buys = sort_and_filter_orders(Buy, buys, mdata, clearing_price) in
      let sells = sort_and_filter_orders(Sell, sells, mdata, clearing_price) in
      Some { fills = auction_loop(buys, sells);
             clearing_price }
;;

(* Computes the total volume traded from a list of fills: *)
let rec total_traded_volume = function
  | [] -> 0
  | fill :: fills -> fill.fill_qty + total_traded_volume(fills)
;;