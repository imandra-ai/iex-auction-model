open Helpers;;
open Iex_model.Prelude;;
open Iex_model.Price;;
open Iex_model.Order;;
open Iex_model.Priority;;
open Iex_model.Fill;;
open Iex_model.Clearing_price;;
open Iex_model.Auction;;

(**********************************************)
(***  Mechanical Markets Blog Post Example  ***)
(**********************************************)

(* https://mechanicalmarkets.wordpress.com/2017/09/20/a-curious-feature-of-iex-auctions/ *)

(* This first example explores the behaviour discussed in
   the paragraph beginning: "My understanding is that a
   hidden order submitted after these LOC orders could
   gain priority without providing a better price."
*)
let blog_post_example =

  let mdata = {
    nbb = cents 1009;
    nbo = cents 1013;
    iex_signal = None;
    short_circuit_breaker = false;
  } in

  (* The order book from clearing price example 1 *)
  (* A limit-on-close buy and sell, both at $10.10 *)
  let buys = [ loc (cents 1010) 1500 ] in
  let sells = [ loc (cents 1010) 1000 ] in

  (* Conduct the auction for the orders from clearing price example 1 *)
  begin match conduct_auction(buys, sells, mdata) with

  (* The auction goes ahead and results in a single fill *)
  | Some { clearing_price; fills = [ fill ] } ->

    (* The auction price is 10.10 *)
    assert (clearing_price = cents 1010);

    (* 1000 shares execute *)
    assert (fill.fill_qty = 1000);

    (* The LOC buy and sell order on the auction book receives an execution *)
    assert (fill.buy_id = (List.hd buys).order_id);
    assert (fill.sell_id = (List.hd sells).order_id);

  | _ -> assert false
  end;

  (* Consider the following hidden buy order *)
  (* A non-displayed limit order at $10.11 *)
  let hidden_bid =
    mk_order
      (ContLimit (cents 1011, NonDisplayed))
      1000
  in

  (* Run the auction again with the hidden bid on the continuous book *)
  begin match conduct_auction(hidden_bid :: buys, sells, mdata) with

  (* Again, the auction goes ahead and results in a single execution *)
  | Some { clearing_price; fills = [ fill ] } ->

    (* The auction price is unaffected by the new order *)
    assert (clearing_price = cents 1010);

    (* 1000 shares still execute *)
    assert (fill.fill_qty = 1000);

    (* But it's now the hidden bid which receives an execution *)
    assert (fill.buy_id <> (List.hd buys).order_id);
    assert (fill.buy_id = hidden_bid.order_id);

    assert (fill.sell_id = (List.hd sells).order_id);

  | _ -> assert false
  end

;;

(* Next we consider the behaviour discussed in the
   "potentially problematic example".
*)

(* If all displayed orders for a security are on IEX then we can
   compute the nbbo just from the continuous order book *)
let rec most_agg_displayed_limit(side, orders) =
  match orders with
  | [] -> if side = Buy then 0 else max_int
  | order :: orders ->
    let most_agg = most_agg_displayed_limit(side, orders) in
    match order.order_type with
    | ContLimit (limit, Displayed) ->
      more_aggressive_of(side, limit, most_agg)
    | _ -> most_agg
;;

let compute_mdata(buys, sells) =
  let nbb = most_agg_displayed_limit(Buy, buys) in
  let nbo = most_agg_displayed_limit(Sell, sells) in
  { nbb = nbb; nbo = nbo; 
    iex_signal = None;
    short_circuit_breaker = false; }
;;

let rec sublist i j = function
  | [] -> []
  | x :: xs -> 
    if j <= 0 then []
    else if i <= 0 then x :: sublist 0 (j - 1) xs
    else sublist (i - 1) (j - 1) xs
;;

(* Blog post example 2 *)
let blog_post_example2 =

  (* Override auction and auction_price so that they calculate
     the NBBO from the displayed limit orders on IEX
  *)
  let auction(buys, sells) =
    let mdata = compute_mdata(buys, sells) in
    match conduct_auction(buys, sells, mdata) with
    | Some { fills; _ } -> fills
    | None -> assert false
  in

  let auction_price(buys, sells) =
    let mdata = compute_mdata(buys, sells) in
    match conduct_auction(buys, sells, mdata) with
    | Some { clearing_price; _ } -> clearing_price
    | None -> assert false
  in

  (* The closing auction book contains 1 LOC buy at $10.00 for
     1,000 shares, and 1 LOC sell at $10.00 for 10,000 shares *)
  let auction_buy = loc (cents 1000) 1000 in
  let auction_sell = loc (cents 1000) 10000 in

  (* A helper for constructing limit orders *)
  let displayed_limit price qty =
    mk_order (ContLimit (price, Displayed)) qty
  in

  (* The continuous book contains displayed bids at every price
     from $10.05 down to $10.00 and a sell at $10.07 *)
  let continuous_sells = 
    [ displayed_limit (cents 1007) 2000 ]
  in
  let continuous_buys =
    [ displayed_limit (cents 1005) 10000;
      displayed_limit (cents 1004) 10000;
      displayed_limit (cents 1003) 10000;
      displayed_limit (cents 1002) 10000;
      displayed_limit (cents 1001) 10000;
      displayed_limit (cents 1000) 10000; ]
  in

  let buys = auction_buy :: continuous_buys in
  let sells = auction_sell :: continuous_sells in

  (* Assume that all displayed quantity is on IEX. Therefore
     the NBBO is $10.05/$10.07 *)
  let mdata = compute_mdata(buys, sells) in
  assert (mdata.nbb = cents 1005);
  assert (mdata.nbo = cents 1007);

  (* If the auction were to take place now the clearing price
     would be $10.05. Also, remember which orders were filled *)
  assert (auction_price(buys, sells) = cents 1005);
  let filled_buys =
    List.map (fun fill -> fill.buy_id) (auction(buys, sells))
  in

  (* Now each bidder considers whether they would benefit
     from cancelling their displayed order and submitting
     a new hidden order at $10.06 *)
  let replace order =
    let new_order =
      mk_order (ContLimit (cents 1006, NonDisplayed)) 10000
    in
    (* We'll reuse the previous order id so we can track
       whose order is whose *)
    { new_order with order_id = order.order_id }
  in
  let consider (current_order, reference_price, mdata) =
    (* The bidder considers whether submitting a new order would increase
       their priority in the auction. They also observe that the auction
       price should decrease (although they don't have enough information
       to actually check this).
    *)
    has_priority(Buy, replace(current_order),
                 current_order, mdata, Some reference_price)
  in

  (* The first five bidders each consider in turn *)
  let five_bidders = sublist 1 6 buys in
  let buys = 
    List.fold_left (fun buys order ->
        let ref_price = auction_price(buys, sells) in
        let mdata = compute_mdata(buys, sells) in
        if consider (order, ref_price, mdata) then
          (* The bidder decides to resubmit their order *)
          (* First they delete their order *)
          let buys =
            List.filter 
              (fun o -> o.order_id <> order.order_id)
              buys
          in
          (* And then submit a new_order *)
          let buys = replace(order) :: buys in
          (* Confirm that the auction price does decrease *)
          assert (auction_price(buys, sells) < ref_price);
          buys
        else (
          (* The bidder decides not to resubmit their order *)
          (* This should not happen *)
          assert false;
        )
      ) buys five_bidders
  in

  (* If the auction were to take place now the clearing price
     would be $10.00. However, the same orders get filled *)
  assert (auction_price(buys, sells) = cents 1000);
  let filled_buys' =
    List.map (fun fill -> fill.buy_id) (auction(buys, sells))
  in
  assert (filled_buys = filled_buys')
;;