open Helpers;;
open Iex_model.Prelude;;
open Iex_model.Price;;
open Iex_model.Order;;
open Iex_model.Priority;;
open Iex_model.Fill;;
open Iex_model.Clearing_price;;
open Iex_model.Auction;;

(*****************************************)
(***  More orders can decrease volume  ***)
(*****************************************)

(* Intuitively, it should be the case that the more orders
   participate in the auction match the greater the matched
   volume should be. This example shows that adding an extra
   order to the auction can sometimes reduce the volume
   traded in the auction.
*)

(* Assume that the current NBBO is $10.00 - $10.10 *)
let mdata = {
  nbb = cents 1000;
  nbo = cents 1010;
  iex_signal = None;
  short_circuit_breaker = false;
};;

let mid = midpoint(mdata.nbb, mdata.nbo) ;;

(* Consider the following order book: *)

(* Buys: A single limit on close order,
   priced at mid for 1000 shares *)
let orig_buy = loc mid 1000;;

(* Sells: a limit on close order, priced at mid, for
   100 shares and a midpoint peg for 500 shares *)

let loc_sell = loc mid 100;;
let mid_peg = mk_order (Pegged (Midpoint, None)) 500;;

(* If the auction were to take place now then the
   clearing price would be = mid and 600 shares
   will trade.
*)

let _ = 
  let buys = [ orig_buy ] in
  let sells = [ loc_sell; mid_peg ] in
  match conduct_auction(buys, sells, mdata) with
  | Some { clearing_price; fills = [fill1; fill2] } ->

    (* The clearing price is mid *)
    assert (clearing_price = mid);

    (* The total volume traded is 600 *)
    assert (fill1.fill_qty = 100);
    assert (fill2.fill_qty = 500);

  | _ -> assert false
;;

(* Consider now sending in an additional limit on
   close order to buy 200 shares at the NBO:
*)
let extra_buy = loc mdata.nbo 200;;

(* If the auction were to take place now then the
   clearing price would be now be at the NBO and
   only 200 shares will trade.
*)

let _ = 
  let buys = [ extra_buy; orig_buy ] in
  let sells = [ loc_sell; mid_peg ] in
  match conduct_auction(buys, sells, mdata) with
  | Some { clearing_price; fills = [fill1; fill2] } ->

    (* The clearing price is now the NBO *)
    assert (clearing_price = mdata.nbo);

    (* The total volume traded is now only 200 *)
    assert (fill1.fill_qty = 100);
    assert (fill2.fill_qty = 100);

  | _ -> assert false
;;

(* This means that adding an extra order into the auction
   reduced the volume traded. Note that all of the orders
   are eligable to trade at mid:
*)
assert(
  eligable_for_auction(Buy, orig_buy, mdata, mid)
  && eligable_for_auction(Buy, extra_buy, mdata, mid)
  && eligable_for_auction(Sell, loc_sell, mdata, mid)
  && eligable_for_auction(Sell, mid_peg, mdata, mid)
)

(* The reason for this is that, for the purposes of
   calculating the clearing price, the midpoint peg
   is priced at the NBO and therefore in the original
   price calculation the logic determines that volume
   is maximised at mid, where 100 shares can trade.
   The peg is then priced at mid again for the actual
   match, and hence 600 shares actually trade.

   In the second example, the pegged order is priced
   at the NBO again, but now the logic determines
   that volume is maximised at the NBO where 200
   shares will trade. The peg is then priced at mid
   again for the match, but by this point the auction
   price is set and the original buy is no longer
   eligable to participate. Therefore only 200 shares
   will trade.

*)