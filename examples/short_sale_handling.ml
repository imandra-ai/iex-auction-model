open Helpers;;
open Iex_model.Prelude;;
open Iex_model.Price;;
open Iex_model.Order;;
open Iex_model.Priority;;
open Iex_model.Fill;;
open Iex_model.Clearing_price;;
open Iex_model.Auction;;

(************************************************************)
(***  Interesting behaviour of short sale order handling  ***)
(************************************************************)

(* In volume_decrease.ml we showed that it it possible for
   the total volume matched to decrease when we add an extra
   order into the auction. Here we show how the short sale
   order handling (to satisfy Reg SHO) can cause similar
   behaviour.

   In the previous example, sending an extra buy order
   caused the volume to decrease, but it did also raise
   the auction price. The latter effect is at least
   intuitive: adding buy pressure should either raise the
   clearing price or not affect it. The short sale handling
   logic can violate this property as well.

   In this example we see how including an extra sell order
   in the auction can not only decrease the volume traded,
   but in fact raise the clearing price.

*)

(* Note that this behaviour is based on our interpretation
   of the information in the section on short sale order
   handling from the IEX auction specification. The wording
   there is quite ambiguous and there are other potential
   interpretations of the wording which display different
   behaviour in this example.

   Even if our interpreation is not the intended one, this
   is still a great example of the benefits of executable
   specificaitons and the ambiguities which are common in
   the current approach of handwritten specifications.

*)

(* Assume that the current NBBO is $10.00 - $10.10 and the
   Reg SHO short sale circuit breaker is currently active.
*)
let mdata = {
  nbb = cents 1000;
  nbo = cents 1010;
  iex_signal = None;
  short_circuit_breaker = true; (* <-- circuit breaker on *)
};;

let mid = midpoint(mdata.nbb, mdata.nbo) ;;

(* Consider the following order book: *)

(* Buys: a single limit on close order,
   priced at the NBO for 1000 shares *)
let orig_buy = loc mdata.nbo 1000;;

(* Sells: a single (short) limit on close order,
   priced at the NBB for 1000 shares *)
let short_sell =
  { (loc mdata.nbb 1000) with short = true } (* <-- short order *)
;;

(* If the auction were to take place now then the
   clearing price would be = mid and 600 shares
   will trade.
*)

let _ = 
  let buys = [ orig_buy ] in
  let sells = [ short_sell ] in
  match conduct_auction(buys, sells, mdata) with
  | Some { clearing_price; fills = [fill] } ->

    (* The clearing price is mid *)
    assert (clearing_price = mid);

    (* The total volume traded is 600 *)
    assert (fill.fill_qty = 1000);

  | _ -> assert false
;;

(* Consider now sending in an additional limit on
   close order to sell 200 shares (long) at the NBB:
*)
let long_sell = loc mdata.nbb 200;; (* <-- Long (default) *)

(* If the auction were to take place now then the
   clearing price would be now be at the NBO and
   only 200 shares will trade.
*)
let _ = 
  let buys = [ orig_buy ] in
  let sells = [ long_sell; short_sell ] in
  match conduct_auction(buys, sells, mdata) with
  | Some { clearing_price; fills = [fill] } ->

    (* The clearing price is now the NBO *)
    assert (clearing_price = mdata.nbo);

    (* The total volume traded is now only 200 *)
    assert (fill.fill_qty = 200);

  | _ -> assert false
;;

(* So, not only does the volume traded decrease, but
   the clearing price increases due to an extra sell
   order.

   What's happening is that in the first auction the
   buys are sells are perfectly balanced and so the
   logic calculates the auction price to be mid. This
   is fine with respect to short sale regulations and
   so everything can trade. However, in the second
   auction the new sell pushes the auction price down
   to be at the NBB, which then triggers the short
   sale order handling logic. This then excludes the
   short order from the auction and starts again.
   This time there is a buy side imbalance which
   pushes the auction price up to the NBO.
   
*)