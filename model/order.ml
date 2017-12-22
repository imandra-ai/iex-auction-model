(* @meta[imandra_ignore] on @end *)
open Prelude;;
open Price;;
(* @meta[imandra_ignore] off @end *)

(* The type of market data, includes whether the
   IEX "crumbling quote" signal is active for
   either to buy or the sell side and whether the
   short sale circuit breaker is active (cf. Reg
   SHO).

   For this model we assume that the NBBO is always
   defined. Originally, we had the nbb and nbo
   fields as optional in order to model auctions
   taking place when the NBBO is one or zero-sided
   -- as you would want in a full model of the
   auction logic. However, we struggled to infer
   what the correct behaviour should be from the
   IEX's documentation.
*)
type market_data = {
  nbb : price;
  nbo : price;
  iex_signal : side option;
  short_circuit_breaker : bool;
};;

type time  = int ;;

type peg = 
  | Primary
  | Midpoint
  | DPeg
;;

type display =
  | Displayed
  | NonDisplayed
;;

type order_type =
  | Market
  | AuctLimit of price * display
  | ContLimit of price * display
  | Pegged of peg * price option
;;

type order = {
  order_type : order_type;
  quantity   : int;
  timestamp  : time;
  short      : bool;
  order_id   : int;
};;

let is_displayed order =
  match order.order_type with
  | AuctLimit (_, d) -> d = Displayed
  | ContLimit (_, d) -> d = Displayed
  | Market -> true
  | Pegged _ -> false
;;

let peg_resting_price (side, peg, limit, mdata) = 
  let peg_price =
    match peg, side with
    | Midpoint, _ -> midpoint (mdata.nbb, mdata.nbo)
    | Primary, Buy  -> dec mdata.nbb
    | Primary, Sell -> inc mdata.nbo
    | DPeg, Buy -> mdata.nbb
    | DPeg, Sell -> mdata.nbo
  in
  begin match limit with
    | Some limit ->
      if is_as_aggressive_as(side, limit, peg_price) then
        peg_price else limit
    | _ -> peg_price
  end
;;

let peg_disc_price (side, peg, limit, mdata) = 
  if mdata.iex_signal = Some side then
    peg_resting_price(side, peg, limit, mdata)
  else
    let peg_price =
      match peg, side with
      | Midpoint, _ -> midpoint (mdata.nbb, mdata.nbo)
      | Primary, Buy  -> mdata.nbb
      | Primary, Sell -> mdata.nbo
      | DPeg, _ -> midpoint (mdata.nbb, mdata.nbo)
    in
    begin match limit with
      | Some limit ->
        if is_as_aggressive_as(side, limit, peg_price) then
          peg_price else limit
      | _ -> peg_price
    end
;;