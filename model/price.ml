(* @meta[imandra_ignore] on @end *)
open Prelude ;;
(* @meta[imandra_ignore] off @end *)

type price = int ;;

let is_as_aggressive_as(side, price1, price2) =
  match side with
  | Buy -> price1 >= price2
  | Sell -> price1 <= price2
;;

let less_aggressive_of(side, price1, price2) =
  match side with
  | Buy -> min(price1, price2)
  | Sell -> max(price1, price2)
;;

let more_aggressive_of(side, price1, price2) =
  match side with
  | Buy -> max(price1, price2)
  | Sell -> min(price1, price2)
;;

let midpoint (p1, p2) = (p1 + p2) / 2 ;;

let half_tick_size = 1 ;;
let tick_size = 2 ;;
let cents p = 2 * p ;;
let inc p = p + tick_size ;;
let dec p = p - tick_size ;;