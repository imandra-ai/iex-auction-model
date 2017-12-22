(* @meta[imandra_ignore] on @end *)
open Prelude;;
open Order;;
(* @meta[imandra_ignore] off @end *)

type fill = {
  buy_id   : int;
  sell_id  : int;
  fill_qty : int;
};;

let create_fill(buy, sell) = {
  buy_id = buy.order_id;
  sell_id = sell.order_id;
  fill_qty = min(buy.quantity, sell.quantity);
};;
