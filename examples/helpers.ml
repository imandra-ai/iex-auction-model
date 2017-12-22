open Iex_model.Order;;

(**********************************************)
(***  Preliminaries. Helper functions etc.  ***)
(**********************************************)

(* Functions for generating timestamps and ids *)
let get_time =
  let next_t = ref 0 in
  fun () ->
    next_t := !next_t + 1;
    !next_t - 1
;;
let gen_id =
  let next_id = ref 0 in
  fun () ->
    next_id := !next_id + 1;
    !next_id - 1
;;

(* Generate an order from an order type and quantity *)
let mk_order ot qty = {
  order_type = ot;
  quantity = qty;
  timestamp = get_time();
  short = false;
  order_id = gen_id();
};;

(* Helper functions to construct limit-on-close and market-on-close orders *)
let loc price qty = mk_order (AuctLimit (price, Displayed)) qty ;;
let moc qty = mk_order Market qty ;;