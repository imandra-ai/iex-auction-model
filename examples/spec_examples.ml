open Helpers;;
open Iex_model.Prelude;;
open Iex_model.Price;;
open Iex_model.Order;;
open Iex_model.Fill;;
open Iex_model.Clearing_price;;
open Iex_model.Auction;;

(* Enable/disable printing expected/actual values in tests *)
let verbose = false ;;

(* Given some buys, sells and market data, conduct the auction
   and compare the results with the expected results
*)
let test (buys, sells, mdata, expected_price, expected_fills) =
  match conduct_auction(buys, sells, mdata) with
  | None -> false
  | Some { fills; clearing_price } ->
    let to_string fills =
      String.concat "; " (
        List.map (fun fill -> string_of_int(fill.fill_qty)) fills)
    in
    if verbose then begin
      Printf.printf "Expected: fills = [%s], price = %d\n" (to_string expected_fills) expected_price;
      Printf.printf "Actual:   fills = [%s], price = %d\n" (to_string fills) clearing_price;
    end;
    expected_price = clearing_price
    && expected_fills = fills
;;

(*******************************************************)
(***  Clearing price examples from IEX Auction Spec  ***)
(*******************************************************)

(* The market data for the clearing price examples *)
let cp_mdata = {
  nbb = cents 1009;
  nbo = cents 1011;
  iex_signal = None;
  short_circuit_breaker = false;
};;

(* Example 1 *)
let cp_ex1 =
  let buys =
    [ loc (cents 1010) 1500 ]
  in
  let sells =
    [ loc (cents 1010) 1000 ]
  in
  let fill = {
    buy_id = (List.hd buys).order_id;
    sell_id = (List.hd sells).order_id;
    fill_qty = 1000;
  }
  in
  assert (test (buys, sells, cp_mdata, cents 1010, [fill]))
;;

(* Example 2 *)
let cp_ex2 =
  let buys = 
    [ loc (cents 1010) 1500 ]
  in
  let sells =
    [ moc 1000 ]
  in
  let fill = {
    buy_id = (List.hd buys).order_id;
    sell_id = (List.hd sells).order_id;
    fill_qty = 1000;
  }
  in
  assert (test (buys, sells, cp_mdata, cents 1010, [fill]))
;;

(* Example 3 *)
let cp_ex3 =
  let buys = [
    loc (cents 1011) 2000;
    mk_order (ContLimit (cents 1009, Displayed)) 500;
  ] in
  let sells = [
    loc (cents 1009) 2000;
    mk_order (ContLimit (cents 1011, Displayed)) 600;
  ] in
  let fill = {
    buy_id = (List.hd buys).order_id;
    sell_id = (List.hd sells).order_id;
    fill_qty = 2000;
  }
  in
  assert (test (buys, sells, cp_mdata, cents 1010, [fill]))
;;

(***************************************************************)
(***  Prioirity of execution examples from IEX Auction Spec  ***)
(***************************************************************)

let poe_mdata = {
  nbb = cents 2019;
  nbo = cents 2021;
  iex_signal = None;
  short_circuit_breaker = false;
}
;;

(* Example 1 *)
let poe_ex1 =
  let buys =
    [ mk_order (Pegged (Midpoint, None)) 2500;
      loc (cents 2018) 500 ;
    ]
  in
  let sells =
    [ loc (cents 2018) 2000 ]
  in
  let fill = {
    buy_id = (List.hd buys).order_id;
    sell_id = (List.hd sells).order_id;
    fill_qty = 2000;
  }
  in
  assert (test (buys, sells, poe_mdata, cents 2019, [fill]))
;;

(* Example 2 *)
let poe_ex2 =
  let buys =
    [ mk_order (Pegged (Midpoint, None)) 2500;
      loc (cents 2019) 500 ;
    ]
  in
  let sells =
    [ loc (cents 2018) 2000 ]
  in
  let fills = [
    { buy_id = (List.hd buys).order_id;
      sell_id = (List.hd sells).order_id;
      fill_qty = 2000;
    };
  ]
  in
  assert (test (buys, sells, poe_mdata, cents 2019, fills))
;;

(* Example 3 *)
let poe_ex3 =
  let buys =
    [ mk_order (Pegged (Primary, Some (cents 2020))) 2500;
      loc (cents 2019) 500 ;
    ]
  in
  let sells =
    [ loc (cents 2019) 2000 ]
  in
  let fills = [
    { buy_id = (List.nth buys 1).order_id;
      sell_id = (List.hd sells).order_id;
      fill_qty = 500;
    };
    { buy_id = (List.hd buys).order_id;
      sell_id = (List.hd sells).order_id;
      fill_qty = 1500;
    };
  ]
  in
  assert (test (buys, sells, poe_mdata, cents 2019, fills));
  let poe_mdata = { poe_mdata with iex_signal = Some Buy } in
  let fills = [ List.hd fills ] in
  assert (test (buys, sells, poe_mdata, cents 2019, fills))
;;

(* Example 4 *)
let poe_ex4 =
  let buys =
    [ mk_order (Pegged (Midpoint, None)) 2500;
      loc (cents 2020) 500 ;
    ]
  in
  let sells =
    [ loc (cents 2020) 2000 ]
  in
  let fills = [
    { buy_id = (List.nth buys 1).order_id;
      sell_id = (List.hd sells).order_id;
      fill_qty = 500;
    };
    { buy_id = (List.hd buys).order_id;
      sell_id = (List.hd sells).order_id;
      fill_qty = 1500;
    };
  ]
  in
  assert (test (buys, sells, poe_mdata, cents 2020, fills))
;;

(* Example 5 *)
let poe_ex5_cont =
  let buys =
    [ mk_order (Pegged (DPeg, Some (cents 2021))) 2500;
      loc (cents 2020) 500 ;
    ]
  in
  let sells =
    [ loc (cents 2020) 2000 ]
  in
  let fills = [
    { buy_id = (List.nth buys 1).order_id;
      sell_id = (List.hd sells).order_id;
      fill_qty = 500;
    };
    { buy_id = (List.hd buys).order_id;
      sell_id = (List.hd sells).order_id;
      fill_qty = 1500;
    };
  ]
  in
  assert (test (buys, sells, poe_mdata, cents 2020, fills));
  let poe_mdata = { poe_mdata with iex_signal = Some Buy } in
  let fills = [ List.hd fills ] in
  assert (test (buys, sells, poe_mdata, cents 2020, fills))
;;