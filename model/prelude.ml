(* @meta[imandra_ignore] on @end *)
let max (x,y) = max x y ;;
let min (x,y) = min x y ;;

module Reflect = struct
  let eval _ = ()
end ;;
(* @meta[imandra_ignore] off @end *)

type side = 
  | Buy
  | Sell
;;