#include "./interface.mligo"
#include "./utils.mligo"
#include "./transfer.mligo"
#include "./update_operators.mligo"
#include "./balance_of.mligo"
#include "./mint_burn.mligo"
#include "./market_place.mligo"

let main (action, storage : parameter * storage) : return =
    match action with
    | Transfer p -> ([]: operation list), transfer (p, storage)
    | Update_operators p -> ([]: operation list), update_operators (p, storage)
    | Balance_of p -> balance_of (p, storage)
    | Mint p -> ([]: operation list), mint (p, storage)
    | Burn p -> ([]: operation list), burn (p, storage)
    | Update_admin p -> ([]: operation list), update_admin (p, storage)
    | Update_metadata p -> ([]: operation list), update_metadata (p, storage)
    | Set_on_market_place p -> set_on_market_place (p, storage)
    | Remove_from_market_place p -> remove_from_market_place (p, storage)
    | Buy_from_market_place p -> buy_from_market_place (p, storage)