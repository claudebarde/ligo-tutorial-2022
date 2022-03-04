type token_id = nat
type ipfs_hash = string

(*
    TRANSFER PARAMS
*)

type transfer_to =
[@layout:comb]
{
    to_     : address;
    token_id: token_id;
    amount  : nat;
}

type transfer_param = 
[@layout:comb]
{
    from_   : address;
    txs     : transfer_to list
}

(*
    UPDATE OPERATORS PARAMS
*)

type operator =
[@layout:comb]
{
    owner   : address;
    operator: address;
    token_id: token_id
}

type update_operators_param =
| Add_operator of operator
| Remove_operator of operator

(*
    BALANCE OF PARAMS
*)

type balance_of_request =
[@layout:comb]
{
    owner   : address;
    token_id: token_id;
}

type balance_of_callback_param =
[@layout:comb]
{
    request: balance_of_request;
    balance: nat;
}

type balance_of_param =
[@layout:comb]
{
    requests: balance_of_request list;
    callback: (balance_of_callback_param list) contract;
}

(*
    MINT & BURN
*)

type mint_param =
[@layout:comb]
{
    token_amount    : nat;
    ipfs_hash       : ipfs_hash;
}

type burn_param =
[@layout:comb]
{
    token_id    : nat;
    token_amount: nat;
}

(*
    MARKET PLACE ACTIONS
*)

type market_place_entry =
{
    price_per_token : tez;
    token_amount    : nat;
    timestamp       : timestamp;
}

type new_market_place_entry =
[@layout:comb]
{
    token_id        : token_id;
    token_amount    : nat;
    price_per_token : tez;
}

type buy_from_market_place_param =
[@layout:comb]
{
    token_id    : token_id;
    token_amount: nat;
    seller      : address;
}

(*
    STORAGE AND PARAMETER
*)

// parameter type
type parameter = 
    | Transfer of transfer_param list
    | Update_operators of update_operators_param list
    | Balance_of of balance_of_param
    | Mint of mint_param
    | Burn of burn_param
    | Update_admin of address
    | Update_metadata of bytes
    | Set_on_market_place of new_market_place_entry
    | Remove_from_market_place of token_id
    | Buy_from_market_place of buy_from_market_place_param

// storage type
type ledger = ((address * token_id), nat) big_map
type market_place = ((token_id * address), market_place_entry) big_map // token_id/seller => token_amount/price
type token_info = 
{
    token_id    : token_id;
    token_info  : (string, bytes) map;
}
type storage =
{
    ledger              : ledger;
    market_place        : market_place;
    operators           : (operator, unit) big_map;
    metadata            : (string, bytes) big_map;
    token_metadata      : (token_id, token_info) big_map;
    total_tokens        : nat;
    admin               : address;
    next_nft_id         : nat;
}

type return = (operation list) * storage