// Allows users to set their NFTs on the marketplace to sell them
let set_on_market_place (new_entry, s: new_market_place_entry * storage): operation list * storage = 
    let { token_id = token_id; token_amount = token_amount; price_per_token = price_per_token } = new_entry in
    if Tezos.amount > 0tez
    then (failwith "NO_XTZ_AMOUNT": operation list * storage)
    else
        // verifies that the token id exists
        if not Big_map.mem token_id s.token_metadata
        then (failwith "FA2_TOKEN_UNDEFINED": operation list * storage)
        else
            // verifies that the sender owns the right amount of tokens
            match Big_map.find_opt (Tezos.sender, token_id) s.ledger with
            | None -> (failwith "FA2_INSUFFICIENT_BALANCE": operation list * storage)
            | Some blnc ->
                if blnc < token_amount
                then (failwith "FA2_INSUFFICIENT_BALANCE": operation list * storage)
                else
                    // ownership of the tokens is transferred to the contract
                    let transfer_op = forge_transfer_op Tezos.sender Tezos.self_address token_id token_amount in
                    // new entry is created on the marketplace
                    let new_entry: market_place_entry = {
                        price_per_token = price_per_token;
                        token_amount    = token_amount;
                        timestamp       = Tezos.now;
                    } in
                    let new_market_place: market_place = Big_map.add (token_id, Tezos.sender) new_entry s.market_place in

                    [transfer_op],
                    {
                        s with
                            market_place = new_market_place
                    }

// Allows users to remove their NFTs from the marketplace
let remove_from_market_place (token_id, s: token_id * storage): operation list * storage =
    if Tezos.amount > 0tez
    then (failwith "NO_XTZ_AMOUNT": operation list * storage)
    else
        // finds the market place entry
        match Big_map.find_opt (token_id, Tezos.sender) s.market_place with
        | None -> (failwith "UNKNOWN_MARKET_PLACE_ENTRY": operation list * storage)
        | Some entry ->
            // transfers the tokens back to their owner
            let transfer_op = forge_transfer_op Tezos.self_address Tezos.sender token_id entry.token_amount in
            // removes entry from marketplace
            let new_market_place: market_place =
                Big_map.remove (token_id, Tezos.sender) s.market_place in

            [transfer_op],
            {
                s with
                    market_place = new_market_place
            }

// Allows users to buy other users' NFTs from the mnarketplace
let buy_from_market_place (p, s: buy_from_market_place_param * storage): operation list * storage = 
    let { token_id = token_id; token_amount = tokens_to_buy; seller = seller } = p in
    // finds the market place entry
    match Big_map.find_opt (token_id, seller) s.market_place with
    | None -> (failwith "UNKNOWN_MARKET_PLACE_ENRTY": operation list * storage)
    | Some entry ->
        // checks if there are enough tokens to buy
        if entry.token_amount < tokens_to_buy
        then (failwith "FA2_INSUFFICIENT_BALANCE": operation list * storage)
        else if Tezos.amount <> entry.price_per_token * tokens_to_buy
        then (failwith "WRONG_AMOUNT_FOR_PAYMENT": operation list * storage)
        else
            let tokens_left = abs (entry.token_amount - tokens_to_buy) in
            // forges transfer operation to buyer
            let transfer_op = forge_transfer_op Tezos.self_address Tezos.sender token_id tokens_to_buy in
            // forges payment transaction to seller
            let seller_contract: unit contract = Tezos.get_contract_with_error seller "UNKNOWN_SELLER_ACCOUNT" in
            let payment_op = Tezos.transaction unit Tezos.amount seller_contract in
            // updates marketplace
            let new_market_place =
                if tokens_left = 0n
                then Big_map.remove (token_id, seller) s.market_place
                else Big_map.update (token_id, seller) (Some { entry with token_amount = tokens_left }) s.market_place
            in

            [payment_op; transfer_op],
            {
                s with
                    market_place = new_market_place
            }