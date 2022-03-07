let apply_transfer (((from, s), transfer): (address * storage) * transfer_to): address * storage =
    let { to_ = recipient; token_id = token_id; amount = amt } = transfer in
    // checks if token_id is valid
    if not Big_map.mem token_id s.token_metadata
    then (failwith "FA2_TOKEN_UNDEFINED": address * storage)
    else
        // fetches user's balance
        let sender_balance: nat =
            match Big_map.find_opt (from, token_id) s.ledger with
            | None -> 0n
            | Some b -> b 
        in
        // calculates sender's balance
        if sender_balance < amt
        then (failwith "FA2_INSUFFICIENT_BALANCE": address * storage)
        else
            // checks is sender is allowed to request a transfer
            let operator = { owner = from; operator = Tezos.sender; token_id = token_id } in
            if Tezos.sender <> from && not Big_map.mem operator s.operators
            then (failwith "FA2_NOT_OPERATOR": address * storage)
            else
                // updates the sender's account
                let new_ledger: ledger = 
                    Big_map.update (from, token_id) (Some (abs (sender_balance - amt))) s.ledger in
                // adds the token to the recipient's account
                let new_ledger: ledger =
                    match Big_map.find_opt (recipient, token_id) new_ledger with
                    | None -> Big_map.add (recipient, token_id) amt new_ledger
                    | Some blnc -> Big_map.update (recipient, token_id) (Some (blnc + amt)) new_ledger
                in

                from, { s with ledger = new_ledger }

let process_transfer (s, transfer: storage * transfer_param): storage =
    let { from_ = from; txs = txs } = transfer in
    let (_, new_storage): address * storage =
        List.fold apply_transfer txs (from, s)
    in new_storage

let transfer (transfer_list, s: (transfer_param list) * storage): storage =
    if Tezos.amount > 0tez
    then (failwith "NO_XTZ_AMOUNT": storage)
    else List.fold process_transfer transfer_list s