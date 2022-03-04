// lets users mint new NFTs
let mint (p, s: mint_param * storage): storage =
    let { token_amount = token_amount; ipfs_hash = ipfs_hash } = p in
    // token_metadata must be a valid IPFS hash
    if String.length ipfs_hash <> 46n || String.sub 0n 1n ipfs_hash <> "Q"
    then (failwith "INVALID_IPFS_HASH": storage)
    else
        // mints the new NFTs
        let new_ledger = Big_map.add (Tezos.sender, s.next_nft_id) token_amount s.ledger in
        let token_info = 
            { token_id = s.next_nft_id; token_info = Map.literal [ ("", Bytes.pack ("ipfs://" ^ ipfs_hash)) ] } in
        let new_token_metadata = Big_map.add s.next_nft_id token_info s.token_metadata in

        {
            s with
                ledger          = new_ledger;
                token_metadata  = new_token_metadata;
                next_nft_id     = s.next_nft_id + 1n;
                total_tokens    = s.total_tokens + token_amount;
        }

// lets users burn existing NFTs
let burn (p, s: burn_param * storage): storage =
    let { token_id = token_id; token_amount = token_amount } = p in
    // user must own the required amount of tokens
    match Big_map.find_opt (Tezos.sender, token_id) s.ledger with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE": storage)
    | Some blnc -> 
        // checks if balance is enough
        if blnc < token_amount
        then (failwith "FA2_INSUFFICIENT_BALANCE": storage)
        else
            let new_balance = abs (blnc - token_amount) in
            let new_ledger = Big_map.update (Tezos.sender, token_id) (Some new_balance) s.ledger in
            // cleans up token_metadata bigmap if new_balance = 0n
            let new_token_metadata =
                if new_balance = 0n
                then Big_map.remove token_id s.token_metadata
                else s.token_metadata
            in
            {
                s with
                    ledger          = new_ledger;
                    token_metadata  = new_token_metadata;
                    total_tokens    = abs (s.total_tokens - token_amount);
            }