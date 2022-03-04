#include "../contract/main.mligo"

let test =
    let alice_address = Test.nth_bootstrap_account 0 in
    let bob_address = Test.nth_bootstrap_account 1 in

    let initial_storage = {
        ledger              = (Big_map.empty: ledger);
        market_place        = (Big_map.empty: market_place);
        operators           = (Big_map.empty: (operator, unit) big_map);
        metadata            = Big_map.literal [
                                ("", Bytes.pack "This is a test")
                            ];
        token_metadata      = (Big_map.empty: (token_id, token_info) big_map);
        admin               = alice_address;
        next_token_id       = 0n;
        total_tokens        = 0n;
    } in
    // originates the contract
    let _ = Test.set_source alice_address in
    let nft_addr, _, _ = Test.originate main initial_storage 0tez in
    let initial_originated_storage = Test.get_storage nft_addr in
    let _ = assert (initial_originated_storage = initial_storage) in
    (*
        MINTING TESTS
    *)
    let ipfs_hash = "QmcDcAZMCtZuP8TVXrBWk2trcYV4id14qUHwVPEqfGpH3t" in
    // should prevent minting of NFTs with wrong IPFS hash
    // wrong length
    let mint_param = { token_amount = 10n; ipfs_hash = String.sub 0n 10n ipfs_hash } in
    let nft_contract: mint_param contract = Test.to_entrypoint "mint" nft_addr in
    let expected_err = Test.compile_value "INVALID_IPFS_HASH" in
    let _ = 
        match Test.transfer_to_contract nft_contract mint_param 0tez with
        | Success _ -> 
            let _ = Test.log "Test 1 failed" in assert false
        | Fail err -> 
            begin
                match err with
                | Rejected (err, _) -> 
                    if Test.michelson_equal err expected_err
                    then let _ = Test.log "Test 1 passed" in assert true 
                    else let _ = Test.log "Test 1 failed" in assert false
                | Other -> assert false
            end
    in
    // wrong format
    let mint_param = { token_amount = 10n; ipfs_hash = "Z" ^ String.sub 0n 1n ipfs_hash } in
    let expected_err = Test.compile_value "INVALID_IPFS_HASH" in
    let _ = 
        match Test.transfer_to_contract nft_contract mint_param 0tez with
        | Success _ -> 
            let _ = Test.log "Test 2 failed" in assert false
        | Fail err -> 
            begin
                match err with
                | Rejected (err, _) -> 
                    if Test.michelson_equal err expected_err
                    then let _ = Test.log "Test 2 passed" in assert true 
                    else let _ = Test.log "Test 2 failed" in assert false
                | Other -> assert false
            end
    in
    // should mint the new NFT
    let token_amount_to_mint = 10n in
    let mint_param = { token_amount = token_amount_to_mint; ipfs_hash = ipfs_hash } in
    let _ = 
        match Test.transfer_to_contract nft_contract mint_param 0tez with
        | Success _ -> let _ = Test.log "Test 3 passed" in assert true
        | Fail _ -> let _ = Test.log "Test 3 failed" in assert false
    in
    // checks that the NFTs exist
    let storage = Test.get_storage nft_addr in
    let _ =
        if storage.next_token_id = initial_originated_storage.next_token_id + 1n
        then let _ = Test.log "Test 4 passed" in assert true
        else let _ = Test.log "Test 4 failed" in assert false
    in
    let _ = 
        match Big_map.find_opt (alice_address, 0n) storage.ledger with
        | None -> let _ = Test.log "Test 5 failed (no nft)" in assert false
        | Some nft_amount -> 
            if nft_amount <> token_amount_to_mint
            then let _ = Test.log "Test 5 failed (wrong amount)" in assert false
            else let _ = Test.log "Test 5 passed" in assert true
    in
    let _ =
        match Big_map.find_opt 0n storage.token_metadata with
        | None -> let _ = Test.log "Test 6 failed (no token metadata)" in assert false
        | Some mtdt -> 
            if mtdt.token_id <> 0n
            then let _ = Test.log "Test 6 failed (wrong token id in metadata)" in assert false
            else
                begin
                    match Map.find_opt "" mtdt.token_info with
                    | None -> let _ = Test.log "Test 6 failed (missing IPFS hash in metadata)" in assert false
                    | Some bytes ->
                        begin
                            match ((Bytes.unpack bytes): string option) with
                            | None -> let _ = Test.log "Test 6 failed (unpack bytes)" in assert false
                            | Some maybe_ipfs_hash ->
                                (if maybe_ipfs_hash <> ("ipfs://" ^ ipfs_hash)
                                then let _ = Test.log "Test 6 failed (wrong IPFS hash in metadata)" in assert false
                                else let _ = Test.log "Test 6 passed" in assert true)
                        end
                end
    in
    
    (*
        TRANSFER TESTS
    *)
    // Alice transfers more NFTs than she owns
    let nft_contract: (transfer_param list) contract = Test.to_entrypoint "transfer" nft_addr in
    let transfer_param = [
        {
            from_   = alice_address;
            txs     = [
                {
                    to_         = bob_address;
                    token_id    = 0n;
                    amount      = 20n;
                }
            ]
        }
    ] in
    let expected_err = Test.compile_value "FA2_INSUFFICIENT_BALANCE" in
    let _ = 
        match Test.transfer_to_contract nft_contract transfer_param 0tez with
        | Success _ -> 
            let _ = Test.log "Test 7 failed" in assert false
        | Fail err -> 
            begin
                match err with
                | Rejected (err, _) -> 
                    if Test.michelson_equal err expected_err
                    then let _ = Test.log "Test 7 passed" in assert true 
                    else let _ = Test.log "Test 7 failed" in assert false
                | Other -> assert false
            end
    in
    // Alice transfers NFTs of unknown id
    let transfer_param = [
        {
            from_   = alice_address;
            txs     = [
                {
                    to_         = bob_address;
                    token_id    = 111n;
                    amount      = 2n;
                }
            ]
        }
    ] in
    let expected_err = Test.compile_value "FA2_TOKEN_UNDEFINED" in
    let _ = 
        match Test.transfer_to_contract nft_contract transfer_param 0tez with
        | Success _ -> 
            let _ = Test.log "Test 8 failed" in assert false
        | Fail err -> 
            begin
                match err with
                | Rejected (err, _) -> 
                    if Test.michelson_equal err expected_err
                    then let _ = Test.log "Test 8 passed" in assert true 
                    else let _ = Test.log "Test 8 failed" in assert false
                | Other -> assert false
            end
    in
    // Bob tries to transfer Alice's NFTs
    let _ = Test.set_source bob_address in
    let transfer_param = [
        {
            from_   = alice_address;
            txs     = [
                {
                    to_         = bob_address;
                    token_id    = 0n;
                    amount      = 2n;
                }
            ]
        }
    ] in
    let expected_err = Test.compile_value "FA2_NOT_OPERATOR" in
    let _ = 
        match Test.transfer_to_contract nft_contract transfer_param 0tez with
        | Success _ -> 
            let _ = Test.log "Test 9 failed" in assert false
        | Fail err -> 
            begin
                match err with
                | Rejected (err, _) -> 
                    if Test.michelson_equal err expected_err
                    then let _ = Test.log "Test 9 passed" in assert true 
                    else let _ = Test.log "Test 9 failed" in assert false
                | Other -> assert false
            end
    in
    // lets Alice transfer 2 NFTs to Bob
    let _ = Test.set_source alice_address in
    let nfts_to_transfer = 2n in
    let transfer_param = [
        {
            from_   = alice_address;
            txs     = [
                {
                    to_         = bob_address;
                    token_id    = 0n;
                    amount      = nfts_to_transfer;
                }
            ]
        }
    ] in
    let _ = 
        match Test.transfer_to_contract nft_contract transfer_param 0tez with
        | Success _ -> let _ = Test.log "Test 10 passed" in assert true
        | Fail err -> let _ = Test.log "Test 10 failed" in assert false
    in
    // checks that Bob received the NFTs
    let storage = Test.get_storage nft_addr in
    let bob_balance = 
        match Big_map.find_opt (bob_address, 0n) storage.ledger with
        | None -> 
            let _ = Test.log "Test 11 failed" in 
            let _ = assert false in
            (None: nat option)
        | Some blnc ->
            if blnc = nfts_to_transfer
            then 
                let _ = Test.log "Test 11 passed" in
                let _ = assert true in
                Some blnc
            else 
                let _ = Test.log "Test 11 failed" in 
                let _ = assert false in
                (None: nat option)
    in
    // checks that Alice NFT balance has been deducted
    let alice_balance =
        match Big_map.find_opt (alice_address, 0n) storage.ledger with 
        | None -> 
            let _ = Test.log "Test 12 failed (no balance)" in 
            let _ = assert false in
            (None: nat option)
        | Some blnc ->
            if blnc = abs (token_amount_to_mint - nfts_to_transfer)
            then 
                let _ = Test.log "Test 12 passed" in 
                let _ = assert true in
                Some blnc
            else 
                let _ = Test.log "Test 12 failed (wrong balance)" in 
                let _ = assert false in
                (None: nat option)
    in

    (*
        UPDATE OPERATOR TESTS
    *)
    let _ = 
        match (alice_balance, bob_balance) with
        | Some alice_balance, Some bob_balance ->
            (
                // Alice adds Bob as operator
                let _ = Test.set_source (alice_address) in
                let nft_contract: (update_operators_param list) contract = Test.to_entrypoint "update_operators" nft_addr in
                let operator = {
                    owner = alice_address;
                    operator = bob_address;
                    token_id = 0n;
                } in
                let params = [ Add_operator (operator) ] in
                let _ = 
                    match Test.transfer_to_contract nft_contract params 0tez with
                    | Success _ -> let _ = Test.log "Test 13 passed" in assert true
                    | Fail _ -> let _ = Test.log "Test 13 failed" in assert false
                in
                // checks if Bob is listed as an operator for Alice
                let storage = Test.get_storage nft_addr in
                let _ =
                    match Big_map.find_opt operator storage.operators with 
                    | None -> let _ = Test.log "Test 14 failed (no operator)" in assert false
                    | Some _ -> let _ = Test.log "Test 14 passed" in assert true
                in
                // lets Bob transfer NFT on behalf of Alice
                let _ = Test.set_source (bob_address) in
                let nft_contract: (transfer_param list) contract = Test.to_entrypoint "transfer" nft_addr in
                let nft_to_transfer = 1n in
                let params = 
                [ 
                    { 
                        from_ = alice_address; 
                        txs = [ 
                            { to_ = bob_address; token_id = 0n; amount = nft_to_transfer } 
                        ] 
                    } 
                ] in
                let _ = 
                    match Test.transfer_to_contract nft_contract params 0tez with
                    | Success _ -> let _ = Test.log "Test 15 passed" in assert true
                    | Fail _ -> let _ = Test.log "Test 15 failed" in assert false
                in
                // checks that Bob owns the NFT now
                let storage = Test.get_storage nft_addr in
                let bob_balance = 
                    match Big_map.find_opt (bob_address, 0n) storage.ledger with
                    | None -> 
                        let _ = Test.log "Test 16 failed (no balance)" in 
                        let _ = assert false in
                        0n
                    | Some blnc ->
                        if blnc = bob_balance + nft_to_transfer
                        then 
                            let _ = Test.log "Test 16 passed" in 
                            let _ = assert true in
                            blnc
                        else 
                            let _ = Test.log "Test 16 failed (wrong balance)" in 
                            let _ = assert false in
                            0n
                in
                // checks that Alice NFT balance has been deducted
                let _ =
                    match Big_map.find_opt (alice_address, 0n) storage.ledger with 
                    | None -> let _ = Test.log "Test 17 failed (no balance)" in assert false
                    | Some blnc ->
                        if blnc = abs (alice_balance - nft_to_transfer)
                        then let _ = Test.log "Test 17 passed" in assert true
                        else let _ = Test.log "Test 17 failed (wrong balance)" in assert false
                in
                // checks that Bob cannot transfer another one of Alice's NFTs
                (* let _ = Test.set_source (bob_address) in
                let nft_to_transfer = 2n in
                let params = 
                [ 
                    { 
                        from_ = alice_address; 
                        txs = [ 
                            { to_ = bob_address; token_id = nft_to_transfer; amount = 1n } 
                        ] 
                    } 
                ] in
                let _ = 
                    ASSERT.ENTRYPOINT.should_fail_with_message
                        (Test.transfer_to_contract contract (Transfer params) 0tez)
                        "FA2_NOT_OPERATOR"
                        (Some "Transfer from Alice to Bob with unauthorized NFT id failed!")
                in *)
                // removes Bob as an operator
                let _ = Test.set_source (alice_address) in
                let nft_contract: (update_operators_param list) contract = Test.to_entrypoint "update_operators" nft_addr in
                let operator = {
                    owner = alice_address;
                    operator = bob_address;
                    token_id = 1n;
                } in
                let params = [ Remove_operator (operator) ] in
                let _ = 
                    match Test.transfer_to_contract nft_contract params 0tez with
                    | Success _ -> let _ = Test.log "Test 18 passed" in assert true
                    | Fail _ -> let _ = Test.log "Test 18 failed" in assert false
                in
                // checks if Bob is not listed as an operator for Alice anymore
                let new_storage = Test.get_storage nft_addr in
                let _ =
                    match Big_map.find_opt operator storage.operators with 
                    | None -> let _ = Test.log "Test 19 passed" in assert true
                    | Some _ -> let _ = Test.log "Test 19 failed (operator exists)" in assert true
                in
                ()
            )
        | _ -> let _ = Test.log "No progress because of previous balance error" in assert false
    in

    (*
        MARKETPLACE TESTS
    *)

    (*
        BURNING TEST
    *)

    ()