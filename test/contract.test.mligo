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
        next_token_id         = 0n;
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

    (*
        UPDATE OPERATOR TESTS
    *)

    (*
        MARKETPLACE TESTS
    *)

    (*
        BURNING TEST
    *)

    ()