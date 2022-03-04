#include "./main.mligo"
#import "./assertions.mligo" "TEST"
module ASSERT = TEST.ASSERT
module FORMAT = TEST.FORMAT

let test =
    let alice_address = Test.nth_bootstrap_account 0 in
    let bob_address = Test.nth_bootstrap_account 1 in

    let initial_storage = {
        ledger              = Big_map.literal [ 
                                ((alice_address, 0n), 1n);
                                ((alice_address, 1n), 1n);
                                ((alice_address, 2n), 1n);
                                ((alice_address, 3n), 1n)
                            ];
        operators           = (Big_map.empty: (operator, unit) big_map);
        metadata            = Big_map.literal [
                                ("", Bytes.pack "This is a test")
                            ];
        token_metadata      = Big_map.literal [ 
                                (0n, Bytes.pack "Mock metadata for NFT 0");
                                (1n, Bytes.pack "Mock metadata for NFT 1");
                                (2n, Bytes.pack "Mock metadata for NFT 2");
                                (3n, Bytes.pack "Mock metadata for NFT 3")
                            ];
        total_supply        = 4n;
        admin               = alice_address
    } in
    // originates the contract
    let nft_addr, _, _ = Test.originate main initial_storage 0tez in
    let _ = assert (Test.get_storage nft_addr = initial_storage) in
    let contract = Test.to_contract nft_addr in
    (*
        TESTS FOR TRANSFERS
    *)
    let _ = FORMAT.add_title "TESTS FOR TRANSFERS" in
    let _ = Test.set_source (alice_address) in
    // Alice transfers an NFT she owns to Bob
    let nft_to_transfer = 0n in
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
        ASSERT.ENTRYPOINT.should_succeed 
            (Test.transfer_to_contract contract (Transfer (params)) 0tez)
            (Some "Transfer from Alice to Bob successful!")
    in
    // checks that the NFT owners have been switched
    let new_storage = Test.get_storage nft_addr in
    let _ = ASSERT.BIG_MAP.has_key (bob_address, nft_to_transfer) new_storage.ledger "ledger" true in
    let _ = ASSERT.BIG_MAP.has_key (alice_address, nft_to_transfer) new_storage.ledger "ledger" false in
    // Alice tries to transfer an NFT of unknown id
    let _ = Test.set_source (alice_address) in
    let nft_to_transfer = 111n in
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
            (Test.transfer_to_contract contract (Transfer (params)) 0tez) 
            "FA2_TOKEN_UNDEFINED"
            (Some "Transfer from Bob to Alice failed!")
    in
    // Bobs tries to transfer an NFT he doesn't own
    let _ = Test.set_source (bob_address) in
    let nft_to_transfer = 1n in
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
            (Test.transfer_to_contract contract (Transfer (params)) 0tez) 
            "FA2_NOT_OPERATOR"
            (Some "Transfer from Alice to Bob with unknown NFT id failed!")
    in
    // Alice tries to transfer more than 1 NFT
    let _ = Test.set_source (alice_address) in
    let nft_to_transfer = 1n in
    let params = 
    [ 
        { 
            from_ = alice_address; 
            txs = [ 
                { to_ = bob_address; token_id = nft_to_transfer; amount = 2n } 
            ] 
        } 
    ] in
    let _ =
        ASSERT.ENTRYPOINT.should_fail_with_message 
            (Test.transfer_to_contract contract (Transfer (params)) 0tez) 
            "AMOUNT_CAN_ONLY_BE_1"
            (Some "Transfer from Alice to Bob with wrong NFT amount failed!")
    in
    // Alice attaches an XTZ amount to the transaction
    let _ = Test.set_source (alice_address) in
    let nft_to_transfer = 1n in
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
            (Test.transfer_to_contract contract (Transfer (params)) 1tez) 
            "NO_XTZ_AMOUNT"
            (Some "Transfer from Alice to Bob with XTZ amount failed!")
    in
    (*
        TESTS FOR UPDATE_OPERATORS
    *)
    let _ = FORMAT.add_title "TESTS FOR UPDATE_OPERATORS" in
    // Alice adds Bob as operator
    let _ = Test.set_source (alice_address) in
    let operator = {
        owner = alice_address;
        operator = bob_address;
        token_id = 1n;
    } in
    let params = [ Add_operator (operator) ] in
    let _ =
        ASSERT.ENTRYPOINT.should_succeed
            (Test.transfer_to_contract contract (Update_operators (params)) 0tez)
            (Some "Bob successfully added as operator for Alice")
    in
    // checks if Bob is listed as an operator for Alice
    let new_storage = Test.get_storage nft_addr in
    let _ = ASSERT.BIG_MAP.has_key operator new_storage.operators "operators" true in
    // lets Bob transfer NFT on behalf of Alice
    let _ = Test.set_source (bob_address) in
    let nft_to_transfer = 1n in
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
        ASSERT.ENTRYPOINT.should_succeed 
            (Test.transfer_to_contract contract (Transfer params) 0tez)
            (Some "Transfer from Alice to Bob with Bob operator successful!")
    in
    // checks that Bob owns the NFT now
    let new_storage = Test.get_storage nft_addr in
    let _ = ASSERT.BIG_MAP.has_key (bob_address, nft_to_transfer) new_storage.ledger "ledger" true in
    let _ = ASSERT.BIG_MAP.has_key (alice_address, nft_to_transfer) new_storage.ledger "ledger" false in
    // checks that Bob cannot transfer another one of Alice's NFTs
    let _ = Test.set_source (bob_address) in
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
    in
    // removes Bob as an operator
    let _ = Test.set_source (alice_address) in
    let operator = {
        owner = alice_address;
        operator = bob_address;
        token_id = 1n;
    } in
    let params = [ Remove_operator (operator) ] in
    let _ =
        ASSERT.ENTRYPOINT.should_succeed
            (Test.transfer_to_contract contract (Update_operators (params)) 0tez)
            (Some "Bob successfully removed as operator for Alice")
    in
    // checks if Bob is not listed as an operator for Alice anymore
    let new_storage = Test.get_storage nft_addr in
    let _ = ASSERT.BIG_MAP.has_key operator new_storage.operators "operators" false in
    (*
        TESTS FOR ADMIN FUNCTIONS
    *)
    let _ = FORMAT.add_title "TESTS FOR ADMIN FUNCTIONS" in
    // should let Alice update the admin address
    let _ = Test.set_source (alice_address) in
    let _ =
        ASSERT.ENTRYPOINT.should_succeed
            (Test.transfer_to_contract contract (Update_admin alice_address) 0tez)
            (Some "Alice successfully updated the admin's address")
    in
    // should prevent Bob from updating the admin address
    let _ = Test.set_source (bob_address) in
    let _ =
        ASSERT.ENTRYPOINT.should_fail_with_message
            (Test.transfer_to_contract contract (Update_admin bob_address) 0tez)
            "NOT_AN_ADMIN"
            (Some "Bob couldn't update the admin's address")
    in
    // should let Alice update the contract metadata
    let _ = Test.set_source (alice_address) in
    let metadata = Bytes.pack "Dummy metadata" in
    let _ =
        ASSERT.ENTRYPOINT.should_succeed
            (Test.transfer_to_contract contract (Update_metadata metadata) 0tez)
            (Some "Alice successfully updated the contract metadata")
    in
    // checks if the metadata are correctly saved
    let new_storage = Test.get_storage nft_addr in
    let _ = ASSERT.BIG_MAP.has_key "contents" new_storage.metadata "metadata" true in
    let _ = ASSERT.BIG_MAP.value_equals "contents" metadata new_storage.metadata in
    // should let Alice update the token metadata
    let _ = Test.set_source (alice_address) in
    let token_metadata = Bytes.pack "Dummy token metadata" in
    let token_id = 0n in
    let _ =
        ASSERT.ENTRYPOINT.should_succeed
            (Test.transfer_to_contract contract (Update_token_metadata (token_id, token_metadata)) 0tez)
            (Some "Alice successfully updated the token metadata")
    in
    // checks if the metadata are correctly saved
    let new_storage = Test.get_storage nft_addr in
    let _ = ASSERT.BIG_MAP.has_key token_id new_storage.token_metadata "token_metadata" true in
    let _ = ASSERT.BIG_MAP.value_equals token_id token_metadata new_storage.token_metadata in
    // should prevent Alice from updating an NFT that doesn't exist
    let token_id = 111n in
    let _ =
        ASSERT.ENTRYPOINT.should_fail_with_message
        (Test.transfer_to_contract contract (Update_token_metadata (token_id, token_metadata)) 0tez)
        "FA2_TOKEN_UNDEFINED"
        (Some "Alice failed to update the metadata of an unknown token")
    in

    ()