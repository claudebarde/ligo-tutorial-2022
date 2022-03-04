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
        next_nft_id         = 0n;
        total_tokens        = 0n;
    } in
    // originates the contract
    let nft_addr, _, _ = Test.originate main initial_storage 0tez in
    let _ = assert (Test.get_storage nft_addr = initial_storage) in
    (*
        MINTING TESTS
    *)

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