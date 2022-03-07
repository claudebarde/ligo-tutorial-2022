(*
    UTILS functions reserved for admin
*)

(* Updates the admin's address *)
let update_admin (p, s: address * storage): storage =
    if Tezos.sender <> s.admin
    then (failwith "NOT_AN_ADMIN": storage)
    else { s with admin = p }

(* Updates the metadata *)
let update_metadata (p, s: bytes * storage): storage =
    if Tezos.sender <> s.admin
    then (failwith "NOT_AN_ADMIN": storage)
    else { s with metadata = Big_map.update "contents" (Some (p)) s.metadata }

let forge_transfer_op (from_: address) (to_: address) (token_id: token_id) (token_amount: nat): operation =
    let transfer_param: transfer_param list = [
        {
            from_   = from_;
            txs     = [
                {
                    to_         = to_;
                    token_id    = token_id;
                    amount      = token_amount;
                }
            ]
        }
    ] in
    let self_contract: (transfer_param list) contract =
        match ((Tezos.get_entrypoint_opt "%transfer" Tezos.self_address): (transfer_param list) contract option) with
        | None -> (failwith "UNKNOWN_CONTRACT_FOR_TRANSFER": (transfer_param list) contract)
        | Some c -> c
    in Tezos.transaction transfer_param 0tez self_contract