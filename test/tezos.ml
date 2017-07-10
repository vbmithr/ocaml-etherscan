open Core
open Async
open Etherscan
open Log.Global

let tezos = `Hex "b56d622DDF60ec532B5f43B4Ff9B0e7b1FF92dB3"

let rec run offset page ((nb_e, nb_ok, v_e, v_ok) as acc) =
  Account.transactions ~page ~offset tezos >>= function
  | Error err ->
    error "%s" (Http_error.to_string err) ;
    Deferred.return acc
  | Ok ts ->
    let ((nb_e', nb_ok', v_e', v_ok') as acc') =
      List.fold_left ts ~init:acc ~f:begin fun (nb_e, nb_ok, v_e, v_ok) t ->
        if t.Transaction.isError then
          (succ nb_e, nb_ok, v_e +. t.value, v_ok)
        else
          (nb_e, succ nb_ok, v_e, v_ok +. t.value)
      end in
    let len = nb_e' + nb_ok' - nb_e - nb_ok in
    debug "Loaded %d transactions, page %d" len page ;
    if len = offset then
      run offset (succ page) acc'
    else
      Deferred.return acc'

let main offset () =
  set_level `Debug ;
  stage begin fun `Scheduler_started ->
    run offset 1 (0, 0, 0., 0.) >>| fun (nb_e, nb_ok, v_e, v_ok) ->
    let nb_total = nb_e + nb_ok in
    let v_total = v_e +. v_ok in
    let error_ratio = nb_e // nb_total in
    let success_ratio = nb_ok // nb_total in
    let error_ratio_v = v_e /. v_total in
    let success_ratio_v = v_ok /. v_total in
    Caml.Format.printf "#TX: %d@." nb_total ;
    Caml.Format.printf "#TX OK: %d@." nb_ok ;
    Caml.Format.printf "#TX Error: %d@." nb_e ;
    Caml.Format.printf "ETH Total: %f@." (v_total /. 1e18) ;
    Caml.Format.printf "ETH OK: %f@." (v_ok /. 1e18) ;
    Caml.Format.printf "ETH Error: %f@." (v_e /. 1e18) ;
    Caml.Format.printf "Success ratio #: %f@." success_ratio ;
    Caml.Format.printf "Error ratio #: %f@." error_ratio ;
    Caml.Format.printf "Success ratio ETH: %f@." success_ratio_v ;
    Caml.Format.printf "Error ratio ETH: %f@." error_ratio_v ;
  end

let command =
  let spec =
    let open Command.Spec in
    Command.Spec.empty
    +> flag "-offset" (optional_with_default 1000 int) ~doc:"int number of records to download at each call"
  in
  Command.Staged.async ~summary:"Etherscan test" spec main

let () = Command.run command

