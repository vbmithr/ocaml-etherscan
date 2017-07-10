open Core
open Async
open Log.Global
open Etherscan

let account () =
  Account.balance
    ~log:(Lazy.force log) (`Hex "fb6916095ca1df60bb79ce92ce3ea74c37c5d359") >>= function
  | Ok v ->
    debug "%f" v ;
    Deferred.unit
  | Error err ->
    error "%s" (Http_error.to_string err) ;
    Deferred.unit

let transactions () =
    Account.transactions
    ~log:(Lazy.force log) (`Hex "fb6916095ca1df60bb79ce92ce3ea74c37c5d359") >>= function
  | Ok t ->
    sexp (Sexplib.Std.sexp_of_list Transaction.sexp_of_t t) ;
    Deferred.unit
  | Error err ->
    error "%s" (Http_error.to_string err) ;
    Deferred.unit

let main () =
  set_level `Debug ;
  stage begin fun `Scheduler_started ->
    Deferred.all_unit [
      account () ;
      (* transactions () ; *)
    ]
  end

let command =
  let spec =
    Command.Spec.empty
  in
  Command.Staged.async ~summary:"Etherscan test" spec main

let () = Command.run command
