open Core
open Async
open Cohttp_async
module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)

let ssl_config = Conduit_async.Ssl.configure ~version:Tlsv1_2 ()

exception Client of string
exception Server of string
exception Etherscan of string

module Http_error = struct
  type t =
    | Cohttp of exn
    | Client of string
    | Server of string
    | Etherscan of string
    | Data_encoding of string
    | Data_shape of string

  let etherscan_str msg = Etherscan msg
  let etherscan k =
    Format.kasprintf (fun msg -> Etherscan msg) k

  let etherscan_fail msg = Result.fail (Etherscan msg)

  let etherscan_failf k =
    Format.kasprintf (fun msg -> Result.fail (Etherscan msg)) k

  let data_encoding exn =
    let msg =
      Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn in
    Result.fail (Data_encoding msg)

  let data_shape msg = Result.fail (Data_shape msg)

  let to_string = function
    | Cohttp exn -> Exn.to_string exn
    | Client msg -> "HTTP Client error: " ^ msg
    | Server msg -> "HTTP Server error: " ^ msg
    | Etherscan msg -> "Etherscan error: " ^ msg
    | Data_encoding msg -> "Data encoding error: " ^ msg
    | Data_shape msg -> "Data_shape error: " ^ msg
end

module Encoding = struct
  open Json_encoding

  let string_bool =
    string_enum [
      "1", true ;
      "0", false ;
    ]

  let string_float =
    conv Float.to_string Float.of_string string

  let string_int =
    conv Int.to_string Int.of_string string

  let string_time =
    conv
      (fun ts -> Time_ns.to_int_ns_since_epoch ts / 1_000_000_000 |> Int.to_string)
      (fun s -> Time_ns.of_int_ns_since_epoch (Int.of_string s * 1_000_000_000))
      string
end

module Response = struct
  type t = Json_repr.any Or_error.t

  let encoding =
    let open Json_encoding in
    conv
      (fun _ -> invalid_arg "Response.encoding: not implemented")
      (fun (status, message, result) ->
         if status then Ok result else Error (Http_error.etherscan_str message))
      (obj3
         (req "status" Encoding.string_bool)
         (req "message" string)
         (req "result" any_value))
end

let safe_get ?buf ?log url =
  Monitor.try_with begin fun () ->
    Client.get ~ssl_config url >>= fun (resp, body) ->
    let status_code = Cohttp.Code.code_of_status resp.status in
    Body.to_string body >>| fun body_str ->
    Option.iter log ~f:(fun log -> Log.debug log "%s" body_str) ;
    let body_json = Yojson.Safe.from_string ?buf body_str in
    if Cohttp.Code.is_client_error status_code then raise (Client body_str)
    else if Cohttp.Code.is_server_error status_code then raise (Server body_str)
    else
      Yojson_encoding.destruct Response.encoding body_json
  end >>| function
  | Ok (Ok res) -> Ok res
  | Ok (Error err) -> Error err
  | Error err -> match err with
    | Client str -> Error (Http_error.Client str)
    | Server str -> Error (Server str)
    | exn -> Error (Cohttp exn)

let base_uri = Uri.of_string "https://api.etherscan.io/api"

module Transaction = struct
  type t = {
    blockNumber: int ;
    timeStamp: Time_ns.t ;
    hash: string ;
    nonce: string ;
    blockHash: string ;
    transactionIndex: int ;
    from: string ;
    to_: string ;
    value: Float.t ;
    gas: int ;
    gasPrice: int ;
    isError: bool ;
    input: string ;
    contractAddress: string ;
    cumulativeGasUsed: int ;
    gasUsed: int ;
    confirmations: int ;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    let open Encoding in
    conv
      (fun _ -> invalid_arg "Transaction.encoding: not implemented")
      (fun ((blockNumber, timeStamp, hash, nonce, blockHash,
             transactionIndex, from, to_, value, gas),
            (gasPrice, isError, input, contractAddress,
             cumulativeGasUsed, gasUsed, confirmations)) ->
        { blockNumber ; timeStamp ; hash ; nonce ; blockHash ;
          transactionIndex ; from ; to_ ; value ; gas ;
          gasPrice ; isError ; input ; contractAddress ;
          cumulativeGasUsed ; gasUsed ; confirmations })
      begin
        merge_objs
          (obj10
             (req "blockNumber" string_int)
             (req "timeStamp" string_time)
             (req "hash" string)
             (req "nonce" string)
             (req "blockHash" string)
             (req "transactionIndex" string_int)
             (req "from" string)
             (req "to" string)
             (req "value" string_float)
             (req "gas" string_int))
          (obj7
             (req "gasPrice" string_int)
             (req "isError" string_bool)
             (req "input" string)
             (req "contractAddress" string)
             (req "cumulativeGasUsed" string_int)
             (req "gasUsed" string_int)
             (req "confirmations" string_int))
      end
end

module Account = struct
  let balance ?buf ?log (`Hex address) =
    let url = Uri.with_query' base_uri [
        "module", "account" ;
        "action", "balance" ;
        "address", "0x" ^ address ;
      ] in
    safe_get ?buf ?log url >>|
    Result.map ~f:begin fun v ->
      Yojson_encoding.destruct
        Encoding.string_float Json_repr.(any_to_repr (module Yojson) v)
    end

  type sort = Asc | Desc
  let string_of_sort = function Asc -> "asc" | Desc -> "desc"

  let transactions ?buf ?log ?start_block ?end_block ?page ?offset ?sort (`Hex address) =
    let url = Uri.with_query' base_uri (List.filter_opt [
        Some ("module", "account") ;
        Some ("action", "txlist") ;
        Some ("address", "0x" ^ address) ;
        Option.map start_block ~f:(fun v -> "startblock", Int.to_string v) ;
        Option.map end_block ~f:(fun v -> "endblock", Int.to_string v) ;
        Option.map page ~f:(fun v -> "page", Int.to_string v) ;
        Option.map offset ~f:(fun v -> "offset", Int.to_string v) ;
        Option.map sort ~f:(fun v -> "sort", string_of_sort v) ;
      ]) in
    safe_get ?buf ?log url >>|
    Result.map ~f:begin fun v ->
      try
        Yojson_encoding.destruct
          Json_encoding.(list Transaction.encoding)
          Json_repr.(any_to_repr (module Yojson) v)
      with exn ->
        let s =
          Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn in
        failwith s
    end
end
