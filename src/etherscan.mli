open Core
open Async

module Http_error : sig
  type t =
    | Cohttp of exn
    | Client of string
    | Server of string
    | Etherscan of string
    | Data_encoding of string
    | Data_shape of string

  val to_string : t -> string
end

module Transaction : sig
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
end

module Account : sig
  val balance :
    ?buf:Bi_outbuf.t ->
    ?log:Async.Log.t ->
    Hex.t ->
    (Float.t, Http_error.t) Result.t Deferred.t

  type sort = Asc | Desc

  val transactions :
    ?buf:Bi_outbuf.t ->
    ?log:Log.t ->
    ?start_block:int ->
    ?end_block:int ->
    ?page:int ->
    ?offset:int ->
    ?sort:sort ->
    Hex.t ->
    (Transaction.t list, Http_error.t) Result.t Deferred.t
end
