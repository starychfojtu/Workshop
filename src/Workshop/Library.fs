module Workshop.Library
open System
open FSharpPlus.Data
open FSharpPlus.Data

module Nothing =
    type Nothing = private T of unit
    
module NonEmptyList =
    let tryCreate (seq: seq<'a>) =
        match Seq.toList seq with
        | [] -> None
        | x::xs -> Some (NonEmptyList.create x xs)
    
module Validation =
    let mapFailure mapper =
        Validation.bimap (fun e -> mapper e) (fun s -> s)
        
    let traverseSeq seqOfValidation =
        Seq.traverse Validation.toResult seqOfValidation |> Validation.ofResult
        
    let traverseNonEmptyList (listOfValidation: NonEmptyList<'a>) =
        traverseSeq listOfValidation |> Validation.map (NonEmptyList.tryCreate >> Option.get)
        
    let traverseOption validationOfOption =
        Option.traverse Validation.toResult validationOfOption |> Validation.ofResult
        
    let ofOption e = function
        | Some s -> Success s
        | None -> Failure e

module Reader =
    let id a =
        ReaderT(fun _ -> a)

    let execute a reader =
        ReaderT.run reader a
        
    let mapDirect f reader = ReaderT(fun env ->
        execute env reader |> f)
    
    let mapError<'e, 'a, 'b, 's> f (reader: ReaderT<'e, Result<'s, 'a>>): ReaderT<'e, Result<'s, 'b>> =
        mapDirect (Result.mapError f) reader
        
    let fromReader r = ReaderT(fun env -> Reader.run r env)
    
    let hoistOk r = Reader.map Ok r |> fromReader
    
module IO =
    open Nothing
    
    type IO<'a, 'env, 'error> = ReaderT<'env, Result<'a, 'error>>

    let success r: IO<'a, 'env, Nothing> = Reader.hoistOk r
    
    let id a: IO<'a, 'env, 'error> = ReaderT(fun _ -> Ok a)
    
    let fromResult r: IO<'a, 'env, 'error> = Reader.id r 
    
    let operation f: IO<'a, 'env, Nothing> = ReaderT(f >> Ok)
    
    let map (f: 'a -> 'b) (io: IO<'a, 'env, 'e>): IO<'b, 'env, 'e> = ReaderT.map f io
    
    let mapError (f: 'e1 -> 'e2) (io: IO<'a, 'env, 'e1>): IO<'a, 'env, 'e2> = Reader.mapError f io
    
    let bindError (f: Result<'a, 'e1> -> Result<'b, 'e2>) (io: IO<'a, 'env, 'e1>): IO<'b, 'env, 'e2> =
        Reader.mapDirect f io
        
    let matchErrorType<'a, 'env, 'e> (io: IO<'a, 'env, Nothing>): IO<'a, 'env, 'e> =
        bindError (function Ok a -> Ok a | Error _ -> failwith "Cannot happen.") io
        
    let execute env (io: IO<'a, 'env, 'error>) = Reader.execute env io