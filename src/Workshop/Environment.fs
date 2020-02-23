module Workshop.Environment

open FSharpPlus.Data
open System
open Workshop.Domain
open Workshop.Library

type AccountRepositoryError =
    | AccountAlreadyExists

type IAccountRepository =
    abstract member add: Account -> Result<Account, AccountRepositoryError>
    
module AccountRepository =
    let add<'e when 'e :> IAccountRepository> account = Reader(fun (e : 'e) -> e.add account)
    let add2<'e when 'e :> IAccountRepository> account = IO.resultOperation (fun (e : 'e) -> e.add account)
    
type IDateTimeProvider =
    abstract member nowUtc: unit -> DateTime
    
module DateTimeProvider =
    let nowUtc<'e when 'e :> IDateTimeProvider> = Reader(fun (e : 'e) -> e.nowUtc ())
    let nowUtc2<'e when 'e :> IDateTimeProvider> = IO.operation (fun (e : 'e) -> e.nowUtc ())
    
type IGuidGenerator =
    abstract member create: unit -> Guid
    
module GuidGenerator =
    let create<'r when 'r :> IGuidGenerator> = Reader(fun (r : 'r) -> r.create ())
    let create2<'r when 'r :> IGuidGenerator> = IO.operation (fun (r : 'r) -> r.create ())
    
