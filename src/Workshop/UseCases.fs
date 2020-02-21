module Workshop.UseCases

open FSharpPlus
open FSharpPlus.Data
open System
open Workshop.Domain
open Workshop.Environment
open Workshop.Library

/// FLOW OF STAGE 3
/// - Create interfaces in environments (datimeprovider, guidgenerators etc. are side effects)
/// - Write a method to create account and check 3 errors
/// - Error handling with Result and bind (sadly known as ROP)
/// - Description of the structure = input and error type for main function
/// - Further description = 1 main method with do-notation of pure binds, calling multiple small functions mapping stuff etc.
/// 
/// FLOW OF STAGE 4 (Reader)
/// - Problem of passing interfaces everywhere
/// - Introducing Reader
/// - Introducing readers on environment modules
/// - Refactoring methods with reader
/// - Note that functions now do not take interfaces, yet they return the same thing
/// - Automatic composing of environment requirements
/// - In order to finish it, we need ReaderT
/// - MonadTransformers
///
/// FLOW OF STATE 5 (IO)
/// - Introducing IO

type CreateAccountError =
    | BirthDateMustBeInPast
    | AddressQuotaExceeded
    | AccountAlreadyExists
    
type AccountParameters = {
    FirstName: NonEmptyString
    LastName: NonEmptyString
    BirthDateUtc: DateTime option
    ContactInfo: ContactInfo
}

let private checkBirthDate (dateTimeProvider: IDateTimeProvider) = function
    | Some birthDate ->
        if birthDate > dateTimeProvider.nowUtc ()
            then Error BirthDateMustBeInPast
            else Ok ()
    | None ->
        Ok ()
    
let private checkAddressQuota addresses =
    if Seq.length addresses > 10
        then Error AddressQuotaExceeded
        else Ok ()
        
let private addAccount (accountRepository: IAccountRepository) (guidGenerator: IGuidGenerator) accountParameters =
    let accountId = AccountId <| guidGenerator.create ()
    Account.create accountId accountParameters.FirstName accountParameters.LastName accountParameters.BirthDateUtc accountParameters.ContactInfo
    |> accountRepository.add
    |> Result.mapError (function AccountRepositoryError.AccountAlreadyExists -> CreateAccountError.AccountAlreadyExists)

let createAccount accountRepository dateTimeProvider guidGenerator accountParameters =
    checkBirthDate dateTimeProvider accountParameters.BirthDateUtc
    >>= (fun _ -> checkAddressQuota accountParameters.ContactInfo.Addresses)
    >>= (fun _ -> addAccount accountRepository guidGenerator accountParameters)
    
let private checkBirthDate2 = function
    | Some birthDate ->
        DateTimeProvider.nowUtc
        |> Reader.map (fun nowUtc ->
            if birthDate > nowUtc 
                then Error BirthDateMustBeInPast
                else Ok ()
        )
    | None ->
        Reader(fun _ -> Ok ())
        
let private addAccount2 accountParameters =
    let createAccount id = Account.create (AccountId id) accountParameters.FirstName accountParameters.LastName accountParameters.BirthDateUtc accountParameters.ContactInfo
    let mapRepositoryError = function AccountRepositoryError.AccountAlreadyExists -> CreateAccountError.AccountAlreadyExists
    
    GuidGenerator.create |> Reader.map createAccount
    >>= AccountRepository.add |> Reader.map (Result.mapError mapRepositoryError)
    
// let createAccount2 accountParameters =
    // checkBirthDate2 accountParameters.BirthDateUtc
    // >>= (fun r -> r >>= (fun _ -> checkAddressQuota accountParameters.ContactInfo.Addresses))
    // not finished -> last binding is a problem, going to stage 5
    
let private checkBirthDate3 = function
    | Some birthDate ->
        DateTimeProvider.nowUtc2 |> IO.matchErrorType
        >>= (fun nowUtc -> IO.fromResult <|
                if birthDate > nowUtc 
                    then Error BirthDateMustBeInPast
                    else Ok ())
    | None ->
        IO.id ()
    
let private saveAccount account =
    AccountRepository.add2 account |> IO.mapError (fun _ -> CreateAccountError.AccountAlreadyExists) // TODO: make repository return error.

let private addAccount4 accountParameters = monad {
    let! id = GuidGenerator.create2 |> IO.matchErrorType
    let account = Account.create (AccountId id) accountParameters.FirstName accountParameters.LastName accountParameters.BirthDateUtc accountParameters.ContactInfo
    return! saveAccount account
}

    
    