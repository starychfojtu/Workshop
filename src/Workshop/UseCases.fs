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
/// - Try to implement it with Reader only -> failure
/// - In order to finish it, we need ReaderT
/// - MonadTransformers
/// - Not future proof, hard to work with -> IO
///
/// FLOW OF STATE 5 (IO)
/// - Introducing IO
/// - Env, Success, Error
/// - Using Nothing type
/// - Function to lift everything to the IO level, then easily work with do-notation
/// - Note that type inference and error message are at its boundaries and the developer experience is not great

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
    
// ---------------- PHASE 4 --------------------
        
let private checkBirthDate2 birthDate = monad {
    let! nowUtc = DateTimeProvider.nowUtc
    return  
        match birthDate with
        | Some d ->     
            if d > nowUtc 
                then Error BirthDateMustBeInPast
                else Ok ()
        | None -> Ok ()
}
        
let private addAccount2 accountParameters = monad {
    let mapRepositoryError = function AccountRepositoryError.AccountAlreadyExists -> CreateAccountError.AccountAlreadyExists
    let! id = GuidGenerator.create
    let account = Account.create (AccountId id) accountParameters.FirstName accountParameters.LastName accountParameters.BirthDateUtc accountParameters.ContactInfo
    let! result = AccountRepository.add account
    return Result.mapError mapRepositoryError result
}
    
// let createAccount2 accountParameters =
    // checkBirthDate2 accountParameters.BirthDateUtc
    // >>= (fun r -> r >>= (fun _ -> checkAddressQuota accountParameters.ContactInfo.Addresses))
    // not finished -> last binding is a problem, going to stage 5
    
// ---------------- PHASE 5 --------------------

let private checkBirthDate3 birthDate = monad {
    let! nowUtc = DateTimeProvider.nowUtc2 |> IO.matchErrorType
    return! IO.fromResult <|    
        match birthDate with
        | Some d ->     
            if d > nowUtc 
                then Error BirthDateMustBeInPast
                else Ok ()
        | None -> Ok ()
}

let private checkAddressQuota3 addresses =
    let result = 
        if Seq.length addresses > 10
            then Error AddressQuotaExceeded
            else Ok ()
    IO.fromResult result
    
let private saveAccount3 account =
    AccountRepository.add2 account |> IO.mapError (function AccountRepositoryError.AccountAlreadyExists -> CreateAccountError.AccountAlreadyExists)

let private addAccount3 accountParameters = monad {
    let! id = GuidGenerator.create2 |> IO.matchErrorType
    let account = Account.create (AccountId id) accountParameters.FirstName accountParameters.LastName accountParameters.BirthDateUtc accountParameters.ContactInfo
    return! saveAccount3 account
}

let createAccount3 accountParameters =
    checkBirthDate3 accountParameters.BirthDateUtc
    >>= (fun _ -> checkAddressQuota3 accountParameters.ContactInfo.Addresses)
    >>= (fun _ -> addAccount3 accountParameters)

    
    