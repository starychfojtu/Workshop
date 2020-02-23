module Workshop.Api

open FSharp.Json
open FSharpPlus
open FSharpPlus.Data
open System
open System.Threading.Tasks
open Workshop.UseCases
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks
open Workshop
open Workshop.Domain
open Workshop.InMemoryEnvironment
open Workshop.Library

/// FLOW OF STAGE 6
/// - What actually needs to be done in API ? = just mapping unsafe protocol to our business protocol
/// - Note that actually most of the standard code in "validate" methods is here
/// - Note about returning all parameter errors at once
/// - Introduce Generic and Dto module
/// - Ask all of them to implement it (will take some while)
/// - Structuring in submodules (can be done also for usecases)
/// - Applicative
/// - Explain <!> and <*>
/// - Introduce Parse module, future parsing library
/// - Show the nice solution with applicatives
/// - Note that hey can build generic code for dealing with errors for non empty strings etc.
/// - Building from the bottom
/// - Validation is not a monad, even though it has bind

module Dto =
    type ParameterError = {
        message: string
        parameter: string
    } 

    type Error = {
        message: string
        parameterErrors: ParameterError list
    }
    
    let error s = {
        message = s
        parameterErrors = List.empty
    }
    
    let invalidParameters errors = {
        message = "Invalid parameters."
        parameterErrors = Seq.toList errors
    }
    
    let parameterError s parameter = {
        message = s
        parameter = parameter
    }

module Generic =
    open Dto
    
    let private deserialize<'a> json =
        try
            Ok <| Json.deserialize<'a> json
        with
            | ex -> Error ex.Message
            
    let private serializationConfig = JsonConfig.create(jsonFieldNaming = Json.lowerCamelCase)
    
    let private serializeToJson (obj: 'a) =
        Json.serializeEx serializationConfig obj
    
    let private setStatusCode (ctx: HttpContext) code =
        ctx.SetStatusCode code
            
    let private bindQueryString<'a> (ctx: HttpContext) =
        try 
            ctx.BindQueryString<'a>()
        with ex -> failwith ex.Message
        
    let private bindModelAsync<'a> (ctx: HttpContext): Task<Result<'a, string>> = task {
        let! body = ctx.ReadBodyFromRequestAsync ()
        return deserialize body
    }
        
    let private getResult env next ctx handler (serialize: Result<'a, 'e>  -> Result<'c, Dto.Error>) parameters =
        let result = handler parameters |> IO.execute env |> serialize
        let response =
            match result with 
            | Ok s -> setStatusCode ctx 200 |> (fun _ -> text <| serializeToJson s) 
            | Error e -> setStatusCode ctx 400 |> (fun _ -> text <| serializeToJson e)
        response next ctx

    let getHandler handler serialize next ctx = 
        bindQueryString<'parameters> ctx |> getResult (InMemoryEnvironment()) next ctx handler serialize
    
    let postHandler handler serialize next ctx = 
        task {
            let! parameters = bindModelAsync ctx
            return!
                match parameters with
                | Ok p -> getResult (InMemoryEnvironment()) next ctx handler serialize p
                | Error e -> getResult (InMemoryEnvironment()) next ctx (fun _ -> error e |> Error |> IO.fromResult) (fun a -> a) ()
        }
        
module Parse =
    let option v =
        Option.map v >> Validation.traverseOption
        
    let seqOf v =
        Seq.map v >> Validation.traverseSeq
    
    let nonEmptyString error =
        NonEmptyString.create >> Validation.ofOption [error]
        
    let nonEmptyStringOption error =
        nonEmptyString error |> option

module CreateAccount =
    type ParameterError =
        | FirstNameCannotBeEmpty
        | LastNameCannotBeEmpty
        | StreetCannotBeEmpty
        | HomeNumberCannotBeEmpty
        | CityCannotBeEmpty
        | ZipCodeCannotBeEmpty
        | InvalidZipCode
        | AddressesCannotBeEmpty
        | InvalidEmail
        | InvalidPhoneNumber
        | EmailOrPhoneNumberCannotBeEmpty
        
    type Error =
        | BusinessError of CreateAccountError
        | ParameterErrors of ParameterError list
        
    [<CLIMutable>]
    type AddressParameters = {
        Street: string
        HomeNumber: string
        City: string
        ZipCode: string
    }
    
    [<CLIMutable>]
    type Parameters = {
        FirstName: string
        LastName: string
        BirthDateUtc: DateTime option
        Email: string option
        PhoneNumber: string option
        Addresses: AddressParameters list
    }
    
    let createAccountParameters firstName lastName birthDateUtc contactInfo = {
        FirstName = firstName
        LastName = lastName
        BirthDateUtc = birthDateUtc
        ContactInfo = contactInfo
    }
    
    let parseZipCode zipCode =
        Parse.nonEmptyString ZipCodeCannotBeEmpty zipCode
        |> Validation.bind (ZipCode.create >> (Validation.ofOption [InvalidZipCode]))
    
    let parseAddress parameters =
        Address.create
        <!> Parse.nonEmptyString StreetCannotBeEmpty parameters.Street
        <*> Parse.nonEmptyString HomeNumberCannotBeEmpty parameters.HomeNumber
        <*> Parse.nonEmptyString CityCannotBeEmpty parameters.City
        <*> parseZipCode parameters.ZipCode
    
    let parseAddresses parameters =
        NonEmptyList.tryCreate parameters
        |> Validation.ofOption [AddressesCannotBeEmpty]
        |> Validation.bind ((NonEmptyList.map parseAddress) >> Validation.traverseNonEmptyList)
        
    let createContactMethod email phone =
        match email with
        | Some m -> Success <| ContactMethod.Email (m, phone)
        | None ->
            match phone with
            | Some p -> Success <| ContactMethod.Phone (p, email)
            | None -> Failure [EmailOrPhoneNumberCannotBeEmpty]
        
    let parseContactMethod parameters =
        let bindOption f =
            Validation.bind (function | Some s -> f s | None -> Success None)
            
        let parse parseF error value =
            value
            |> Parse.nonEmptyStringOption error
            |> bindOption (parseF >> (Validation.ofOption [error]) >> (Validation.map Some))
            
        createContactMethod
        <!> parse Email.create InvalidEmail parameters.Email
        <*> parse PhoneNumber.create InvalidPhoneNumber parameters.PhoneNumber
        |> Validation.bind (fun v -> v)
    
    let parseContactInfo parameters =
        ContactInfo.create
        <!> parseContactMethod parameters
        <*> parseAddresses parameters.Addresses
    
    let parse parameters =
        createAccountParameters
        <!> Parse.nonEmptyString FirstNameCannotBeEmpty parameters.FirstName
        <*> Parse.nonEmptyString LastNameCannotBeEmpty parameters.LastName
        <*> Validation.Success parameters.BirthDateUtc
        <*> parseContactInfo parameters
        
    let parseParameters parameters =
        parse parameters
        |> Validation.mapFailure ParameterErrors
        |> Validation.toResult
        |> IO.fromResult
        
    let execute parameters =
        parseParameters parameters
        >>= (UseCases.createAccount3 >> (IO.mapError BusinessError))
        
    let serialize = function
        | Ok a -> Ok "Account was created."
        | Error e -> Error <| Dto.error "Things went wrong."
    
    let handler<'a> =
        Generic.postHandler execute serialize
