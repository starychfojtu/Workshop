module Workshop.Domain

open System
open FSharpPlus.Data
open System.Net.Mail

/// Anemic domain model reference ?
/// FLOW OF STAGE 1
/// - Open your giraffe project, create Domain.fs
/// - Install FsharpPlus
/// - Specification, Domain-first approach, we will choose specifics later
/// - Discuss some solutions, show this one
/// - How can invalid state be achieved here ? How can we prevent it ? Is it worth it ? (e.g. ZipCode)
/// - Smart constructors as a compromise
/// - Why type aliases on guid ?
/// - Mention of NodaTime
/// - Cyclic dependencies, why are they wrong ? Show typical usecase with 1-N relationship in EF, cannot be done with immutability properly.
/// - Modeling data as data, not references etc. follow-up on point 7
///
/// FLOW OF STAGE 2
/// - Sub modules of all types
/// - .Value functions
/// - Securing constructors with private keyword
/// - Why create methods ? record type constructors are not a functions (lang proposal on github), we will operate with functions
/// - Discussion: Why don't we validate birth date ? (Side effect, valid use-cases with birth date in future etc.) 

type NonEmptyString = private NonEmptyString of string
    with member s.Value = match s with NonEmptyString v -> v

module NonEmptyString =
    let create s =
        if String.IsNullOrEmpty s  
            then None
            else Some <| NonEmptyString s

type PhoneNumber = private PhoneNumber of NonEmptyString
    with member s.Value = match s with PhoneNumber v -> v

module PhoneNumber =
    let private isValid (s: NonEmptyString) =
        String.length s.Value = 13 && s.Value.StartsWith "+420"
            
    let create s =
        if isValid s
            then Some <| PhoneNumber s
            else None

type ZipCode = private ZipCode of NonEmptyString
    with member s.Value = match s with ZipCode v -> v
    
module ZipCode =
    let private isValid (s: NonEmptyString) =
        let chars = s.Value.ToCharArray()
        String.length s.Value = 5 && Seq.forall Char.IsDigit chars
            
    let create s =
        if isValid s
            then Some <| ZipCode s
            else None

type Address = {
    Street: NonEmptyString
    HomeNumber: NonEmptyString
    City: NonEmptyString
    ZipCode: ZipCode
}

module Address =
    let create street homeNumber city zipCode = {
        Street = street
        HomeNumber = homeNumber
        City = city
        ZipCode = zipCode
    }

type ContactMethod =
    | Email of MailAddress * PhoneNumber option
    | Phone of PhoneNumber * MailAddress option

type ContactInfo = {
    ContactMethod: ContactMethod
    Addresses: NonEmptyList<Address> 
}

module ContactInfo =
    let create method addresses = {
        ContactMethod = method
        Addresses = addresses
    }
    
type AccountId = AccountId of Guid

type Account = {
    Id: AccountId
    FirstName: NonEmptyString
    LastName: NonEmptyString
    BirthDateUtc: DateTime option
    ContactInfo: ContactInfo
}

module Account =
    let create id firstName lastName birthDateUtc contactInfo = {
        Id = id
        FirstName = firstName
        LastName = lastName
        BirthDateUtc = birthDateUtc
        ContactInfo = contactInfo
    }

// Lens usecase - update single Address
// Account.ContactInfo.Addresses[addressId].Street = "new value"