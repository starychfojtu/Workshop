module Workshop.Domain

open FSharpPlus
open FSharpPlus.Data
open Lens
open System
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
/// - Writing domain functions version 1 (moveToCity, change phone, get phone)
/// - Lenses definitions
/// - Writing domain functions version 2
/// - Discuss how the mapping of addresses is now declarative
/// - Discuss how the lens can point to multiple places vs. object oriented mutable assignment cannot (as well as over etc.)
/// - Discuss how there is no need to match on contact info
/// - Discuss Lens operators
/// - Discuss competition for why we need to specify :Account return value
/// - Show that we only extracted phone functions to create a lens, but that gives us more power (nested access, over etc.)

type NonEmptyString = private NonEmptyString of string
    with member s.Value = match s with NonEmptyString v -> v

module NonEmptyString =
    let create s =
        if String.IsNullOrEmpty s  
            then None
            else Some <| NonEmptyString s
            
module Email =
    let create (NonEmptyString s) =
        try
            Some (new MailAddress(s))
        with
        | ex -> None

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


type City = NonEmptyString

type Address = {
    Street: NonEmptyString
    HomeNumber: NonEmptyString
    City: City
    ZipCode: ZipCode
}

module Address =
    let create street homeNumber city zipCode = {
        Street = street
        HomeNumber = homeNumber
        City = city
        ZipCode = zipCode
    }
    
    let inline street f address = map (fun v -> { address with Street = v}) (f address.Street)
    let inline homeNumber f address = map (fun v -> { address with HomeNumber = v}) (f address.HomeNumber)
    let inline city f address = map (fun v -> { address with City = v}) (f address.City)
    let inline zipCode f address = map (fun v -> { address with ZipCode = v}) (f address.ZipCode)

type ContactMethod =
    | Email of MailAddress * PhoneNumber option
    | Phone of PhoneNumber * MailAddress option
    
module ContactMethod =
    let defaultPhoneNumber = PhoneNumber.create (NonEmptyString.create "+420111222333" |> Option.get) |> Option.get
    let setPhoneNumber v = function
        | Email (email, _) -> Email (email, Some v)
        | Phone (_, email) -> Phone (v, email)
        
    let getPhoneNumber = function
        | Email (_, phone) -> Option.defaultValue defaultPhoneNumber phone
        | Phone (phone, _) -> phone
        
    let inline phoneNumber f method = map (fun v -> setPhoneNumber v method) (f (getPhoneNumber method))

type ContactInfo = {
    ContactMethod: ContactMethod
    Addresses: NonEmptyList<Address> 
}

module ContactInfo =
    let create method addresses = {
        ContactMethod = method
        Addresses = addresses
    }
    
    let inline contactMethod f info = map (fun v -> { info with ContactMethod = v}) (f info.ContactMethod)
    let inline addresses f info = map (fun v -> { info with Addresses = v}) (f info.Addresses)
    
type AccountId = AccountId of Guid

type Account = {
    Id: AccountId
    FirstName: NonEmptyString
    LastName: NonEmptyString
    BirthDateUtc: DateTime option
    ContactInfo: ContactInfo
}

module Account =
    open ContactInfo
    open ContactMethod
    open Address
    
    let create id firstName lastName birthDateUtc contactInfo = {
        Id = id
        FirstName = firstName
        LastName = lastName
        BirthDateUtc = birthDateUtc
        ContactInfo = contactInfo
    }
    
    let inline id f account = map (fun v -> { account with Id = v}) (f account.Id)
    let inline firstName f account = map (fun v -> { account with FirstName = v}) (f account.FirstName)
    let inline lastName f account = map (fun v -> { account with LastName = v}) (f account.LastName)
    let inline birthDateUtc f account = map (fun v -> { account with BirthDateUtc = v}) (f account.BirthDateUtc)
    let inline contactInfo f account = map (fun v -> { account with ContactInfo = v}) (f account.ContactInfo)
    
    
    let moveToCityVersion1 account oldCity newCity =
        let transformAddress a =
            if a.City = oldCity
                then { a with City = newCity }
                else a
                
        let newAddresses = NonEmptyList.map transformAddress account.ContactInfo.Addresses
        { account with ContactInfo = { account.ContactInfo with Addresses = newAddresses } }
        
    let moveToCityVersion2 account oldCity newCity: Account =
        let transformAddress a =
            if a.City = oldCity
                then { a with City = newCity }
                else a
                
        let newAddresses = NonEmptyList.map transformAddress account.ContactInfo.Addresses
        setl (contactInfo << addresses) newAddresses account
        
    let moveToCity account oldCity newCity: Account =
        setl (contactInfo << addresses << items << (filtered (fun a -> a.City = oldCity)) << city) newCity account
        
    let getPhoneNumberVersion1 account =
        match account.ContactInfo.ContactMethod with
        | Email (_, phone) -> phone
        | Phone (phone, _) -> Some phone
        
    let getPhoneNumber account: PhoneNumber =
        view (contactInfo << contactMethod << phoneNumber) account
        
    let changePhoneNumberVersion1 account phone =
        match account.ContactInfo.ContactMethod with
        | Email (email, _) -> { account with ContactInfo = { account.ContactInfo with ContactMethod = Email (email, Some phone) } }
        | Phone (_, email) -> { account with ContactInfo = { account.ContactInfo with ContactMethod = Phone (phone, email) } }
        
    let changePhoneNumber account phone: Account =
        setl (contactInfo << contactMethod << phoneNumber) phone account