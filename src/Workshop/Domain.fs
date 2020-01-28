module Workshop.Domain

open System
open FSharpPlus.Data
open System.Net.Mail

// Person registry - contact info

// Aliasing, smart constructor, can be improved - try it
type NonEmptyString = NonEmptyString of string
type PhoneNumber = Telephone of NonEmptyString
type ZipCode = ZipCode of NonEmptyString

type Address = {
    Street: NonEmptyString
    HomeNumber: NonEmptyString
    City: NonEmptyString
    ZipCode: ZipCode
}

type ContactMethod =
    | Email of MailAddress * PhoneNumber option
    | Phone of PhoneNumber * MailAddress option

type ContactInfo = {
    ContactMethod: ContactMethod
    Addresses: NonEmptyList<Address> 
}

type AccountId = AccountId of Guid
type AccountName = AccountName of string

type Account = {
    Id: AccountId
    FirstName: NonEmptyString
    LastName: NonEmptyString
    BirthDateUtc: DateTime option
    ContactInfo: ContactInfo
}
/// Anemic domain model reference ?
/// 3
/// FLOW OF STAGE 1
/// 0) Open your giraffe project, create Domain.fs
/// 1) Install FsharpPlus
/// 2) Specification, tell them that this will be saved in database
/// 3) Discuss some solutions, show this one
/// 4) How can invalid state be achieved here ? How can we prevent it ? Is it worth it ?
/// 5) Smart constructors as a compromise
/// 6) Why type aliases on guid ?
/// 7) Cyclic dependencies, why are they wrong ? Show typical usecase with 1-N relationship in EF, cannot be done with immutability properly.
/// 8) Modeling data as data, not references etc. follow-up on point 7

// Lens usecase - update single Address
// Account.ContactInfo.Addresses[addressId].Street = "new value"