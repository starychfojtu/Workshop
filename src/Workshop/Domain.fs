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

// Lens usecase - update single Address
// Account.ContactInfo.Addresses[addressId].Street = "new value"