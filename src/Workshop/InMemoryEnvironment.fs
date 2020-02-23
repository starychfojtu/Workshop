module Workshop.InMemoryEnvironment

open System
open Workshop.Environment
    
type InMemoryEnvironment() =

    interface IAccountRepository with 
        member e.add account = Ok account
        
    interface IGuidGenerator with 
        member e.create () = Guid.NewGuid()
        
    interface IDateTimeProvider with
        member p.nowUtc () = DateTime.UtcNow
    
