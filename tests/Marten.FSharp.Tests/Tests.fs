module Tests


open Expecto
module Expecto =
    module Expect =
        open Expecto.Logging
        open Expecto.Logging.Message
        let throwsTAsync<'texn> f message = async {
            try
                do! f
                Tests.failtestf "%s. Expected f to throw." message
            with
            | e when e.GetType() <> typeof<'texn> ->
                Tests.failtestf "%s. Expected f to throw an exn of type %s, but one of type %s was thrown."
                                message
                                (typeof<'texn>.FullName)
                                (e.GetType().FullName)
            | e ->
                ()
        }
        let isType<'a> x message =
            if x.GetType() <> typeof<'a> then
                Tests.failtestf "%s. Expected x to be type %s, but was one of type %s."
                                message
                                (typeof<'a>.FullName)
                                (x.GetType().FullName)

open System
open System.Linq

open Expecto
open Marten
open Marten.Linq
open Npgsql

module DatabaseTestHelpers =
    let execNonQuery connStr commandStr =
        use conn = new NpgsqlConnection(connStr)
        use cmd = new NpgsqlCommand(commandStr,conn)
        conn.Open()
        cmd.ExecuteNonQuery()

    let createDatabase connStr databaseName =
        databaseName
        |> sprintf "CREATE database \"%s\" ENCODING = 'UTF8'"
        |> execNonQuery connStr
        |> ignore

    let dropDatabase connStr databaseName =
        //kill out all connections
        databaseName
        |> sprintf "select pg_terminate_backend(pid) from pg_stat_activity where datname='%s';"
        |> execNonQuery connStr
        |> ignore

        databaseName
        |> sprintf "DROP database \"%s\""
        |> execNonQuery connStr
        |> ignore

    let createConnString host user pass database =
        sprintf "Host=%s;Username=%s;Password=%s;Database=%s" host user pass database
        |> NpgsqlConnectionStringBuilder


    type DisposableDatabase (superConn : NpgsqlConnectionStringBuilder, databaseName : string) =
        static member Create(connStr) =
            let databaseName = System.Guid.NewGuid().ToString("n")
            createDatabase (connStr |> string) databaseName

            new DisposableDatabase(connStr,databaseName)
        member x.SuperConn = superConn
        member x.Conn =
            let builder = x.SuperConn |> string |> NpgsqlConnectionStringBuilder
            builder.Database <- x.DatabaseName
            builder
        member x.DatabaseName = databaseName
        interface IDisposable with
            member x.Dispose() =
                dropDatabase (superConn |> string) databaseName

    let getEnvOrDefault defaultVal str =
        let envVar = System.Environment.GetEnvironmentVariable str
        if String.IsNullOrEmpty envVar then defaultVal
        else envVar


    let host () = "POSTGRES_HOST" |> getEnvOrDefault "localhost"
    let user () = "POSTGRES_USER" |> getEnvOrDefault "postgres"
    let pass () =  "POSTGRES_PASS"|> getEnvOrDefault "postgres"
    let db () = "POSTGRES_DB"|> getEnvOrDefault "postgres"
    let superUserConnStr () = createConnString (host ()) (user ()) (pass()) (db())

    let getNewDatabase () = superUserConnStr () |>  DisposableDatabase.Create

    let getStore (database : DisposableDatabase) = database.Conn |> string |> DocumentStore.For


[<CLIMutable>]
type Dog = {
    Id : Guid
    Name : string
    FavoriteChewToy : string
}

let newDog name favoriteChewToy =
     { Id = Guid.NewGuid(); Name = name; FavoriteChewToy = favoriteChewToy }

let saveDog  (store : IDocumentStore) (dog : Dog) =
    use session = store.OpenSession()
    session.Store(dog)
    session.SaveChanges()
    dog
let saveDog' (store : IDocumentStore)  =
    let dog = newDog "Spark" "Macbook"
    saveDog store dog

open DatabaseTestHelpers

let withDatabase f () =
    use database =  getNewDatabase ()
    f database

open System.Reflection
open Microsoft.FSharp.Reflection
let inline withDatabaseAndStore (f : _ -> _)  = async {
    use database =  getNewDatabase ()
    use store = getStore database :> IDocumentStore
    return! f (database, store)
}

type ParameterizedTest<'a> =
    | Sync of string * ('a -> unit)
    | Async of string * ('a -> Async<unit>)


let testCase' name test =
     ParameterizedTest.Sync(name,test)

let testCaseAsync' name test  =
    ParameterizedTest.Async(name,test)

let inline testFixture'<'a> setup =
    Seq.map (fun ( parameterizedTest : ParameterizedTest<'a>) ->
        match parameterizedTest with
        | Sync (name, test) ->
            testCase name <| fun () -> test >> async.Return |> setup |> Async.RunSynchronously
        | Async (name, test) ->
            testCaseAsync name <| setup test

    )


[<Tests>]
let ``Litmus Tests`` =
  testList "litmus Tests" [
    yield! testFixture withDatabase [
        "Can create/destroy database", ignore
        "Can Save/Load Record",
            fun db ->
                use store = getStore db
                let expectedDog = saveDog' store

                use session2 = store.OpenSession()
                let actualDog = session2.Load<Dog>(expectedDog.Id)
                Expect.equal expectedDog actualDog "Same dog!"
    ]
  ]
let exactlyOnceTests = [
    testCase'
        "exactlyOne, Get Record Back" <|
            fun (db, store) ->
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let actualDog =
                      session
                        |> Session.query<Dog>
                        |> Queryable.exactlyOne
                Expect.equal expectedDog actualDog  "Should be one dog!"


    testCase'
        "exactlyOne, Throws if more than one" <|
            fun (db, store) ->
                Expect.throwsT<InvalidOperationException>(fun _ ->
                    let _ = saveDog' store
                    let _ = saveDog' store
                    use session = store.OpenSession()
                    let actualDog =
                        session
                            |> Session.query<Dog>
                            |> Queryable.exactlyOne
                    ()
                ) "Should be too many dogs!"


    testCase'
        "exactlyOne, Throws if none" <|
            fun (db, store) ->
                Expect.throwsT<InvalidOperationException>(fun _ ->
                    use session = store.OpenSession()
                    let actualDog =
                        session
                            |> Session.query<Dog>
                            |> Queryable.exactlyOne
                    ()
                ) "Should be no dog!"


    testCaseAsync'
        "exactlyOneAsync, Get Record Back" <|
            fun (db, store) -> async {
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let! actualDog =
                        session
                        |> Session.query<Dog>
                        |> Queryable.exactlyOneAsync
                Expect.equal expectedDog actualDog  "Should be one dog!"
            }
    testCaseAsync'
        "exactlyOneAsync, Throws if more than one" <|
        fun (db, store) -> async {
            do!
                Expect.throwsTAsync<AggregateException>(async {
                    let _ = saveDog' store
                    let _ = saveDog' store
                    use session = store.OpenSession()
                    let! actualDog =
                        session
                            |> Session.query<Dog>
                            |> Queryable.exactlyOneAsync
                    ()
                }) "Should be too many dogs!"

    }
    testCaseAsync'
        "exactlyOneAsync, Throws if none" <|
        fun (db, store) -> async {
            do!
                Expect.throwsTAsync<AggregateException>(async {
                    use session = store.OpenSession()
                    let! actualDog =
                        session
                            |> Session.query<Dog>
                            |> Queryable.exactlyOneAsync
                    ()
                }) "Should be no dogs!"

        }
]

let filterTests = [
    testCase'
        "filter by, Get Record Back" <|
            fun (db, store) ->
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let actualDog =
                    session
                    |> Session.query<Dog>
                    |> Queryable.filter <@fun d -> d.Name = "Spark" @>
                    |> Queryable.head
                Expect.equal expectedDog actualDog  "Should be one dog!"
    testCase'
        "filter IsOneOf" <|
            fun (db,store) ->
                let expectedDog = saveDog' store
                let names = [|expectedDog.Name; "Fido"; "Snape"|]
                use session = store.OpenSession()
                let actualDog =
                    session
                    |> Session.query<Dog>
                    |> Queryable.filter <@fun d -> d.Name.IsOneOf(names) @>
                    |> Queryable.head
                Expect.equal expectedDog actualDog  "Should be one dog!"
    testCase'
        "filter IsOneOf not in list" <|
            fun (db,store) ->
                let expectedDog = saveDog' store
                let names = [|"Fido"; "Snape"|]
                use session = store.OpenSession()
                let actualDog =
                    session
                    |> Session.query<Dog>
                    |> Queryable.filter <@fun d -> d.Name.IsOneOf(names) @>
                    |> Queryable.tryHead

                Expect.isNone actualDog "No dog!"

]
type User = {
    Name : string
}
let mapTests = [
    testCase'
        "map, Get Name Back" <|
            fun (db, store) ->
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let actualDog =
                    session
                    |> Session.query<Dog>
                    |> Queryable.map(<@fun s -> s.Name@> )
                    |> Queryable.head
                Expect.equal expectedDog.Name actualDog  "Should be one dog!"

    testCase'
        "map to another type, Get User Back" <|
            fun (db, store) ->
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let actualUser =
                    session
                    |> Session.query<Dog>
                    |> Queryable.map(<@fun s -> { Name = s.Name }@> )
                    |> Queryable.head
                Expect.isType<User> (box actualUser) "Should be User type"
                Expect.equal expectedDog.Name actualUser.Name  "Should be a user!"

]

let headTests = [
    testCase'
        "head, single dog, Get Record Back" <|
            fun (db, store) ->
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let actualDog =
                    session
                    |> Session.query<Dog>
                    |> Queryable.head
                Expect.equal expectedDog actualDog  "Should be a same dog!"

    testCase'
        "head, multiple dogs, get top record back" <|
            fun (db, store) ->
                let expectedDog = saveDog' store
                let _ = saveDog' store
                use session = store.OpenSession()
                let actualDog =
                    session
                    |> Session.query<Dog>
                    |> Queryable.head
                Expect.equal expectedDog actualDog  "Should be a same dog!"

    testCase'
        "head, no dogs, throws exception" <|
            fun (db, store) ->
                Expect.throwsT<InvalidOperationException>(fun _ ->
                use session = store.OpenSession()
                let actualDog =
                    session
                        |> Session.query<Dog>
                        |> Queryable.head
                ()
                ) "Should be no dog!"


    testCaseAsync'
        "headAsync, single dog, Get Record Back" <|
            fun (db, store) -> async {
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let! actualDog =
                    session
                    |> Session.query<Dog>
                    |> Queryable.headAsync
                Expect.equal expectedDog actualDog  "Should be a same dog!"
            }
    testCaseAsync'
        "headAsync, multiple dogs, get top record back" <|
            fun (db, store) -> async {
                let expectedDog = saveDog' store
                let _ = saveDog' store
                use session = store.OpenSession()
                let! actualDog =
                    session
                    |> Session.query<Dog>
                    |> Queryable.headAsync
                Expect.equal expectedDog actualDog  "Should be a same dog!"
            }
    testCaseAsync'
        "headAsync, no dogs, throws exception" <|
            fun (db, store) -> async {
                do!
                    Expect.throwsTAsync<AggregateException>(async {
                        use session = store.OpenSession()
                        let! actualDog =
                            session
                                |> Session.query<Dog>
                                |> Queryable.headAsync
                        ()
                    }) "Should be no dog!"
            }
]


let toListTests = [
    testCase'
        "toList, single dog, get single back" <|
            fun (db, store) ->
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toList
                Expect.contains actualDogs expectedDog "Should contain same dog!"

    testCase'
        "toList, multiple dogs, get multple back" <|
        fun (db, store) ->
            let expectedDog = saveDog' store
            let expectedDog2 = saveDog' store
            use session = store.OpenSession()
            let actualDogs =
                session
                |> Session.query<Dog>
                |> Queryable.toList
            Expect.contains actualDogs expectedDog "Should contain same dog!"
            Expect.contains actualDogs expectedDog2 "Should contain same dog!"

    testCase'
        "toList, no dogs, get empty list" <|
        fun (db, store) ->
            use session = store.OpenSession()
            let actualDogs =
                session
                |> Session.query<Dog>
                |> Queryable.toList
            Expect.isEmpty actualDogs "Should be no dogs!"

    testCaseAsync'
        "toListAsync, single dog, get single back" <|
            fun (db, store) -> async {
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let! actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toListAsync
                Expect.contains actualDogs expectedDog "Should contain same dog!"
            }
    testCaseAsync'
        "toListAsync, multiple dogs, get multple back" <|
            fun (db, store) -> async {
                let expectedDog = saveDog' store
                let expectedDog2 = saveDog' store
                use session = store.OpenSession()
                let! actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toListAsync
                Expect.contains actualDogs expectedDog "Should contain same dog!"
                Expect.contains actualDogs expectedDog2 "Should contain same dog!"
            }
    testCaseAsync'
        "toListAsync, no dogs, get empty list" <|
            fun (db, store) -> async {
                use session = store.OpenSession()
                let! actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toListAsync
                Expect.isEmpty actualDogs "Should be no dogs!"
            }
]


let CRUDTests = [
    testCase'
        "loadByGuid, Get Some Record back" <|
            fun (db, store) ->
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let actualDog =
                    session
                    |> Session.loadByGuid<Dog> expectedDog.Id
                Expect.equal (Some expectedDog) actualDog  "Same dog!"
    testCase'
        "loadByGuid, Get None Record back" <|
            fun (db, store) ->
                let _ = saveDog' store
                use session = store.OpenSession()
                let actualDog =
                    session
                    |> Session.loadByGuid<Dog> (Guid.NewGuid())
                Expect.equal None actualDog  "Should be no dog!"
    testCaseAsync'
        "loadByGuidAsync, get Some record back" <|
            fun (db, store) -> async {
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let! actualDog =
                    session
                    |> Session.loadByGuidAsync<Dog> expectedDog.Id

                Expect.equal (Some expectedDog) actualDog  "Same dog!"
            }
    testCaseAsync'
        "loadByGuidAsync, get None record back" <|
            fun (db, store) -> async {
                let expectedDog = saveDog' store
                use session = store.OpenSession()
                let! actualDog =
                    session
                    |> Session.loadByGuidAsync<Dog> (Guid.NewGuid())

                Expect.equal None actualDog  "Should be no dog!"
            }
    testCase'
        "storeSingle/saveChanges" <|
            fun (db, store) ->
                let sparky = newDog "Sparky" "Shoes"

                use session = store.OpenSession()
                session |> Session.storeSingle  sparky
                session |> Session.saveChanges
                let actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toList
                Expect.contains actualDogs sparky "Should contain same dog!"


    testCase'
        "storeMany/delete/saveChanges" <|
            fun (db, store) ->
                let sparky = newDog "Sparky" "Shoes"
                let spot = newDog "Spot" "Macbook"

                use session = store.OpenSession()
                session |> Session.storeMany [sparky ; spot]
                session |> Session.saveChanges
                let actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toList
                Expect.contains actualDogs sparky "Should contain same dog!"
                Expect.contains actualDogs spot "Should contain same dog!"

                session |> Session.deleteEntity spot
                session |> Session.saveChanges
                let actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toList
                Expect.contains actualDogs sparky "Should contain same dog!"

                session |> Session.deleteByGuid<Dog> sparky.Id
                session |> Session.saveChanges
                let actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toList
                Expect.isEmpty actualDogs "Should be no more dogs"


    testCase'
        "storeMany/deleteBy/saveChanges" <|
            fun (db, store) ->
                let sparky = newDog "Sparky" "Shoes"
                let spot = newDog "Spot" "Macbook"

                use session = store.OpenSession()
                session |> Session.storeMany [sparky ; spot]
                session |> Session.saveChanges
                let actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toList
                Expect.contains actualDogs sparky "Should contain same dog!"
                Expect.contains actualDogs spot "Should contain same dog!"

                session |> Session.deleteBy<Dog> <@fun d -> d.Name = "Sparky"@>
                session |> Session.saveChanges
                let actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toList
                Expect.contains actualDogs spot "Should contain same dog!"

    testCaseAsync'
        "storeSingle/saveChangesAsync" <|
            fun (db, store) -> async {
                let sparky = newDog "Sparky" "Shoes"

                use session = store.OpenSession()
                session |> Session.storeSingle sparky
                do! session |> Session.saveChangesAsync
                let! actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toListAsync
                Expect.contains actualDogs sparky "Should contain same dog!"

            }
    testCaseAsync'
        "storeMany/saveChangesAsync" <|
            fun (db, store) -> async {
                let sparky = newDog "Sparky" "Shoes"
                let spot = newDog "Spot" "Macbook"

                use session = store.OpenSession()
                session |> Session.storeMany [sparky ; spot]
                do! session |> Session.saveChangesAsync
                let! actualDogs =
                    session
                    |> Session.query<Dog>
                    |> Queryable.toListAsync
                Expect.contains actualDogs sparky "Should contain same dog!"
                Expect.contains actualDogs spot "Should contain same dog!"
            }
]

[<CLIMutableAttribute>]
type Person = {
    Id : Guid
    Name : string
    Age : int
}

let newPerson name age =
    { Id = Guid.NewGuid (); Name = name; Age = age }

let savePerson  (store : #IDocumentStore) (person : Person) =
    use session = store.OpenSession()
    session.Store(person)
    session.SaveChanges()
    person

let PatchTests = [
    testCase'
        "patch and then set" <|
            fun (db, store)->
                let marcoPolo = newPerson "Marco Polo" 500
                let edittedMarco = { marcoPolo with Age = 200 }
                savePerson store marcoPolo |> ignore
                use session = store.OpenSession()

                session
                |> Session.patch<Person>(marcoPolo.Id)
                |> Session.Patch.set <@ fun dog -> dog.Age @> 200
                Session.saveChanges session

                let actualMarco =
                    session
                    |> Session.loadByGuid<Person> marcoPolo.Id
                Expect.equal (Some edittedMarco) actualMarco "Edited successfully"

    testCase'
        "patch and then increment" <|
            fun (db, store) ->
                let marcoPolo = newPerson "Marco Polo" 500
                let edittedMarco1 = { marcoPolo with Age = 501 }
                let edittedMarco3 = { marcoPolo with Age = 503 }
                savePerson store marcoPolo |> ignore
                use session = store.OpenSession()

                session
                |> Session.patch<Person>(marcoPolo.Id)
                |> Session.Patch.inc<Person> <@ fun marco -> marco.Age @>
                Session.saveChanges session

                let actualMarco =
                    session
                    |> Session.loadByGuid<Person> marcoPolo.Id
                Expect.equal (Some edittedMarco1) actualMarco "Edited successfully"

    testCase'
        "patch and then increment not by one" <|
            fun  (db, store) ->
                let marcoPolo = newPerson "Marco Polo" 500
                let edittedMarco3 = { marcoPolo with Age = 503 }
                savePerson store marcoPolo |> ignore

                use session = store.OpenSession()
                session
                |> Session.patch<Person>(marcoPolo.Id)
                |> Session.Patch.incPlural<Person> <@ fun marco -> marco.Age @> 3
                Session.saveChanges session

                let actualMarco =
                    session
                    |> Session.loadByGuid<Person> marcoPolo.Id
                Expect.equal (Some edittedMarco3) actualMarco "Edited successfully"

]

let LinQQueryTests = [
    testCase'
        "count, min, and max" <|
            fun  (db, store : IDocumentStore) ->

                let marcoPolo = newPerson "Marco Polo" 500
                let niccoloPolo = newPerson "Niccolo Polo" 800
                let maffeoPolo = newPerson "Maffeo Polo" 801
                use session = store.OpenSession ()

                session |> Session.storeMany [ marcoPolo; niccoloPolo; maffeoPolo ]
                Session.saveChanges session

                let peopleCount =
                    session
                    |> Session.query<Person>
                    |> Queryable.count<Person> <@ fun person -> person.Age > 500 @>
                Expect.equal 2 peopleCount "Should be the same"

                let oldest =
                    session
                    |> Session.query<Person>
                    |> Queryable.max<Person, int> <@ fun person -> person.Age @>
                Expect.equal maffeoPolo.Age oldest "Should be Maffeo Polo"

                let youngest =
                    session
                    |> Session.query<Person>
                    |> Queryable.min<Person, int> <@ fun person -> person.Age @>
                Expect.equal marcoPolo.Age youngest "Should be Marco Polo"

    testCase'
        "paging (skip and take)" <|
            fun  (db, store) ->
                let marcoPolo = newPerson "Marco Polo" 500
                let niccoloPolo = newPerson "Niccolo Polo" 800
                let maffeoPolo = newPerson "Maffeo Polo" 801
                let magellan = newPerson "Ferdinand Magellan" 600
                let columbus = newPerson "Christopher Columbus" 550
                use session = store.OpenSession ()

                let people = [ marcoPolo; niccoloPolo; maffeoPolo; magellan; columbus ]
                let peopleGeneric = new System.Collections.Generic.List<Person>([ niccoloPolo; maffeoPolo; magellan ])

                session |> Session.storeMany  people
                Session.saveChanges session

                let paged =
                    session
                    |> Session.query<Person>
                    |> Queryable.paging 1 3
                    |> Queryable.toList

                Seq.zip peopleGeneric paged
                |> fun x -> Seq.iter (fun (p, db) -> Expect.equal p db "Same people (paging)") x

    testCase'
        "orderBy, orderByDescending, and thenBy" <|
            fun  (db, store) ->
                let marcoPolo = newPerson "Marco Polo" 500
                let niccoloPolo = newPerson "Niccolo Polo" 800
                let maffeoPolo = newPerson "Maffeo Polo" 801
                let magellan = newPerson "Ferdinand Magellan" 600
                let columbus = newPerson "Christopher Columbus" 550
                use session = store.OpenSession ()

                let people =
                    [ marcoPolo; niccoloPolo; maffeoPolo; magellan; columbus ]
                    |> List.sortBy (fun person -> person.Age)
                    |> fun x -> Collections.Generic.List<Person>(x)

                let peopleReversed =
                    [ marcoPolo; niccoloPolo; maffeoPolo; magellan; columbus ]
                    |> List.sortBy (fun person -> person.Age)
                    |> List.rev
                    |> fun x -> Collections.Generic.List<Person>(x)

                session |> Session.storeMany people
                Session.saveChanges session

                let ordered =
                    session
                    |> Session.query<Person>
                    |> Queryable.orderBy<Person, int> <@ fun person -> person.Age @>
                    |> Queryable.toList

                // Expect.equal people ordered "Same people."
                Seq.zip people ordered
                |> fun x -> Seq.iter (fun (r, db) -> Expect.equal r db "same person") x

                let reversed =
                    session
                    |> Session.query<Person>
                    |> Queryable.orderByDescending<Person, int> <@ fun person -> person.Age @>
                    |> Queryable.toList
                // Expect.equal peopleReversed reversed "Same people. Reversed."
                Seq.zip peopleReversed reversed
                |> fun x -> Seq.iter (fun (r, db) -> Expect.equal r db "same person (rev)") x
]

type personByNameParameter = {
    name : string
}
let sqlTests = [
    testCase'
        "sql with string parameter" <|
            fun (db,store : IDocumentStore) ->
                let marcoPolo = newPerson "Marco Polo" 500
                let niccoloPolo = newPerson "Niccolo Polo" 800
                let maffeoPolo = newPerson "Maffeo Polo" 801
                use session = store.OpenSession ()

                session |> Session.storeMany  [ marcoPolo; niccoloPolo; maffeoPolo ]
                Session.saveChanges session

                use session2 = store.OpenSession()
                let personNameParameter =  { name = "Marco Polo"}
                let person = session2 |> Session.sql<Person>  "select data from mt_doc_tests_person where data->>'Name' = :name" [|personNameParameter|] |> Seq.head
                Expect.equal person marcoPolo "Not marco"
    testCaseAsync'
        "sql with string paramter async" <|
            fun (db,store : IDocumentStore) -> async {
                let marcoPolo = newPerson "Marco Polo" 500
                let niccoloPolo = newPerson "Niccolo Polo" 800
                let maffeoPolo = newPerson "Maffeo Polo" 801
                use session = store.OpenSession ()

                session |> Session.storeMany  [ marcoPolo; niccoloPolo; maffeoPolo ]
                Session.saveChanges session

                use session2 = store.OpenSession()
                let personNameParameter =  { name = "Marco Polo"}
                let! person = session2 |>Session.sqlAsync<Person>  "select data from mt_doc_tests_person where data->>'Name' = :name" [|personNameParameter|] |> Async.map(Seq.head)
                Expect.equal person marcoPolo "Not marco"
            }
]

[<Tests>]
let ``API Tests`` =
    testList "API Tests" [
        yield! testFixture' withDatabaseAndStore [
            yield! exactlyOnceTests
            yield! filterTests
            yield! mapTests
            yield! headTests
            yield! toListTests
            yield! CRUDTests
            yield! PatchTests
            yield! LinQQueryTests
            yield! sqlTests
        ]
    ]
