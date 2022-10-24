module Tests


open Expecto
open DotNet.Testcontainers
open DotNet.Testcontainers.Builders
open DotNet.Testcontainers.Containers
open DotNet.Testcontainers.Configurations

module Expecto =
    module Expect =
        open Expecto.Logging
        open Expecto.Logging.Message

        let throwsTAsync<'texn> f message =
            async {
                try
                    do! f
                    Tests.failtestf "%s. Expected f to throw." message
                with
                | e when e.GetType() <> typeof<'texn> ->
                    Tests.failtestf
                        "%s. Expected f to throw an exn of type %s, but one of type %s was thrown."
                        message
                        (typeof<'texn>.FullName)
                        (e.GetType().FullName)
                | e -> ()
            }

        let isType<'a> x message =
            if x.GetType() <> typeof<'a> then
                Tests.failtestf
                    "%s. Expected x to be type %s, but was one of type %s."
                    message
                    (typeof<'a>.FullName)
                    (x.GetType().FullName)

open System
open System.Linq

open Expecto
open Marten
open Marten.PLv8
open Marten.Linq
open Npgsql

type Author = { Id: int; Name: string }

type Book =
    { Id: int
      AuthorId: int
      Title: string }

[<CLIMutableAttribute>]
type Person = { Id: Guid; Name: string; Age: int }

module DatabaseTestHelpers =
    let execNonQuery connStr commandStr =
        use conn = new NpgsqlConnection(connStr)
        use cmd = new NpgsqlCommand(commandStr, conn)
        conn.Open()
        cmd.ExecuteNonQuery()

    let createDatabase connStr databaseName =
        databaseName
        |> sprintf "CREATE database \"%s\" ENCODING = 'UTF8'"
        |> execNonQuery connStr
        |> ignore

type DisposableDatabase() =
    let container =
        TestcontainersBuilder<PostgreSqlTestcontainer>()
            .WithDatabase(
                new PostgreSqlTestcontainerConfiguration(
                    Database = "db",
                    Username = "postgres",
                    Password = "postgres",
                    Port = 5432
                )
            )
            .WithExposedPort(5432)
            .WithWaitStrategy(Wait.ForUnixContainer().UntilPortIsAvailable(5432))
            .Build()

    member val Container = container


    interface IDisposable with
        member this.Dispose() =
            this.Container.StopAsync() |> Async.AwaitTask |> Async.RunSynchronously

            this.Container.DisposeAsync().AsTask()
            |> Async.AwaitTask
            |> Async.RunSynchronously

[<CLIMutable>]
type Dog =
    { Id: Guid
      Name: string
      FavoriteChewToy: string }

let newDog name favoriteChewToy =
    { Id = Guid.NewGuid()
      Name = name
      FavoriteChewToy = favoriteChewToy }

let withDatabaseAndStore (database: DisposableDatabase) (f: _ -> _) =
    async {
        let databaseName = Guid.NewGuid().ToString("n")
        DatabaseTestHelpers.createDatabase database.Container.ConnectionString databaseName

        let connectionString =
            NpgsqlConnectionStringBuilder(
                Host = database.Container.Hostname,
                Port = database.Container.Port,
                Username = database.Container.Username,
                Password = database.Container.Password,
                Database = databaseName
            )
            |> string

        use store =
            DocumentStore.For(fun options ->
                options.Connection(connectionString)

                options.Schema.For<Book>().ForeignKey<Author>(fun book -> book.AuthorId)
                |> ignore)

        let! result = f (database, store)
        store.Advanced.Clean.DeleteAllDocuments()
        return result
    }

type ParameterizedTest<'a> =
    | Sync of string * ('a -> unit)
    | Async of string * ('a -> Async<unit>)


let testCase' name test = ParameterizedTest.Sync(name, test)

let testCaseAsync' name test = ParameterizedTest.Async(name, test)

let testFixture'<'a> setup =
    let database = new DisposableDatabase() // TODO: FIGURE OUT HOW TO DISPOSE OF THIS PROPERLY.
    database.Container.StartAsync() |> Async.AwaitTask |> Async.RunSynchronously

    Seq.map (fun (parameterizedTest: ParameterizedTest<'a>) ->
        match parameterizedTest with
        | Sync (name, test) ->
            testCase name
            <| fun () -> test >> async.Return |> setup database |> Async.RunSynchronously
        | Async (name, test) -> testCaseAsync name <| setup database test)

let exactlyOnceTests =
    [ testCase' "exactlyOne, Get Record Back"
      <| fun (db: DisposableDatabase, (store: DocumentStore)) ->
          let expectedDog = newDog "Sparky" "Macbook"

          use session = store.OpenSession()
          session |> Session.storeSingle expectedDog
          do session |> Session.saveChanges

          let actualDog = session |> Session.query<Dog> |> Queryable.exactlyOne
          Expect.equal expectedDog actualDog "Actual dog doesn't match our precious Sparky"


      testCase' "exactlyOne, Throws if more than one"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->

          Expect.throwsT<InvalidOperationException>
              (fun _ ->
                  use session = store.OpenSession()
                  newDog "Spark" "Macbook" |> session.Insert
                  newDog "Sparky" "Macbooky" |> session.Insert
                  session |> Session.saveChanges
                  let _ = session |> Session.query<Dog> |> Queryable.exactlyOne
                  ())
              "There are too many dogs!!!"


      testCase' "exactlyOne, Throws if none"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          Expect.throwsT<InvalidOperationException>
              (fun _ ->
                  use session = store.OpenSession()
                  let actualDog = session |> Session.query<Dog> |> Queryable.exactlyOne
                  ())
              "There are too many dogs!!! I'm more of a cat person myself."


      testCaseAsync' "exactlyOneAsync, Get Record Back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              let expectedDog = newDog "Spark" "Macbook"

              use session = store.OpenSession()
              session |> Session.storeSingle expectedDog
              do! session |> Session.saveChangesAsync
              let! actualDog = session |> Session.query<Dog> |> Queryable.exactlyOneAsync
              Expect.equal expectedDog actualDog "There shouldn't be that many dogs!"
          }
      testCaseAsync' "exactlyOneAsync, Throws if more than one"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              do!
                  Expect.throwsTAsync<AggregateException>
                      (async {
                          use session = store.OpenSession()
                          newDog "Spark" "Macbook" |> session.Insert
                          newDog "Sparky" "Macbooky" |> session.Insert
                          session |> Session.saveChanges
                          let! _ = session |> Session.query<Dog> |> Queryable.exactlyOneAsync
                          ()
                      })
                      "Should be too many dogs!"
          }
      testCaseAsync' "exactlyOneAsync, Throws if none"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              do!
                  Expect.throwsTAsync<AggregateException>
                      (async {
                          use session = store.OpenSession()
                          let! actualDog = session |> Session.query<Dog> |> Queryable.exactlyOneAsync
                          ()
                      })
                      "Should be no dogs!"

          } ]

let filterTests =
    [ testCase' "filter by, Get Record Back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()
          let expectedDog = newDog "Spark" "Macbook"

          session |> Session.storeSingle expectedDog
          session |> Session.saveChanges

          let actualDog =
              session
              |> Session.query<Dog>
              |> Queryable.filter <@ fun d -> d.Name = "Spark" @>
              |> Queryable.head

          Expect.equal expectedDog actualDog "There shouldn't be that many dogs!"
      testCase' "filter IsOneOf"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()
          let expectedDog = newDog "Spark" "Macbook"

          session |> Session.storeSingle expectedDog
          session |> Session.saveChanges

          let names = [| expectedDog.Name; "Fido"; "Snape" |]

          let actualDog =
              session
              |> Session.query<Dog>
              |> Queryable.filter <@ fun d -> d.Name.IsOneOf(names) @>
              |> Queryable.head

          Expect.equal expectedDog actualDog "There shouldn't be that many dogs!"
      testCase' "filter IsOneOf not in list"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()
          let expectedDog = newDog "Spark" "Macbook"

          session |> Session.storeSingle expectedDog
          session |> Session.saveChanges

          let names = [| "Fido"; "Snape" |]

          let actualDog =
              session
              |> Session.query<Dog>
              |> Queryable.filter <@ fun d -> d.Name.IsOneOf(names) @>
              |> Queryable.tryHead

          Expect.isNone actualDog "No dog!"

      ]

type User = { Name: string }

let mapTests =
    [ testCase' "map, Get Name Back"
      <| fun (db: DisposableDatabase, (store: DocumentStore)) ->
          use session = store.OpenSession()
          let expectedDog = newDog "Spark" "Macbook"

          session |> Session.storeSingle expectedDog
          session |> Session.saveChanges

          let actualDog =
              session
              |> Session.query<Dog>
              |> Queryable.map (<@ fun s -> s.Name @>)
              |> Queryable.head

          Expect.equal expectedDog.Name actualDog "There shouldn't be that many dogs!"

      testCase' "map to another type, Get User Back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()
          let expectedDog = newDog "Spark" "Macbook"

          session |> Session.storeSingle expectedDog
          session |> Session.saveChanges

          let actualUser =
              session
              |> Session.query<Dog>
              |> Queryable.map (<@ fun s -> { Name = s.Name } @>)
              |> Queryable.head

          Expect.isType<User> (box actualUser) "Should be User type"
          Expect.equal expectedDog.Name actualUser.Name "Should be a user!"

      ]

let headTests =
    [ testCase' "head, single dog, Get Record Back"
      <| fun (db: DisposableDatabase, (store: DocumentStore)) ->
          use session = store.OpenSession()
          let expectedDog = newDog "Spark" "Macbook"

          session |> Session.storeSingle expectedDog
          session |> Session.saveChanges

          let actualDog = session |> Session.query<Dog> |> Queryable.head
          Expect.equal expectedDog actualDog "Should be a same dog!"

      testCase' "head, multiple dogs, get top record back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()

          let expectedDog = newDog "Spark" "Macbook"
          let otherDog = newDog "Sparky" "Lightning Bolt"

          session |> Session.storeMany [| expectedDog; otherDog |]
          session |> Session.saveChanges

          let actualDog = session |> Session.query<Dog> |> Queryable.head
          Expect.equal expectedDog actualDog "Should be a same dog!"

      testCase' "head, no dogs, throws exception"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          Expect.throwsT<InvalidOperationException>
              (fun _ ->
                  use session = store.OpenSession()
                  let actualDog = session |> Session.query<Dog> |> Queryable.head
                  ())
              "Should be no dog!"


      testCaseAsync' "headAsync, single dog, Get Record Back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              use session = store.OpenSession()
              let expectedDog = newDog "Spark" "Macbook"
              session |> Session.storeSingle expectedDog
              session |> Session.saveChanges
              let! actualDog = session |> Session.query<Dog> |> Queryable.headAsync
              Expect.equal expectedDog actualDog "Should be a same dog!"
          }
      testCaseAsync' "headAsync, multiple dogs, get top record back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              use session = store.OpenSession()
              let expectedDog = newDog "Spark" "Macbook"
              session |> Session.storeSingle expectedDog
              session.Insert(newDog "Sparky" "Lightning Bolt")
              session |> Session.saveChanges
              let! actualDog = session |> Session.query<Dog> |> Queryable.headAsync
              Expect.equal expectedDog actualDog "Should be a same dog!"
          }
      testCaseAsync' "headAsync, no dogs, throws exception"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              do!
                  Expect.throwsTAsync<AggregateException>
                      (async {
                          use session = store.OpenSession()
                          let! actualDog = session |> Session.query<Dog> |> Queryable.headAsync
                          ()
                      })
                      "Should be no dog!"
          } ]

let toListTests =
    [ testCase' "toList, single dog, get single back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()
          let expectedDog = newDog "Spark" "Macbook"
          session |> Session.storeSingle expectedDog
          session |> Session.saveChanges
          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.contains actualDogs expectedDog "Should contain same dog!"

      testCase' "toList, multiple dogs, get multple back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()

          let expectedDog = newDog "Spark" "Macbook"
          let expectedDog2 = newDog "Sparky" "Lightning bolt"

          session |> Session.storeSingle expectedDog
          session.Insert(expectedDog2)

          session |> Session.saveChanges

          use session = store.OpenSession()
          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.contains actualDogs expectedDog "Should contain same dog!"
          Expect.contains actualDogs expectedDog2 "Should contain same dog!"

      testCase' "toList, no dogs, get empty list"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()
          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.isEmpty actualDogs "Should be no dogs!"

      testCaseAsync' "toListAsync, single dog, get single back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              use session = store.OpenSession()

              let expectedDog = newDog "Spark" "Macbook"
              session |> Session.storeSingle expectedDog
              session |> Session.saveChanges

              let! actualDogs = session |> Session.query<Dog> |> Queryable.toListAsync
              Expect.contains actualDogs expectedDog "Should contain same dog!"
          }
      testCaseAsync' "toListAsync, multiple dogs, get multple back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              use session = store.OpenSession()
              let expectedDog = newDog "Spark" "Macbook"
              let expectedDog2 = newDog "Sparky" "Lightning bolt"
              session |> Session.storeSingle expectedDog
              session.Insert(expectedDog2)
              session |> Session.saveChanges
              let! actualDogs = session |> Session.query<Dog> |> Queryable.toListAsync
              Expect.contains actualDogs expectedDog "Should contain same dog!"
              Expect.contains actualDogs expectedDog2 "Should contain same dog!"
          }
      testCaseAsync' "toListAsync, no dogs, get empty list"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              use session = store.OpenSession()
              let! actualDogs = session |> Session.query<Dog> |> Queryable.toListAsync
              Expect.isEmpty actualDogs "Should be no dogs!"
          } ]

let CRUDTests =
    [ testCase' "loadByGuid, Get Some Record back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()
          let expectedDog = newDog "Spark" "Macbook"
          session |> Session.storeSingle expectedDog
          session |> Session.saveChanges
          let actualDog = session |> Session.loadByGuid<Dog> expectedDog.Id
          Expect.equal (Some expectedDog) actualDog "Same dog!"
      testCase' "loadByGuid, Get None Record back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          use session = store.OpenSession()
          let dog = newDog "Spark" "Macbook"
          session.Insert(dog)
          session |> Session.saveChanges
          let actualDog = session |> Session.loadByGuid<Dog> (Guid.NewGuid())
          Expect.equal None actualDog "Should be no dog!"
      testCaseAsync' "loadByGuidAsync, get Some record back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              use session = store.OpenSession()
              let expectedDog = newDog "Spark" "Macbook"
              session |> Session.storeSingle expectedDog
              session |> Session.saveChanges
              let! actualDog = session |> Session.loadByGuidAsync<Dog> expectedDog.Id
              Expect.equal (Some expectedDog) actualDog "Same dog!"
          }
      testCaseAsync' "loadByGuidAsync, get None record back"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              use session = store.OpenSession()
              let dog = newDog "Spark" "Macbook"
              session.Insert(dog)
              session |> Session.saveChanges
              use session = store.OpenSession()
              let! actualDog = session |> Session.loadByGuidAsync<Dog> (Guid.NewGuid())

              Expect.equal None actualDog "Should be no dog!"
          }
      testCase' "storeSingle/saveChanges"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let sparky = newDog "Sparky" "Shoes"

          use session = store.OpenSession()
          session |> Session.storeSingle sparky
          session |> Session.saveChanges
          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.contains actualDogs sparky "Should contain same dog!"


      testCase' "storeMany/delete/saveChanges"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let sparky = newDog "Sparky" "Shoes"
          let spot = newDog "Spot" "Macbook"

          use session = store.OpenSession()
          session |> Session.storeSingle sparky
          session |> Session.storeSingle spot
          session |> Session.saveChanges

          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.contains actualDogs sparky "Sequence should contain sparky."
          Expect.contains actualDogs spot "Sequence should contain spot"

          session |> Session.deleteEntity spot
          session |> Session.saveChanges
          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.contains actualDogs sparky "Sequence should contain sparky."

          session |> Session.deleteByGuid<Dog> sparky.Id
          session |> Session.saveChanges
          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.isEmpty actualDogs "Sequence should be empty."


      testCase' "storeMany/deleteBy/saveChanges"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let sparky = newDog "Sparky" "Shoes"
          let spot = newDog "Spot" "Macbook"

          use session = store.OpenSession()
          session |> Session.storeMany [ sparky; spot ]
          session |> Session.saveChanges
          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.contains actualDogs sparky "Should contain same dog!"
          Expect.contains actualDogs spot "Should contain same dog!"

          session |> Session.deleteBy<Dog> <@ fun d -> d.Name = "Sparky" @>
          session |> Session.saveChanges
          let actualDogs = session |> Session.query<Dog> |> Queryable.toList
          Expect.contains actualDogs spot "Should contain same dog!"

      testCaseAsync' "storeSingle/saveChangesAsync"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              let sparky = newDog "Sparky" "Shoes"

              use session = store.OpenSession()
              session |> Session.storeSingle sparky
              do! session |> Session.saveChangesAsync
              let! actualDogs = session |> Session.query<Dog> |> Queryable.toListAsync
              Expect.contains actualDogs sparky "Should contain same dog!"

          }
      testCaseAsync' "storeMany/saveChangesAsync"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              let sparky = newDog "Sparky" "Shoes"
              let spot = newDog "Spot" "Macbook"

              use session = store.OpenSession()
              session |> Session.storeMany [ sparky; spot ]
              do! session |> Session.saveChangesAsync
              let! actualDogs = session |> Session.query<Dog> |> Queryable.toListAsync
              Expect.contains actualDogs sparky "Should contain same dog!"
              Expect.contains actualDogs spot "Should contain same dog!"
          } ]

let newPerson name age =
    { Id = Guid.NewGuid()
      Name = name
      Age = age }

// let savePerson (store: #IDocumentStore) (person: Person) =
//     use session = store.OpenSession()
//     session.Store(person)
//     session |> Session.saveChanges
//     person

// let PatchTests =
//     [ testCase' "patch and then set"
//       <| fun (db: DisposableDatabase, store: DocumentStore) ->
//           let marcoPolo = newPerson "Marco Polo" 500
//           let edittedMarco = { marcoPolo with Age = 200 }
//           savePerson store marcoPolo |> ignore
//           use session = store.OpenSession()

//           session
//           |> Session.patchByGuid<Person> (marcoPolo.Id)
//           |> Session.Patch.set <@ fun dog -> dog.Age @> 200

//           Session.saveChanges session

//           let actualMarco = session |> Session.loadByGuid<Person> marcoPolo.Id
//           Expect.equal (Some edittedMarco) actualMarco "Edited successfully"

//       testCase' "patch and then increment"
//       <| fun (db: DisposableDatabase, store: DocumentStore) ->
//           let marcoPolo = newPerson "Marco Polo" 500
//           let edittedMarco1 = { marcoPolo with Age = 501 }
//           let edittedMarco3 = { marcoPolo with Age = 503 }
//           savePerson store marcoPolo |> ignore
//           use session = store.OpenSession()

//           session
//           |> Session.patchByGuid<Person> (marcoPolo.Id)
//           |> Session.Patch.inc<Person> <@ fun marco -> marco.Age @>

//           Session.saveChanges session

//           let actualMarco = session |> Session.loadByGuid<Person> marcoPolo.Id
//           Expect.equal (Some edittedMarco1) actualMarco "Edited successfully"

//       testCase' "patch and then increment not by one"
//       <| fun (db: DisposableDatabase, store: DocumentStore) ->
//           let marcoPolo = newPerson "Marco Polo" 500
//           let edittedMarco3 = { marcoPolo with Age = 503 }
//           savePerson store marcoPolo |> ignore

//           use session = store.OpenSession()

//           session
//           |> Session.patchByGuid<Person> (marcoPolo.Id)
//           |> Session.Patch.incPlural<Person> <@ fun marco -> marco.Age @> 3

//           Session.saveChanges session

//           let actualMarco = session |> Session.loadByGuid<Person> marcoPolo.Id
//           Expect.equal (Some edittedMarco3) actualMarco "Edited successfully"

//       ]

let linqQueryTests =
    [ testCase' "count, min, and max"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let marcoPolo = newPerson "Marco Polo" 500
          let niccoloPolo = newPerson "Niccolo Polo" 800
          let maffeoPolo = newPerson "Maffeo Polo" 801
          use session = store.OpenSession()

          session |> Session.storeMany [ marcoPolo; niccoloPolo; maffeoPolo ]
          Session.saveChanges session

          let peopleCount =
              session
              |> Session.query<Person>
              |> Queryable.countWhere<Person> <@ fun person -> person.Age > 500 @>

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

      testCase' "paging (skip and take)"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let marcoPolo = newPerson "Marco Polo" 500
          let niccoloPolo = newPerson "Niccolo Polo" 800
          let maffeoPolo = newPerson "Maffeo Polo" 801
          let magellan = newPerson "Ferdinand Magellan" 600
          let columbus = newPerson "Christopher Columbus" 550

          use session = store.OpenSession()
          let people = [ marcoPolo; niccoloPolo; maffeoPolo; magellan; columbus ]
          let peopleGeneric = people[1..3] |> List.toSeq

          session |> Session.storeMany people
          Session.saveChanges session

          let paged =
              session |> Session.query<Person> |> Queryable.paging 1 3 |> Queryable.toList

          Seq.zip peopleGeneric paged
          |> fun x -> Seq.iter (fun (p, db) -> Expect.equal p db "Same people (paging)") x

      testCase' "orderBy, orderByDescending, and thenBy"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let marcoPolo = newPerson "Marco Polo" 500
          let niccoloPolo = newPerson "Niccolo Polo" 800
          let maffeoPolo = newPerson "Maffeo Polo" 801
          let magellan = newPerson "Ferdinand Magellan" 600
          let columbus = newPerson "Christopher Columbus" 550
          use session = store.OpenSession()

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
          |> fun x -> Seq.iter (fun (r, db) -> Expect.equal r db "same person (rev)") x ]

type personByNameParameter = { name: string }

let sqlTests =
    [ testCase' "sql with string parameter"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let marcoPolo = newPerson "Marco Polo" 500
          let niccoloPolo = newPerson "Niccolo Polo" 800
          let maffeoPolo = newPerson "Maffeo Polo" 801
          use session = store.OpenSession()

          session |> Session.storeMany [ marcoPolo; niccoloPolo; maffeoPolo ]
          Session.saveChanges session

          use session2 = store.OpenSession()
          let personNameParameter = { name = "Marco Polo" }

          let person =
              session2
              |> Session.sql<Person>
                  "select data from mt_doc_tests_person where data->>'Name' = :name"
                  [| personNameParameter |]
              |> Seq.head

          Expect.equal person marcoPolo "Not marco"
      testCaseAsync' "sql with string paramter async"
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          async {
              let marcoPolo = newPerson "Marco Polo" 500
              let niccoloPolo = newPerson "Niccolo Polo" 800
              let maffeoPolo = newPerson "Maffeo Polo" 801
              use session = store.OpenSession()

              session |> Session.storeMany [ marcoPolo; niccoloPolo; maffeoPolo ]
              Session.saveChanges session

              use session2 = store.OpenSession()
              let personNameParameter = { name = "Marco Polo" }

              let! person =
                  session2
                  |> Session.sqlAsync<Person>
                      "select data from mt_doc_tests_person where data->>'Name' = :name"
                      [| personNameParameter |]
                  |> Async.map (Seq.head)

              Expect.equal person marcoPolo "Not marco"
          } ]

let includeTests =
    [ testCase' "include single related document."
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let author = { Id = 1; Name = "John Doe" }

          let book =
              { Id = 1
                AuthorId = 1
                Title = "The adventures of a mysterious man." }

          use session = store.OpenSession()

          session |> Session.storeSingle author
          session |> Session.storeSingle book
          Session.saveChanges session

          use session2 = store.OpenSession()

          let mutable bookAuthor: Author option = None

          let bookFromDb =
              session2
              |> Session.query<Book>
              |> Queryable.includeSingle <@ fun book -> book.AuthorId @> (fun value -> bookAuthor <- Some value)
              |> Queryable.tryHead

          Expect.equal bookFromDb (Some book) "Not the same book"
          Expect.equal bookAuthor (Some author) "Not the same author"

      testCase' "include list of related documents."
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let authors = [ { Id = 1; Name = "John Doe" }; { Id = 2; Name = "Jane Doe" } ]

          let books =
              [ { Id = 1
                  AuthorId = 1
                  Title = "The adventures of a mysterious man." }

                { Id = 2
                  AuthorId = 2
                  Title = "The adventures of a mysterious woman." } ]

          use session = store.OpenSession()

          session |> Session.storeMany authors
          session |> Session.storeMany books
          Session.saveChanges session

          use session2 = store.OpenSession()

          let bookAuthors = Collections.Generic.List<Author>()

          let booksFromDb =
              session2
              |> Session.query<Book>
              |> Queryable.includeList <@ fun book -> book.AuthorId @> bookAuthors
              |> Queryable.toList

          Expect.equal (booksFromDb |> Seq.toList) books "Not the same books"
          Expect.equal (bookAuthors |> Seq.toList) authors "Not the same authors"

      testCase' "include dictionary of related documents."
      <| fun (db: DisposableDatabase, store: DocumentStore) ->
          let authors = [ { Id = 1; Name = "John Doe" }; { Id = 2; Name = "Jane Doe" } ]

          let books =
              [ { Id = 1
                  AuthorId = 1
                  Title = "The adventures of a mysterious man." }

                { Id = 2
                  AuthorId = 2
                  Title = "The adventures of a mysterious woman." } ]

          use session = store.OpenSession()

          session |> Session.storeMany authors
          session |> Session.storeMany books
          Session.saveChanges session

          use session2 = store.OpenSession()

          let bookAuthors = Collections.Generic.Dictionary<int, Author>()

          let booksFromDb =
              session2
              |> Session.query<Book>
              |> Queryable.includeDict <@ fun book -> book.AuthorId @> bookAuthors
              |> Queryable.toList

          Expect.equal (booksFromDb |> Seq.toList) books "Not the same books"
          Expect.equal bookAuthors.Count 2 "Count is greater than or less than 2"
          Expect.equal bookAuthors[1] authors[0] "Authors do not match"
          Expect.equal bookAuthors[2] authors[1] "Authors do not match" ]

[<Tests>]
let ``API Tests`` =
    testList
        "API Tests"
        [ yield!
              testFixture'
                  withDatabaseAndStore
                  [ yield! exactlyOnceTests
                    yield! filterTests
                    yield! mapTests
                    yield! headTests
                    yield! toListTests
                    yield! CRUDTests
                    yield! linqQueryTests
                    yield! sqlTests
                    yield! includeTests ] ]
