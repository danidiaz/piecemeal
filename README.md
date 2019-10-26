# What's this?

An experiment in finding a generic way to handle HTTP PATCH updates for state
kept in a server.

The `defaultMain` function exported by the library takes a type-level map of
field names to types, which defines a record. (This map can be derived from a
regular record through generics, as in the example `piecemeal` executable). 

It then launches a server which keeps a record with "nullable" fields as state.
The record starts with all fields set to `Nothing`. 

The url of the server is

    http://127.0.0.1:8081/piecemeal

PATCH updates can be used to give values to fields.

GET requests return either a fully constructed record in a Left, or a "still in
construction" record in a Right.

# Some curls

The `piecemeal` executable uses the following record as server state:

    data Person = Person { name :: String, age :: Int } 
        deriving (Show,Generic,ToRecord)

The server can be exercised with these requests:

    curl -X GET http://127.0.0.1:8081/piecemeal
    curl -H "Content-Type: application/json" -d '{ "age" : 55 }' -X PATCH http://127.0.0.1:8081/piecemeal
    curl -H "Content-Type: application/json" -d '{ "name" : "John" }' -X PATCH http://127.0.0.1:8081/piecemeal
