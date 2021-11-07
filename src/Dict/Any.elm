module Dict.Any exposing
    ( AnyDict, equal
    , empty, singleton, insert, update, remove, removeAll
    , isEmpty, member, get, getKey, size, any, all
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition, filterMap
    , union, intersect, diff, merge, groupBy
    , toDict
    , decode, decode_, decodeList, encode, encodeList
    )

{-| A dictionary mapping unique keys to values.
Similar and based on Dict but without restriction on comparable keys.

Insert, remove, and query operations all take O(log n) time.


# Converting Types to Comparable

When writing a function for conversion from the type you want to use for keys to comparable
it's very important to make sure every distinct member of type k produces different value in set o of comparables.

Take for instance those two examples:

We can use Bool as a key for our Dict (No matter how unpractical it might seem)

    boolToInt : Bool -> Int
    boolToInt bool =
        case bool of
            False -> 0
            True -> 1

    empty boolToInt
    |> insert True "foo"
    |> get True
    --> Just "foo"

or Maybe String.

    comparableKey : Maybe String -> (Int, String)
    comparableKey maybe =
        case maybe of
            Nothing -> (0, "")
            Just str -> (1, str)

    empty comparableKey
        |> insert (Just "foo") 42
        |> get (Just "foo")
    --> Just 42

Note that we give Int code to either constructor and in Case of Nothing we default to `""` (empty string).
There is still a difference between `Nothing` and `Just ""` (`Int` value in the pair is different).
In fact, you can "hardcode" any value as the second member of the pair
in case of nothing but empty string seems like a reasonable option for this case.
Generally, this is how I would implement `toComparable` function for most of your custom data types.
Have a look at the longest constructor,
Define tuple where the first key is int (number of the constructor)
and other are types within the constructor and you're good to go.


# Dictionaries

@docs AnyDict, equal


# Build

@docs empty, singleton, insert, update, remove, removeAll


# Query

@docs isEmpty, member, get, getKey, size, any, all


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition, filterMap


# Combine

@docs union, intersect, diff, merge, groupBy


# Dict

@docs toDict


# Json

@docs decode, decode_, decodeList, encode, encodeList

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode


{-| Be aware that AnyDict stores a function internally.

Use [`equal`](#equal) function to check equality of two `AnyDict`s.
Using `(==)` would result in runtime exception because `AnyDict` type
contains a function.

-}
type AnyDict comparable k v
    = AnyDict
        { dict : Dict comparable ( k, v )
        , toKey : k -> comparable
        }


{-| Check equality of two `AnyDict`s

    * returns `True` if AnyDicts are equal
    * returns `False` if AnyDicts are not equal

-}
equal : AnyDict comparable k v -> AnyDict comparable k v -> Bool
equal (AnyDict r1) (AnyDict r2) =
    r1.dict == r2.dict



-- Build


{-| Create an empty dictionary by suppling function used for comparing keys.

**Note that it's important to make sure every key is turned to different comparable.**
Otherwise keys would conflict and overwrite each other.

-}
empty : (k -> comparable) -> AnyDict comparable k v
empty toKey =
    AnyDict
        { dict = Dict.empty
        , toKey = toKey
        }


{-| Create a dictionary with one key-value pair.

**Note that it's important to make sure every key is turned to different comparable.**
Otherwise keys would conflict and overwrite each other.

-}
singleton : k -> v -> (k -> comparable) -> AnyDict comparable k v
singleton k v f =
    empty f
        |> insert k v


{-| Insert a key-value pair into a dictionary. Replaces value when there is a collision.
-}
insert : k -> v -> AnyDict comparable k v -> AnyDict comparable k v
insert k v (AnyDict inner) =
    AnyDict { inner | dict = Dict.insert (inner.toKey k) ( k, v ) inner.dict }


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> AnyDict comparable k v -> AnyDict comparable k v
update k f (AnyDict inner) =
    let
        updateDict =
            Maybe.map (\b -> ( k, b )) << f << Maybe.map Tuple.second
    in
    AnyDict { inner | dict = Dict.update (inner.toKey k) updateDict inner.dict }


{-| Remove a key-value pair from a dictionary.
If the key is not found, no changes are made.
-}
remove : k -> AnyDict comparable k v -> AnyDict comparable k v
remove k (AnyDict inner) =
    AnyDict { inner | dict = Dict.remove (inner.toKey k) inner.dict }


{-| Remove all entries from AnyDict.

Useful when you need to create new empty AnyDict using
same comparable function for key type.

-}
removeAll : AnyDict comparable k v -> AnyDict comparable k x
removeAll (AnyDict inner) =
    AnyDict
        { toKey = inner.toKey
        , dict = Dict.empty
        }



-- Query


{-| Determine if a dictionary is empty.

    isEmpty (empty identity)
    --> True

    singleton 1 "foo" identity
        |> isEmpty
    --> False

-}
isEmpty : AnyDict comparable k v -> Bool
isEmpty (AnyDict { dict }) =
    Dict.isEmpty dict


{-| Determine if a key is in a dictionary.
-}
member : k -> AnyDict comparable k v -> Bool
member k (AnyDict { dict, toKey }) =
    Dict.member (toKey k) dict


{-| Get the value associated with a key.
If the key is not found, return Nothing.
This is useful when you are not sure
if a key will be in the dictionary.

    type Animal = Cat | Mouse | Dog

    animalToInt : Animal -> Int
    animalToInt animal =
        case animal of
            Cat -> 0
            Mouse -> 1
            Dog -> 2

    animals : AnyDict Int Animal String
    animals =
        [ (Cat, "Tom"), (Mouse, "Jerry") ]
            |> fromList animalToInt

    get Cat animals
    -> Just "Tom"

    get Mouse animals
    --> Just "Jerry"

    get Dog animals
    --> Nothing

-}
get : k -> AnyDict comparable k v -> Maybe v
get k (AnyDict { dict, toKey }) =
    Dict.get (toKey k) dict
        |> Maybe.map Tuple.second


{-| Get a key associated with key.

This is useful in case of `AnyDict` because
some parts of a key might not be used
for generating comparable.
This function allows quering `AnyDict` with old
key to obtain updated one in such cases.

-}
getKey : k -> AnyDict comparable k v -> Maybe k
getKey k (AnyDict { dict, toKey }) =
    Dict.get (toKey k) dict
        |> Maybe.map Tuple.first


{-| Determine the number of key-value pairs in the dictionary.
-}
size : AnyDict comparable k v -> Int
size (AnyDict { dict }) =
    Dict.size dict



-- List


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : AnyDict comparable k v -> List k
keys =
    List.map Tuple.first << toList


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : AnyDict comparable k v -> List v
values =
    List.map Tuple.second << toList


{-| Convert a dictionary into an association list of key-value pairs,
sorted by keys.
-}
toList : AnyDict comparable k v -> List ( k, v )
toList (AnyDict { dict }) =
    Dict.values dict


{-| Convert an association list into a dictionary.

**Note that it's important to make sure every key is turned to different comparable.**
Otherwise keys would conflict and overwrite each other.

-}
fromList : (k -> comparable) -> List ( k, v ) -> AnyDict comparable k v
fromList f xs =
    AnyDict
        { toKey = f
        , dict = Dict.fromList <| List.map (\( k, v ) -> ( f k, ( k, v ) )) xs
        }



-- Transform


{-| Apply a function to all values in a dictionary.
-}
map : (a -> b -> c) -> AnyDict comparable a b -> AnyDict comparable a c
map f (AnyDict { dict, toKey }) =
    AnyDict
        { dict = Dict.map (\_ ( k, v ) -> ( k, f k v )) dict
        , toKey = toKey
        }


{-| Fold over the key-value pairs in a dictionary, in order from lowest key to highest key.
-}
foldl : (k -> v -> b -> b) -> b -> AnyDict comparable k v -> b
foldl f acc (AnyDict { dict }) =
    Dict.foldl (\_ ( k, v ) -> f k v) acc dict


{-| Fold over the key-value pairs in a dictionary, in order from highest key to lowest key.
-}
foldr : (k -> v -> b -> b) -> b -> AnyDict comparable k v -> b
foldr f acc (AnyDict { dict }) =
    Dict.foldr (\_ ( k, v ) -> f k v) acc dict


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> AnyDict comparable k v -> AnyDict comparable k v
filter f (AnyDict inner) =
    AnyDict { inner | dict = Dict.filter (\_ ( k, v ) -> f k v) inner.dict }


{-| Partition a dictionary according to a predicate.
The first dictionary contains all key-value pairs which satisfy the predicate,
and the second contains the rest.
-}
partition : (k -> v -> Bool) -> AnyDict comparable k v -> ( AnyDict comparable k v, AnyDict comparable k v )
partition f (AnyDict inner) =
    let
        ( left, right ) =
            Dict.partition (\_ ( k, v ) -> f k v) inner.dict
    in
    ( AnyDict { inner | dict = left }
    , AnyDict { inner | dict = right }
    )



-- Combine


{-| Combine two dictionaries. If there is a collision, preference is given to the first dictionary.
-}
union : AnyDict comparable k v -> AnyDict comparable k v -> AnyDict comparable k v
union (AnyDict inner) (AnyDict { dict }) =
    AnyDict { inner | dict = Dict.union inner.dict dict }


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : AnyDict comparable k v -> AnyDict comparable k v -> AnyDict comparable k v
intersect (AnyDict inner) (AnyDict { dict }) =
    AnyDict { inner | dict = Dict.intersect inner.dict dict }


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : AnyDict comparable k v -> AnyDict comparable k v -> AnyDict comparable k v
diff (AnyDict inner) (AnyDict { dict }) =
    AnyDict { inner | dict = Dict.diff inner.dict dict }


{-| The most general way of combining two dictionaries.
You provide three accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

Only in the left dictionary.
In both dictionaries.
Only in the right dictionary.

-}
merge :
    (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> AnyDict comparable k a
    -> AnyDict comparable k b
    -> result
    -> result
merge f g h (AnyDict inner) (AnyDict { dict }) =
    let
        l fc _ ( k, v ) =
            fc k v
    in
    Dict.merge (l f) (\_ ( k, a ) ( _, b ) -> g k a b) (l h) inner.dict dict



-- Dict


{-| Convert `AnyDict` to plain dictionary with comparable keys.
-}
toDict : AnyDict comparable k v -> Dict comparable v
toDict (AnyDict { dict }) =
    Dict.map (always Tuple.second) dict


{-| Decode a JSON object into an `AnyDict`.

This encoder is limitted for cases where JSON representation
for a given type is an JSON Object. In JSON, object keys must be of type
`String`.
If you need to decode different representation into `AnyDict` value,
just use primitive `Decoder` types directly and map `AnyDict` constructors
over these.

    import Json.Decode

    type Key = Foo | Bar

    fromString : String -> Key
    fromString str =
       case str of
          "foo" -> Foo
          _     -> Bar

    toString : Key -> String
    toString key =
      case key of
         Foo -> "foo"
         Bar -> "bar"

    type alias Data = AnyDict String Key Int

    dataDecoder : Json.Decode.Decoder Data
    dataDecoder =
       decode (\str _ -> fromString str) toString Json.Decode.int


    Json.Decode.decodeString dataDecoder "{\"foo\":1,\"bar\":2}"
    |> Result.map toList
    --> Ok [(Bar, 2), (Foo, 1)]

-}
decode : (String -> v -> k) -> (k -> comparable) -> Decode.Decoder v -> Decode.Decoder (AnyDict comparable k v)
decode fromStr toComparable valueD =
    let
        construct strK v acc =
            insert (fromStr strK v) v acc
    in
    Decode.dict valueD
        |> Decode.map (Dict.foldr construct (empty toComparable))


{-| Decode a JSON object into an `AnyDict`.

This variant of decode allows you to fail with error while parsing key from String.
In such case whole Dict decoding will fail.

    import Json.Decode

    type Key = Foo | Bar

    fromString : String -> Result String Key
    fromString str =
       case str of
          "foo" -> Ok Foo
          "bar" -> Ok Bar
          _     -> Err <| "Unknown key " ++ str

    toString : Key -> String
    toString key =
      case key of
         Foo -> "foo"
         Bar -> "bar"

    type alias Data = AnyDict String Key Int

    dataDecoder : Json.Decode.Decoder Data
    dataDecoder =
       decode_ (\str _ -> fromString str) toString Json.Decode.int


    Json.Decode.decodeString dataDecoder "{\"foo\":1,\"bar\":2}"
    |> Result.map toList
    --> Ok [(Bar, 2), (Foo, 1)]

    Json.Decode.decodeString dataDecoder "{\"foo\":1,\"baz\":2}"
    |> Result.map toList
    --> Json.Decode.decodeString (Json.Decode.fail "Unknown key baz") "{}"

-}
decode_ : (String -> v -> Result String k) -> (k -> comparable) -> Decode.Decoder v -> Decode.Decoder (AnyDict comparable k v)
decode_ fromStr toComparable valueD =
    let
        construct strK v =
            Result.andThen
                (\acc ->
                    fromStr strK v
                        |> Result.map (\key -> insert key v acc)
                )
    in
    Decode.dict valueD
        |> Decode.map (Dict.foldr construct (Ok <| empty toComparable))
        |> Decode.andThen
            (\res ->
                case res of
                    Ok val ->
                        Decode.succeed val

                    Err err ->
                        Decode.fail err
            )


{-| Turn an `AnyDict` into a JSON object.

    import Json.Encode

    type Key = Foo | Bar

    toString : Key -> String
    toString key =
      case key of
         Foo -> "foo"
         Bar -> "bar"

    type alias Data = AnyDict String Key Int

    encodeData : Data -> Json.Encode.Value
    encodeData =
      encode toString Json.Encode.int

    fromList toString [(Foo, 1), (Bar, 2)]
    |> encodeData
    |> Json.Encode.encode 0
    --> "{\"bar\":2,\"foo\":1}"

-}
encode : (k -> String) -> (v -> Encode.Value) -> AnyDict comparable k v -> Encode.Value
encode keyE valueE =
    Encode.object
        << List.map (Tuple.mapBoth keyE valueE)
        << toList


{-| Turn an AnyDict into a JSON list of tuples. This is useful when you have more complex types as keys

    import Json.Decode as Decode
    import Json.Encode as Encode

    type alias Person = {first : String, last : String}
    type alias Age = Int

    personToString : Person -> String
    personToString {first, last} = first ++ last

    personEncode : Person -> Encode.Value
    personEncode {first, last} =
        Encode.object [("first", (Encode.string first)), ("last", (Encode.string last))]

    example : AnyDict String Person Age
    example =
        fromList personToString [(Person "Jeve" "Sobs", 9001), (Person "Tim" "Berners-Lee", 1234)]

    encodeList (\k v -> Encode.list identity [ personEncode k, Encode.int v ]) example
        |> Encode.encode 0
        --> "[[{\"first\":\"Jeve\",\"last\":\"Sobs\"},9001],[{\"first\":\"Tim\",\"last\":\"Berners-Lee\"},1234]]"

-}
encodeList : (k -> v -> Encode.Value) -> AnyDict comparable k v -> Encode.Value
encodeList encodeF =
    Encode.list (\( k, v ) -> encodeF k v) << toList


{-| Decode an AnyDict from a JSON list of tuples.

    import Json.Decode as Decode
    import Json.Encode as Encode

    type alias Person = {first : String, last : String}
    type alias Age = Int

    personToString : Person -> String
    personToString {first, last} = first ++ last

    personDecode : Decode.Decoder Person
    personDecode =
        Decode.map2
            Person
                (Decode.field "first" Decode.string)
                (Decode.field "last" Decode.string)

    "[[{\"first\":\"Jeve\",\"last\":\"Sobs\"},9001],[{\"first\":\"Tim\",\"last\":\"Berners-Lee\"},1234]]"
        |> Decode.decodeString (decodeList personToString (Decode.map2 Tuple.pair (Decode.index 0 personDecode) (Decode.index 1 Decode.int)))
        --> Ok (fromList personToString [(Person "Jeve" "Sobs", 9001), (Person "Tim" "Berners-Lee", 1234)])

-}
decodeList : (k -> comparable) -> Decode.Decoder ( k, v ) -> Decode.Decoder (AnyDict comparable k v)
decodeList keyToComparable =
    Decode.map (fromList keyToComparable) << Decode.list


{-| Takes a key-fn and a list.
Creates an `AnyDict` which maps the key to a list of matching elements.
-}
groupBy : (value -> key) -> (key -> comparable) -> List value -> AnyDict comparable key (List value)
groupBy toKey keyToComparable list =
    List.foldr
        (\x acc ->
            update
                (toKey x)
                (\maybeValues ->
                    maybeValues
                        |> Maybe.map ((::) x)
                        |> Maybe.withDefault [ x ]
                        |> Just
                )
                acc
        )
        (empty keyToComparable)
        list


{-| Find out if there is any instance of something in a Dictionary.

    type Animal = Cat | Mouse | Dog

    animalToInt : Animal -> Int
    animalToInt animal =
        case animal of
            Cat -> 0
            Mouse -> 1
            Dog -> 2

    animals : AnyDict Int Animal String
    animals =
        [ (Cat, "Tom"), (Mouse, "Jerry") ]
            |> fromList animalToInt

    isACat : Animal -> String -> Bool
    isACat animal _ =
        case animal of
            Cat -> True
            _ -> False

    any isACat animals
    --> True

-}
any : (k -> v -> Bool) -> AnyDict comparable k v -> Bool
any predicate dict =
    foldl
        (\k v acc -> acc || predicate k v)
        False
        dict


{-| Find out if all instances of a Dictionary match a predicate.

    type Animal = Cat | Mouse | Dog

    animalToInt : Animal -> Int
    animalToInt animal =
        case animal of
            Cat -> 0
            Mouse -> 1
            Dog -> 2

    animals : AnyDict Int Animal String
    animals =
        [ (Cat, "Tom"), (Mouse, "Jerry") ]
            |> fromList animalToInt

    aristocats : AnyDict Int Animal String
    aristocats =
        [ (Cat, "Marie"), (Cat, "Duchess"), (Cat, "Toulouse"), (Cat, "Berlioz") ]
            |> fromList animalToInt

    isACat : Animal -> String -> Bool
    isACat animal _ =
        case animal of
            Cat -> True
            _ -> False

    all isACat animals
    --> False

    all isACat aristocats
    --> True

-}
all : (k -> v -> Bool) -> AnyDict comparable k v -> Bool
all predicate dict =
    foldl
        (\k v acc -> acc && predicate k v)
        True
        dict


{-| Apply a function that may or may not succeed to all entries in a dictionary, but only keep the successes.

    type Animal = Cat | Mouse | Dog

    animalToInt : Animal -> Int
    animalToInt animal =
        case animal of
            Cat -> 0
            Mouse -> 1
            Dog -> 2

    animals : AnyDict Int Animal String
    animals =
        [ (Cat, "Tom"), (Mouse, "Jerry") ]
            |> fromList animalToInt

    onlyTom : AnyDict Int Animal String
    onlyTom =
        [ (Cat, "Tom") ]
            |> fromList animalToInt

    getCatName : Animal -> String -> Maybe String
    getCatName animal name =
        case animal of
            Cat -> Just name
            _ -> Nothing

    filterMap getCatName animals == onlyTom
    --> True

-}
filterMap : (k -> v1 -> Maybe v2) -> AnyDict comparable k v1 -> AnyDict comparable k v2
filterMap f dict =
    foldl
        (\k v acc ->
            case f k v of
                Just newVal ->
                    insert k newVal acc

                Nothing ->
                    acc
        )
        (removeAll dict)
        dict
