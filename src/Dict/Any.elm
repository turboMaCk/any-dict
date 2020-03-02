module Dict.Any exposing
    ( AnyDict
    , empty, singleton, insert, update, remove, removeAll
    , isEmpty, member, get, getKey, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , toDict
    , decode, encode
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

@docs AnyDict


# Build

@docs empty, singleton, insert, update, remove, removeAll


# Query

@docs isEmpty, member, get, getKey, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Dict

@docs toDict


# Json

@docs decode, encode

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode


{-| Be aware that AnyDict stores a function internally.

If you want to use `(==)` for comparing two AnyDicts
use [equal](#qual) function.

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
-}
decode : (String -> v -> k) -> (k -> comparable) -> Decode.Decoder v -> Decode.Decoder (AnyDict comparable k v)
decode fromStr toComparable valueD =
    let
        construct strK v acc =
            insert (fromStr strK v) v acc
    in
    Decode.dict valueD
        |> Decode.map (Dict.foldr construct (empty toComparable))


{-| Turn an `AnyDict` into a JSON object.
-}
encode : (k -> String) -> (v -> Encode.Value) -> AnyDict comparable k v -> Encode.Value
encode keyE valueE =
    Encode.object
        << List.map (Tuple.mapBoth keyE valueE)
        << toList
