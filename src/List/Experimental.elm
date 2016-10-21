module List.Experimental
    exposing
        ( and
        , or
        , conjunction
        , disjunction
        , lookup
        , filter2
        , filterMap2
        , filterM
        , at
        , removeAt
        , removeFirst
        , removeFirstBy
        , removeFirsts
        , removeFirstsBy
        , difference
        , union
        , intersect
        , insert
        , differenceBy
        , unionBy
        , intersectBy
        , insertBy
        , iterate
        )

{-| List.Experimental is a testing playground for various List related functions. It contains functions that are experimental, unidiomatic, controversial or downright silly. This is specifically to not clutter List and List.Extra, and also have an isolated place to test crazy ideas.

*Do not* use this module in production code. Try your best to come up with equivalent functionality or solve your problem in a different way, and if you fail, consider contributing to List and List.Extra packages.

*Do not* import functions from this module unqualified if you do use it.

This package has the lowest possible bar for inclusion of List related functions. If you have some code that you want to publish somewhere, but not necessarily contribute to core libraries, feel absolutely free to contribute here. Treat this package as a safe sandbox. The GitHub page for ideas, suggestions, discussions, and pull requests is:

https://github.com/sindikat/elm-list-experimental

# List functions
@docs and, or, conjunction, disjunction, lookup, filter2, filterMap2, filterM

# Elements
@docs at

# Removal
@docs removeAt, removeFirst, removeFirstBy, removeFirsts, removeFirstsBy

# Set functions
@docs difference, union, intersect, insert, differenceBy, unionBy, intersectBy, insertBy

# Misc
@docs iterate
-}

import List exposing (..)
import List.Extra exposing (..)


{-| Return the conjunction of all `Bool`s in a list. In other words, return True if all elements are True, return False otherwise. `and` is equivalent to `all identity`. Return True on an empty list.
-}
and : List Bool -> Bool
and =
    all identity


{-| Return the disjunction of all `Bool`s in a list. In other words, return True if any element is True, return False otherwise. `or` is equivalent to `any identity`. Return False on an empty list.
-}
or : List Bool -> Bool
or =
    any identity


{-| Same as `and`.
-}
conjunction : List Bool -> Bool
conjunction =
    and


{-| Same as `or`.
-}
disjunction : List Bool -> Bool
disjunction =
    or


{-| Look up a key in an association list, return corresponding value, wrapped in `Just`. If no value is found, return `Nothing`. If multiple values correspond to the same key, return the first found value.

    lookup 'a' [('a',1),('b',2),('c',3)] == Just 1
    lookup 'd' [('a',1),('b',2),('c',3)] == Nothing
    lookup 3 [(1,"John"),(1,"Paul"),(2,"Mary")] == Just "John"
-}
lookup : a -> List ( a, b ) -> Maybe b
lookup key =
    Maybe.map snd << find (\item -> fst item == key)


{-| Filter over two lists simultaneously using a custom comparison function, return a list of pairs.
-}
filter2 : (a -> b -> Bool) -> List a -> List b -> List ( a, b )
filter2 p xs ys =
    filter (uncurry p) (map2 (,) xs ys)


{-| Apply a function, which may succeed, to all values of two lists, but only keep the successes.
-}
filterMap2 : (a -> b -> Maybe c) -> List a -> List b -> List c
filterMap2 f xs ys =
    List.filterMap identity <| map2 f xs ys


{-| Filter that exploits the behavior of `andThen`.

Return all subsequences of a list:

    filterM (\x -> [True, False]) [1,2,3] == [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

Return all subsequences that contain 2:

    filterM (\x -> if x==2 then [True] else [True,False]) [1,2,3] == [[1,2,3],[1,2],[2,3],[2]]

-}
filterM : (a -> List Bool) -> List a -> List (List a)
filterM p =
    let
        andThen =
            flip concatMap

        go x r =
            p x
                `andThen`
                    (\flg ->
                        r
                            `andThen`
                                (\ys ->
                                    [ if flg then
                                        x :: ys
                                      else
                                        ys
                                    ]
                                )
                    )
    in
        foldr go [ [] ]



-- come up with a better name?


{-| Extract *n*th element of a list, index counts from 0.

    at 1 [1,2,3] == Just 2
    at 3 [1,2,3] == Nothing
    at -1 [1,2,3] == Nothing
-}
at : Int -> List a -> Maybe a
at n xs =
    if n >= 0 then
        head <| drop n xs
    else
        Nothing


{-| Remove *n*th element of a list. If index is negative or out of bounds of the list, return the list unchanged.

    removeAt 0 [1,2,3] == [2,3]
    removeAt 3 [1,2,3] == [1,2,3]
    removeAt -1 [1,2,3] == [1,2,3]
-}
removeAt : Int -> List a -> List a
removeAt n xs =
    take n xs ++ drop (n + 1) xs


{-| Remove first occurrence of element from list.

    removeFirst 2 [1,2,1,2] == [1,1,2]
-}
removeFirst : a -> List a -> List a
removeFirst =
    removeFirstBy (==)


{-| Remove first occurence of each element from list.

    removeFirsts [1,2] [1,2,3,1,2,3] == [3,1,2,3]
-}
removeFirsts : List a -> List a -> List a
removeFirsts =
    removeFirstsBy (==)


{-| Remove last occurrence of element from list.

    removeLast 2 [1,2,1,2] == [1,2,1]
-}
removeLast : a -> List a -> List a
removeLast =
    removeLastBy (==)


{-| Generic version of removeFirst with a custom predicate. For example, remove first element that is greater than 2:

    removeFirstBy (>) 2 [1..4] == [1,2,4]
-}
removeFirstBy : (a -> a -> Bool) -> a -> List a -> List a
removeFirstBy eq x xs =
    let
        ( ys, zs ) =
            break (flip eq x) xs
    in
        ys ++ drop 1 zs


{-| Generic version of removeFirsts with a custom predicate. For example, remove first element that evenly divides by 2, then remove first element that evenly divides by 3:

    removeFirstsBy (\a b -> a `rem` b == 0) [2,3] [5..10] == [5,7,8,10]
-}
removeFirstsBy : (a -> a -> Bool) -> List a -> List a -> List a
removeFirstsBy eq =
    flip <| foldl (removeFirstBy eq)


{-| Generic version of removeLast with a custom predicate.
-}
removeLastBy : (a -> a -> Bool) -> a -> List a -> List a
removeLastBy eq x =
    List.reverse << removeFirstBy eq x << List.reverse


{-| Set difference between lists.

    [1,2,3] `difference` [3,4,5] == [1,2]
-}
difference : List a -> List a -> List a
difference =
    differenceBy (==)


{-| Union between lists.

   [3,2,1] `union` [3,4,5] == [3,2,1,4,5]
-}
union : List a -> List a -> List a
union =
    unionBy (==)


{-| Intersection between lists.

   [1,2,3] `intersect` [3,4,5] == [3]
-}
intersect : List a -> List a -> List a
intersect =
    intersectBy (==)


{-| Take an element and a list and insert the element into the list at the first position where it is less than or equal to the next element. If the list is sorted before the call, the result will also be sorted.

   insert 4 [1,3,5] == [1,3,4,5]
-}
insert : comparable -> List comparable -> List comparable
insert =
    insertBy compare


{-| Generic version of difference with a custom predicate.
-}
differenceBy : (a -> a -> Bool) -> List a -> List a -> List a
differenceBy eq xs ys =
    filter (\x -> not <| any (eq x) ys) xs


{-| Generic version of union with a custom predicate.
-}
unionBy : (a -> a -> Bool) -> List a -> List a -> List a
unionBy eq xs ys =
    xs ++ filter (\y -> not <| any (eq y) xs) ys


{-| Generic version of intersect with a custom predicate.
-}
intersectBy : (a -> a -> Bool) -> List a -> List a -> List a
intersectBy eq xs ys =
    filter (\y -> any (eq y) xs) ys



-- insertWith?


{-| Generic version of insert with a custom predicate.
-}
insertBy : (a -> a -> Order) -> a -> List a -> List a
insertBy cmp e xs =
    let
        ( ys, zs ) =
            break (\x -> cmp e x == GT) xs
    in
        ys ++ [ e ] ++ zs


{-| Return a list of repeated applications of `f` to `x`.
-}
iterate : Int -> (a -> a) -> a -> List a
iterate n f x =
    let
        step ( n, x' ) =
            if n == 0 then
                Nothing
            else
                Just ( x', ( n - 1, f x' ) )
    in
        unfoldr step ( n, x )



-- move to extra?


{-| -}
unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 pairs =
    let
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
        foldr step ( [], [], [] ) pairs


{-| -}
unzip4 : List ( a, b, c, d ) -> ( List a, List b, List c, List d )
unzip4 pairs =
    let
        step ( w, x, y, z ) ( ws, xs, ys, zs ) =
            ( w :: ws, x :: xs, y :: ys, z :: zs )
    in
        foldr step ( [], [], [], [] ) pairs


{-| -}
unzip5 : List ( a, b, c, d, e ) -> ( List a, List b, List c, List d, List e )
unzip5 pairs =
    let
        step ( v, w, x, y, z ) ( vs, ws, xs, ys, zs ) =
            ( v :: vs, w :: ws, x :: xs, y :: ys, z :: zs )
    in
        foldr step ( [], [], [], [], [] ) pairs


mapWhen : (a -> Bool) -> (a -> a) -> List a -> List a
mapWhen p f xs =
    map
        (\x ->
            if p x then
                f x
            else
                x
        )
        xs



-- mapFirst : (a -> Bool) -> (a -> a) -> List a -> List a
-- mapFirst p f xs =
--   let
--     (ys, zs) = span p xs
--   in
-- mapLast : (a -> Bool) -> (a -> a) -> List a -> List a
-- mapLast


annonate : (a -> b) -> List a -> List ( b, a )
annonate f xs =
    map (\x -> ( f x, x )) xs


splice : (a -> Bool) -> (a -> List a) -> List a -> List a
splice p f xs =
    concatMap
        (\x ->
            if p x then
                f x
            else
                [ x ]
        )
        xs


spliceList : (a -> Bool) -> List a -> List a -> List a
spliceList p ys xs =
    splice p (always ys) xs


selectByIndices : List Int -> List a -> List a
selectByIndices ns xs =
    filterMap (\n -> at n xs) ns
