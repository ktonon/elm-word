module Word.Helpers exposing (..)

{-| Helpers functions.

    import Array
    import Byte

-}

import Array exposing (Array)
import Bitwise


{-| A bitmask for the lower `n` bits.

    lowMask 0
    --> 0x00000000

    lowMask 1
    --> 0x00000001

    lowMask 2
    --> 0x00000003

    lowMask 4
    --> 0x0000000F

    lowMask 16
    --> 0x0000FFFF

-}
lowMask : Int -> Int
lowMask n =
    List.range 0 (n - 1)
        |> List.foldl (\i acc -> acc + 2 ^ i) 0


{-| Rotate the low `n` bits of a 32 bit value and adds as high bits to another.

    rotatedLowBits 2 0xFFFFFFFF 0xBEEF
    --> 0xC000BEEF

    rotatedLowBits 2 0x03 0xBEEF
    --> 0xC000BEEF

    rotatedLowBits 28 0xFFFFFFFF 7
    --> 0xFFFFFFF7

-}
rotatedLowBits : Int -> Int -> Int -> Int
rotatedLowBits n val =
    (+)
        (val
            |> Bitwise.and (lowMask n)
            |> safeShiftLeftBy (32 - n)
        )


safeShiftRightZfBy : Int -> Int -> Int
safeShiftRightZfBy n val =
    if n > 31 then
        0
    else
        Bitwise.shiftRightZfBy n val


{-| Unsigned shift left by `n`.

    safeShiftLeftBy 1 0x7FFFFFFF
    --> 0xFFFFFFFE

    safeShiftLeftBy 1 0x3FFFFFFF
    --> 0x7FFFFFFE

    safeShiftLeftBy 2 0x3FFFFFFF
    --> 0xFFFFFFFC

    safeShiftLeftBy 31 0xFFFFFFFF
    --> 0x80000000

-}
safeShiftLeftBy : Int -> Int -> Int
safeShiftLeftBy n val =
    (val * 2 ^ n) % 2 ^ 32


safeAnd : Int -> Int -> Int
safeAnd =
    safeBitwise Bitwise.and


safeXor : Int -> Int -> Int
safeXor =
    safeBitwise Bitwise.xor


safeBitwise : (Int -> Int -> Int) -> Int -> Int -> Int
safeBitwise op x y =
    let
        h =
            2 ^ 16

        l =
            0xFFFF
    in
    (+)
        (op (x // h) (y // h) |> (*) h)
        (op
            (Bitwise.and x l)
            (Bitwise.and y l)
        )


safeComplement : Int -> Int
safeComplement x =
    let
        h =
            2 ^ 16

        l =
            0xFFFF
    in
    (+)
        (Bitwise.complement (x // h) |> Bitwise.and l |> (*) h)
        (Bitwise.complement (Bitwise.and x l) |> Bitwise.and l)


{-| Maximum unsigned 32-bit integer
-}
max32 : Int
max32 =
    0xFFFFFFFF


{-| Split list into smaller lists of length `k`, starting from the left.

    chunkedMap identity 3 0 <| List.range 1 9
    --> [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ] |> Array.fromList

    chunkedMap identity 3 0 [ 1, 2, 3, 4, 5, 6, 7, 8 ]
    --> [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 0 ] ] |> Array.fromList

    chunkedMap identity 3 0 []
    --> Array.empty

    chunkedMap identity 0 0 [ 1, 2, 3 ]
    --> Array.empty

    chunkedMap identity -1 0 []
    --> Array.empty

-}
chunkedMap :
    (List a -> b)
    -> Int
    -> a
    -> List a
    -> Array b
chunkedMap mapper k default list =
    chunkedMap_ mapper k default list Array.empty


chunkedMap_ :
    (List a -> b)
    -> Int
    -> a
    -> List a
    -> Array b
    -> Array b
chunkedMap_ mapper k default list acc =
    if k == 0 then
        Array.empty
    else if k < 0 then
        Array.empty
    else
        case nextChunkPart k default (k - 1) [] list of
            ( Nothing, _ ) ->
                acc

            ( Just chunk, [] ) ->
                Array.push (mapper chunk) acc

            ( Just chunk, rest ) ->
                chunkedMap_ mapper
                    k
                    default
                    rest
                    (Array.push (mapper chunk) acc)


nextChunkPart : Int -> a -> Int -> List a -> List a -> ( Maybe (List a), List a )
nextChunkPart k default i acc list =
    if i >= 0 then
        case list of
            x :: rest ->
                nextChunkPart
                    k
                    default
                    (i - 1)
                    (List.append acc [ x ])
                    rest

            _ ->
                if List.isEmpty acc then
                    ( Nothing, [] )
                else
                    ( Just <|
                        List.append acc
                            (List.repeat (k - List.length acc) default)
                    , []
                    )
    else
        ( Just acc, list )
