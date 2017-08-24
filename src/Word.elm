module Word
    exposing
        ( Size(..)
        , Word(..)
        , add
        , and
        , complement
        , fromBytes
        , fromUTF8
        , rotateRightBy
        , shiftRightZfBy
        , sizeInBytes
        , toBytes
        , toHex
        , xor
        , zero
        )

{-| Unsigned 32 or 64 bit integers and related operations.


## Table of Contents

  - [Types](#types)
  - [Constructors](#constructors)
  - [Conversions](#conversions)
  - [Arithmetic](#arithmetic)
  - [Bitwise](#bitwise)
  - [Misc](#misc)

This package was developed to facilitate computations for [SHA-2](https://en.wikipedia.org/wiki/SHA-2).
It contains the minimal set of functions required by those algorithms.

Examples below assume the following imports:

    import Array
    import Byte exposing (Byte)


## Types

@docs Word, Size


## Constructors

@docs fromBytes, fromUTF8, zero


## Conversions

@docs toBytes, toHex


## Arithmetic

@docs add


## Bitwise

@docs and, xor, complement, rotateRightBy, shiftRightZfBy


## Misc

@docs sizeInBytes

-}

import Array exposing (Array)
import Bitwise
import Byte exposing (Byte)
import Word.Bytes as Bytes
import Word.Helpers exposing (..)


{-| Unsigned integers of size 32 or 64 bits.

  - `W` a "word" of size 32 bits
  - `D` a "double word" of size 64 bits
  - `Mismatch` value of any operation that mixes word types

These constructors are provided as a convenience, but are unsafe.
Use `fromBytes` or `fromUTF8` to safely create arrays of words of the same type.

-}
type Word
    = W Int
    | D Int Int
    | Mismatch


{-| Size of a word.
-}
type Size
    = Bit32
    | Bit64


{-| Convert the given word size to a byte count.
-}
sizeInBytes : Size -> Int
sizeInBytes s =
    case s of
        Bit32 ->
            4

        Bit64 ->
            8


{-| The integer zero as a word of the given size.
-}
zero : Size -> Word
zero wordSize =
    case wordSize of
        Bit32 ->
            W 0

        Bit64 ->
            D 0 0



-- CONVERSIONS


{-| Convert a list of bytes to an array of words of the given size.

    fromBytes Bit32 ([ 0xDE, 0xAD, 0xBE, 0xEF ] |> List.map Byte.fromInt)
    --> [ W 0xDEADBEEF ] |> Array.fromList

    fromBytes Bit32 (
        [ 0xDE, 0xAD, 0xBE, 0xEF
        , 0x01, 0x23, 0x45, 0x67
        ] |> List.map Byte.fromInt)
    --> [ W 0xDEADBEEF, W 0x01234567 ] |> Array.fromList

    fromBytes Bit64 (
        [ 0xDE, 0xAD, 0xBE, 0xEF
        , 0x01, 0x23, 0x45, 0x67
        ] |> List.map Byte.fromInt)
    --> [ D 0xDEADBEEF 0x01234567 ] |> Array.fromList

-}
fromBytes : Size -> List Byte -> Array Word
fromBytes wordSize bytes =
    chunkedMap
        wordFromBytes
        (sizeInBytes wordSize)
        (Byte.fromInt 0)
        bytes


{-| Convert a UTF8 string to an array of words of the given size.

    fromUTF8 Bit32 "I ❤ UTF strings!" |> Array.toList
    --> [ W 0x4920E29D  -- 'I', ' ', 226, 157
    --> , W 0xA4205554  -- 164, ' ', 'U', 'T'
    --> , W 0x46207374  -- 'F', ' ', 's', 't'
    --> , W 0x72696E67  -- 'r', 'i', 'n', 'g'
    --> , W 0x73210000  -- 's', '!'
    --> ]

    fromUTF8 Bit64 "I ❤ UTF strings!" |> Array.toList
    --> [ D 0x4920E29D 0xA4205554 -- 'I', ' ', 226, 157, 164, ' ', 'U', 'T'
    --> , D 0x46207374 0x72696E67 -- 'F', ' ', 's', 't', 'r', 'i', 'n', 'g'
    --> , D 0x73210000 0x00000000 -- 's', '!'
    --> ]

-}
fromUTF8 : Size -> String -> Array Word
fromUTF8 wordSize =
    Bytes.fromUTF8 >> fromBytes wordSize


wordFromBytes : List Byte -> Word
wordFromBytes bytes =
    case bytes of
        [ a, b, c, d ] ->
            W
                (singleFromFourple ( a, b, c, d ))

        [ a, b, c, d, f, g, h, i ] ->
            D
                (singleFromFourple ( a, b, c, d ))
                (singleFromFourple ( f, g, h, i ))

        _ ->
            Mismatch


singleFromFourple : ( Byte, Byte, Byte, Byte ) -> Int
singleFromFourple ( a, b, c, d ) =
    Byte.toInt d
        |> (+) (2 ^ 8 * Byte.toInt c)
        |> (+) (2 ^ 16 * Byte.toInt b)
        |> (+) (2 ^ 24 * Byte.toInt a)


{-| Convert an array of words to a list of bytes.

    [ W 0 ] |> Array.fromList |> toBytes
    --> [ 0, 0, 0, 0 ] |> List.map Byte.fromInt

    [ D 0 0 ] |> Array.fromList |> toBytes
    --> [ 0, 0, 0, 0, 0, 0, 0, 0 ] |> List.map Byte.fromInt

    [ W 16843010 ] |> Array.fromList |> toBytes
    --> [ 1, 1, 1, 2 ] |> List.map Byte.fromInt

-}
toBytes : Array Word -> List Byte
toBytes =
    Array.toList
        >> List.concatMap
            (\word ->
                case word of
                    W x ->
                        Bytes.fromInt 4 x

                    D xh xl ->
                        List.append
                            (Bytes.fromInt 4 xh)
                            (Bytes.fromInt 4 xl)

                    _ ->
                        []
            )


{-| Convert a list of words to a string of hexadecimal characters.

    [ D 0xDEADBEEF 0x00112233, D 0x44556677 0x8899AABB ]
        |> Array.fromList
        |> toHex
    --> "deadbeef00112233445566778899aabb"

-}
toHex : Array Word -> String
toHex =
    toBytes >> Bytes.toHex



-- OPERATIONS


{-| Modulo adds two words of the same type.

    add (W 0x80000000)
        (W 0x7FFFFFFF)
    --> W 0xFFFFFFFF

    add (W 0x80000000)
        (W 0x80000003)
    --> W 3

    add (D 0 0xFFFFFFFF)
        (D 0 1)
    --> D 1 0

    add (D 0xFFFFFFFF 0xFFFFFFFF)
        (D 0 2)
    --> D 0 1

    add (W 0)
        (D 0 0)
    --> Mismatch

-}
add : Word -> Word -> Word
add x y =
    case ( x, y ) of
        ( W x, W y ) ->
            W <|
                mod32 (x + y)

        ( D xh xl, D yh yl ) ->
            let
                zl =
                    xl + yl

                zh =
                    xh + yh + rem32 zl
            in
            D
                (mod32 zh)
                (mod32 zl)

        _ ->
            Mismatch


{-| Rotate bits to the right by the given offset.

<https://en.wikipedia.org/wiki/Bitwise_operation#Rotate_no_carry>

    rotateRightBy 4 (W 0xDEADBEEF)
    --> W 0xFDEADBEE

    rotateRightBy 4 (D 0xDDEEAADD 0xBBEEAAFF)
    --> D 0xFDDEEAAD 0xDBBEEAAF

    rotateRightBy 7 Mismatch
    --> Mismatch

-}
rotateRightBy : Int -> Word -> Word
rotateRightBy unboundN word =
    case word of
        W x ->
            let
                n =
                    unboundN % 32
            in
            x
                |> safeShiftRightZfBy n
                |> rotatedLowBits n x
                |> W

        D xh xl ->
            let
                n =
                    unboundN % 64
            in
            if n > 32 then
                let
                    n_ =
                        n - 32

                    ( zh, zl ) =
                        dShiftRightZfBy n_ ( xl, xh )
                in
                D (zh |> rotatedLowBits n_ xh) zl
            else
                let
                    ( zh, zl ) =
                        dShiftRightZfBy n ( xh, xl )
                in
                D (zh |> rotatedLowBits n xl) zl

        _ ->
            Mismatch


{-| Shift bits to the right by a given offset, filling new bits with zeros.

<https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift>

    shiftRightZfBy 9 (W 0xFFFF)
    --> W 0x7F

    shiftRightZfBy 142 (W 0xFFFF)
    --> W 0

    shiftRightZfBy 8 (D 0x01234567 0x89abcdef)
    --> D 0x00012345 0x6789abcd

    shiftRightZfBy 4 Mismatch
    --> Mismatch

-}
shiftRightZfBy : Int -> Word -> Word
shiftRightZfBy n word =
    case word of
        W x ->
            W <|
                safeShiftRightZfBy n x

        D xh xl ->
            let
                ( zh, zl ) =
                    dShiftRightZfBy n ( xh, xl )
            in
            D zh zl

        _ ->
            Mismatch


{-| Bitwise and.

    Word.and
        (W 0xFF00FF00)
        (W 0xFFFF0000)
    -->  W 0xFF000000

    Word.and
        (D 0xFF00FF00 0xFFFF0000)
        (D 0xFFFF0000 0xFF00FF00)
    -->  D 0xFF000000 0xFF000000

-}
and : Word -> Word -> Word
and x y =
    case ( x, y ) of
        ( W x, W y ) ->
            W <| safeAnd x y

        ( D xh xl, D yh yl ) ->
            D
                (safeAnd xh yh)
                (safeAnd xl yl)

        _ ->
            Mismatch


{-| Bitwise xor.

    Word.xor
        (W 0xFF00FF00)
        (W 0x00FFFF00)
    -->  W 0xFFFF0000

    Word.xor
        (D 0xFF00FF00 0x00FFFF00)
        (D 0x00FFFF00 0xFF00FF00)
    -->  D 0xFFFF0000 0xFFFF0000

-}
xor : Word -> Word -> Word
xor x y =
    case ( x, y ) of
        ( W x, W y ) ->
            W <| safeXor x y

        ( D xh xl, D yh yl ) ->
            D
                (safeXor xh yh)
                (safeXor xl yl)

        _ ->
            Mismatch


{-| Bitwise complement.

    Word.complement
        (W 0x00FF00FF)
    -->  W 0xFF00FF00

    Word.complement
        (D 0x00FF00FF 0x00FF00FF)
    -->  D 0xFF00FF00 0xFF00FF00

-}
complement : Word -> Word
complement x =
    case x of
        W x ->
            W <| safeComplement x

        D xh xl ->
            D
                (safeComplement xh)
                (safeComplement xl)

        _ ->
            Mismatch



-- HELPERS


dShiftRightZfBy : Int -> ( Int, Int ) -> ( Int, Int )
dShiftRightZfBy n ( xh, xl ) =
    if n > 32 then
        ( 0
        , safeShiftRightZfBy (n - 32) xh
        )
    else
        ( safeShiftRightZfBy n xh
        , (+)
            (safeShiftRightZfBy n xl)
            (xh
                |> Bitwise.and (lowMask n)
                |> safeShiftLeftBy (32 - n)
            )
        )


rem32 : Int -> Int
rem32 val =
    val // (2 ^ 32)


mod32 : Int -> Int
mod32 val =
    val % (2 ^ 32)
