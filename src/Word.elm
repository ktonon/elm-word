module Word
    exposing
        ( Size(..)
        , Word(..)
        , add
        , and
        , complement
        , fromBytes
        , fromUTF8
        , rotateLeftBy
        , rotateRightBy
        , shiftRightZfBy
        , sizeInBytes
        , toBytes
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
    import Word.Hex as Hex


## Types

@docs Word, Size


## Constructors

@docs fromBytes,fromUTF8, zero


## Conversions

@docs toBytes


## Arithmetic

@docs add


## Bitwise

@docs and, xor, complement, rotateLeftBy, rotateRightBy, shiftRightZfBy


## Misc

@docs sizeInBytes

-}

import Array exposing (Array)
import Bitwise
import Word.Bytes as Bytes
import Word.Helpers
    exposing
        ( lowMask
        , rotatedLowBits
        , safeAnd
        , safeShiftLeftBy
        , safeShiftRightZfBy
        , safeXor
        )


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

    fromBytes Bit32 [ 0xDE, 0xAD, 0xBE, 0xEF ]
    --> [ W 0xDEADBEEF ] |> Array.fromList

    fromBytes Bit32
        [ 0xDE, 0xAD, 0xBE, 0xEF
        , 0x01, 0x23, 0x45, 0x67
        ]
    --> [ W 0xDEADBEEF, W 0x01234567 ] |> Array.fromList

    fromBytes Bit64
        [ 0xDE, 0xAD, 0xBE, 0xEF
        , 0x01, 0x23, 0x45, 0x67
        ]
    --> [ D 0xDEADBEEF 0x01234567 ] |> Array.fromList

-}
fromBytes : Size -> List Int -> Array Word
fromBytes wordSize bytes =
    accWords wordSize bytes Array.empty


accWords : Size -> List Int -> Array Word -> Array Word
accWords wordSize bytes acc =
    case ( wordSize, bytes ) of
        ( Bit32, x3 :: x2 :: x1 :: x0 :: rest ) ->
            acc
                |> Array.push
                    (W
                        (int32FromBytes ( x3, x2, x1, x0 ))
                    )
                |> accWords wordSize rest

        ( Bit64, x7 :: x6 :: x5 :: x4 :: x3 :: x2 :: x1 :: x0 :: rest ) ->
            acc
                |> Array.push
                    (D
                        (int32FromBytes ( x7, x6, x5, x4 ))
                        (int32FromBytes ( x3, x2, x1, x0 ))
                    )
                |> accWords wordSize rest

        ( _, [] ) ->
            acc

        ( Bit32, rest ) ->
            acc
                |> Array.push
                    (W (int32FromBytes (pad4 rest)))

        ( Bit64, rest ) ->
            acc
                |> Array.push
                    (D
                        (int32FromBytes (pad4 (List.take 4 rest)))
                        (int32FromBytes (pad4 (List.drop 4 rest)))
                    )


pad4 : List Int -> ( Int, Int, Int, Int )
pad4 bytes =
    case bytes of
        [ x3, x2, x1, x0 ] ->
            ( x3, x2, x1, x0 )

        [ x3, x2, x1 ] ->
            ( x3, x2, x1, 0 )

        [ x3, x2 ] ->
            ( x3, x2, 0, 0 )

        [ x3 ] ->
            ( x3, 0, 0, 0 )

        _ ->
            ( 0, 0, 0, 0 )


int32FromBytes : ( Int, Int, Int, Int ) -> Int
int32FromBytes ( x3, x2, x1, x0 ) =
    x0
        + (x1 * 2 ^ 8)
        + (x2 * 2 ^ 16)
        + (x3 * 2 ^ 24)


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


{-| Convert an array of words to a list of bytes.

    [ W 0 ] |> Array.fromList |> toBytes
    --> [ 0, 0, 0, 0 ]

    [ D 0 0 ] |> Array.fromList |> toBytes
    --> [ 0, 0, 0, 0, 0, 0, 0, 0 ]

    [ W 16843010 ] |> Array.fromList |> toBytes
    --> [ 1, 1, 1, 2 ]

-}
toBytes : Array Word -> List Int
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



-- OPERATIONS


{-| Modulo adds two words of the same type.

    add (W 0x80000000) (W 0x7FFFFFFF) |> Hex.fromWord
    --> "ffffffff"

    add (W 0x80000000) (W 0x80000003) |> Hex.fromWord
    --> "00000003"

    add (D 0 0xFFFFFFFF) (D 0 1) |> Hex.fromWord
    --> "0000000100000000"

    add (D 0xFFFFFFFF 0xFFFFFFFF) (D 0 2) |> Hex.fromWord
    --> "0000000000000001"

    add (W 0) (D 0 0)
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


{-| Rotate bits to the left by the given offset.

<https://en.wikipedia.org/wiki/Bitwise_operation#Rotate_no_carry>

    rotateLeftBy 4 (W 0xDEADBEEF) |> Hex.fromWord
    --> "eadbeefd"

    rotateLeftBy 4 (D 0xDDEEAADD 0xBBEEAAFF) |> Hex.fromWord
    --> "deeaaddbbeeaaffd"

    rotateLeftBy 7 Mismatch
    --> Mismatch

-}
rotateLeftBy : Int -> Word -> Word
rotateLeftBy n word =
    case word of
        W _ ->
            rotateRightBy (32 - n) word

        D _ _ ->
            rotateRightBy (64 - n) word

        _ ->
            Mismatch


{-| Rotate bits to the right by the given offset.

<https://en.wikipedia.org/wiki/Bitwise_operation#Rotate_no_carry>

    rotateRightBy 4 (W 0xDEADBEEF) |> Hex.fromWord
    --> "fdeadbee"

    rotateRightBy 4 (D 0xDDEEAADD 0xBBEEAAFF) |> Hex.fromWord
    --> "fddeeaaddbbeeaaf"

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


{-| Shift bits to the right by a given offset, filling new bits with zeros.

<https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift>

    shiftRightZfBy 9 (W 0xFFFF) |> Hex.fromWord
    --> "0000007f"

    shiftRightZfBy 32 (W 0xFFFF) |> Hex.fromWord
    --> "00000000"

    shiftRightZfBy 8 (D 0x01234567 0x89abcdef) |> Hex.fromWord
    --> "000123456789abcd"

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
        (W 0xFFFF0000) |> Hex.fromWord
    --> "ff000000"

    Word.and
        (D 0xFF00FF00 0xFFFF0000)
        (D 0xFFFF0000 0xFF00FF00) |> Hex.fromWord
    --> "ff000000ff000000"

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
        (W 0x00FFFF00) |> Hex.fromWord
    --> "ffff0000"

    Word.xor
        (D 0xFF00FF00 0x00FFFF00)
        (D 0x00FFFF00 0xFF00FF00) |> Hex.fromWord
    --> "ffff0000ffff0000"

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
        (W 0x00FF00FF) |> Hex.fromWord
    --> "ff00ff00"

    Word.complement
        (D 0x00FF00FF 0x00FF00FF) |> Hex.fromWord
    --> "ff00ff00ff00ff00"

-}
complement : Word -> Word
complement x =
    case x of
        W x ->
            W <| Bitwise.complement x

        D xh xl ->
            D
                (Bitwise.complement xh)
                (Bitwise.complement xl)

        _ ->
            Mismatch



-- HELPERS


rem32 : Int -> Int
rem32 val =
    val // (2 ^ 32)


mod32 : Int -> Int
mod32 val =
    val % (2 ^ 32)
