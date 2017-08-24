module Word.Bytes exposing (ByteCount, fromHex, fromInt, fromUTF8, toHex)

{-| Helper functions for creating lists of bytes.

    import Byte

@docs ByteCount, fromInt, fromUTF8, fromHex, toHex

-}

import Bitwise
import Byte exposing (Byte)
import Char


{-| Total number of bytes
-}
type alias ByteCount =
    Int


{-| Convert a character into a list of bytes

    fromUTF8 "a"
    --> [ Byte.fromInt 97 ]

    fromUTF8 "I ❤ cheese"
    --> [ 73, 32,
    -->   226, 157, 164,
    -->   32, 99, 104, 101, 101, 115, 101 ]
    --> |> List.map Byte.fromInt

    fromUTF8 "dѐf"
    --> [ 100, 209, 144, 102 ] |> List.map Byte.fromInt

-}
fromUTF8 : String -> List Byte
fromUTF8 =
    String.toList
        >> List.foldl
            (\char acc ->
                List.append acc (char |> Char.toCode |> splitUtf8)
            )
            []
        >> List.map Byte.fromInt


splitUtf8 : Int -> List Int
splitUtf8 x =
    if x < 128 then
        [ x ]
    else if x < 2048 then
        [ x |> Bitwise.and 0x07C0 |> Bitwise.shiftRightZfBy 6 |> Bitwise.or 0xC0
        , x |> Bitwise.and 0x3F |> Bitwise.or 0x80
        ]
    else
        [ x |> Bitwise.and 0xF000 |> Bitwise.shiftRightZfBy 12 |> Bitwise.or 0xE0
        , x |> Bitwise.and 0x0FC0 |> Bitwise.shiftRightZfBy 6 |> Bitwise.or 0x80
        , x |> Bitwise.and 0x3F |> Bitwise.or 0x80
        ]


{-| Split an integer value into a list of bytes with the given length.

    fromInt 4 0
    --> [ 0, 0, 0, 0 ] |> List.map Byte.fromInt

    fromInt 4 1
    --> [ 0, 0, 0, 1 ] |> List.map Byte.fromInt

    fromInt 2 2
    --> [ 0, 2 ] |> List.map Byte.fromInt

    fromInt 1 255
    --> [ 255 ] |> List.map Byte.fromInt

    fromInt 4 256
    --> [ 0, 0, 1, 0 ] |> List.map Byte.fromInt

    fromInt 4 65537
    --> [ 0, 1, 0, 1 ] |> List.map Byte.fromInt

    fromInt 4 16777216
    --> [ 1, 0, 0, 0 ] |> List.map Byte.fromInt

    fromInt 8 72058693549555970
    --> [ 1, 0, 1, 0, 0, 0, 1, 0 ] |> List.map Byte.fromInt

-}
fromInt : ByteCount -> Int -> List Byte
fromInt pad val =
    let
        n =
            if val > 1 then
                logBase 2 (toFloat val)
                    / 8
                    |> floor
            else
                0
    in
    List.range 0 n
        |> List.map
            (\b ->
                val
                    // (2 ^ (b * 8))
                    |> Byte.fromInt
            )
        |> List.reverse
        |> fixLength pad 0


fixLength : Int -> Int -> List Byte -> List Byte
fixLength byteCount val list =
    case compare (List.length list) byteCount of
        EQ ->
            list

        LT ->
            List.append
                (List.repeat (byteCount - List.length list) (Byte.fromInt val))
                list

        GT ->
            List.take byteCount list


{-| Convert a list of bytes to a string of hexadecimal characters.

    [ 0xDE, 0xAD, 0xBE, 0xEF ]
        |> List.map Byte.fromInt
        |> toHex
    --> "deadbeef"

-}
toHex : List Byte -> String
toHex =
    List.foldl
        (\byte acc ->
            List.append acc
                [ byte |> Byte.toInt |> Bitwise.shiftRightZfBy 4 |> Byte.fromInt |> charToHex
                , byte |> charToHex
                ]
        )
        []
        >> String.fromList


charToHex : Byte -> Char
charToHex byte =
    let
        x2 =
            byte |> Byte.and (Byte.fromInt 0x0F) |> Byte.toInt
    in
    (x2
        + (if x2 < 10 then
            Char.toCode '0'
           else
            -10 + Char.toCode 'a'
          )
    )
        |> Char.fromCode


{-| Attempt to interpret a string as a sequence of hexadecimal encoded bytes.

Fails for non-hex strings.

    fromHex "not hex"
    --> []

Each byte requires 2 characters, so odd length strings fail

    fromHex "000"
    --> []

Some passing examples

    fromHex "00"
    --> [ 0x00 ] |> List.map Byte.fromInt

    fromHex "010203040506DEADbeef"
    --> [ 0x01, 0x02, 0x03, 0x04
    --> , 0x05, 0x06, 0xDE, 0xAD
    --> , 0xBE, 0xEF
    --> ] |> List.map Byte.fromInt

-}
fromHex : String -> List Byte
fromHex string =
    fromHex_ (string |> String.toLower |> String.toList) []


fromHex_ : List Char -> List Byte -> List Byte
fromHex_ chars acc =
    case chars of
        h :: l :: rest ->
            byteFromHex h l
                |> (\byte -> List.append acc [ byte ])
                |> fromHex_ rest

        [] ->
            acc

        _ ->
            []


byteFromHex : Char -> Char -> Byte
byteFromHex hChar lChar =
    case ( hexFromChar hChar, hexFromChar lChar ) of
        ( h, l ) ->
            Byte.fromInt <| h * 2 ^ 4 + l


hexFromChar : Char -> Int
hexFromChar char =
    case char of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        'f' ->
            15

        _ ->
            0
