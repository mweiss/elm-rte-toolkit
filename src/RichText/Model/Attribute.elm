module RichText.Model.Attribute exposing
    ( Attribute(..)
    , findBoolAttribute, findFloatAttribute, findIntegerAttribute, findStringAttribute, replaceOrAddBoolAttribute, replaceOrAddFloatAttribute, replaceOrAddIntegerAttribute, replaceOrAddStringAttribute
    )

{-| This module contains basic methods for accessing and defining element and mark attributes.


# Attribute type

@docs Attribute


# Helpers

@docs findBoolAttribute, findFloatAttribute, findIntegerAttribute, findStringAttribute, replaceOrAddBoolAttribute, replaceOrAddFloatAttribute, replaceOrAddIntegerAttribute, replaceOrAddStringAttribute

-}

import String


{-| An attribute is a key value pair. It's used to store information on an element or mark.
Information you can store are things like color, font type, or image or link locations.

    StringAttribute "href" "www.google.com"

-}
type Attribute
    = StringAttribute String String
    | IntegerAttribute String Int
    | BoolAttribute String Bool
    | FloatAttribute String Float


{-| Find the first BoolAttribute that has the name provided and return Just that value. If no
attribute exists, return Nothing.

    findBoolAttribute "foo" [ BoolAttribute "foo" True, StringAttribute "foo2" "bar2" ]
    --> Just True

-}
findBoolAttribute : String -> List Attribute -> Maybe Bool
findBoolAttribute name attributes =
    case attributes of
        [] ->
            Nothing

        x :: xs ->
            case x of
                BoolAttribute k v ->
                    if k == name then
                        Just v

                    else
                        findBoolAttribute name xs

                _ ->
                    findBoolAttribute name xs


{-| Find the first FloatAttribute that has the name provided and return Just that value. If no
attribute exists, return Nothing.

    findFloatAttribute "foo" [ FloatAttribute "foo" 1.1, StringAttribute "foo2" "bar2" ]
    --> Just 1.1

-}
findFloatAttribute : String -> List Attribute -> Maybe Float
findFloatAttribute name attributes =
    case attributes of
        [] ->
            Nothing

        x :: xs ->
            case x of
                FloatAttribute k v ->
                    if k == name then
                        Just v

                    else
                        findFloatAttribute name xs

                _ ->
                    findFloatAttribute name xs


{-| Find the first StringAttribute that has the name provided and return Just that value. If no
attribute exists, return Nothing.

    findStringAttribute "foo2" [ FloatAttribute "foo" 1.1, StringAttribute "foo2" "bar2" ]
    --> Just "bar2"

-}
findStringAttribute : String -> List Attribute -> Maybe String
findStringAttribute name attributes =
    case attributes of
        [] ->
            Nothing

        x :: xs ->
            case x of
                StringAttribute k v ->
                    if k == name then
                        Just v

                    else
                        findStringAttribute name xs

                _ ->
                    findStringAttribute name xs


{-| Find the first IntegerAttribute that has the name provided and return Just that value. If no
attribute exists, return Nothing.

    findIntegerAttribute "foo" [ IntegerAttribute "foo" 1, StringAttribute "foo2" "bar2" ]
    --> Just 1

-}
findIntegerAttribute : String -> List Attribute -> Maybe Int
findIntegerAttribute name attributes =
    case attributes of
        [] ->
            Nothing

        x :: xs ->
            case x of
                IntegerAttribute k v ->
                    if k == name then
                        Just v

                    else
                        findIntegerAttribute name xs

                _ ->
                    findIntegerAttribute name xs


{-| Replaces all BoolAttributes that have the name provided in the list,
or add it to the beginning of the list if no pre existing BoolAttribute exists.

    replaceOrAddBoolAttribute "foo" True [ BoolAttribute "foo" False, StringAttribute "foo2" "bar2" ]
    --> [ BoolAttribute "foo" True, StringAttribute "foo2" "bar2" ]

-}
replaceOrAddBoolAttribute : String -> Bool -> List Attribute -> List Attribute
replaceOrAddBoolAttribute name value attributes =
    case findStringAttribute name attributes of
        Nothing ->
            BoolAttribute name value :: attributes

        Just _ ->
            List.map
                (\x ->
                    case x of
                        BoolAttribute k v ->
                            if k == name then
                                BoolAttribute name value

                            else
                                x

                        _ ->
                            x
                )
                attributes


{-| Replaces all StringAttributes that have the name provided in the list,
or add it to the beginning of the list if no pre existing StringAttribute exists.

    replaceOrAddStringAttribute "foo2" "bar3" [ BoolAttribute "foo" False, StringAttribute "foo2" "bar2" ]
    --> [ BoolAttribute "foo" False, StringAttribute "foo2" "bar3" ]

-}
replaceOrAddStringAttribute : String -> String -> List Attribute -> List Attribute
replaceOrAddStringAttribute name value attributes =
    case findStringAttribute name attributes of
        Nothing ->
            StringAttribute name value :: attributes

        Just _ ->
            List.map
                (\x ->
                    case x of
                        StringAttribute k v ->
                            if k == name then
                                StringAttribute name value

                            else
                                x

                        _ ->
                            x
                )
                attributes


{-| Replaces all IntegerAttributes that have the name provided in the list,
or add it to the beginning of the list if no pre existing IntegerAttribute exists.

    replaceOrAddIntegerAttribute "foo" 2 [ IntegerAttribute "foo" 1, StringAttribute "foo2" "bar2" ]
    --> [ IntegerAttribute "foo" 2, StringAttribute "foo2" "bar2" ]

-}
replaceOrAddIntegerAttribute : String -> Int -> List Attribute -> List Attribute
replaceOrAddIntegerAttribute name value attributes =
    case findStringAttribute name attributes of
        Nothing ->
            IntegerAttribute name value :: attributes

        Just _ ->
            List.map
                (\x ->
                    case x of
                        IntegerAttribute k v ->
                            if k == name then
                                IntegerAttribute name value

                            else
                                x

                        _ ->
                            x
                )
                attributes


{-| Replaces all FloatAttributes that have the name provided in the list,
or add it to the beginning of the list if no pre existing FloatAttribute exists.

    replaceOrAddFloatAttribute "foo" 2.2 [ FloatAttribute "foo" 1.1, StringAttribute "foo2" "bar2" ]
    --> [ FloatAttribute "foo" 2.2, StringAttribute "foo2" "bar2" ]

-}
replaceOrAddFloatAttribute : String -> Float -> List Attribute -> List Attribute
replaceOrAddFloatAttribute name value attributes =
    case findStringAttribute name attributes of
        Nothing ->
            FloatAttribute name value :: attributes

        Just _ ->
            List.map
                (\x ->
                    case x of
                        FloatAttribute k v ->
                            if k == name then
                                FloatAttribute name value

                            else
                                x

                        _ ->
                            x
                )
                attributes
