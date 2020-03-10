module RichTextEditor.Internal.Model.Attribute exposing
    ( Attribute
    , bool
    , findBoolAttribute
    , findFloatAttribute
    , findIntegerAttribute
    , findStringAttribute
    , float
    , int
    , string
    )

{-| An editor attribute is a key value pair. It's used to store information on a node or mark,
like color, font type, or image or link locations.
-}


type Attribute
    = StringAttribute String String
    | IntegerAttribute String Int
    | BoolAttribute String Bool
    | FloatAttribute String Float


string : String -> String -> Attribute
string k v =
    StringAttribute k v


int : String -> Int -> Attribute
int k v =
    IntegerAttribute k v


bool : String -> Bool -> Attribute
bool k v =
    BoolAttribute k v


float : String -> Float -> Attribute
float k v =
    FloatAttribute k v


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
