module RichTextEditor.Annotation exposing
    ( add
    , addAnnotationAtPath
    , clearAnnotations
    , findPathsWithAnnotation
    , getAnnotationsFromNode
    , remove
    , removeAnnotationAtPath
    , toggle
    , toggleElementParameters
    )

import RichTextEditor.Model.Element as Element exposing (Element)
import RichTextEditor.Model.InlineElement as InlineElement
import RichTextEditor.Model.Node
    exposing
        ( Block
        , Inline(..)
        , Path
        , elementFromBlockNode
        , withElement
        )
import RichTextEditor.Model.Text as Text
import RichTextEditor.Node exposing (Node(..), indexedFoldl, map, nodeAt, replace)
import Set exposing (Set)


addAnnotationAtPath : String -> Path -> Block -> Result String Block
addAnnotationAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (add annotation n) node


removeAnnotationAtPath : String -> Path -> Block -> Result String Block
removeAnnotationAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (remove annotation n) node


remove : String -> Node -> Node
remove =
    toggle Set.remove


add : String -> Node -> Node
add =
    toggle Set.insert


toggleElementParameters : (String -> Set String -> Set String) -> String -> Element -> Element
toggleElementParameters func annotation parameters =
    let
        annotations =
            Element.annotations parameters
    in
    Element.withAnnotations (func annotation annotations) parameters


toggle : (String -> Set String -> Set String) -> String -> Node -> Node
toggle func annotation node =
    case node of
        Block bn ->
            let
                newParameters =
                    toggleElementParameters func annotation (elementFromBlockNode bn)

                newBlockNode =
                    bn |> withElement newParameters
            in
            Block newBlockNode

        Inline il ->
            Inline <|
                case il of
                    InlineElement l ->
                        let
                            newParameters =
                                toggleElementParameters func annotation (InlineElement.element l)
                        in
                        InlineElement <| InlineElement.withElement newParameters l

                    Text tl ->
                        Text <| (tl |> Text.withAnnotations (func annotation <| Text.annotations tl))


clearAnnotations : String -> Block -> Block
clearAnnotations annotation root =
    case map (remove annotation) (Block root) of
        Block bn ->
            bn

        _ ->
            root


getAnnotationsFromNode : Node -> Set String
getAnnotationsFromNode node =
    case node of
        Block blockNode ->
            Element.annotations <| elementFromBlockNode blockNode

        Inline inlineLeaf ->
            case inlineLeaf of
                InlineElement p ->
                    Element.annotations <| InlineElement.element p

                Text p ->
                    Text.annotations p


findPathsWithAnnotation : String -> Block -> List Path
findPathsWithAnnotation annotation node =
    indexedFoldl
        (\path n agg ->
            if Set.member annotation <| getAnnotationsFromNode n then
                path :: agg

            else
                agg
        )
        []
        (Block node)
