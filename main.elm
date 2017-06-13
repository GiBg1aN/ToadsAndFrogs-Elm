module ToadsAndFrogs exposing (..)

import Html exposing (Html, beginnerProgram, button, div, h1, h3, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { board : Board
    , status : Status
    }


type CellState
    = Empty
    | Toad
    | Frog


type Status
    = Playing
    | Win


type alias Position =
    Int


type alias Board =
    List CellState


model : Model
model =
    { board = [ Frog, Frog, Frog, Empty, Toad, Toad, Toad ], status = Playing }


end_state : Board
end_state =
    [ Toad, Toad, Toad, Empty, Frog, Frog, Frog ]


isPosFree : Board -> Position -> Bool
isPosFree b p =
    case ( b, p ) of
        ( [], _ ) ->
            False

        ( x :: xs, 0 ) ->
            x == Empty

        ( x :: xs, _ ) ->
            isPosFree xs (p - 1)


updateField : Position -> CellState -> Board -> Board
updateField p x b =
    case ( b, p ) of
        ( [], _ ) ->
            []

        ( y :: ys, 0 ) ->
            x :: ys

        ( y :: ys, _ ) ->
            y :: updateField (p - 1) x ys


getOp : CellState -> number -> number -> number
getOp o =
    case o of
        Toad ->
            (-)

        Frog ->
            (+)

        Empty ->
            Debug.log "Warning!: " (*)


move : Board -> Position -> Board
move field pos =
    if pos >= List.length field || pos < 0 then
        field
    else
        let
            elem =
                field |> getAt pos
        in
        let
            getNewField op x =
                if isPosFree field (op pos 1) then
                    field |> (updateField (op pos 1) x >> updateField pos Empty)
                else if isPosFree field (op pos 2) then
                    field |> (updateField (op pos 2) x >> updateField pos Empty)
                else
                    field
        in
        if elem == Empty then
            field
        else
            getNewField (getOp elem) elem


getAt : Int -> List a -> a
getAt p b =
    let
        t =
            b |> (List.drop p >> List.head)
    in
    case t of
        Just j ->
            j

        Nothing ->
            Debug.crash "getAt Error"



-- UPDATE


type Msg
    = Reset
    | Move Position


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            { model | board = [ Frog, Frog, Frog, Empty, Toad, Toad, Toad ], status = Playing }

        Move m ->
            let
                temp_board =
                    move model.board m
            in
            if temp_board == end_state then
                { model | board = temp_board, status = Win }
            else
                { model | board = temp_board, status = Playing }



-- VIEW


renderCell : Position -> Board -> Html Msg
renderCell p b =
    let
        c =
            getAt p b
    in
    case c of
        Empty ->
            div [ onClick (Move p), style [ ( "padding", "30px" ), ( "width", "50px" ), ( "height", "50px" ), ( "display", "flex" ) ] ] []

        Toad ->
            div [ onClick (Move p), class "cell", style [ ( "width", "50px" ), ( "height", "50px" ), ( "background-color", "orange" ), ( "display", "flex" ) ] ] []

        Frog ->
            div [ onClick (Move p), class "cell", style [ ( "width", "50px" ), ( "height", "50px" ), ( "background-color", "green" ), ( "display", "flex" ) ] ] []


view : Model -> Html Msg
view model =
    let
        cells =
            List.map (\x -> renderCell x model.board) (List.range 0 6)

        board =
            if model.status /= Win then
                div [ style [ ( "display", "flex" ), ( "padding", "25px" ) ] ] cells
            else
                div [ style [ ( "display", "flex" ), ( "padding", "20px" ) ] ] [ h1 [] [ text "You Won!" ] ]

        reset =
            div [] [ button [ onClick Reset, class "btn" ] [ text "Reset" ] ]
    in
    div [] [ div [] [ h3 [ style [ ( "padding-bottom", "5px" ) ] ] [] ], board, reset ]
