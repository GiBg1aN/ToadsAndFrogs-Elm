module ToadsAndFrogs exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

main : Program Never Model Msg
main = Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model = {
    board : Board,
    status : Status
}

type CellState = Empty | Toad | Frog
type Status = Playing | Win

type alias Position = Int
type alias Board = List CellState


model : Model
model = { board = [ Frog,Frog,Frog,Empty,Toad,Toad,Toad ], status = Playing }

end_state : Board
end_state = [ Toad,Toad,Toad,Empty,Frog,Frog,Frog ]


isPosFree : Board -> Position -> Bool
isPosFree l i =
    case (l, i) of
        ([], _) -> False
        (x :: xs, 0) -> (x == Empty)
        (x :: xs, _) -> isPosFree xs (i - 1)

updateField : Position -> CellState -> Board -> Board
updateField i x l =
    case (l, i) of
        ([], _) -> []
        (y :: ys, 0) -> x :: ys
        (y :: ys, _) -> y :: (updateField (i - 1) x ys)

getOp : CellState -> number -> number -> number
getOp o =
    case o of
        Toad -> (-)
        Frog -> (+)
        Empty -> Debug.log "ATTENZIONE: "(*)

move : Board -> Position -> Board
move field pos =
    if pos >= List.length field || pos < 0 then field
    else
        let elem = field |> getAt pos in

        let getNewField op x = 
            if isPosFree field (op pos 1) then 
                field |> (updateField (op pos 1) x >> updateField pos Empty)
            else if isPosFree field (op pos 2) then 
                field |> (updateField (op pos 2) x >> updateField pos Empty)
            else field
        in 
        if elem == Empty then field
        else getNewField (getOp elem) elem

getAt : Int -> List a -> a
getAt i l =
    let t = (l |> (List.drop i >> List.head)) in
    case t of
        Just j -> j
        Nothing -> Debug.crash "Head Error"

-- UPDATE

type Msg = Reset | Move Position

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset ->
        { model | board = [ Frog,Frog,Frog,Empty,Toad,Toad,Toad ], status = Playing }
    Move m ->
        let temp_board = move model.board m in
        if temp_board == end_state then { model | board = temp_board, status = Win }
        else { model | board = temp_board, status = Playing }


-- VIEW

renderCell : Position -> Board -> Html Msg
renderCell i l =
    let c = getAt i l in
    case c of
        Empty -> div [ onClick (Move i), class "cell", style [("width", "50px"), ("height", "50px"), ("background-color", "white"), ("border", "1px solid black"), ("justify-content", "center"), ("align-items", "center") ,("display", "flex")] ] []
        Toad -> div [ onClick (Move i), class "cell", style [("width", "50px"), ("height", "50px"), ("background-color", "red"), ("justify-content", "center"), ("align-items", "center") ,("display", "flex")] ] []
        Frog -> div [ onClick (Move i), class "cell", style [("width", "50px"), ("height", "50px"), ("background-color", "green"), ("justify-content", "center"), ("align-items", "center") ,("display", "flex")] ] []

view : Model -> Html Msg
view model =
    let
        status_message =
            (case model.status of
                Playing -> "Playing"
                
                Win -> "You Won")
        cells =  List.map (\x -> renderCell x model.board) (List.range 0 6)
        
        board = div [ style [("display", "flex")]] cells

        reset = div [] [ button [ onClick Reset, class "btn" ] [ text "Reset" ]]
    in
    div [] [ div [] [ h3 [ style [("padding-bottom","5px")]] [ text status_message ]] ,board,reset ]

        
