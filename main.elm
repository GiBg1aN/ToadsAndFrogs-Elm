import Html exposing (..)


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
        Empty -> (*)

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
    case l |> (List.drop (i + 1) >> List.head) of
        Just j -> j
        Nothing -> Debug.crash "Head Error"

-- UPDATE

type Msg = Reset | Move Int

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

-- view : Model -> Html Msg
-- view model =
