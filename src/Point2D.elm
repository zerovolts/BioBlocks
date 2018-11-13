module Point2D exposing
    ( Point2D
    , add
    , average
    , clamp
    , distance
    , distanceSq
    , div
    , isNeighbor
    , magnitude
    , mul
    , neighbors
    , normalize
    , origin
    , sub
    )


type alias Point2D =
    { x : Float
    , y : Float
    }


origin : Point2D
origin =
    { x = 0
    , y = 0
    }


add : Point2D -> Point2D -> Point2D
add p1 p2 =
    Point2D (p1.x + p2.x) (p1.y + p2.y)


sub : Point2D -> Point2D -> Point2D
sub p1 p2 =
    Point2D (p1.x - p2.x) (p1.y - p2.y)


div : Point2D -> Float -> Point2D
div point divisor =
    map (\x -> x / divisor) point


mul : Point2D -> Float -> Point2D
mul point scalar =
    map ((*) scalar) point


map : (Float -> Float) -> Point2D -> Point2D
map fn { x, y } =
    Point2D (fn x) (fn y)


normalize : Point2D -> Point2D
normalize point =
    div point (magnitude point)


magnitude : Point2D -> Float
magnitude point =
    sqrt (magnitudeSq point)


magnitudeSq : Point2D -> Float
magnitudeSq { x, y } =
    x ^ 2 + y ^ 2


average : List Point2D -> Point2D
average points =
    points
        |> List.foldl (\cur acc -> add acc cur) origin
        |> (\point -> div point (toFloat (List.length points)))


clamp : Float -> Point2D -> Point2D
clamp max point =
    if magnitude point > max then
        mul (normalize point) 1

    else
        point


distance : Point2D -> Point2D -> Float
distance p1 p2 =
    magnitude (sub p1 p2)


distanceSq : Point2D -> Point2D -> Float
distanceSq p1 p2 =
    magnitudeSq (sub p1 p2)


isNeighbor : Float -> Point2D -> Point2D -> Bool
isNeighbor radius p1 p2 =
    magnitudeSq (sub p1 p2) < (radius ^ 2)


neighbors : Float -> List Point2D -> Point2D -> List Point2D
neighbors radius allPoints point =
    List.filter (isNeighbor radius point) allPoints



-- direction : Point2D -> Float
