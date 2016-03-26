import Svg as S
import Svg.Attributes as A
import Html exposing (Html)
import String
import List

unitSize : Int
unitSize = 12

type alias Node' =
  { tag : String
  , fill : String
  , transform : String
  , attrs : List S.Attribute
  }

defaultNode : Node'
defaultNode =
  { tag = ""
  , fill = ""
  , transform = ""
  , attrs = []
  }

type Node = Prim Node' | Group (List Node) String (List S.Attribute)

toSvg' : Node' -> List S.Svg -> S.Svg
toSvg'  n children =
  let attrs =
        [ A.fill n.fill
        , A.transform n.transform
        ]
  in
    (S.node n.tag) ( n.attrs ++ attrs) children

group : String -> (List S.Attribute) -> Node'
group transform attrs =
  { tag = "g"
  , fill = "" -- group has no fill
  , transform = transform
  , attrs = attrs
  }

toSvg : Node -> S.Svg
toSvg n =
  case n of
    Prim n' ->
      toSvg' n' []
    Group ns transform attrs ->
      toSvg' (group transform attrs) (List.map toSvg ns)

fill : String -> Node -> Node
fill color n =
  case n of
    Prim n' -> Prim { n' | fill = color }
    x -> x -- we don't fill group

transform : (String -> String) -> Node -> Node
transform modifier n =
  case n of
    Prim n' -> Prim { n' | transform = modifier n'.transform }
    Group ns trans attrs -> Group ns (modifier trans) attrs

translate : Float -> Float -> Node -> Node
translate x y n =
  let s oldTransform = String.concat [" translate(", toString(x), ", ", toString(y), ") ", oldTransform]
  in transform s n

rotate : Float -> Node -> Node
rotate degree n =
  let s oldTransform = String.concat [" rotate(", toString(degree), ") ", oldTransform]
  in transform s n

scale : Float -> Node -> Node
scale degree n =
   let s oldTransform = String.concat [" scale(", toString(degree), ") ", oldTransform]
  in transform s n

embed : String -> Node -> Node
embed color n =
  case n of
    Prim n' -> Group ((n |> fill color |> scale 1.5) :: [n])  "" []
    Group ns transform attrs ->
      case ns of
        [] -> Group [] "" []
        hd :: tl -> Group ((hd |> fill color |> scale 1.5 ) :: hd :: tl) "" []

rect : Node
rect = Prim
       { defaultNode | tag = "rect"
       , attrs = [ A.x (toString (-unitSize // 2))
                 , A.y (toString (-unitSize // 2))
                 , A.width (toString unitSize)
                 , A.height (toString unitSize)
                 ]
       }

round : Node -> Node
round n =
  let a = [ A.rx (toString (unitSize //3))
          , A.ry (toString (unitSize //3))
          ]
  in case n of
       Prim n' -> Prim { n' | attrs = n'.attrs ++ a }
       x -> x

circle : Node
circle =
  Prim
    { defaultNode | tag = "circle"
    , attrs = [ unitSize // 2 |> toString |> A.r]
    }


polyPath : Float -> Float -> String
polyPath n unitSize =
  [0 .. n-1]
    |> List.map (\x -> x * ( 2 * pi / n) + pi/2)
    |> List.concatMap (\x -> [unitSize * cos x, unitSize * sin x])
    |> List.map toString
    |> List.intersperse " "
    |> String.concat

gon : Float -> Node
gon n =
  Prim
    { defaultNode | tag = "polygon"
    , attrs = [A.points (polyPath n (toFloat unitSize)) ]
    }
triangle : Node
triangle = gon 3
pentagon : Node
pentagon = gon 5
hexagon : Node
hexagon = gon 6
heptagon : Node
heptagon = gon 7
octagon : Node
octagon = gon 8

type Shape = Circle | Triangle | Rectangle | Pentagon | Hexagon | Heptagon | Octagon
toNode : Shape -> Node
toNode s =
  case s of
    Circle -> circle
    Triangle -> triangle
    Rectangle -> rect
    Pentagon -> pentagon
    Hexagon -> hexagon
    Heptagon -> heptagon
    Octagon -> octagon

halfArrow : Float
halfArrow = 60

line : Float -> Float -> Float -> Float -> Int -> Node
line x1 y1 x2 y2 strokeWidth =
  Prim
    { tag = "line"
    , fill = "#000"
    , transform = ""
    , attrs =
        [ A.x1 (toString x1)
        , A.y1 (toString y1)
        , A.x2 (toString x2)
        , A.y2 (toString y2)
        , A.strokeWidth (toString strokeWidth)
        , A.stroke "#000"
        ]
    }

type Arrow = Arrow Shape String Shape

arrowToNode : Arrow -> Node
arrowToNode (Arrow src label dst) =
  let ns =
        [ line 0 (-halfArrow) 0 halfArrow 5
        , toNode src |> translate 0 (-halfArrow)
        , toNode dst |> translate 0 halfArrow
        ]
  in
    Group ns "" []

main : Html
main =
  let hw = 250
      hh = 250
  in S.svg [ A.version "1.1"
            , A.x "0", A.y "0"
            , 2 * hw |> toString |> A.width
            , 2 * hh |> toString |> A.width
            , [-hw, -hh, 2*hw, 2*hh] |> List.map toString |> String.join " " |>  A.viewBox
            ]
       ([
        arrowToNode (Arrow Circle "f" Triangle) |> embed "#f00"
        ] |> List.map toSvg)
{-
TODO: arrow
-}
