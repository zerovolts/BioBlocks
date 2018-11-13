module Rigidbody exposing (Rigidbody, addForce, update)

import Point2D exposing (Point2D)


type alias Rigidbody =
    { position : Point2D
    , velocity : Point2D
    , acceleration : Point2D
    , mass : Float
    , maxSpeed : Float
    , maxForce : Float
    , forces : List Point2D
    }


addForce : Point2D -> Rigidbody -> Rigidbody
addForce point rb =
    { rb
        | acceleration =
            Point2D.clamp 1 <|
                Point2D.add rb.acceleration
                    (Point2D.clamp 1 point)
        , forces = Point2D.clamp 1 point :: rb.forces
    }


update : Rigidbody -> Rigidbody
update rb =
    { rb
        | position = Point2D.add rb.position rb.velocity
        , velocity = Point2D.clamp 4 <| Point2D.add rb.velocity rb.acceleration
        , forces = []
    }
