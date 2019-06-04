module Scene where

import Data.Glome.Vec

import Ray
import Materials

cornellBox :: World
cornellBox = World
  { camera = Ray (Vec 0 (-150) 0) vy
  , objects =
      [ Object (Sphere (Vec (1e5 + 50) 0 0) 1e5) (mkColour 0.75 0.25 0.25)
      , Object (Sphere (Vec (-1e5 - 50) 0 0) 1e5) (mkColour 0.25 0.25 0.75)
      , Object (Sphere (Vec 0 (1e5 + 50) 0) 1e5) (mkColour 0.75 0.75 0.75)
      , Object (Sphere (Vec 0 0 (1e5 + 50)) 1e5) (mkColour 0.75 0.75 0.75)
      , Object (Sphere (Vec 0 0 (-1e5 - 50)) 1e5) (mkColour 0.75 0.75 0.75)
      , Object (Sphere (Vec 0 0 649.5) 600) (mkLight 0.005 0.005 0.005)
      , Object (Sphere (Vec (-30) (-30) (-35)) 15) glass
      , Object (Sphere (Vec (30) (30) (-35)) 15) mirror
      , Object (Sphere (Vec (-20) (15) (-35)) 15) pink
      , Object (Sphere (Vec (20) (-30) (-40)) 10) brown
      ]
  , sky = blackDiffuse
  }
