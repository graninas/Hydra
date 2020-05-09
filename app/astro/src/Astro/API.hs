module Astro.API
  ( module API
  , astroAPI
  , AstroAPI
  ) where


import           Hydra.Prelude
import           Servant

import           Astro.API.Meteor as API
import           Astro.API.Asteroid as API
import           Astro.API.AstroObject as API
import           Astro.Domain.Meteor
import           Astro.Domain.Asteroid
import           Astro.Domain.Types
import           Astro.Domain.AstroObject
import           Astro.Catalogue


type AstroAPI =
  (  "meteors"
    :> QueryParam "mass" Int
    :> QueryParam "size" Int
    :> Get '[JSON] Meteors
    )
  :<|>
    (  "meteor"
    :> ReqBody '[JSON] API.MeteorTemplate
    :> Post '[JSON] MeteorId
    )
  :<|>
    (  "asteroid"
    :> ReqBody '[JSON] API.AsteroidTemplate
    :> Post '[JSON] AsteroidId
    )
  :<|>
    (  "object_template"                                  -- route POST "/object_template"
    :> ReqBody '[JSON] API.AstroObjectTemplate
    :> Post '[JSON] AstroObjectId
    )
  :<|>
    "object" :>
    (
       ( Capture "object_id" AstroObjectId                -- route GET "/object"
       :> Get '[JSON] (Maybe AstroObject)
       )
     :<|>
       ( "orbital"                                        -- route POST "/object/orbital"
       :> Capture "object_id" AstroObjectId
       :> ReqBody '[JSON] Orbital
       :> Post '[JSON] AstroObjectId
       )
     :<|>
       ( "physical"                                       -- route POST "/object/physical
       :> Capture "object_id" AstroObjectId
       :> ReqBody '[JSON] Physical
       :> Post '[JSON] AstroObjectId
       )
    )


astroAPI :: Proxy AstroAPI
astroAPI = Proxy
