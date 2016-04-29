module Walls.Utils.Servant where

import Servant.API
import Servant.Server.Internal

type Get' = Get '[JSON]
type Post' = Post '[JSON]
type Delete' = Delete '[JSON]
type ReqBody' = ReqBody '[JSON]
