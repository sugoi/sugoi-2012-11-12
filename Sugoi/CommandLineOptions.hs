module Sugoi.CommandLineOptions where

import qualified Options.Applicative as P

data CommandLineOptions = CommandLineOptions
  { mode :: Mode }
