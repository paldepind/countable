module Config where

import Database.Persist.TH

persistSettings = sqlSettings {mpsPrefixFields = False}
