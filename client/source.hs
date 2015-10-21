{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Safe (readMay)
import Control.Applicative ((<*>), (<$>))
import Control.Monad
import Data.Maybe
import GHCJS.DOM.EventM (event, preventDefault)
import GHCJS.DOM.Element

data Counter = Counter { number :: Int
                       , name :: String
                       } deriving (Show)

insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew_ v m = if null m then Map.singleton (toEnum 0) v
                           else Map.insert (succ . fst . Map.findMax $ m) v m

initCounters :: Map Int Counter
initCounters = Map.singleton (toEnum 0) (Counter 0 "Dog")

createCounter = Counter 0

displayCounter :: MonadWidget t m => Dynamic t Counter -> m (Event t (Counter -> Counter))
displayCounter c = el "div" $ do
  incrementBtn <- button "+"
  decrementBtn <- button "-"
  text "Name: "
  dynText =<< mapDyn name c
  text " Count: "
  dynText =<< mapDyn (show . number) c
  return $ mergeWith (.) [ fmap (const (incrCounter  1)) incrementBtn
                         , fmap (const (incrCounter (-1))) decrementBtn
                         ]

incrCounter :: Int -> Counter -> Counter
incrCounter d c = c { number = number c + d }

mapToModFns :: (Reflex t, Ord k) => (Map k (Event t (a -> a))) ->  Event t (Map k a -> Map k a)
mapToModFns = fmap ((Map.fold (.) id) . (Map.mapWithKey (flip Map.adjust))) . mergeMap

form :: MonadWidget t m => m a -> m (Event t (), a)
form child = do
  (form, ch) <- el' "form" child
  submit <- wrapDomEvent (_el_element form) elementOnsubmit (void $ preventDefault)
  performEvent_ (return () <$ submit)
  return (submit, ch)

main = mainWidget $ el "div" $ do
  rec (submit, _) <- form $ do
      rec newCounterInput <- textInput $ def & setValue .~ fmap (const "") createCounterBtn
          createCounterBtn <- button "Create counter"
          createCounterName <- return $ tag (current (_textInput_value newCounterInput)) createCounterBtn
          counters <- foldDyn ($) initCounters $ mergeWith (.)
                            [ fmap (insertNew_ . createCounter) createCounterName
                            , counterInc
                            ]
          el "br" blank
          counterEvents <- el "div" $ list counters displayCounter
          let combineIncrements = fmap (Map.foldWithKey (\k mod f -> f . Map.adjust mod k) id) . mergeMap
          counterIncrement <- mapDyn combineIncrements counterEvents
          let counterInc = switch . current $ counterIncrement
          el "br" blank
          display counters
      return ()
  display =<< foldDyn (const (+1)) 0 submit
  (submitAppend, appendText) <- form $ do
    appendText <- textInput def
    button "Append"
    return appendText
  appendText <- return $ tag (current (_textInput_value appendText)) submitAppend
  display =<< foldDyn (++) "" appendText
  el "br" blank
