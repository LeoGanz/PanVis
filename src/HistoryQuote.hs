--{-# LANGUAGE QuasiQuotes #-}

module HistoryQuote where

import History
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

history :: QuasiQuoter
history =
  QuasiQuoter
    { quoteExp = quoteHistoryExp,
      quotePat = notHandled "patterns",
      quoteType = notHandled "types",
      quoteDec = notHandled "declarations"
    }
  where
    notHandled things = error $ things ++ " are not handled by the history quasiquoter."

quoteHistoryExp :: String -> TH.ExpQ
quoteHistoryExp s = do
  loc <- TH.location
  let pos =
        ( TH.loc_filename loc,
          fst (TH.loc_start loc),
          snd (TH.loc_start loc)
        )
  hist <- parseHistory pos s
  dataToExpQ (const Nothing) hist

quoteHistoryPat :: String -> TH.PatQ
quoteHistoryPat = undefined
