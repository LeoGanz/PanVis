{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module HistoryQuote where

import Data.Either (fromRight)
import Data.Generics
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

-- works only for small files, otherwise GHC crashes because it runs out of labels
history_f :: QuasiQuoter
history_f = quoteFile history

quoteHistoryExp :: String -> TH.ExpQ
quoteHistoryExp s = do
  loc <- TH.location
  let pos =
        ( TH.loc_filename loc,
          fst (TH.loc_start loc),
          snd (TH.loc_start loc)
        )
  hist <- parseHistory pos s
  dataToExpQ (const Nothing `extQ` antiHistoryExp) hist

antiHistoryExp :: History -> Maybe TH.ExpQ
antiHistoryExp (AntiHistory s) = Just [|toHistory $(TH.varE $ TH.mkName s)|]
antiHistoryExp _ = Nothing

class ToHistory a where
  toHistory :: a -> History

instance ToHistory String where
  toHistory = fromRight (History (Header []) (Body [])) . parseHistoryPlain
