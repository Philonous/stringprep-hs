module Text.SaslPrep
  ( saslPrepQuery
  , saslPrepStore
  ) where

import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Text(singleton)
import           Text.StringPrep

nonAsciiSpaces :: Set.Set Char
nonAsciiSpaces = Set.fromList [ '\x00A0', '\x1680', '\x2000', '\x2001', '\x2002'
                              , '\x2003', '\x2004', '\x2005', '\x2006', '\x2007'
                              , '\x2008', '\x2009', '\x200A', '\x200B', '\x202F'
                              , '\x205F', '\x3000'
                              ]

toSpace :: Char -> Text
toSpace x = if x `Set.member` nonAsciiSpaces then singleton ' ' else singleton x

saslPrepQuery :: StringPrepProfile
saslPrepQuery = Profile
    [b1, toSpace]
    True
    [c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
    True

saslPrepStore :: StringPrepProfile
saslPrepStore = Profile
    [b1, toSpace]
    True
    [a1, c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
    True
