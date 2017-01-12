module Text.Megaparsec.Char.Tibetan where

import Text.Megaparsec
import Text.Megaparsec.Text

-- | Parse a consonant
consonantCharBo :: Parser Char
consonantCharBo = oneOf ("ཨཅཆརཏཡཕཙཚཛའསདབངམགལཞཟཤཀཁཔནཐཇཉཝཧ" :: String)
