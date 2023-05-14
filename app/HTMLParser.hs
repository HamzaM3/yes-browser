module HTMLParser where

import Control.Applicative (Alternative (..), (<**>))
import Control.Lens.Lens
import Data.Char (isSpace)
import ParserUtils.BaseParser
import ParserUtils.Parser
import ParserUtils.ParsingUtils

data Tag = DivTag | PTag | OtherTag String
  deriving (Eq, Show)

data HTMLDOM = SelfClosed Tag | TextNode String | EmptyNode Tag | Node Tag [HTMLDOM]
  deriving (Eq, Show)

stringToTag :: String -> Tag
stringToTag "p" = PTag
stringToTag "div" = DivTag
stringToTag s = OtherTag s

tagToString :: Tag -> String
tagToString PTag = "p"
tagToString DivTag = "div"
tagToString (OtherTag s) = s

groupSpaces :: String -> String
groupSpaces s = group $ span isSpace s
  where
    group :: (String, String) -> String
    group (_, "") = ""
    group ("", s) = a ++ group (span isSpace b)
      where
        (a, b) = break isSpace s
    group (_, s) = " " ++ group ("", s)

forbiddenChars :: [Char]
forbiddenChars = ['<', '>', '/']

isNotForbidden :: Char -> Bool
isNotForbidden = and . (map (/=) forbiddenChars ??)

parseNotForbidden :: Parser String
parseNotForbidden = parseSpan isNotForbidden

parseOpeningTag :: Parser Tag
parseOpeningTag = stringToTag <$> (parseChar '<' *\\> parseAlpha <//* parseChar '>')

parseClosingTag :: Parser Tag
parseClosingTag = stringToTag <$> (parseString "</" *\\> parseAlpha <//* parseChar '>')

parseSpecificClosingTag :: Tag -> Parser Tag
parseSpecificClosingTag tag = tag <$ (parseString "</" *\\> parseString (tagToString tag) <//* parseChar '>')

parseSelfClosingTag :: Parser Tag
parseSelfClosingTag = stringToTag <$> (parseChar '<' *\\> parseAlpha <//* parseString "/>")

parseNode :: Parser HTMLDOM
parseNode = parseOpeningTag >>= (\tag -> Node tag <$> parseMaxPossible parseHTML <//* parseSpecificClosingTag tag)

parseTextNode :: Parser HTMLDOM
parseTextNode = TextNode . groupSpaces . removeTailWhiteSpace <$> (parseWhiteSpace *> parseNotForbidden)

parseSelfClosed :: Parser HTMLDOM
parseSelfClosed = SelfClosed <$> parseSelfClosingTag

parseHTML :: Parser HTMLDOM
parseHTML = parseWhiteSpace *\\> (parseSelfClosed <|> parseTextNode <|> parseNode)

parsePage :: Parser HTMLDOM
parsePage = parseWhiteSpace *\\> parseNode <//* parseWhiteSpace <//* parseEnd

mainParser :: IO ()
mainParser = do
  print $
    runParser
      parsePage
      "<HTML>\
      \<HEAD>\
      \<TITLE>Auto-generated html formated source</TITLE>\
      \<META/>\
      \</HEAD>\
      \<BODY>\
      \<P> okk </P>\
      \<PRE> okok</PRE>\
      \</BODY>\
      \</HTML>\
      \"
