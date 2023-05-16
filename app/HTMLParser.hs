module HTMLParser where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isSpace)
import ParserUtils.BaseParser
  ( parseAlpha,
    parseChar,
    parseEnd,
    parseSpan,
    parseString,
  )
import ParserUtils.CheckParser (checkString)
import ParserUtils.Parser
  ( Parser (runParser),
    parseMaxPossible,
    parseWhiteSpace,
    (*\\>),
    (<//*),
  )
import ParserUtils.ParsingUtils
  ( groupSpaces,
    isNotForbidden,
    removeTailWhiteSpace,
  )

{-
  TODO:
    - add more tags
      - tags with numbers (h1, h2...)
    - add attributes
    - deal with ids
    - deal with classes
    - deal with head and body
    - implement forgiveness (imcomplete doesn't imply exception)
    - implement the whole spec
-}

data Tag = DivTag | PTag | OtherTag String
  deriving (Eq, Show, Ord)

data HTMLDOM = SelfClosed Tag | TextNode String | Node Tag [HTMLDOM]
  deriving (Eq, Show)

stringToTag :: String -> Tag
stringToTag "p" = PTag
stringToTag "div" = DivTag
stringToTag s = OtherTag s

tagToString :: Tag -> String
tagToString PTag = "p"
tagToString DivTag = "div"
tagToString (OtherTag s) = s

forbiddenChars :: [Char]
forbiddenChars = ['<', '>', '/']

parseNotForbidden :: Parser String
parseNotForbidden = parseSpan $ isNotForbidden forbiddenChars

parseOpeningTag :: Parser Tag
parseOpeningTag = stringToTag <$> (parseChar '<' *\\> parseAlpha <//* parseChar '>')

parseClosingTag :: Parser Tag
parseClosingTag = stringToTag <$> (parseString "</" *\\> parseAlpha <//* parseChar '>')

parseSpecificClosingTag :: Tag -> Parser Tag
parseSpecificClosingTag tag = tag <$ (parseString "</" *\\> parseAlpha <//* parseChar '>' >>= checkString (tagToString tag))

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
      "<html>\
      \<head>\
      \<title>Auto-generated html formated source</title>\
      \<meta/>\
      \</head>\
      \<body>\
      \<p> okk </p>\
      \<pre> okok</pre>\
      \</body>\
      \</html>\
      \"
