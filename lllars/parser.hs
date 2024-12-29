import           Data.Functor                   ( ($>) )
import           Data.List                      ( intercalate )
import           Data.Void
import           System.Environment
import           Text.Megaparsec         hiding ( Label
                                                , Pos
                                                , label
                                                )
import           Text.Megaparsec.Char    hiding ( space )
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String
type Program = [Instr]

type Address = Int
type Label = String

data Access = Access Address | SAccess Access
data Operator = ADD | MUL | SUB | DIV | AND | OR | XOR
  deriving Show
data Addressation = Address Access | BinaryOperation Access Operator Access
data Call = WriteCall | ReadCall
  deriving Show
data BranchPolarity = IfTrue | IfFalse
  deriving Show
data Instr = Comment String | Write Address Addressation | LarsCall Call | Label Label | GoTo Label | Branch BranchPolarity Access Label

instance Show Access where
  show (Access  address) = "{ \"address\": " <> show address <> " }"
  show (SAccess access ) = "{ \"sAddress\": " <> show access <> " }"

instance Show Addressation where
  show (Address access) = "{ \"access\": " <> show access <> " }"
  show (BinaryOperation a op b) =
    "{ \"binaryOperation\": { \"a\": "
      <> show a
      <> ", \"op\": \""
      <> show op
      <> "\", \"b\": "
      <> show b
      <> " }}"

instance Show Instr where
  show (Comment string) = "{ \"comment\": \"" <> string <> "\" }"
  show (Write target source) =
    "{ \"write\": { \"target\": "
      <> show target
      <> ", \"source\": "
      <> show source
      <> " }}"
  show (LarsCall call ) = "{ \"call\": " <> show call <> " }"
  show (Label    label) = "{ \"label\": " <> show label <> " }"
  show (GoTo     label) = "{ \"goto\": " <> show label <> " }"
  show (Branch pol jmp label) =
    "{ \"branch\": { \"polarity\": "
      <> show pol
      <> ", \"jmp\": "
      <> show jmp
      <> ", \"label\": "
      <> show label
      <> "}}"

space :: Parser ()
space = some (char ' ') $> ()

comment :: Parser Instr
comment =
  Comment <$> (some (string "lars") *> space *> many (satisfy (/= '\n')))

access :: Parser Access
access = (SAccess <$> (string "sral" *> access)) <|> (Access <$> L.decimal)

binaryOperator :: Parser Operator
binaryOperator =
  (char '+' $> ADD)
    <|> (char '-' $> SUB)
    <|> (char '*' $> MUL)
    <|> (char '/' $> DIV)

addressation :: Parser Addressation
addressation =
  try (BinaryOperation <$> access <*> binaryOperator <*> access)
    <|> (Address <$> access)

-- TODO: arguments
call :: Parser Instr
call =
  LarsCall
    <$> (  string "larssral"
        *> space
        *> ((string "lars" $> WriteCall) <|> (string "sral" $> ReadCall))
        )

write :: Parser Instr
write = do
  target <- L.decimal
  string "lars"
  source <- addressation
  return $ Write target source

label :: Parser Label
label = concat <$> some (string "lars" <|> string "sral")

namedLabel :: Parser Instr
namedLabel = Label <$> (char '@' *> label)

goto :: Parser Instr
goto = GoTo <$> (string "srallars " *> label)

branch :: Parser Instr
branch =
  Branch
    <$> (  ((string "lars|sral" $> IfTrue) <|> (string "sral|lars" $> IfFalse))
        <* space
        )
    <*> (access <* space)
    <*> label

instr :: Parser Instr
instr = try comment <|> write <|> call <|> namedLabel <|> goto <|> branch

license :: Parser String
license = string "!!! all rights reserved to lars <3 !!!\n\n"

program :: Parser Program
program = license *> sepEndBy instr (some $ char '\n')

parseProgram :: String -> IO ()
parseProgram p = case runParser (program <* many (char '\n') <* eof) "" p of
  Right ps ->
    putStrLn $ "{ \"instructions\": [" <> intercalate "," (show <$> ps) <> "]}"
  Left err -> putStrLn $ errorBundlePretty err

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      p <- readFile file
      parseProgram p
    _ -> putStrLn "Wrong number of arguments"
