import           Data.Functor                   ( ($>) )
import           Data.List                      ( intercalate )
import           Data.Void
import           Text.Megaparsec         hiding ( Label
                                                , Pos
                                                , label
                                                )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String
type Program = [Instr]

type Address = Int
type Label = String

data Access = Access Address | SAccess Access
data Operation = ADD | MUL | SUB | DIV | AND | OR | XOR
  deriving Show
data Addressation = Address Access | BinaryOperation Access Operation Access
data Call = WriteCall | ReadCall
  deriving Show
data BranchPolarity = IfTrue | IfFalse
  deriving Show
data Instr = Comment String | Write Address Addressation | LarsCall Call | Label Label | GoTo Label | Branch BranchPolarity Address Label

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

comment :: Parser Instr
comment = Comment <$> (some (string "lars") *> many (satisfy (/= '\n')))

access :: Parser Access
access = (SAccess <$> (string "sral" *> access)) <|> (Access <$> L.decimal)

binaryOperation :: Parser Operation
binaryOperation =
  (char '+' $> ADD)
    <|> (char '-' $> SUB)
    <|> (char '*' $> MUL)
    <|> (char '/' $> DIV)

addressation :: Parser Addressation
addressation =
  try (BinaryOperation <$> access <*> binaryOperation <*> access)
    <|> (Address <$> access)

-- TODO: arguments
call :: Parser Instr
call =
  LarsCall
    <$> (  string "larssral"
        *> ((string "lars" $> ReadCall) <|> (string "sral" $> WriteCall))
        )

write :: Parser Instr
write = do
  target <- L.decimal
  string "lars"
  source <- addressation
  return $ Write target source

label :: Parser Label
label = char '@' *> (concat <$> some (string "lars" <|> string "sral"))

namedLabel :: Parser Instr
namedLabel = Label <$> label

goto :: Parser Instr
goto = GoTo <$> (string "sralllars " *> label)

branch :: Parser Instr
branch =
  Branch
    <$> ((string "lars|sral" $> IfTrue) <|> (string "sral|lars" $> IfFalse))
    <*> L.decimal
    <*> label

instr :: Parser Instr
instr = comment <|> write <|> call <|> namedLabel <|> goto <|> branch

program :: Parser Program
program = sepEndBy instr (some $ char '\n')

main :: IO ()
main = do
  f <- readFile "lars.lll"
  case runParser (program <* eof) "" f of
    Right res -> putStrLn $ "[" <> intercalate "," (show <$> res) <> "]"
    Left  err -> putStrLn $ errorBundlePretty err
