import           Control.Monad.State
import           Data.Bits                      ( (.&.)
                                                , (.^.)
                                                , (.|.)
                                                )
import           Data.Functor                   ( ($>) )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as M
import           Data.List                      ( intercalate )
import           Data.Void
import           Text.Megaparsec         hiding ( Label
                                                , Pos
                                                , State
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

type EvalState = HashMap Address Int

evalAccess :: Access -> State EvalState Int
evalAccess = go 0
 where
  go 0 (Access address) = return address
  go n (Access address) = do
    m <- get
    go (n - 1) (Access $ M.lookupDefault 0 address m)
  go n (SAccess access) = go (n + 1) access

evilOperation :: Int -> Operator -> Int -> Int
evilOperation a ADD b = a + b
evilOperation a MUL b = a * b
evilOperation a SUB b = a - b
evilOperation a DIV b = a `div` b
evilOperation a AND b = a .&. b
evilOperation a OR  b = a .|. b
evilOperation a XOR b = a .^. b

evalAddressation :: Addressation -> State EvalState Int
evalAddressation (Address access              ) = evalAccess access
evalAddressation (BinaryOperation a operator b) = do
  resA <- evalAccess a
  resB <- evalAccess b
  return $ evilOperation resA operator resB

eval :: Int -> Program -> State EvalState ()
eval n p = go (drop n p)
 where
  go ((Comment _                 ) : ps) = go ps
  go ((Write address addressation) : ps) = do
    source <- evalAddressation addressation
    modify (M.insert address source)
    go ps
  go ((LarsCall call ) : ps) = go ps -- TODO
  go ((Label    _    ) : ps) = go ps -- TODO: better
  go ((GoTo     label) : ps) = do
    let is = [ i | (i, l@(Label n)) <- zip [0 ..] p, n == label ]
    case is of
      [i] -> eval (i + 1) p
      _   -> error $ "invalid jump " <> label
  go ((Branch IfTrue access label) : ps) = do
    address <- evalAccess access
    m       <- get
    case address of
      0 -> go ps
      _ -> go [GoTo label]
  go ((Branch IfFalse access label) : ps) = do
    address <- evalAccess access
    m       <- get
    case address of
      0 -> go [GoTo label]
      _ -> go ps
  go [] = return ()

compileOperation :: String -> Operator -> String -> String
compileOperation a ADD b = a <> " + " <> b
compileOperation a MUL b = a <> " * " <> b
compileOperation a SUB b = a <> " - " <> b
compileOperation a DIV b = a <> " / " <> b
compileOperation a AND b = a <> " & " <> b
compileOperation a OR  b = a <> " | " <> b
compileOperation a XOR b = a <> " ^ " <> b

compileAccess :: Access -> String
compileAccess (Access  address) = show address
compileAccess (SAccess access ) = "heap[" <> compileAccess access <> "]"

compileAddressation :: Addressation -> String
compileAddressation (Address access) = compileAccess access
compileAddressation (BinaryOperation a operator b) =
  compileOperation (compileAccess a) operator (compileAccess b)

compile :: Program -> String
compile ((Comment comment) : ps) = "// " <> comment <> "\n" ++ compile ps
compile ((Write address addressation) : ps) =
  "heap["
    <> show address
    <> "] = "
    <> compileAddressation addressation
    <> ";\n"
    ++ compile ps
compile ((LarsCall WriteCall) : ps) =
  "printf(\"%x\", heap[8159]); " ++ compile ps
compile ((LarsCall call ) : ps) = compile ps -- TODO
compile ((Label    label) : ps) = "" <> label <> ":\n" ++ compile ps
compile ((GoTo     label) : ps) = "goto " <> label <> ";\n" ++ compile ps
compile ((Branch IfTrue access label) : ps) =
  "if ("
    <> compileAccess access
    <> ")"
    <> " goto "
    <> label
    <> ";\n"
    ++ compile ps
compile ((Branch IfFalse access label) : ps) =
  "if (!("
    <> compileAccess access
    <> "))"
    <> " goto "
    <> label
    <> ";\n"
    ++ compile ps
compile [] = ""

preamble :: String
preamble =
  "#include <stdio.h>\nint main(int argc, char *argv[]) { unsigned int heap[10000] = { 0 }; "

postamble :: String
postamble = "printf(\"amen lars\\n\"); }"

preamblify :: String -> String
preamblify amble = preamble ++ amble ++ postamble

main :: IO ()
main = do
  f <- readFile "fac.lll"
  case runParser (program <* many (char '\n') <* eof) "" f of
    Right ps -> do
      -- let ev = runState (eval 0 ps) M.empty
      -- print ev
      putStrLn $ preamblify $ compile ps
      -- putStrLn $ "[" <> intercalate "," (show <$> ps) <> "]"
    Left err -> putStrLn $ errorBundlePretty err
