-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> not (isAscii c)
                                          || c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [Var] Lexp      -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
-- s2l :: Sexp -> Lexp
-- s2l (Snum n) = Lnum n                      -- Conversion des nombres
-- s2l (Ssym "true") = Lbool True             -- Conversion du booléen true
-- s2l (Ssym "false") = Lbool False           -- Conversion du booléen false
-- s2l (Ssym v) = Lvar v                      -- Conversion des variables

-- -- Cas d'une expression "let"
-- s2l (Snode (Ssym "let") [Snode (Ssym v) [e], body]) =
--   Llet v (s2l e) (s2l body)

-- -- Cas d'une expression conditionnelle `if`
-- s2l (Snode (Ssym "if") [cond, e1, e2]) =
--   Ltest (s2l cond) (s2l e1) (s2l e2)

-- -- Cas d'une fonction fob
-- s2l (Snode (Ssym "fob") [Snode _ params, body]) =
--   Lfob (map (\(Ssym p) -> p) params) (s2l body)

-- -- Cas d'un appel de fonction
-- s2l (Snode f args) = Lsend (s2l f) (map s2l args)

-- s2l expr = error ("Expression Psil inconnue: " ++ showSexp expr)

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n                      -- Conversion des nombres
s2l (Ssym "true") = Lbool True             -- Conversion du booléen true
s2l (Ssym "false") = Lbool False           -- Conversion du booléen false
s2l (Ssym v) = Lvar v                      -- Conversion des variables

-- Cas d'une expression "let"
s2l (Snode (Ssym "let") [Snode (Ssym v) [e], body]) =
  Llet v (s2l e) (s2l body)

-- Cas d'une expression conditionnelle `if`
s2l (Snode (Ssym "if") [cond, e1, e2]) =
  Ltest (s2l cond) (s2l e1) (s2l e2)

-- Cas d'une fonction fob
s2l (Snode (Ssym "fob") [Snode _ params, body]) =
    Lfob (map extractVar params) (s2l body)

-- Cas d'un appel de fonction
s2l (Snode f args) = Lsend (s2l f) (map s2l args)

s2l expr = error ("Expression Psil inconnue: " ++ showSexp expr)

-- Fonction utilitaire pour extraire les variables (symboles)
extractVar :: Sexp -> String
extractVar (Ssym s) = s
extractVar x = error ("Expected a symbol in parameter list, got: " ++ show x)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")

          in [("+", binop Vnum (+)),
              ("*", binop Vnum (*)),
              ("/", binop Vnum div),
              ("-", binop Vnum (-)),
              ("<", binop Vbool (<)),
              (">", binop Vbool (>)),
              ("≤", binop Vbool (<=)),
              ("≥", binop Vbool (>=)),
              ("=", binop Vbool (==)),
              ("true",  Vbool True),
              ("false", Vbool False)]

-- this is a comment

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: VEnv -> Lexp -> Value
eval _ (Lnum n) = Vnum n
eval _ (Lbool b) = Vbool b
eval env (Lvar v) = case lookup v env of
                      Just val -> val
                      Nothing -> error ("Variable non définie: " ++ v)
eval env (Llet v e body) = eval ((v, eval env e) : env) body
eval env (Ltest cond e1 e2) = case eval env cond of
                                Vbool True -> eval env e1
                                Vbool False -> eval env e2
                                _ -> error "Condition non booléenne dans if"
eval env (Lfob params body) = Vfob env params body
eval env (Lsend f args) = case eval env f of
                            Vbuiltin f' -> f' (map (eval env) args)
                            Vfob env' params body ->
                              let newEnv = zip params (map (eval env) args) ++ env'
                              in eval newEnv body
                            _ -> error "Appel de fonction non valide"


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf


main :: IO ()
main = do
  -- let expr1 = Snode (Ssym "let") [Snode (Ssym "x") [Snum 5], Ssym "x"]
  -- print (s2l expr1)  -- Doit donner : Llet "x" (Lnum 5) (Lvar "x")

  -- let expr2 = Snode (Ssym "if") [Ssym "true", Snum 1, Snum 0]
  -- print (s2l expr2)  -- Doit donner : Ltest (Lbool True) (Lnum 1) (Lnum 0)

  -- let expr3 = Snode (Ssym "+") [Snum 1, Snum 2]
  -- print (s2l expr3)  -- Doit donner : Lsend (Lvar "+") [Lnum 1, Lnum 2]

  -- let expr4 = Snode (Ssym "fob") 
  --               [ Snode (Ssym "") [Ssym "x", Ssym "y"],
  --                 Snode (Ssym "+") [Ssym "x", Ssym "y"]]

  -- print (s2l expr4) -- Doit donner : Lfob ["x", "y"] 
  --                   -- (Lsend (Lvar "+") [Lvar "x", Lvar "y"])

  -- Définir des expressions Lisp
  let expr3 = "((x y z) (+ x y))"


  -- Convertir les chaînes en Sexp
  let sexp3 = sexpOf expr3
  let lexp3 = s2l sexp3
  let val3 = eval env0 lexp3

  print sexp3
  print lexp3
  print val3

  -- -- Utiliser s2l pour convertir les Sexp en Lexp
  -- let lexp1 = s2l sexp1
  -- let lexp2 = s2l sexp2
  -- let lexp3 = s2l sexp3

  -- -- Évaluer les Lexp
  -- let val1 = eval env0 lexp1
  -- let val2 = eval env0 lexp2
  -- let val3 = eval env0 lexp3

  -- -- Afficher les résultats
  -- putStrLn "Expression 1:"
  -- print sexp1
  -- print lexp1
  -- print val1

  -- putStrLn "\nExpression 2:"
  -- print sexp2
  -- print lexp2
  -- print val2

  -- putStrLn "\nExpression 3:"
  -- print sexp3
  -- print lexp3
  -- print val3

    --     -- Évaluer les Lexp
    -- let lexp1 = Lnum 42
    -- let val1 = eval env0 lexp1
    -- print val1  -- Devrait afficher Vnum 42

    -- let lexp2 = Lvar "true"
    -- let val2 = eval env0 lexp2
    -- print val2  -- Devrait afficher Vbool True

    -- let lexp3 = Ltest (Lbool True) (Lnum 1) (Lnum 2)
    -- let val3 = eval env0 lexp3
    -- print val3  -- Devrait afficher Vnum 1

    -- let lexp4 = Llet "z" (Lnum 5) (Lvar "z")
    -- let val4 = eval env0 lexp4
    -- print val4  -- Devrait afficher Vnum 5

    -- -- Tester une fonction anonyme (fob)
    -- let fobExpr = Lfob ["a"] (Llet "z" (Lvar "a") (Lvar "z"))
    -- let lexp5 = Lsend fobExpr [Lnum 15]
    -- let val5 = eval env0 lexp5
    -- print val5  -- Devrait afficher Vnum 15

    -- -- Tester une fonction avec plusieurs paramètres
    -- let fobMultiParams = Lfob ["a", "b"] (Ltest (Lbool True) (Lvar "a") (Lvar "b"))
    -- let lexp6 = Lsend fobMultiParams [Lnum 5, Lnum 10]
    -- let val6 = eval env0 lexp6
    -- print val6  -- Devrait afficher Vnum 5

    -- -- Autres tests supplémentaires
    -- let lexp7 = Ltest (Lbool False) (Lnum 1) (Lnum 2)
    -- let val7 = eval env0 lexp7
    -- print val7  -- Devrait afficher Vnum 2

    -- let lexp8 = Llet "a" (Lnum 10) (Llet "b" (Lnum 20) (Lvar "a"))
    -- let val8 = eval env0 lexp8
    -- print val8  -- Devrait afficher Vnum 10

    -- let lexp9 = Lsend (Lfob ["c"] (Lvar "c")) [Lnum 100]
    -- let val9 = eval env0 lexp9
    -- print val9  -- Devrait afficher Vnum 100

  -- run "exemples.slip"