{-# OPTIONS_GHC -Wall #-}
module Leitura where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, nub)
import Text.Read (readMaybe)
import Classificacao (pontosLinha, saldo)
import Tipos

-- Linha do texto onde o erro aconteceu ou etapa do conflito.
data Erro
  = CamposInvalidos Int                -- linha sem 5 campos ou com título vazio
  | NumeroInvalido Int String          -- linha e nome do campo ("dois", "2a", etapa <= 0)
  | VotosNegativos Int                 -- linha com votos negativos
  | MesmoFilme Int                     -- linha em que o mesmo filme aparece como as duas opções da votação
  | FilmeRepetidoNaEtapa Int Filme    -- etapa e filme incluído duas vezes
  deriving (Show, Eq)

-- Quebra a linha nos separadores com foldr: cada separador abre um campo novo.
separar :: Char -> String -> [String]
separar sep = foldr passo [""]
  where
    passo :: Char -> [String] -> [String]
    passo c campos
      | c == sep  = "" : campos
      | otherwise = case campos of
          (campo : resto) -> (c : campo) : resto
          []              -> [[c]]

aparar :: String -> String
aparar = dropWhileEnd isSpace . dropWhile isSpace

-- Converte Maybe em Either, acrescentando o motivo do erro.
paraEither :: e -> Maybe a -> Either e a
paraEither e Nothing  = Left e
paraEither _ (Just x) = Right x

-- readMaybe devolve Nothing para "dois" ou "2a"; >>= descarta números
-- grandes demais para Int (read daria a volta e geraria lixo).
lerNumero :: Int -> String -> String -> Either Erro Int
lerNumero numLinha campo texto =
  paraEither (NumeroInvalido numLinha campo) (readMaybe texto >>= caberEmInt)
  where
    caberEmInt :: Integer -> Maybe Int
    caberEmInt n
      | n >= toInteger (minBound :: Int)
          && n <= toInteger (maxBound :: Int) = Just (fromInteger n)
      | otherwise = Nothing

-- Bloco do na mônada Either: o primeiro Left interrompe a leitura.
lerVotacao :: Int -> String -> Either Erro Votacao
lerVotacao numLinha texto =
  case map aparar (separar ';' texto) of
    [r, a, va, vb, b] -> do
      numEtapa <- lerNumero numLinha "etapa" r
      votA     <- lerNumero numLinha "votos do filme A" va
      votB     <- lerNumero numLinha "votos do filme B" vb
      validar (Votacao numEtapa a votA votB b)
    _ -> Left (CamposInvalidos numLinha)
  where
    validar :: Votacao -> Either Erro Votacao
    validar p
      | null (filmeA p) || null (filmeB p) = Left (CamposInvalidos numLinha)
      | etapa p <= 0                      = Left (NumeroInvalido numLinha "etapa")
      | votosA p < 0 || votosB p < 0       = Left (VotosNegativos numLinha)
      | filmeA p == filmeB p               = Left (MesmoFilme numLinha)
      | otherwise                          = Right p

-- Linhas numeradas a partir de 1, sem as vazias e os comentários (#).
linhasUteis :: String -> [(Int, String)]
linhasUteis texto = [(n, l) | (n, l) <- zip [1 ..] (lines texto), util (aparar l)]
  where
    util :: String -> Bool
    util l = not (null l) && take 1 l /= "#"

-- Filmes incluídos mais de uma vez na mesma etapa.
conflitos :: [Votacao] -> [Erro]
conflitos votacoes =
  [ FilmeRepetidoNaEtapa r f
  | (r, f) <- nub inscricoes
  , length (filter (== (r, f)) inscricoes) > 1
  ]
  where
    inscricoes :: [(Int, Filme)]
    inscricoes = concatMap (\p -> [(etapa p, filmeA p), (etapa p, filmeB p)]) votacoes

verificarEtapas :: [Votacao] -> Either Erro [Votacao]
verificarEtapas votacoes = case conflitos votacoes of
  []         -> Right votacoes
  (erro : _) -> Left erro

-- traverse para no primeiro erro de leitura; depois confere as etapas.
lerPremiacao :: String -> Either Erro [Votacao]
lerPremiacao texto = do
  votacoes <- traverse (uncurry lerVotacao) (linhasUteis texto)
  verificarEtapas votacoes

-- Todos os erros: os de cada linha e os conflitos entre as linhas válidas.
todosOsErros :: String -> [Erro]
todosOsErros texto = [erro | Left erro <- lidas] ++ conflitos [p | Right p <- lidas]
  where
    lidas :: [Either Erro Votacao]
    lidas = map (uncurry lerVotacao) (linhasUteis texto)

mensagemErro :: Erro -> String
mensagemErro (CamposInvalidos n)   = "Linha " ++ show n ++ ": esperados 5 campos com dois títulos."
mensagemErro (NumeroInvalido n c)  = "Linha " ++ show n ++ ": " ++ c ++ " inválido."
mensagemErro (VotosNegativos n)    = "Linha " ++ show n ++ ": os votos não podem ser negativos."
mensagemErro (MesmoFilme n)        = "Linha " ++ show n ++ ": um filme não pode aparecer nas duas opções da votação."
mensagemErro (FilmeRepetidoNaEtapa r f) =
  "Etapa " ++ show r ++ ": " ++ f ++ " aparece em mais de uma votação."

votacaoFormatada :: Votacao -> String
votacaoFormatada p =
  "Etapa " ++ show (etapa p) ++ ": " ++ filmeA p ++ " " ++ show (votosA p)
    ++ " x " ++ show (votosB p) ++ " " ++ filmeB p

tabelaFormatada :: [Linha] -> String
tabelaFormatada [] = "Nenhuma votação registrada.\n"
tabelaFormatada linhas = unlines (map (intercalate " | ") (cabecalho : corpo))
  where
    largura :: Int
    largura = foldr (max . length . filme) (length "Filme") linhas

    cabecalho :: [String]
    cabecalho = [esquerda 3 "Pos", esquerda largura "Filme", "Pts", "Par", "  V", "  E", "  D", "Saldo"]

    corpo :: [[String]]
    corpo = [formatar pos linha | (pos, linha) <- zip [1 ..] linhas]

    formatar :: Int -> Linha -> [String]
    formatar pos linha =
      [ esquerda 3 (show pos ++ "º")
      , esquerda largura (filme linha)
      ] ++ map (direita 3 . show)
          [pontosLinha linha, participacoes linha, vitorias linha, empates linha, derrotas linha]
        ++ [direita 5 (comSinal (saldo linha))]

    comSinal :: Int -> String
    comSinal s = if s > 0 then '+' : show s else show s

    esquerda, direita :: Int -> String -> String
    esquerda n s = s ++ espacos n s
    direita  n s = espacos n s ++ s

    espacos :: Int -> String -> String
    espacos n s = replicate (n - length s) ' '


