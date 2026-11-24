{-# OPTIONS_GHC -Wall #-}
module Tipos where

import Data.List (nub, sort)

-- Um filme é identificado pelo seu título.
type Filme = String

data Resultado = Vitoria | Empate | Derrota
  deriving (Show, Eq)

-- Votação entre os filmes A e B em uma etapa da premiação.
data Votacao = Votacao
  { etapa :: Int
  , filmeA :: Filme
  , votosA :: Int
  , votosB :: Int
  , filmeB :: Filme
  } deriving (Show, Eq)

-- Uma linha da classificação, com os votos do filme e dos demais indicados.
data Linha = Linha
  { filme         :: Filme
  , participacoes :: Int
  , vitorias      :: Int
  , empates       :: Int
  , derrotas      :: Int
  , votosPro      :: Int
  , votosContra   :: Int
  } deriving (Show, Eq)

participou :: Filme -> Votacao -> Bool
participou titulo votacao = titulo == filmeA votacao || titulo == filmeB votacao

resultadoPara :: Filme -> Votacao -> Maybe Resultado
resultadoPara titulo votacao
  | titulo == filmeA votacao = Just (comparar (votosA votacao) (votosB votacao))
  | titulo == filmeB votacao = Just (comparar (votosB votacao) (votosA votacao))
  | otherwise                = Nothing
  where
    comparar :: Int -> Int -> Resultado
    comparar meus deles = case compare meus deles of
      GT -> Vitoria
      EQ -> Empate
      LT -> Derrota

-- Todos os filmes, sem repetição e em ordem alfabética.
filmes :: [Votacao] -> [Filme]
filmes votacoes = sort (nub (map filmeA votacoes ++ map filmeB votacoes))

