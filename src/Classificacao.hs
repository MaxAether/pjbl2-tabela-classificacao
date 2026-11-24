{-# OPTIONS_GHC -Wall #-}
module Classificacao where

import Data.List (sortBy)
import qualified Data.List as L (foldl')
import Data.Ord (Down(..), comparing)
import Tipos

pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

-- Linha de um filme: filtra as votações dele e acumula com L.foldl',
-- O acumulador começa zerado e recebe os dados de cada votação.
linhaDoFilme :: [Votacao] -> Filme -> Linha
linhaDoFilme votacoes titulo =
  L.foldl' somar (Linha titulo 0 0 0 0 0 0) (filter (participou titulo) votacoes)
  where
    somar :: Linha -> Votacao -> Linha
    somar linha votacao =
      maybe linha (registrar linha votacao) (resultadoPara titulo votacao)

    registrar :: Linha -> Votacao -> Resultado -> Linha
    registrar linha votacao res = linha
      { participacoes = participacoes linha + 1
      , vitorias      = vitorias linha + contar Vitoria
      , empates       = empates linha + contar Empate
      , derrotas      = derrotas linha + contar Derrota
      , votosPro      = votosPro linha + meus
      , votosContra   = votosContra linha + deles
      }
      where
        contar :: Resultado -> Int
        contar r = if r == res then 1 else 0

        ehFilmeA :: Bool
        ehFilmeA = titulo == filmeA votacao

        meus, deles :: Int
        meus  = if ehFilmeA then votosA votacao else votosB votacao
        deles = if ehFilmeA then votosB votacao else votosA votacao

pontosLinha :: Linha -> Int
pontosLinha linha = pontos Vitoria * vitorias linha + pontos Empate * empates linha

saldo :: Linha -> Int
saldo linha = votosPro linha - votosContra linha

-- Percentual dos pontos disputados; fromIntegral faz a ponte Int -> Double.
aproveitamento :: Linha -> Double
aproveitamento linha
  | participacoes linha == 0 = 0
  | otherwise = 100 * fromIntegral (pontosLinha linha)
                    / fromIntegral (pontos Vitoria * participacoes linha)

-- Critérios: pontos, vitórias, saldo, votos pró (todos decrescentes)
-- e, por fim, ordem alfabética. Uma única comparação combinada com <>.
classificacao :: [Votacao] -> [Linha]
classificacao votacoes = sortBy criterios (map (linhaDoFilme votacoes) (filmes votacoes))
  where
    criterios :: Linha -> Linha -> Ordering
    criterios = comparing (Down . pontosLinha)
             <> comparing (Down . vitorias)
             <> comparing (Down . saldo)
             <> comparing (Down . votosPro)
             <> comparing filme

ateEtapa :: Int -> [Votacao] -> [Votacao]
ateEtapa limite = filter ((<= limite) . etapa)

diferenca :: Votacao -> Int
diferenca votacao = abs (votosA votacao - votosB votacao)

-- L.foldl' começando de Nothing: total, sem maximum nem head.
-- Em caso de empate na diferença, fica a primeira votação da lista.
maiorDiferenca :: [Votacao] -> Maybe Votacao
maiorDiferenca = L.foldl' escolher Nothing
  where
    escolher :: Maybe Votacao -> Votacao -> Maybe Votacao
    escolher Nothing votacao = Just votacao
    escolher (Just maior) votacao
      | diferenca votacao > diferenca maior = Just votacao
      | otherwise                           = Just maior

mediaVotos :: [Votacao] -> Maybe Double
mediaVotos []       = Nothing
mediaVotos votacoes = Just (fromIntegral total / fromIntegral (length votacoes))
  where
    total :: Int
    total = sum (map (\p -> votosA p + votosB p) votacoes)

