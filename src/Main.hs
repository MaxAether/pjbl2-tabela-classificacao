{-# OPTIONS_GHC -Wall #-}
module Main where

import Classificacao
import Dados
import Leitura
import Tipos (Linha, filme)

main :: IO ()
main = do
  putStrLn "=== Oscar fictício: classificação final ==="
  case lerPremiacao dadosPremiacao of
    Left erro      -> putStrLn ("Erro nos dados: " ++ mensagemErro erro)
    Right votacoes -> do
      putStr (tabelaFormatada (classificacao votacoes))

      putStrLn "\n=== Classificação após a 3ª etapa ==="
      putStr (tabelaFormatada (classificacao (ateEtapa 3 votacoes)))

      putStrLn "\n=== Estatísticas ==="
      putStrLn ("Maior diferença: " ++ maybe "sem votações" votacaoFormatada (maiorDiferenca votacoes))
      putStrLn ("Média de votos por votação: " ++ maybe "sem votações" show (mediaVotos votacoes))
      mapM_ (putStrLn . descreverAproveitamento) (classificacao votacoes)

  putStrLn "\n=== Entrada com erros ==="
  putStrLn ("lerPremiacao para no primeiro: " ++ either mensagemErro (const "aceita") (lerPremiacao dadosComErros))
  putStrLn "todosOsErros lista todos:"
  mapM_ (putStrLn . ("  " ++) . mensagemErro) (todosOsErros dadosComErros)

  putStrLn "\n=== Premiação vazia ==="
  putStr (tabelaFormatada (classificacao []))
  putStrLn ("Maior diferença: " ++ maybe "sem votações" votacaoFormatada (maiorDiferenca []))
  where
    descreverAproveitamento :: Linha -> String
    descreverAproveitamento linha =
      filme linha ++ ": " ++ show (umaCasa (aproveitamento linha)) ++ "% de aproveitamento"

    umaCasa :: Double -> Double
    umaCasa x = fromIntegral (round (x * 10) :: Int) / 10

