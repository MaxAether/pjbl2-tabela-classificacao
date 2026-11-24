{-# OPTIONS_GHC -Wall #-}
module Main where

import Classificacao
import Dados
import Leitura
import Tipos

-- =====================================================================
-- Testes com resultado esperado
-- Cada teste imprime OK ou FALHOU; no fim, o total.
-- =====================================================================

testar :: (Eq a, Show a) => String -> a -> a -> IO Bool
testar nome obtido esperado
  | obtido == esperado = do
      putStrLn ("OK      " ++ nome)
      return True
  | otherwise = do
      putStrLn ("FALHOU  " ++ nome ++ ": esperado " ++ show esperado ++ ", obtido " ++ show obtido)
      return False

-- Premiação de teste lida de dadosPremiacao ([] se houver erro: os testes falham).
premiacao :: [Votacao]
premiacao = either (const []) id (lerPremiacao dadosPremiacao)

exemplo :: Votacao
exemplo = Votacao 1 "Sonho de Liberdade" 4 0 "Bastardos Inglórios"

-- Tabelas conferidas à mão (ver docs/decisoes.md).
tabelaFinal :: [Linha]
tabelaFinal =
  [ Linha "Ainda Estou Aqui"          5 3 1 1  6 3
  , Linha "Sonho de Liberdade"        5 3 0 2 10 4
  , Linha "Como Eu Era Antes de Você" 5 3 0 2  6 4
  , Linha "Toy Story 3"               5 3 0 2  7 6
  , Linha "Bastardos Inglórios"       5 1 1 3  3 8
  , Linha "Vitória"                   5 0 2 3  1 8
  ]

tabelaEtapa3 :: [Linha]
tabelaEtapa3 =
  [ Linha "Ainda Estou Aqui"          3 2 1 0 3 0
  , Linha "Sonho de Liberdade"        3 2 0 1 6 2
  , Linha "Como Eu Era Antes de Você" 3 2 0 1 4 2
  , Linha "Toy Story 3"               3 1 0 2 3 5
  , Linha "Vitória"                   3 0 2 1 1 3
  , Linha "Bastardos Inglórios"       3 0 1 2 1 6
  ]

-- Três filmes com 3 pontos, 1 vitória e saldo +1. Alvorada fica em último
-- por ter menos votos pró (2 contra 3); Cedro e Lume empatam até nos votos
-- pró, e a ordem alfabética coloca Cedro na frente.
desempate :: [Votacao]
desempate =
  [ Votacao 1 "Lume" 3 2 "Duna"
  , Votacao 1 "Alvorada" 2 1 "Brisa"
  , Votacao 1 "Cedro" 3 2 "Fagulha"
  ]

-- Mesmos pontos, vitórias, saldo e votos pró: só a ordem alfabética decide.
soAlfabetica :: [Votacao]
soAlfabetica = [Votacao 1 "Zênite" 1 1 "Brisa"]

testesCamada1 :: [IO Bool]
testesCamada1 =
  [ testar "resultadoPara filme A (opção A)" (resultadoPara "Sonho de Liberdade" exemplo) (Just Vitoria)
  , testar "resultadoPara filme B (opção B)" (resultadoPara "Bastardos Inglórios" exemplo) (Just Derrota)
  , testar "resultadoPara filme que não participou" (resultadoPara "Ainda Estou Aqui" exemplo) Nothing
  , testar "resultadoPara empate" (resultadoPara "Ainda Estou Aqui" (Votacao 1 "Ainda Estou Aqui" 0 0 "Vitória")) (Just Empate)
  , testar "participou" (map (`participou` exemplo) ["Sonho de Liberdade", "Bastardos Inglórios", "Ainda Estou Aqui"]) [True, True, False]
  , testar "filmes sem repetição e em ordem"
      (filmes premiacao) ["Ainda Estou Aqui", "Bastardos Inglórios", "Como Eu Era Antes de Você", "Sonho de Liberdade", "Toy Story 3", "Vitória"]
  , testar "filmes de lista vazia" (filmes []) []
  , testar "premiação de teste com 15 votações" (length premiacao) 15
  ]

testesCamada2 :: [IO Bool]
testesCamada2 =
  [ testar "pontos" (map pontos [Vitoria, Empate, Derrota]) [3, 1, 0]
  , testar "linhaDoFilme Ainda Estou Aqui" (linhaDoFilme premiacao "Ainda Estou Aqui") (Linha "Ainda Estou Aqui" 5 3 1 1 6 3)
  , testar "linhaDoFilme de quem não participou" (linhaDoFilme premiacao "Oppenheimer") (Linha "Oppenheimer" 0 0 0 0 0 0)
  , testar "pontosLinha e saldo de Sonho de Liberdade"
      (let l = linhaDoFilme premiacao "Sonho de Liberdade" in (pontosLinha l, saldo l)) (9, 6)
  , testar "classificação final (conferida à mão)" (classificacao premiacao) tabelaFinal
  , testar "empate em 9 pontos e 3 vitórias decidido pelo saldo"
      [(filme l, saldo l) | l <- classificacao premiacao, pontosLinha l == 9]
      [("Sonho de Liberdade", 6), ("Como Eu Era Antes de Você", 2), ("Toy Story 3", 1)]
  , testar "empate em pontos, vitórias e saldo: votos pró e depois alfabética"
      (map filme (filter ((== 3) . pontosLinha) (classificacao desempate)))
      ["Cedro", "Lume", "Alvorada"]
  , testar "empate total decidido pela ordem alfabética"
      (map filme (classificacao soAlfabetica)) ["Brisa", "Zênite"]
  , testar "classificação após a 3ª etapa" (classificacao (ateEtapa 3 premiacao)) tabelaEtapa3
  , testar "etapa 3 difere da final" (classificacao (ateEtapa 3 premiacao) /= classificacao premiacao) True
  , testar "ateEtapa 0 não tem votações" (ateEtapa 0 premiacao) []
  , testar "maiorDiferenca" (maiorDiferenca premiacao) (Just exemplo)
  , testar "mediaVotos (33 votos / 15 votações)" (mediaVotos premiacao) (Just 2.2)
  , testar "maiorDiferenca [] é Nothing" (maiorDiferenca []) Nothing
  , testar "mediaVotos [] é Nothing" (mediaVotos []) Nothing
  , testar "classificacao [] é vazia" (classificacao []) []
  , testar "aproveitamento sem participações é 0" (aproveitamento (linhaDoFilme [] "Ainda Estou Aqui")) 0
  , testar "aproveitamento de 50%" (aproveitamento (Linha "Ainda Estou Aqui" 2 1 0 1 2 2)) 50
  ]

testesCamada3 :: [IO Bool]
testesCamada3 =
  [ testar "separar" (separar ';' "1;A;2;;B") ["1", "A", "2", "", "B"]
  , testar "separar texto vazio" (separar ';' "") [""]
  , testar "lerVotacao válida" (lerVotacao 1 "1;Sonho de Liberdade;4;0;Bastardos Inglórios") (Right exemplo)
  , testar "lerVotacao ignora espaços nas pontas" (lerVotacao 1 " 1 ; Sonho de Liberdade ; 4 ; 0 ; Bastardos Inglórios ") (Right exemplo)
  , testar "lerPremiacao válido devolve Right" (fmap length (lerPremiacao dadosPremiacao)) (Right 15)
  , testar "premiação vazia (só comentários)" (lerPremiacao "\n# nada\n   \n") (Right [])
  , testar "CamposInvalidos (4 campos)" (lerVotacao 7 "1;Ainda Estou Aqui;0;Bastardos Inglórios") (Left (CamposInvalidos 7))
  , testar "CamposInvalidos (título vazio)" (lerVotacao 7 "1;Ainda Estou Aqui;0;0;  ") (Left (CamposInvalidos 7))
  , testar "NumeroInvalido (votos \"dois\")" (lerVotacao 2 "1;Ainda Estou Aqui;dois;0;Bastardos Inglórios")
      (Left (NumeroInvalido 2 "votos do filme A"))
  , testar "NumeroInvalido (\"2a\")" (lerVotacao 2 "1;Ainda Estou Aqui;0;2a;Bastardos Inglórios")
      (Left (NumeroInvalido 2 "votos do filme B"))
  , testar "NumeroInvalido (etapa 0)" (lerVotacao 2 "0;Ainda Estou Aqui;1;0;Bastardos Inglórios")
      (Left (NumeroInvalido 2 "etapa"))
  , testar "NumeroInvalido (acima do limite de Int)" (lerVotacao 2 "1;Ainda Estou Aqui;99999999999999999999;0;Bastardos Inglórios")
      (Left (NumeroInvalido 2 "votos do filme A"))
  , testar "VotosNegativos" (lerVotacao 3 "1;Como Eu Era Antes de Você;-1;0;Bastardos Inglórios") (Left (VotosNegativos 3))
  , testar "VotosNegativos no menor Int"
      (lerVotacao 3 ("1;Como Eu Era Antes de Você;" ++ show (minBound :: Int) ++ ";0;Bastardos Inglórios"))
      (Left (VotosNegativos 3))
  , testar "MesmoFilme" (lerVotacao 4 "1;Bastardos Inglórios;1;0;Bastardos Inglórios") (Left (MesmoFilme 4))
  , testar "FilmeRepetidoNaEtapa"
      (lerPremiacao (unlines ["1;Ainda Estou Aqui;2;0;Bastardos Inglórios", "1;Como Eu Era Antes de Você;1;1;Ainda Estou Aqui"]))
      (Left (FilmeRepetidoNaEtapa 1 "Ainda Estou Aqui"))
  , testar "mesmo filme em etapas diferentes é válido"
      (fmap length (lerPremiacao (unlines ["1;Ainda Estou Aqui;2;0;Bastardos Inglórios", "2;Como Eu Era Antes de Você;1;1;Ainda Estou Aqui"]))) (Right 2)
  , testar "lerPremiacao para no primeiro erro (número da linha original)"
      (lerPremiacao dadosComErros) (Left (NumeroInvalido 2 "votos do filme A"))
  , testar "todosOsErros lista todos"
      (todosOsErros dadosComErros)
      [ NumeroInvalido 2 "votos do filme A", VotosNegativos 3, MesmoFilme 4
      , CamposInvalidos 5, FilmeRepetidoNaEtapa 3 "Ainda Estou Aqui" ]
  , testar "todosOsErros de entrada válida é vazio" (todosOsErros dadosPremiacao) []
  , testar "mensagemErro indica a linha" (mensagemErro (VotosNegativos 3))
      "Linha 3: os votos não podem ser negativos."
  , testar "mensagemErro indica a etapa" (mensagemErro (FilmeRepetidoNaEtapa 3 "Ainda Estou Aqui"))
      "Etapa 3: Ainda Estou Aqui aparece em mais de uma votação."
  , testar "votacaoFormatada" (votacaoFormatada exemplo) "Etapa 1: Sonho de Liberdade 4 x 0 Bastardos Inglórios"
  , testar "tabelaFormatada vazia" (tabelaFormatada []) "Nenhuma votação registrada.\n"
  , testar "tabelaFormatada tem cabeçalho e 6 linhas"
      (length (lines (tabelaFormatada (classificacao premiacao)))) 7
  ]

main :: IO ()
main = do
  putStrLn "--- Camada 1 ---"
  r1 <- sequence testesCamada1
  putStrLn "--- Camada 2 ---"
  r2 <- sequence testesCamada2
  putStrLn "--- Camada 3 ---"
  r3 <- sequence testesCamada3
  let resultados = r1 ++ r2 ++ r3
      falhas = length (filter not resultados)
  putStrLn ""
  putStrLn (show (length resultados - falhas) ++ "/" ++ show (length resultados) ++ " testes passaram.")


