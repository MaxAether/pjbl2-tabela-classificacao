{-# OPTIONS_GHC -Wall #-}
module Dados where

-- Dados fictícios: 6 filmes, 5 etapas e 15 votações.
-- Cada par de filmes é avaliado uma vez.

dadosPremiacao :: String
dadosPremiacao =
  unlines
    [ "# etapa;filme A;votos A;votos B;filme B"
    , "1;Ainda Estou Aqui;0;0;Vitória"                       -- empate sem votos para nenhum dos filmes
    , "1;Como Eu Era Antes de Você;2;1;Toy Story 3"
    , "1;Sonho de Liberdade;4;0;Bastardos Inglórios"         -- filme escolhido com 4 votos de diferença
    , ""
    , "2;Ainda Estou Aqui;2;0;Toy Story 3"
    , "2;Vitória;1;1;Bastardos Inglórios"
    , "2;Como Eu Era Antes de Você;0;1;Sonho de Liberdade"
    , ""
    , "3;Ainda Estou Aqui;1;0;Bastardos Inglórios"
    , "3;Toy Story 3;2;1;Sonho de Liberdade"
    , "3;Vitória;0;2;Como Eu Era Antes de Você"
    , ""
    , "4;Ainda Estou Aqui;2;1;Sonho de Liberdade"
    , "4;Bastardos Inglórios;1;0;Como Eu Era Antes de Você"
    , "4;Toy Story 3;2;0;Vitória"
    , ""
    , "5;Ainda Estou Aqui;1;2;Como Eu Era Antes de Você"
    , "5;Sonho de Liberdade;3;0;Vitória"
    , "5;Bastardos Inglórios;1;2;Toy Story 3"
    ]

-- Uma linha inválida para cada construtor de Erro.
dadosComErros :: String
dadosComErros =
  unlines
    [ "# entrada com erros de digitação"
    , "1;Ainda Estou Aqui;dois;0;Vitória"                     -- linha 2: NumeroInvalido
    , "1;Como Eu Era Antes de Você;-2;1;Bastardos Inglórios"  -- linha 3: VotosNegativos
    , "2;Sonho de Liberdade;1;1;Sonho de Liberdade"           -- linha 4: MesmoFilme
    , "2;Toy Story 3;3;Ainda Estou Aqui"                      -- linha 5: CamposInvalidos
    , "3;Ainda Estou Aqui;1;0;Bastardos Inglórios"
    , "3;Como Eu Era Antes de Você;2;2;Ainda Estou Aqui"      -- Ainda Estou Aqui duas vezes na etapa 3
    ]

