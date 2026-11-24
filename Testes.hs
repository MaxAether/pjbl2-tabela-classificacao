{-# OPTIONS_GHC -Wall #-}
-- PjBL2 - Tabela de Classificação (Oscar fictício)
-- Integrantes: Richard M. V. Ludke e Carolina Reis
-- TESTES. Mesmas seções do Main.hs (copiadas sem mudança), seguidas dos testes.
-- Cole o arquivo inteiro no playground e clique em Run.

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, nub, sort, sortBy)
-- foldl' qualificado (aula 12): do GHC 9.10 em diante ele já vem no Prelude, e o
-- import comum geraria aviso no -Wall; assim compila sem aviso em qualquer versão.
import qualified Data.List as L (foldl')
import Data.Ord (Down(..), comparing)
import Text.Read (readMaybe)

-- =====================================================================
-- Camada 1: tipos e consultas básicas
-- Cada partida é um duelo entre dois filmes, decidido pelos votos do júri.
-- =====================================================================

-- Equivale a "type Time = String" do enunciado.
type Filme = String

data Resultado = Vitoria | Empate | Derrota
  deriving (Show, Eq)

-- Equivale a Partida do enunciado: filmeA = mandante, filmeB = visitante.
data Partida = Partida
  { rodada :: Int
  , filmeA :: Filme
  , votosA :: Int
  , votosB :: Int
  , filmeB :: Filme
  } deriving (Show, Eq)

-- Uma linha da tabela. votosPro/votosContra equivalem a gols pró/contra.
data Linha = Linha
  { filme       :: Filme
  , jogos       :: Int
  , vitorias    :: Int
  , empates     :: Int
  , derrotas    :: Int
  , votosPro    :: Int
  , votosContra :: Int
  } deriving (Show, Eq)

participou :: Filme -> Partida -> Bool
participou titulo partida = titulo == filmeA partida || titulo == filmeB partida

resultadoPara :: Filme -> Partida -> Maybe Resultado
resultadoPara titulo partida
  | titulo == filmeA partida = Just (comparar (votosA partida) (votosB partida))
  | titulo == filmeB partida = Just (comparar (votosB partida) (votosA partida))
  | otherwise                = Nothing
  where
    comparar :: Int -> Int -> Resultado
    comparar meus deles = case compare meus deles of
      GT -> Vitoria
      EQ -> Empate
      LT -> Derrota

-- Todos os filmes, sem repetição e em ordem alfabética.
filmes :: [Partida] -> [Filme]
filmes partidas = sort (nub (map filmeA partidas ++ map filmeB partidas))

-- =====================================================================
-- Camada 2: classificação com funções de ordem superior
-- Nenhuma função desta seção usa recursão explícita.
-- =====================================================================

pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

-- Linha de um filme: filtra as partidas dele e acumula com L.foldl',
-- como o saldoFinal do extrato (acumulado primeiro, partida depois).
linhaDoFilme :: [Partida] -> Filme -> Linha
linhaDoFilme partidas titulo =
  L.foldl' somar (Linha titulo 0 0 0 0 0 0) (filter (participou titulo) partidas)
  where
    somar :: Linha -> Partida -> Linha
    somar linha partida =
      maybe linha (registrar linha partida) (resultadoPara titulo partida)

    registrar :: Linha -> Partida -> Resultado -> Linha
    registrar linha partida res = linha
      { jogos       = jogos linha + 1
      , vitorias    = vitorias linha + contar Vitoria
      , empates     = empates linha + contar Empate
      , derrotas    = derrotas linha + contar Derrota
      , votosPro    = votosPro linha + meus
      , votosContra = votosContra linha + deles
      }
      where
        contar :: Resultado -> Int
        contar r = if r == res then 1 else 0

        ehFilmeA :: Bool
        ehFilmeA = titulo == filmeA partida

        meus, deles :: Int
        meus  = if ehFilmeA then votosA partida else votosB partida
        deles = if ehFilmeA then votosB partida else votosA partida

pontosLinha :: Linha -> Int
pontosLinha linha = pontos Vitoria * vitorias linha + pontos Empate * empates linha

saldo :: Linha -> Int
saldo linha = votosPro linha - votosContra linha

-- Percentual dos pontos disputados; fromIntegral faz a ponte Int -> Double.
aproveitamento :: Linha -> Double
aproveitamento linha
  | jogos linha == 0 = 0
  | otherwise        = 100 * fromIntegral (pontosLinha linha)
                           / fromIntegral (pontos Vitoria * jogos linha)

-- Critérios: pontos, vitórias, saldo, votos pró (todos decrescentes)
-- e, por fim, ordem alfabética. Uma única comparação combinada com <>.
classificacao :: [Partida] -> [Linha]
classificacao partidas = sortBy criterios (map (linhaDoFilme partidas) (filmes partidas))
  where
    criterios :: Linha -> Linha -> Ordering
    criterios = comparing (Down . pontosLinha)
             <> comparing (Down . vitorias)
             <> comparing (Down . saldo)
             <> comparing (Down . votosPro)
             <> comparing filme

ateRodada :: Int -> [Partida] -> [Partida]
ateRodada limite = filter ((<= limite) . rodada)

diferenca :: Partida -> Int
diferenca partida = abs (votosA partida - votosB partida)

-- L.foldl' começando de Nothing: total, sem maximum nem head.
-- Em caso de empate na diferença, fica a primeira partida da lista.
maiorDiferenca :: [Partida] -> Maybe Partida
maiorDiferenca = L.foldl' escolher Nothing
  where
    escolher :: Maybe Partida -> Partida -> Maybe Partida
    escolher Nothing partida = Just partida
    escolher (Just maior) partida
      | diferenca partida > diferenca maior = Just partida
      | otherwise                           = Just maior

mediaVotos :: [Partida] -> Maybe Double
mediaVotos []       = Nothing
mediaVotos partidas = Just (fromIntegral total / fromIntegral (length partidas))
  where
    total :: Int
    total = sum (map (\p -> votosA p + votosB p) partidas)

-- =====================================================================
-- Camada 3: leitura, validação (Maybe e Either) e formatação
-- Tudo aqui é puro; só o main faz IO.
-- =====================================================================

-- Linha do texto onde o erro aconteceu ou rodada do conflito.
data Erro
  = CamposInvalidos Int                -- linha sem 5 campos ou com título vazio
  | NumeroInvalido Int String          -- linha e nome do campo ("dois", "2a", rodada <= 0)
  | PlacarNegativo Int                 -- linha com votos negativos
  | MesmoFilme Int                     -- linha em que o filme enfrenta a si mesmo
  | FilmeRepetidoNaRodada Int Filme    -- rodada e filme escalado duas vezes
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

-- Converte Maybe em Either, acrescentando o motivo do erro (como na aula 14).
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
      | abs n <= toInteger (maxBound :: Int) = Just (fromInteger n)
      | otherwise                            = Nothing

-- Bloco do na mônada Either: o primeiro Left interrompe a leitura.
lerPartida :: Int -> String -> Either Erro Partida
lerPartida numLinha texto =
  case map aparar (separar ';' texto) of
    [r, a, va, vb, b] -> do
      rod   <- lerNumero numLinha "rodada" r
      votA  <- lerNumero numLinha "votos do filme A" va
      votB  <- lerNumero numLinha "votos do filme B" vb
      validar (Partida rod a votA votB b)
    _ -> Left (CamposInvalidos numLinha)
  where
    validar :: Partida -> Either Erro Partida
    validar p
      | null (filmeA p) || null (filmeB p) = Left (CamposInvalidos numLinha)
      | rodada p <= 0                      = Left (NumeroInvalido numLinha "rodada")
      | votosA p < 0 || votosB p < 0       = Left (PlacarNegativo numLinha)
      | filmeA p == filmeB p               = Left (MesmoFilme numLinha)
      | otherwise                          = Right p

-- Linhas numeradas a partir de 1, sem as vazias e os comentários (#).
linhasUteis :: String -> [(Int, String)]
linhasUteis texto = [(n, l) | (n, l) <- zip [1 ..] (lines texto), util (aparar l)]
  where
    util :: String -> Bool
    util l = not (null l) && take 1 l /= "#"

-- Filmes escalados mais de uma vez na mesma rodada.
conflitos :: [Partida] -> [Erro]
conflitos partidas =
  [ FilmeRepetidoNaRodada r f
  | (r, f) <- nub escalacoes
  , length (filter (== (r, f)) escalacoes) > 1
  ]
  where
    escalacoes :: [(Int, Filme)]
    escalacoes = concatMap (\p -> [(rodada p, filmeA p), (rodada p, filmeB p)]) partidas

verificarRodadas :: [Partida] -> Either Erro [Partida]
verificarRodadas partidas = case conflitos partidas of
  []         -> Right partidas
  (erro : _) -> Left erro

-- traverse para no primeiro erro de leitura; depois confere as rodadas.
lerCampeonato :: String -> Either Erro [Partida]
lerCampeonato texto = do
  partidas <- traverse (uncurry lerPartida) (linhasUteis texto)
  verificarRodadas partidas

-- Todos os erros: os de cada linha e os conflitos entre as linhas válidas.
todosOsErros :: String -> [Erro]
todosOsErros texto = [erro | Left erro <- lidas] ++ conflitos [p | Right p <- lidas]
  where
    lidas :: [Either Erro Partida]
    lidas = map (uncurry lerPartida) (linhasUteis texto)

mensagemErro :: Erro -> String
mensagemErro (CamposInvalidos n)   = "Linha " ++ show n ++ ": esperados 5 campos com dois títulos."
mensagemErro (NumeroInvalido n c)  = "Linha " ++ show n ++ ": " ++ c ++ " inválido."
mensagemErro (PlacarNegativo n)    = "Linha " ++ show n ++ ": os votos não podem ser negativos."
mensagemErro (MesmoFilme n)        = "Linha " ++ show n ++ ": um filme não pode enfrentar a si mesmo."
mensagemErro (FilmeRepetidoNaRodada r f) =
  "Rodada " ++ show r ++ ": " ++ f ++ " aparece em mais de uma partida."

partidaFormatada :: Partida -> String
partidaFormatada p =
  "Rodada " ++ show (rodada p) ++ ": " ++ filmeA p ++ " " ++ show (votosA p)
    ++ " x " ++ show (votosB p) ++ " " ++ filmeB p

tabelaFormatada :: [Linha] -> String
tabelaFormatada [] = "Nenhuma partida registrada.\n"
tabelaFormatada linhas = unlines (map (intercalate " | ") (cabecalho : corpo))
  where
    largura :: Int
    largura = foldr (max . length . filme) (length "Filme") linhas

    cabecalho :: [String]
    cabecalho = [esquerda 3 "Pos", esquerda largura "Filme", "Pts", "  J", "  V", "  E", "  D", "Saldo"]

    corpo :: [[String]]
    corpo = [formatar pos linha | (pos, linha) <- zip [1 ..] linhas]

    formatar :: Int -> Linha -> [String]
    formatar pos linha =
      [ esquerda 3 (show pos ++ "º")
      , esquerda largura (filme linha)
      ] ++ map (direita 3 . show)
          [pontosLinha linha, jogos linha, vitorias linha, empates linha, derrotas linha]
        ++ [direita 5 (comSinal (saldo linha))]

    comSinal :: Int -> String
    comSinal s = if s > 0 then '+' : show s else show s

    esquerda, direita :: Int -> String -> String
    esquerda n s = s ++ espacos n s
    direita  n s = espacos n s ++ s

    espacos :: Int -> String -> String
    espacos n s = replicate (n - length s) ' '

-- =====================================================================
-- Dados de teste, como String (o playground não tem teclado)
-- 6 filmes, 5 rodadas, 15 partidas (todos contra todos, uma vez).
-- =====================================================================

dadosCampeonato :: String
dadosCampeonato =
  unlines
    [ "# rodada;filme A;votos A;votos B;filme B"
    , "1;Ainda Estou Aqui;0;0;Vitória"                       -- empate em 0 x 0
    , "1;Como Eu Era Antes de Você;2;1;Toy Story 3"
    , "1;Sonho de Liberdade;4;0;Bastardos Inglórios"         -- vitória por 4 de diferença
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
    , "1;Como Eu Era Antes de Você;-2;1;Bastardos Inglórios"  -- linha 3: PlacarNegativo
    , "2;Sonho de Liberdade;1;1;Sonho de Liberdade"           -- linha 4: MesmoFilme
    , "2;Toy Story 3;3;Ainda Estou Aqui"                      -- linha 5: CamposInvalidos
    , "3;Ainda Estou Aqui;1;0;Bastardos Inglórios"
    , "3;Como Eu Era Antes de Você;2;2;Ainda Estou Aqui"      -- Ainda Estou Aqui duas vezes na rodada 3
    ]

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

-- Campeonato de teste lido de dadosCampeonato ([] se houver erro: os testes falham).
campeonato :: [Partida]
campeonato = either (const []) id (lerCampeonato dadosCampeonato)

exemplo :: Partida
exemplo = Partida 1 "Sonho de Liberdade" 4 0 "Bastardos Inglórios"

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

tabelaRodada3 :: [Linha]
tabelaRodada3 =
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
desempate :: [Partida]
desempate =
  [ Partida 1 "Lume" 3 2 "Duna"
  , Partida 1 "Alvorada" 2 1 "Brisa"
  , Partida 1 "Cedro" 3 2 "Fagulha"
  ]

-- Mesmos pontos, vitórias, saldo e votos pró: só a ordem alfabética decide.
soAlfabetica :: [Partida]
soAlfabetica = [Partida 1 "Zênite" 1 1 "Brisa"]

testesCamada1 :: [IO Bool]
testesCamada1 =
  [ testar "resultadoPara filme A (mandante)" (resultadoPara "Sonho de Liberdade" exemplo) (Just Vitoria)
  , testar "resultadoPara filme B (visitante)" (resultadoPara "Bastardos Inglórios" exemplo) (Just Derrota)
  , testar "resultadoPara filme que não jogou" (resultadoPara "Ainda Estou Aqui" exemplo) Nothing
  , testar "resultadoPara empate" (resultadoPara "Ainda Estou Aqui" (Partida 1 "Ainda Estou Aqui" 0 0 "Vitória")) (Just Empate)
  , testar "participou" (map (`participou` exemplo) ["Sonho de Liberdade", "Bastardos Inglórios", "Ainda Estou Aqui"]) [True, True, False]
  , testar "filmes sem repetição e em ordem"
      (filmes campeonato) ["Ainda Estou Aqui", "Bastardos Inglórios", "Como Eu Era Antes de Você", "Sonho de Liberdade", "Toy Story 3", "Vitória"]
  , testar "filmes de lista vazia" (filmes []) []
  , testar "campeonato de teste com 15 partidas" (length campeonato) 15
  ]

testesCamada2 :: [IO Bool]
testesCamada2 =
  [ testar "pontos" (map pontos [Vitoria, Empate, Derrota]) [3, 1, 0]
  , testar "linhaDoFilme Ainda Estou Aqui" (linhaDoFilme campeonato "Ainda Estou Aqui") (Linha "Ainda Estou Aqui" 5 3 1 1 6 3)
  , testar "linhaDoFilme de quem não jogou" (linhaDoFilme campeonato "Oppenheimer") (Linha "Oppenheimer" 0 0 0 0 0 0)
  , testar "pontosLinha e saldo de Sonho de Liberdade"
      (let l = linhaDoFilme campeonato "Sonho de Liberdade" in (pontosLinha l, saldo l)) (9, 6)
  , testar "classificação final (conferida à mão)" (classificacao campeonato) tabelaFinal
  , testar "empate em 9 pontos e 3 vitórias decidido pelo saldo"
      [(filme l, saldo l) | l <- classificacao campeonato, pontosLinha l == 9]
      [("Sonho de Liberdade", 6), ("Como Eu Era Antes de Você", 2), ("Toy Story 3", 1)]
  , testar "empate em pontos, vitórias e saldo: votos pró e depois alfabética"
      (map filme (filter ((== 3) . pontosLinha) (classificacao desempate)))
      ["Cedro", "Lume", "Alvorada"]
  , testar "empate total decidido pela ordem alfabética"
      (map filme (classificacao soAlfabetica)) ["Brisa", "Zênite"]
  , testar "classificação após a 3ª rodada" (classificacao (ateRodada 3 campeonato)) tabelaRodada3
  , testar "rodada 3 difere da final" (classificacao (ateRodada 3 campeonato) /= classificacao campeonato) True
  , testar "ateRodada 0 não tem partidas" (ateRodada 0 campeonato) []
  , testar "maiorDiferenca" (maiorDiferenca campeonato) (Just exemplo)
  , testar "mediaVotos (33 votos / 15 partidas)" (mediaVotos campeonato) (Just 2.2)
  , testar "maiorDiferenca [] é Nothing" (maiorDiferenca []) Nothing
  , testar "mediaVotos [] é Nothing" (mediaVotos []) Nothing
  , testar "classificacao [] é vazia" (classificacao []) []
  , testar "aproveitamento sem jogos é 0" (aproveitamento (linhaDoFilme [] "Ainda Estou Aqui")) 0
  , testar "aproveitamento de 50%" (aproveitamento (Linha "Ainda Estou Aqui" 2 1 0 1 2 2)) 50
  ]

testesCamada3 :: [IO Bool]
testesCamada3 =
  [ testar "separar" (separar ';' "1;A;2;;B") ["1", "A", "2", "", "B"]
  , testar "separar texto vazio" (separar ';' "") [""]
  , testar "lerPartida válida" (lerPartida 1 "1;Sonho de Liberdade;4;0;Bastardos Inglórios") (Right exemplo)
  , testar "lerPartida ignora espaços nas pontas" (lerPartida 1 " 1 ; Sonho de Liberdade ; 4 ; 0 ; Bastardos Inglórios ") (Right exemplo)
  , testar "lerCampeonato válido devolve Right" (fmap length (lerCampeonato dadosCampeonato)) (Right 15)
  , testar "campeonato vazio (só comentários)" (lerCampeonato "\n# nada\n   \n") (Right [])
  , testar "CamposInvalidos (4 campos)" (lerPartida 7 "1;Ainda Estou Aqui;0;Bastardos Inglórios") (Left (CamposInvalidos 7))
  , testar "CamposInvalidos (título vazio)" (lerPartida 7 "1;Ainda Estou Aqui;0;0;  ") (Left (CamposInvalidos 7))
  , testar "NumeroInvalido (placar \"dois\")" (lerPartida 2 "1;Ainda Estou Aqui;dois;0;Bastardos Inglórios")
      (Left (NumeroInvalido 2 "votos do filme A"))
  , testar "NumeroInvalido (\"2a\")" (lerPartida 2 "1;Ainda Estou Aqui;0;2a;Bastardos Inglórios")
      (Left (NumeroInvalido 2 "votos do filme B"))
  , testar "NumeroInvalido (rodada 0)" (lerPartida 2 "0;Ainda Estou Aqui;1;0;Bastardos Inglórios")
      (Left (NumeroInvalido 2 "rodada"))
  , testar "NumeroInvalido (acima do limite de Int)" (lerPartida 2 "1;Ainda Estou Aqui;99999999999999999999;0;Bastardos Inglórios")
      (Left (NumeroInvalido 2 "votos do filme A"))
  , testar "PlacarNegativo" (lerPartida 3 "1;Como Eu Era Antes de Você;-1;0;Bastardos Inglórios") (Left (PlacarNegativo 3))
  , testar "MesmoFilme" (lerPartida 4 "1;Bastardos Inglórios;1;0;Bastardos Inglórios") (Left (MesmoFilme 4))
  , testar "FilmeRepetidoNaRodada"
      (lerCampeonato (unlines ["1;Ainda Estou Aqui;2;0;Bastardos Inglórios", "1;Como Eu Era Antes de Você;1;1;Ainda Estou Aqui"]))
      (Left (FilmeRepetidoNaRodada 1 "Ainda Estou Aqui"))
  , testar "mesmo filme em rodadas diferentes é válido"
      (fmap length (lerCampeonato (unlines ["1;Ainda Estou Aqui;2;0;Bastardos Inglórios", "2;Como Eu Era Antes de Você;1;1;Ainda Estou Aqui"]))) (Right 2)
  , testar "lerCampeonato para no primeiro erro (número da linha original)"
      (lerCampeonato dadosComErros) (Left (NumeroInvalido 2 "votos do filme A"))
  , testar "todosOsErros lista todos"
      (todosOsErros dadosComErros)
      [ NumeroInvalido 2 "votos do filme A", PlacarNegativo 3, MesmoFilme 4
      , CamposInvalidos 5, FilmeRepetidoNaRodada 3 "Ainda Estou Aqui" ]
  , testar "todosOsErros de entrada válida é vazio" (todosOsErros dadosCampeonato) []
  , testar "mensagemErro indica a linha" (mensagemErro (PlacarNegativo 3))
      "Linha 3: os votos não podem ser negativos."
  , testar "mensagemErro indica a rodada" (mensagemErro (FilmeRepetidoNaRodada 3 "Ainda Estou Aqui"))
      "Rodada 3: Ainda Estou Aqui aparece em mais de uma partida."
  , testar "partidaFormatada" (partidaFormatada exemplo) "Rodada 1: Sonho de Liberdade 4 x 0 Bastardos Inglórios"
  , testar "tabelaFormatada vazia" (tabelaFormatada []) "Nenhuma partida registrada.\n"
  , testar "tabelaFormatada tem cabeçalho e 6 linhas"
      (length (lines (tabelaFormatada (classificacao campeonato)))) 7
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
