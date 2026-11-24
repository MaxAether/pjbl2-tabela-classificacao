{-# OPTIONS_GHC -Wall #-}
-- PjBL2 - Tabela de Classificação (Oscar fictício)
-- Integrantes: Richard M. V. Ludke e Carolina Reis
-- Ambiente: Haskell Playground (play.haskell.org). Cole o arquivo inteiro e clique em Run.

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
-- Main: demonstração das três camadas
-- main é a única função com IO.
-- =====================================================================

main :: IO ()
main = do
  putStrLn "=== Oscar fictício: classificação final ==="
  case lerCampeonato dadosCampeonato of
    Left erro      -> putStrLn ("Erro nos dados: " ++ mensagemErro erro)
    Right partidas -> do
      putStr (tabelaFormatada (classificacao partidas))

      putStrLn "\n=== Classificação após a 3ª rodada ==="
      putStr (tabelaFormatada (classificacao (ateRodada 3 partidas)))

      putStrLn "\n=== Estatísticas ==="
      putStrLn ("Maior diferença: " ++ maybe "sem partidas" partidaFormatada (maiorDiferenca partidas))
      putStrLn ("Média de votos por partida: " ++ maybe "sem partidas" show (mediaVotos partidas))
      mapM_ (putStrLn . descreverAproveitamento) (classificacao partidas)

  putStrLn "\n=== Entrada com erros ==="
  putStrLn ("lerCampeonato para no primeiro: " ++ either mensagemErro (const "aceita") (lerCampeonato dadosComErros))
  putStrLn "todosOsErros lista todos:"
  mapM_ (putStrLn . ("  " ++) . mensagemErro) (todosOsErros dadosComErros)

  putStrLn "\n=== Campeonato vazio ==="
  putStr (tabelaFormatada (classificacao []))
  putStrLn ("Maior diferença: " ++ maybe "sem partidas" partidaFormatada (maiorDiferenca []))
  where
    descreverAproveitamento :: Linha -> String
    descreverAproveitamento linha =
      filme linha ++ ": " ++ show (umaCasa (aproveitamento linha)) ++ "% de aproveitamento"

    umaCasa :: Double -> Double
    umaCasa x = fromIntegral (round (x * 10) :: Int) / 10
