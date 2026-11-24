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
-- Cada votação compara dois filmes pelos votos recebidos do júri.
-- =====================================================================

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
  { filme       :: Filme
  , participacoes       :: Int
  , vitorias    :: Int
  , empates     :: Int
  , derrotas    :: Int
  , votosPro    :: Int
  , votosContra :: Int
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

-- =====================================================================
-- Camada 2: classificação com funções de ordem superior
-- Nenhuma função desta seção usa recursão explícita.
-- =====================================================================

pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

-- Linha de um filme: filtra as votações dele e acumula com L.foldl',
-- como o saldoFinal do extrato (acumulado primeiro, votação depois).
linhaDoFilme :: [Votacao] -> Filme -> Linha
linhaDoFilme votacoes titulo =
  L.foldl' somar (Linha titulo 0 0 0 0 0 0) (filter (participou titulo) votacoes)
  where
    somar :: Linha -> Votacao -> Linha
    somar linha votacao =
      maybe linha (registrar linha votacao) (resultadoPara titulo votacao)

    registrar :: Linha -> Votacao -> Resultado -> Linha
    registrar linha votacao res = linha
      { participacoes       = participacoes linha + 1
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
  | otherwise        = 100 * fromIntegral (pontosLinha linha)
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

-- =====================================================================
-- Camada 3: leitura, validação (Maybe e Either) e formatação
-- Tudo aqui é puro; só o main faz IO.
-- =====================================================================

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
lerVotacao :: Int -> String -> Either Erro Votacao
lerVotacao numLinha texto =
  case map aparar (separar ';' texto) of
    [r, a, va, vb, b] -> do
      numEtapa   <- lerNumero numLinha "rodada" r
      votA  <- lerNumero numLinha "votos do filme A" va
      votB  <- lerNumero numLinha "votos do filme B" vb
      validar (Votacao numEtapa a votA votB b)
    _ -> Left (CamposInvalidos numLinha)
  where
    validar :: Votacao -> Either Erro Votacao
    validar p
      | null (filmeA p) || null (filmeB p) = Left (CamposInvalidos numLinha)
      | etapa p <= 0                      = Left (NumeroInvalido numLinha "rodada")
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
mensagemErro (MesmoFilme n)        = "Linha " ++ show n ++ ": um filme não pode enfrentar a si mesmo."
mensagemErro (FilmeRepetidoNaEtapa r f) =
  "Rodada " ++ show r ++ ": " ++ f ++ " aparece em mais de uma partida."

votacaoFormatada :: Votacao -> String
votacaoFormatada p =
  "Rodada " ++ show (etapa p) ++ ": " ++ filmeA p ++ " " ++ show (votosA p)
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
          [pontosLinha linha, participacoes linha, vitorias linha, empates linha, derrotas linha]
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
-- 6 filmes, 5 etapas, 15 votações (cada par de filmes é avaliado uma vez).
-- =====================================================================

dadosPremiacao :: String
dadosPremiacao =
  unlines
    [ "# rodada;filme A;votos A;votos B;filme B"
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

-- =====================================================================
-- Main: demonstração das três camadas
-- main é a única função com IO.
-- =====================================================================

main :: IO ()
main = do
  putStrLn "=== Oscar fictício: classificação final ==="
  case lerPremiacao dadosPremiacao of
    Left erro      -> putStrLn ("Erro nos dados: " ++ mensagemErro erro)
    Right votacoes -> do
      putStr (tabelaFormatada (classificacao votacoes))

      putStrLn "\n=== Classificação após a 3ª rodada ==="
      putStr (tabelaFormatada (classificacao (ateEtapa 3 votacoes)))

      putStrLn "\n=== Estatísticas ==="
      putStrLn ("Maior diferença: " ++ maybe "sem partidas" votacaoFormatada (maiorDiferenca votacoes))
      putStrLn ("Média de votos por partida: " ++ maybe "sem partidas" show (mediaVotos votacoes))
      mapM_ (putStrLn . descreverAproveitamento) (classificacao votacoes)

  putStrLn "\n=== Entrada com erros ==="
  putStrLn ("lerCampeonato para no primeiro: " ++ either mensagemErro (const "aceita") (lerPremiacao dadosComErros))
  putStrLn "todosOsErros lista todos:"
  mapM_ (putStrLn . ("  " ++) . mensagemErro) (todosOsErros dadosComErros)

  putStrLn "\n=== Campeonato vazio ==="
  putStr (tabelaFormatada (classificacao []))
  putStrLn ("Maior diferença: " ++ maybe "sem partidas" votacaoFormatada (maiorDiferenca []))
  where
    descreverAproveitamento :: Linha -> String
    descreverAproveitamento linha =
      filme linha ++ ": " ++ show (umaCasa (aproveitamento linha)) ++ "% de aproveitamento"

    umaCasa :: Double -> Double
    umaCasa x = fromIntegral (round (x * 10) :: Int) / 10
