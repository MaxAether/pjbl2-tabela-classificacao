# Decisões de modelagem

## O campeonato

Competição fictícia inspirada no Oscar: seis filmes, cinco rodadas, cada filme
enfrenta todos os outros uma vez (15 partidas, nenhum filme joga duas vezes na
mesma rodada). Em cada partida o júri vota; quem tem mais votos vence.
Vitória vale 3 pontos, empate 1 e derrota 0.

### Correspondência com o enunciado

O enunciado permite trocar o esporte, desde que a quantidade e a relação das
funções se mantenham. Os nomes que mudaram foram só os que citam futebol:

| Enunciado (futebol) | Este projeto |
| --- | --- |
| `type Time = String` | `type Filme = String` |
| `mandante`, `visitante` | `filmeA`, `filmeB` |
| `golsMandante`, `golsVisitante` | `votosA`, `votosB` |
| gols pró / gols contra (`Linha`) | `votosPro` / `votosContra` |
| `times` | `filmes` |
| `linhaDoTime` | `linhaDoFilme` |
| `maiorGoleada` | `maiorDiferenca` |
| `mediaGols` | `mediaVotos` |
| `MesmoTime`, `TimeRepetidoNaRodada` | `MesmoFilme`, `FilmeRepetidoNaRodada` |

Todos os outros nomes (`Partida`, `Resultado`, `Linha`, `participou`,
`resultadoPara`, `pontos`, `pontosLinha`, `saldo`, `aproveitamento`,
`classificacao`, `ateRodada`, `Erro`, `separar`, `lerPartida`, `lerCampeonato`,
`todosOsErros`, `tabelaFormatada`, `main`) são os do enunciado.

### Critérios de classificação

Na ordem, inspirados no Campeonato Brasileiro:

1. pontos (maior primeiro);
2. número de vitórias;
3. saldo de votos (votos pró − votos contra);
4. votos pró;
5. ordem alfabética do título (garante uma ordem total, sem sorteio).

## Camada 1: tipos

- **`Partida` e `Linha` são records**, não tuplas. Com `(Int, String, Int, Int, String)`
  seria fácil trocar `votosA` com `votosB` sem o compilador perceber; com nomes de
  campo isso não acontece.
- **`Resultado` é um tipo soma** (`Vitoria | Empate | Derrota`), não `String` nem `Int`.
  Um `"v"` minúsculo não compila como `Resultado`, e o `-Wall` avisa se algum caso
  ficar sem tratamento (por exemplo, em `pontos`).
- `resultadoPara` devolve `Maybe Resultado`: `Nothing` quando o filme não jogou,
  em vez de um valor inventado. Usa `compare` + `case` sobre `Ordering` (Aula 12).
- `Linha` guarda só contagens (jogos, V, E, D, votos pró e contra). Pontos e saldo
  são **derivados** (`pontosLinha`, `saldo`), assim nunca ficam inconsistentes com
  as contagens.

## Camada 2: classificação

- **Sem recursão explícita.** Usamos `map` (linhas de cada filme), `filter`
  (`ateRodada` e as partidas de um filme), dobra (`foldl'`) e composição `(.)`
  (`(<= limite) . rodada`, `Down . pontosLinha`).
- **`linhaDoFilme` usa `foldl'`.** A linha é um acumulador que começa zerado e é
  atualizado partida a partida, na ordem cronológica, igual ao `saldoFinal` do
  extrato (Aula 11): o acumulado vem primeiro em `somar linha partida`. Escolhemos
  `foldl'` (e não `foldr`) porque o resultado é um valor acumulado, não uma lista;
  e `foldl'` em vez de `foldl` porque calcula o acumulador a cada passo, sem guardar
  contas pendentes (regra prática da Aula 12).
  Ele é importado qualificado (`import qualified Data.List as L`, aula 12) e usado
  como `L.foldl'`: do GHC 9.10 em diante o `foldl'` já vem no Prelude, e o import
  comum geraria um aviso no `-Wall`. Assim o código compila sem aviso tanto nas
  versões antigas quanto nas novas do playground.
- **Ordenação com uma única comparação**: `sortBy` com `comparing` combinado por
  `<>` (`Ordering` é um Monoid) e `Down` para ordem decrescente. Nada de vários
  `sortBy` encadeados nem ordenação escrita à mão.
- **`aproveitamento`** usa `fromIntegral` para converter os `Int` em `Double` antes
  de dividir com `/` (com `div` o resultado seria truncado). Devolve 0 se não houve jogos.
- **Funções totais**: `maiorDiferenca` é um `foldl'` que começa em `Nothing`
  (sem `maximum`, `head` ou `foldr1`, que quebram em lista vazia); `mediaVotos`
  trata `[]` com `Nothing`. Se duas partidas têm a mesma diferença máxima, fica a
  primeira da lista.

## Camada 3: leitura e validação

- **`separar`** é um `foldr`: começa com um campo vazio e cada separador abre um
  campo novo. `"1;A;;B"` vira `["1","A","","B"]` (campos vazios são preservados,
  para que a contagem de campos detecte erros).
- **`readMaybe`, nunca `read`.** `"dois"` ou `"2a"` viram `Nothing` em vez de
  derrubar o programa. O valor é lido como `Integer` e `>>=` (Maybe como mônada)
  recusa números que não cabem em `Int`, porque `readMaybe` direto para `Int` daria
  a volta silenciosamente (`99999999999999999999` viraria outro número). O
  `Nothing` vira um `Left` com motivo pela função `paraEither`, como na aula 14.
- **`lerPartida` usa `do` na mônada Either**: ler rodada, votos A, votos B e validar.
  O primeiro `Left` interrompe a sequência, sem `case` aninhados ("escadinha").
- **`lerCampeonato`** é outro `do` em Either: `traverse` aplica `lerPartida` a todas
  as linhas e para no primeiro erro; depois `verificarRodadas` confere os conflitos.
- **`todosOsErros`** lê todas as linhas e usa list comprehension com padrão no
  gerador: `[erro | Left erro <- lidas]` junta os erros de cada linha e
  `[p | Right p <- lidas]` separa as partidas válidas para conferir as rodadas.
- Os conflitos de rodada são calculados com uma **list comprehension** (Aula 12):
  cada par `(rodada, filme)` que aparece mais de uma vez gera um erro.
- O número da linha nos erros é o **número real no texto**, contando comentários
  e linhas em branco, para o usuário achar a linha no arquivo.
- **Núcleo puro e casca de IO separados**: só o `main` faz IO; tudo o mais pode ser
  testado sem entrada pelo teclado.

### Erros e o que cada um carrega

| Construtor | Carrega | Exemplo |
| --- | --- | --- |
| `CamposInvalidos` | linha | `1;Ainda Estou Aqui;0;Bastardos Inglórios` (4 campos) ou título vazio |
| `NumeroInvalido` | linha e campo | `dois`, `2a`, rodada `0` |
| `PlacarNegativo` | linha | `1;Como Eu Era Antes de Você;-2;1;Bastardos Inglórios` |
| `MesmoFilme` | linha | `2;Sonho de Liberdade;1;1;Sonho de Liberdade` |
| `FilmeRepetidoNaRodada` | rodada e filme | Ainda Estou Aqui em duas partidas da rodada 3 |

## Tabelas conferidas à mão

### Após a 3ª rodada

| Pos. | Filme | Pts | J | V | E | D | Pró | Contra | Saldo |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | Ainda Estou Aqui | 7 | 3 | 2 | 1 | 0 | 3 | 0 | +3 |
| 2 | Sonho de Liberdade | 6 | 3 | 2 | 0 | 1 | 6 | 2 | +4 |
| 3 | Como Eu Era Antes de Você | 6 | 3 | 2 | 0 | 1 | 4 | 2 | +2 |
| 4 | Toy Story 3 | 3 | 3 | 1 | 0 | 2 | 3 | 5 | −2 |
| 5 | Vitória | 2 | 3 | 0 | 2 | 1 | 1 | 3 | −2 |
| 6 | Bastardos Inglórios | 1 | 3 | 0 | 1 | 2 | 1 | 6 | −5 |

### Classificação final

| Pos. | Filme | Pts | J | V | E | D | Pró | Contra | Saldo |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | Ainda Estou Aqui | 10 | 5 | 3 | 1 | 1 | 6 | 3 | +3 |
| 2 | Sonho de Liberdade | 9 | 5 | 3 | 0 | 2 | 10 | 4 | +6 |
| 3 | Como Eu Era Antes de Você | 9 | 5 | 3 | 0 | 2 | 6 | 4 | +2 |
| 4 | Toy Story 3 | 9 | 5 | 3 | 0 | 2 | 7 | 6 | +1 |
| 5 | Bastardos Inglórios | 4 | 5 | 1 | 1 | 3 | 3 | 8 | −5 |
| 6 | Vitória | 2 | 5 | 0 | 2 | 3 | 1 | 8 | −7 |

- Sonho de Liberdade, Como Eu Era Antes de Você e Toy Story 3 terminam com 9 pontos e 3 vitórias: **o saldo decide**.
- Bastardos Inglórios passa Vitória entre a 3ª e a 5ª rodada (a tabela intermediária é diferente da final).
- Empate em 0 x 0: Ainda Estou Aqui x Vitória (rodada 1). Vitória por 3 ou mais: Sonho de Liberdade 4 x 0 Bastardos Inglórios (rodada 1).
- Média: 33 votos em 15 partidas = 2,2 votos por partida.

## Armadilhas do enunciado (seção 4) e como foram evitadas

| Armadilha | Como o projeto evita | Onde |
| --- | --- | --- |
| Partida como tupla | `Partida` é um record com campos nomeados | Camada 1 |
| Resultado como `String` ou `Int` | `Resultado` é um tipo soma; `-Wall` confere os casos | Camada 1, `pontos` |
| `head`, `maximum`, `foldr1` em lista vazia | `maiorDiferenca` dobra a partir de `Nothing`; `mediaVotos [] = Nothing`; a largura da tabela usa `foldr max` com valor inicial | Camadas 2 e 3 |
| Aproveitamento com `div` ou `/` entre `Int` | `fromIntegral` nos dois lados antes de `/` | `aproveitamento` |
| Ordenar só por pontos ou encadear `sortBy` | um único `sortBy` com `comparing ... <> ...` e `Down` | `classificacao` |
| Recursão explícita onde `map`/`filter`/dobra resolvem | Camada 2 sem recursão; `separar` com `foldr`; conflitos com list comprehension | Camadas 2 e 3 |
| `read` no placar | `readMaybe`, com `Integer` + `>>=` para recusar números fora do `Int` | `lerNumero` |
| `case` aninhados sobre `Either` | `do` na mônada Either em `lerPartida` e `lerCampeonato` | Camada 3 |
| `getLine` no `main` | dados como `String` na seção de dados; núcleo puro, IO só no `main` | Dados e Main |
| Campeonato vazio | tabela "Nenhuma partida registrada.", estatísticas com `Nothing`, demonstrado no `main` e testado | `main`, `Testes.hs` |

## Limitações conhecidas

- Títulos são comparados exatamente como escritos (após tirar espaços nas pontas):
  `"Ainda Estou Aqui"` e `"aurora"` contam como filmes diferentes.
- O `;` é o separador, então não pode fazer parte de um título.
- A ordem alfabética usa a ordem padrão de `String` do Haskell (por código de
  caractere), então letras acentuadas maiúsculas podem não ficar onde o português
  esperaria.
- `lerCampeonato` primeiro lê todas as linhas e só depois confere as rodadas. Se o
  texto tiver um erro de digitação e um conflito de rodada, o erro de digitação é
  relatado primeiro, mesmo que esteja numa linha posterior. `todosOsErros` lista os
  dois.
- Rodadas incompletas são aceitas: o programa não exige que todos joguem em todas
  as rodadas.
- Não há empate de critérios "por sorteio": a ordem alfabética sempre decide no fim.
- `conflitos` compara cada escalação com todas as outras (custo quadrático), o que
  é irrelevante para um campeonato, mas não escalaria para milhões de partidas.
- O ambiente escolhido é o Haskell Playground, que só aceita um arquivo. Por isso
  `Testes.hs` repete as seções do `Main.hs` (sem nenhuma mudança) antes dos testes:
  assim ele pode ser colado e rodado sozinho. Se uma função mudar no `Main.hs`, a
  cópia em `Testes.hs` precisa ser atualizada também.
- No GHC local (opcional), num terminal que não usa UTF-8 (como o `cmd` do Windows
  sem `chcp 65001`), imprimir os acentos encerra o programa com
  `commitBuffer: invalid argument`. No playground isso não acontece.
