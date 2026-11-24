# Decisões de modelagem

## 1. Escolha do tema

A gente escolheu uma premiação fictícia de filmes inspirada no Oscar para
adaptar a proposta do trabalho. No nosso exemplo, seis filmes participam de
cinco etapas. Cada votação compara dois filmes pelos votos recebidos do júri,
e cada par de filmes é avaliado uma vez.

São quinze votações ao todo, sem repetir um filme na mesma etapa. Os votos
foram definidos para os testes, incluindo um empate sem votos e uma diferença
de quatro votos. Não estamos reproduzindo o funcionamento do Oscar real.

Para deixar o código mais próximo do tema, usamos estes nomes:

| Na proposta original | No nosso projeto |
| --- | --- |
| `Time` | `Filme` |
| `Partida` | `Votacao` |
| `rodada` | `etapa` |
| `mandante` e `visitante` | `filmeA` e `filmeB` |
| Gols | Votos |
| `jogos` | `participacoes` |
| `linhaDoTime` | `linhaDoFilme` |
| `ateRodada` | `ateEtapa` |
| `maiorGoleada` | `maiorDiferenca` |
| `mediaGols` | `mediaVotos` |
| `lerPartida` e `lerCampeonato` | `lerVotacao` e `lerPremiacao` |

## 2. Como guardamos os dados

`Filme` é uma `String` com o título. O tipo `Resultado` tem três possibilidades:
`Vitoria`, `Empate` e `Derrota`. Assim, o resultado não depende de escrever uma
palavra ou uma letra do jeito certo.

`Votacao` guarda a etapa, os dois filmes e os votos de cada um. Usamos campos
com nomes para deixar claro a quem cada informação pertence.

`Linha` reúne as participações, vitórias, empates, derrotas, votos a favor e
votos contra de um filme. Os pontos e o saldo são calculados a partir desses
valores, sem guardar a mesma informação duas vezes. Os votos contra são os
votos recebidos pelos outros filmes nas votações em que aquele filme participou.

Quando um filme não participou de uma votação, `resultadoPara` devolve
`Nothing`. Achamos melhor representar a ausência de resultado do que atribuir
uma derrota a quem nem estava naquela votação.

## 3. Pontuação e classificação

Mantivemos a pontuação da proposta: 3 pontos por vitória, 1 por empate e 0 por
derrota. Para ordenar os filmes, usamos estes critérios:

1. Mais pontos.
2. Mais vitórias.
3. Maior saldo de votos, calculado por votos a favor menos votos contra.
4. Mais votos a favor.
5. Ordem alfabética do título.

O último critério garante uma ordem definida mesmo quando todos os números
empatam. São regras da nossa premiação fictícia.

## 4. Organização e cálculos

Separamos o código nas três camadas pedidas. `Tipos.hs` define os dados,
`Classificacao.hs` faz os cálculos e `Leitura.hs` cuida da entrada e da
formatação. Os exemplos ficam em `Dados.hs`, e `Main.hs` reúne a demonstração.
Os testes ficam em uma pasta própria e importam esses mesmos módulos.

Para montar a linha de cada filme, filtramos suas votações e usamos `foldl'`
para acumular as estatísticas, começando com tudo zerado. `map` monta as linhas
dos filmes, e `filter` também permite selecionar as votações até uma etapa.
Essa parte não usa recursão explícita.

A classificação usa `sortBy`, com os critérios reunidos em uma comparação.
`Down` coloca os maiores valores primeiro, e `<>` passa ao próximo critério
quando há empate. Usamos a ordenação da biblioteca em vez de criar outra.

O aproveitamento é a porcentagem dos pontos possíveis que o filme conseguiu.
Usamos `fromIntegral` antes da divisão para obter um resultado com casas
decimais. Se o filme não participou, o aproveitamento é zero.

`maiorDiferenca` e `mediaVotos` devolvem `Nothing` quando a lista está vazia.
Assim, não precisamos buscar um elemento que não existe.

## 5. Leitura e tratamento dos erros

Os exemplos ficam como texto no código, com os campos separados por `;`.
Isso permite testar tanto dados corretos quanto linhas com erros, sem pedir
entrada pelo teclado.

Usamos `readMaybe` para ler os números. Se aparecer algo como `dois` ou `2a`,
a leitura informa o erro. Antes de converter para `Int`, também conferimos
se o número cabe nesse tipo.

`lerVotacao` usa um bloco `do` com `Either` para ler os campos e validar a
votação. `lerPremiacao` usa `traverse` para aplicar essa leitura ao texto e
depois verifica os filmes repetidos em uma etapa. Um `Left` traz o motivo da
falha; um `Right` traz os dados aceitos.

| Erro | O que significa |
| --- | --- |
| `CamposInvalidos` | A linha não tem cinco campos ou um dos títulos está vazio |
| `NumeroInvalido` | Um campo numérico é inválido, não cabe em `Int` ou a etapa não é positiva |
| `VotosNegativos` | Um dos filmes recebeu uma quantidade negativa de votos |
| `MesmoFilme` | O mesmo título aparece nas duas opções da votação |
| `FilmeRepetidoNaEtapa` | Um filme aparece em mais de uma votação na mesma etapa |

As mensagens indicam a linha original do texto ou a etapa do conflito.
`lerPremiacao` retorna o primeiro erro de leitura; se as linhas estiverem
corretas, verifica os conflitos entre elas. `todosOsErros` reúne os erros de
cada linha e os conflitos entre as votações válidas.

## 6. Resultados esperados e testes

Os testes comparam o resultado obtido com um valor esperado. Mantivemos os
valores das tabelas escritos nos testes, separados do cálculo da classificação,
para que a comparação não dependa de repetir a mesma função.

### Depois da terceira etapa

| Pos. | Filme | Pontos | Participações | V | E | D | Votos a favor | Votos contra | Saldo |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | Ainda Estou Aqui | 7 | 3 | 2 | 1 | 0 | 3 | 0 | +3 |
| 2 | Sonho de Liberdade | 6 | 3 | 2 | 0 | 1 | 6 | 2 | +4 |
| 3 | Como Eu Era Antes de Você | 6 | 3 | 2 | 0 | 1 | 4 | 2 | +2 |
| 4 | Toy Story 3 | 3 | 3 | 1 | 0 | 2 | 3 | 5 | -2 |
| 5 | Vitória | 2 | 3 | 0 | 2 | 1 | 1 | 3 | -2 |
| 6 | Bastardos Inglórios | 1 | 3 | 0 | 1 | 2 | 1 | 6 | -5 |

### Classificação final

| Pos. | Filme | Pontos | Participações | V | E | D | Votos a favor | Votos contra | Saldo |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | Ainda Estou Aqui | 10 | 5 | 3 | 1 | 1 | 6 | 3 | +3 |
| 2 | Sonho de Liberdade | 9 | 5 | 3 | 0 | 2 | 10 | 4 | +6 |
| 3 | Como Eu Era Antes de Você | 9 | 5 | 3 | 0 | 2 | 6 | 4 | +2 |
| 4 | Toy Story 3 | 9 | 5 | 3 | 0 | 2 | 7 | 6 | +1 |
| 5 | Bastardos Inglórios | 4 | 5 | 1 | 1 | 3 | 3 | 8 | -5 |
| 6 | Vitória | 2 | 5 | 0 | 2 | 3 | 1 | 8 | -7 |

Sonho de Liberdade, Como Eu Era Antes de Você e Toy Story 3 terminam com
9 pontos e 3 vitórias. O saldo decide a ordem entre eles: +6, +2 e +1.
A posição de Bastardos Inglórios e Vitória muda entre a terceira etapa e a
classificação final.

A maior diferença é de quatro votos, na votação entre Sonho de Liberdade e
Bastardos Inglórios. Ao todo são 33 votos em 15 votações, então a média é 2,2.

Os 50 testes cobrem as três camadas, incluindo lista vazia, filme sem
participações, critérios de desempate, todos os tipos de erro e uma entrada
com vários erros. Na compilação com GHC 9.6.6 e `-Wall`, não houve avisos e
todos passaram.

## 7. Limitações conhecidas

- Os dados são fictícios e ficam em `Dados.hs`. Não há cadastro durante a
  execução nem consulta a resultados reais.
- Títulos são comparados como foram escritos, depois de remover espaços nas
  pontas. Diferenças de maiúsculas, minúsculas ou acentos podem representar
  filmes diferentes.
- Um título não pode conter `;`, pois esse caractere separa os campos.
- A ordem alfabética segue a comparação padrão de texto do Haskell, que pode
  diferir da ordem esperada em português para letras acentuadas.
- Etapas incompletas são aceitas. O programa não exige que todos os filmes
  participem de todas as etapas nem que cada par apareça uma única vez ao longo
  de toda a premiação.
- Se duas votações tiverem a mesma maior diferença, fica a primeira da lista.
- `todosOsErros` procura conflitos apenas nas votações que puderam ser lidas
  corretamente. Uma linha inválida não entra nessa verificação.
- Os valores individuais são conferidos, mas os totais também usam `Int`.
  Somar valores muito grandes ainda pode ultrapassar o limite desse tipo.
- A verificação de filmes repetidos percorre as participações várias vezes.
  Isso atende ao tamanho dos nossos exemplos, mas pode ficar lento com uma
  quantidade muito grande de votações.
