# PjBL 2: Tabela de Classificação (Oscar fictício)

Trabalho de Programação Lógica e Funcional.

A gente escolheu fazer um "campeonato" de filmes inspirado no Oscar. São seis
filmes, cinco rodadas, e cada filme enfrenta todos os outros uma vez. Em cada
partida o júri vota, e o filme com mais votos ganha. Vitória vale 3 pontos,
empate vale 1 e derrota vale 0, igual no futebol.

## Integrantes

- Richard M. V. Ludke
- Carolina Reis

## Onde roda

Usamos o Haskell Playground (https://play.haskell.org). Como ele só aceita um
arquivo, o código ficou todo no `Main.hs`, separado em seções com comentários na
mesma ordem dos módulos do enunciado: Camada 1, Camada 2, Camada 3, Dados e Main.

## Como rodar

1. Entrar em https://play.haskell.org
2. Apagar o código de exemplo que aparece lá
3. Colar o `Main.hs` inteiro e clicar em Run

Para rodar os testes é o mesmo processo com o `Testes.hs`. No final tem que
aparecer `50/50 testes passaram.`

O `Testes.hs` tem uma cópia das seções do `Main.hs` antes dos testes, porque o
playground não consegue importar outro arquivo.

A primeira linha dos dois arquivos é `{-# OPTIONS_GHC -Wall #-}`, que liga todos
os avisos do compilador. Os dois compilam sem nenhum aviso.

Se quiser rodar no GHC instalado no computador também funciona:
`runghc Main.hs` e `runghc Testes.hs`.

## O que aparece quando roda

1. A tabela final do campeonato
2. A tabela depois da 3ª rodada
3. As estatísticas (maior diferença de votos, média de votos por partida e
   aproveitamento de cada filme)
4. Um exemplo de entrada com erros, mostrando primeiro só o primeiro erro e
   depois a lista com todos
5. O campeonato vazio, pra mostrar que o programa não quebra

## Formato dos dados

Cada linha é uma partida, com os campos separados por ponto e vírgula:

```
# rodada;filme A;votos A;votos B;filme B
1;Ainda Estou Aqui;0;0;Vitória
1;Como Eu Era Antes de Você;2;1;Toy Story 3
```

Linhas em branco e linhas que começam com `#` são ignoradas.

## Arquivos

- `Main.hs`: o programa inteiro
- `Testes.hs`: os testes, cada um compara o resultado com o valor esperado
- `docs/decisoes.md`: por que modelamos do jeito que modelamos, as tabelas que
  conferimos à mão e as limitações que conhecemos
