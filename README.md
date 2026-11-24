# PjBL 2: Tabela de Classificação (Oscar fictício)

Trabalho de Programação Lógica e Funcional.

A gente escolheu adaptar a proposta para uma premiação fictícia de filmes,
inspirada no Oscar. São seis filmes, cinco etapas e quinze votações. Em cada
votação, o júri compara dois filmes e o que recebe mais votos vence.

Vitória vale 3 pontos, empate vale 1 e derrota vale 0. A classificação considera
pontos, vitórias, saldo de votos, votos a favor e, por último, a ordem alfabética.
Os resultados são inventados para testar o programa; não são as regras nem os
resultados do Oscar real.

## Integrantes

- Richard M. V. Ludke
- Carolina Reis

## Organização do projeto

Os arquivos estão separados por camada. A raiz do repositório é a pasta do
projeto.

| Arquivo | O que tem nele |
| --- | --- |
| `src/Tipos.hs` | Camada 1: tipos e consultas básicas sobre os filmes e as votações |
| `src/Classificacao.hs` | Camada 2: pontuação, classificação e estatísticas com funções de ordem superior |
| `src/Leitura.hs` | Camada 3: leitura, validação com `Maybe` e `Either` e formatação da tabela |
| `src/Dados.hs` | Dados da premiação e uma entrada com erros, guardados como `String` |
| `src/Main.hs` | Demonstração das três camadas |
| `tests/Testes.hs` | Testes que comparam os resultados com os valores esperados |
| `docs/decisoes.md` | Decisões de modelagem, tabelas esperadas e limitações conhecidas |
| `README.md` | Instruções para compilar e executar |

Os testes importam os módulos de `src/`, então não precisam repetir o código do
programa.

## Como executar

Esta versão usa o GHC instalado no computador. Ela depende apenas da biblioteca
padrão do Haskell (`base`), sem pacotes adicionais. Como agora há vários módulos,
não basta colar apenas o `Main.hs` no Playground.

Abra o terminal na raiz do projeto, onde está este README, e execute:

```sh
runghc -isrc src/Main.hs
```

Para rodar os testes:

```sh
runghc -isrc tests/Testes.hs
```

O `-isrc` indica onde estão os módulos importados. No final dos testes, o
resultado esperado é `51/51 testes passaram.`

## Como compilar

Para gerar os executáveis:

```sh
ghc -Wall -isrc -outputdir build/app src/Main.hs -o oscar
ghc -Wall -isrc -outputdir build/tests tests/Testes.hs -o testes
```

Os arquivos gerados pela compilação ficam em `build/`, e os executáveis ficam na
raiz. As pastas de compilação são separadas porque o programa e os testes têm
seus próprios `main`.

No Linux ou macOS, execute:

```sh
./oscar
./testes
```

No Windows, pelo PowerShell:

```powershell
.\oscar.exe
.\testes.exe
```

O código foi conferido no GHC 9.6.6, compilando os dois programas com `-Wall`
sem avisos e executando os 51 testes.

Se o terminal do Windows tiver problemas com os acentos, use `chcp 65001` antes
de executar.

## O que aparece na execução

1. A classificação final da premiação.
2. A classificação depois da terceira etapa.
3. A maior diferença de votos, a média de votos por votação e o aproveitamento
   de cada filme.
4. Uma entrada com erros, mostrando o primeiro erro e depois a lista completa.
5. O resultado de uma premiação vazia.

Na tabela, `Par` significa participações, `V` vitórias, `E` empates e `D` derrotas.

## Formato dos dados

Cada linha representa uma votação, com cinco campos separados por ponto e
vírgula:

```text
# etapa;filme A;votos A;votos B;filme B
1;Ainda Estou Aqui;0;0;Vitória
1;Como Eu Era Antes de Você;2;1;Toy Story 3
```

Linhas vazias e comentários iniciados por `#` são ignorados. O programa verifica
os números, os títulos, os votos negativos e a repetição de um filme na mesma
etapa.

Para alterar os exemplos, edite `dadosPremiacao` ou `dadosComErros` em
`src/Dados.hs`. As regras e as limitações estão em [docs/decisoes.md](docs/decisoes.md).

