
# Faxina de dados

<!-- README.md is generated from README.Rmd. Please edit that file -->

Repositório principal do curso Faxina de dados.

Inscreva-se no curso: <https://www.curso-r.com/cursos/faxina>

**Acesse o material completo do curso escolhendo uma das turmas
abaixo**.

| Turma           | Material                                  | Github                                     |
|:----------------|:------------------------------------------|:-------------------------------------------|
| outubro de 2021 | <https://curso-r.github.io/202110-faxina> | <https://github.com/curso-r/202110-faxina> |
| maio de 2021    | <https://curso-r.github.io/202105-faxina> | <https://github.com/curso-r/202105-faxina> |

<div class="figure">

<img src="https://raw.githubusercontent.com/allisonhorst/stats-illustrations/master/rstats-artwork/tidydata_3.jpg" alt="Imagem de Allison Horst." width="80%" />
<p class="caption">
Imagem de Allison Horst.
</p>

</div>

# Ementa

-   O que são bases bagunças e arrumadas?
    -   Conceito de *tidy data*
    -   Reprodutibilidade fraca e forte
    -   Organizando projetos de faxina de dados
-   Ferramentas úteis
    -   Pacote `{janitor}`
    -   Funções menos conhecidas do `{tidyverse}`
-   Resolvendo problemas de importação
    -   Encoding
    -   Bases grandes
    -   PDF e OCR
-   Cases de preparação de dados

# Plano de aulas

**Aula 01**

-   Organização de projetos
-   Conceito de *tidy data*
-   Faxina de dados da SSP
    -   Problemas de encoding
-   Faxina de dados em um projeto de consultoria

**Aula 02**

-   Pacote `{janitor}`
-   Funções menos conhecidas do `{dplyr}` e do `{tidyr}`
-   Funções de leitura de dados
-   Faxina de dados em um projeto de consultoria (continuação)
-   Leitura de dados em outros formatos
    -   PDF e OCR
    -   Json e HTML/XML
-   Leitura de dados grandes
    -   Dados da RFB ([brasil.io](https://brasil.io))

**Aula 03**

-   Integração de dados
-   Fazendo um projeto completo do zero
-   Atividades que não deu tempo de fazer

## Pacotes necessários

Este curso tem algumas dependências. Separamos em dois grupos de
dependências:

-   Principal: scripts principais do curso.
-   Difíceis: pacotes que podem ser difíceis de instalar e que vamos
    usar apenas em situações muito específicas. Tente instalar, mas se
    não conseguir não se preocupe!

``` r
# Principal
principal <- c(
  "tidyverse",
  "flexdashboard",
  "fs",
  "janitor",
  "data.table",
  "vroom",
  "usethis",
  "remotes",
  "tictoc",
  "reprex",
  "padr",
  "rmarkdown",
  "openxlsx",
  "writexl",
  "stringdist",
  "fuzzyjoin"
)

install.packages(principal)

# Pacotes que podem ser mais dificeis de instalar
# dependendo do seu sistema operacional
dificeis <- c(
  "arrow",
  "tesseract",
  "magick",
  "pdftools",
  "sf"
)

install.packages(dificeis)
```
