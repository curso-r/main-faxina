library(readxl)
library(tidyverse)
library(janitor)


# Importação --------------------------------------------------------------
abas <- readxl::excel_sheets("exercicios/02-excels/REVISÃO DADOS - CONSULTORIA.xlsx")

le_uma_aba <- function(aba) {
  read_excel("exercicios/02-excels/REVISÃO DADOS - CONSULTORIA.xlsx", sheet = aba, skip = 1) %>%
    mutate(cidade = aba)
}

le_uma_aba("Campos do Jordão")

indicadores <- map_dfr(abas, le_uma_aba)

glimpse(indicadores)


# Limpeza geral -----------------------------------------------------------------
indicadores_limpo <- indicadores %>%
  # limpa nomes das colunas
  clean_names() %>%
  mutate(
    # porcentagem de texto para número
    across(starts_with("percent_"), parse_number, locale = locale(decimal_mark = ","))/100,
    # ano mes para data
    data = lubridate::ymd(paste0(ano, mes, "01", sep = "-")),
    # id para texto
    id = as.character(id)
  ) %>%
  # retira "percent", numeros e underlines dos nomes
  rename_with(~str_remove_all(., "percent|[0-9_]"))


# Limpeza com informações do cliente --------------------------------------

# Informação 1 - Sobre os IDs, o cliente informou que deveria ter apenas uma linha para cada trinca (id-ano-mes).
# Por conta de uma inconsistência, poderia acontecer de virem duas ou mais linhas para o mesma trinca (id-ano-mes).
# O correto é ter apenas uma linha apenas. Eles disseram que a linha com o maior valor de agendamento tem mais chance
# de ser a correta.

# Solução: arrange() + distinct()
indicadores_limpo <- indicadores_limpo %>%
  arrange(desc(agendamento)) %>%
  distinct(id, ano, mes, .keep_all = TRUE)




# Informação 2 - Sobre as séries mensais, o cliente informou que:
# 1) IDs podem ter início e fim distintos.
# 2) A série de meses de um ID não teve ter mês faltante entre seu início e seu fim, 
#    porém, em virtude de problemas técnicos, pode haver perda de informação no meio
#    do processo. Assim, nesses casos, orienta-se substituir o valor faltante pelo 
#    valor do mês anterior.

# Olhando o problema dos meses faltantes
indicadores_limpo %>%
  ggplot(aes(x = data, y = id, colour = id)) +
  geom_point(size = 5) 

# Solução: {padr} + {tidyr} (exemplo com o id 970)
indicadores_limpo %>%
  filter(id == 970) %>%
  arrange(data) %>%
  padr::pad(interval = "month", group = "id")

indicadores_limpo_com_pad <- indicadores_limpo  %>%
  padr::pad(interval = "month", group = "id")  
  
# padr::pad() consertou!
indicadores_limpo_com_pad %>%
  ggplot(aes(x = data, y = id, colour = id)) +
  geom_point(size = 5) 

# agora tem que preencher os NAs com fill.
indicadores_limpo_com_pad <- indicadores_limpo_com_pad %>%
  arrange(id, data) %>%
  fill(agendamento:cidade) %>%
  mutate(
    # mes e ano não dá pra preencher com fill diretamente
    mes = as.character(lubridate::month(data)),
    ano = as.character(lubridate::year(data))
  )

indicadores_limpo_com_pad

bind_rows(
    indicadores_limpo %>% mutate(padded = "nao"),
    indicadores_limpo_com_pad %>% mutate(padded = "sim")
  ) %>%
  ggplot(aes(x = data, y = alocacao, colour = padded)) +
  geom_point(size = 4, alpha = 0.2) +
  geom_line(size = 2) +
  facet_wrap(~id)
