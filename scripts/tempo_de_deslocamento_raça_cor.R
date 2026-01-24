# Tempo de deslocamento ao trabalho por cor ou raça
# -------------------------------------

#Instalar pacotes
install.packages("sidrar",
                 "dplyr",
                 "ggplot2",
                 "gt",
                 "geobr",
                 "sf",
                "stringr",
                "showtext")


# Pacotes
library(tidyverse)
library(sidrar)
library(dplyr)
library(ggplot2)
library(gt)
library(geobr)
library(sf)
library(stringr)
library(showtext)

# Uma das formas de se "puxar" os dados 
info_sidra(10333, wb = T) 

dados = get_sidra(x = "10333", 
                  geo = "Brazil", 
                  variable = 1013377, 
                  format = 2)


# 1. Baixando dados via API do SIDRA
# -------------------------------------

api <- "/t/10333/n1/all/n3/all/v/1013377/p/all/c537/allxt/c1568/120704/c58/95253/c86/all/d/v1013377%202"
dados_api <- get_sidra(api = api)
colnames(dados_api)


# 2. Seleção e limpeza das variáveis
# -------------------------------------

dados_api <- dados_api %>%
  select(
    raca_cor = `Cor ou raça`,
    valor = Valor,
    uf = `Brasil e Unidade da Federação`,
    tempo = `Tempo habitual de deslocamento do domicílio para o trabalho principal`
  ) %>%
  mutate(valor = as.numeric(valor))

# Visualizando as primeiras linhas
head(dados_api, 8)


# 4. Visualização gráfica com {ggplot2}
# -------------------------------------

dados_api <- dados_api %>%
  mutate(tempo = factor(
    tempo,
    levels = c(
      "Até cinco minutos",
      "De seis minutos até quinze minutos",
      "Mais de quinze minutos até meia hora",
      "Mais de meia hora até uma hora",
      "Mais de uma hora até duas horas",
      "Mais de duas horas até quatro horas",
      "Mais de quatro horas"
    )
  ))

dados_api %>% 
  filter(raca_cor != "Total") %>% 
  ggplot(aes(x = tempo, y = valor, fill = raca_cor)) +
  geom_col() +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Tempo de deslocamento até o trabalho por cor ou raça",
    subtitle = "Percentual da população ocupada",
    x = "Tempo de deslocamento (minutos)",
    y = "Percentual (%)",
    fill = "Cor ou raça",
    caption = "Fonte: IBGE - SIDRA (10333) | Elaboração: Amanda Ravelly"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )










