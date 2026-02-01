
# PIB por Setor (IBGE / SIDRA)


# Pacotes
library(sidrar)
library(tidyverse)
library(scales)


# 1. Baixando os dados

# Tabela 5938 - PIB Municipal a preços correntes
# Município de Teresina PI

pib <- get_sidra(
  api = "/t/5938/n3/21,22,23,26/v/37/p/all/d/v37%200"
) %>%
  select(ano = Ano,
         var = Variável,
         valor = Valor)

# Visualizando variáveis
unique(pib$var)

# 2. Renomeando variáveis

pib <- pib %>%
  mutate(var = case_when(
    var == "Produto Interno Bruto a preços correntes" ~ "PIB",
    
    TRUE ~ var
  ))

# 3. Gráfico - PIB por setor
# -------------------------------------
ggplot(pib, aes(x = ano, y = valor / 1e6)) +
  geom_col(fill = "#3182bd") +
  facet_wrap(~var, ncol = 3, scales = "free_y") +
  theme_bw(base_family = "Ubuntu") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    y = "Bilhões (R$)",
    x = "",
    title = "PIB a preços correntes por setor",
    subtitle = "Município de Teresina – 2002–2023",
    caption = "Fonte: IBGE (SIDRA 5938) | Elaboração: Amanda Ravelly"
  )


