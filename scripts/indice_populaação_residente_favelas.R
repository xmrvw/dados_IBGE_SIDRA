# População residentes em favelas
# -------------------------------------

# Pacotes
library(sidrar)
library(tidyverse)
library(gt)
library(stringr)
library(showtext)

# 1. Baixando dados via API do SIDRA
# -------------------------------------

api <- "/t/9885/n1/all/n6/all/v/9614,9905,9906/p/all/c86/95251/d/v9614%202"
dados_api <- get_sidra(api = api)
colnames(dados_api)


# 2. Seleção e limpeza das variáveis
# -------------------------------------

dados_api <- dados_api %>%
  select(
    municipio = `Brasil e Município`,
    var = Variável,
    valor = Valor
  ) %>%
  mutate(valor = as.numeric(valor))

# Selecionar 3 municípios aleatórios
set.seed(123) # para reprodutibilidade
municipios_selecionados <- sample(unique(dados_api$municipio), 3)

# Filtrar apenas esses municípios
dados_ord <- dados_api %>%
  filter(municipio %in% municipios_selecionados)

# 3. Tabela resumida com {gt}
# -------------------------------------

dados_ord %>%
  pivot_wider(names_from = municipio, values_from = valor) %>%
  gt() %>%
  cols_label(
    var = md("**Indicador**")   # muda o nome da coluna 'var'
  ) %>%
  tab_header(
    title = md("**Indicadores Demográficos da População Residente em Favelas e Comunidades Urbanas**"),
    subtitle = md("Comparativo entre municípios selecionados segundo o índice de envelhecimento, idade mediana e razão de sexo da população residente.")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_footnote(
    footnote = "O índice de envelhecimento (IE) indica o grau de envelhecimento populacional: quanto maior o valor, mais envelhecida é a população local.",
    locations = cells_title(groups = "title")
  ) %>%
  tab_source_note(
    source_note = "Fonte: IBGE - SIDRA (Tabela 9885) | Elaboração: Amanda Ravelly"
  )

# 4. Visualização gráfica com {ggplot2}
# -------------------------------------
# Lembrar das configurações de customização

dados_largos <- dados_api %>%
  pivot_wider(
    names_from = var,
    values_from = valor)

# Renomear colunas para facilitar
dados_largos <- dados_largos %>%
  rename(
    indice_envelhecimento = `Índice de envelhecimento da população residente em favelas e comunidades urbanas (Idosos: 60 anos ou mais de idade)`,
    idade_mediana = `Idade mediana da população residente em favelas e comunidades urbanas`,
    razao_sexo = `Razão de sexo da população residente em favelas e comunidades urbanas`
  )

# Selecionar 5 municípios com maior e 5 com menor índice de envelhecimento
maiores <- dados_largos %>%
  arrange(desc(razao_sexo)) %>%
  slice_head(n = 5) %>%
  mutate(grupo = "Maior IE")

menores <- dados_largos %>%
  arrange(razao_sexo) %>%
  slice_head(n = 5) %>%
  mutate(grupo = "Menor IE")

# Combinar os dois grupos
dados_plot <- bind_rows(maiores, menores)

dados_plot <- dados_plot %>%
  mutate(grupo = factor(grupo, 
                        levels = c("Maior IE", "Menor IE")))

# Fonte Ubuntu para manter padrão visual
font_add_google("Ubuntu", "Ubuntu")
showtext_auto()

# Criar o gráfico de dispersão com bolhas
ggplot(dados_plot, 
       aes(x = reorder(municipio, razao_sexo), 
           y = razao_sexo, fill = grupo)) +
  coord_flip() +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = razao_sexo),
    hjust = -0.1,
    size = 3,
    family = "Ubuntu",
    show.legend = FALSE) +
  scale_fill_manual(
    name = "Grupo",
    values = c("Maior IE" = "#0072B2", "Menor IE" = "#D55E00"),
    labels = c("5 maiores",
               "5 menores")) +
  labs(
    title = "Razão de sexo da população residente em favelas \ne comunidades urbanas",
    subtitle = "Municípios com os 5 maiores e 5 menores",
    x = "",
    y = "Razão de Sexo (homens por 100 mulheres)",
    caption = "Fonte: IBGE - SIDRA (Tabela 9885) | Elaboração: Amanda Ravelly"
  ) +
  theme_bw(base_family = "Ubuntu") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    axis.text = element_text(size = 10)
  )





