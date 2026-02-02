
#IBGE / SIDRA
# Rendimento médio mensal habitualmente recebido (PNAD Contínua)


# Pacotes
library(sidrar)
library(tidyverse)
library(scales)
library(ipeadatar)

# Fonte: https://sidra.ibge.gov.br/tabela/6390
# Variável 5933 = Rendimento médio real habitualmente recebido (R$)

rendimentos <- get_sidra(api = "/t/6390/n1/all/v/5933/p/all") %>%
  mutate(
    date = parse_date(`Trimestre Móvel (Código)`, format = "%Y%m"),
    Valor = as.numeric(Valor)
  ) %>%
  select(Valor, date)

# Último valor e data mais recente
ultimo_valor <- round(tail(rendimentos$Valor, 1), 1)
ultima_data <- tail(rendimentos$date, 1)

# -------------------------------------
# Gráfico de evolução do rendimento médio
# -------------------------------------

ggplot(rendimentos, aes(x = date, y = Valor)) +
  
  # Recessão econômica (2014–2016)
  annotate("rect", fill = "gray", alpha = 0.3,
           xmin = as.Date("2014-01-01"), xmax = as.Date("2016-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.Date("2015-01-01"),
           y = max(rendimentos$Valor, na.rm = TRUE) * 0.97,
           label = "Recessão 2014–2016", colour = "black", size = 4, face = "bold") +
  
  # Impeachment de Dilma (2016)
  annotate("rect", fill = "gray", alpha = 0.5,
           xmin = as.Date("2016-08-31"), xmax = as.Date("2016-09-15"),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.Date("2016-11-01"),
           y = max(rendimentos$Valor, na.rm = TRUE) * 0.93,
           label = "Impeachment Dilma", colour = "black", size = 4, face = "bold") +
  
  # Reforma trabalhista (2017)
  annotate("rect", fill = "gray", alpha = 0.5,
           xmin = as.Date("2017-07-13"), xmax = as.Date("2017-08-19"),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.Date("2018-01-01"),
           y = max(rendimentos$Valor, na.rm = TRUE) * 0.89,
           label = "Reforma Trabalhista", colour = "black", size = 4, face = "bold") +
  
  # Pandemia de COVID-19 (2020)
  annotate("rect", fill = "gray", alpha = 0.4,
           xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.Date("2020-09-01"),
           y = max(rendimentos$Valor, na.rm = TRUE) * 0.98,
           label = "Pandemia COVID-19", colour = "black", size = 4, face = "bold") +
  
  # Auxílio Emergencial (2020)
  annotate("rect", fill = "gray", alpha = 0.3,
           xmin = as.Date("2020-04-06"), xmax = as.Date("2021-10-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.Date("2021-05-01"),
           y = max(rendimentos$Valor, na.rm = TRUE) * 0.92,
           label = "Auxílio Emergencial", colour = "black", size = 4, face = "bold") +
  
  # Início do governo Lula 3 (2023)
  annotate("rect", fill = "gray", alpha = 0.4,
           xmin = as.Date("2023-01-01"), xmax = as.Date("2023-02-28"),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.Date("2023-08-01"),
           y = max(rendimentos$Valor, na.rm = TRUE) * 0.88,
           label = "Início Lula 3", colour = "black", size = 4, face = "bold") +
  
  # Linha principal
  geom_line(size = 1, colour = "#1f78b4") +
  
  # Linha horizontal no último valor
  geom_hline(yintercept = ultimo_valor, colour = "red", linetype = "dashed") +
  
  # Valor mais recente anotado no gráfico
  annotate("text",
           label = paste0("R$ ", ultimo_valor),
           colour = "red",
           size = 5,
           fontface = "bold",
           x = ultima_data + 60,
           y = ultimo_valor + 0.3) +
  
  # Estética geral
  theme_bw(base_family = "Ubuntu") +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(face = "italic", size = 9),
    panel.grid.major = element_line(color = "gray", size = 0.25, linetype = "dashed"), 
    panel.grid.minor = element_line(color = "lightgray", size = 0.25, linetype = "dotted"),
    legend.position = "none"
  ) +
  
  scale_x_date(breaks = date_breaks("6 months"),
               labels = date_format("%b/%Y")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
  
  labs(
    x = "",
    y = "R$ (reais de 2025)",
    title = "Rendimento médio mensal real habitualmente recebido",
    subtitle = "Brasil, março/2012 – junho/2025",
    caption = "Elaboração: Fábio Rocha | Dados: PNAD Contínua - IBGE / SIDRA 6390"
  )