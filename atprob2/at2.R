# = = = = = = = = = = = = = = = = = = = = = =  
# EXERCÍCIO 02: Análise de dados: dados_salarios.csv
# Autor: Guilherme Ricci Machado Villela
# = = = = = = = = = = = = = = = = = = = = = =

library(dplyr)
library(ggplot2)

# 1. RETIRANDO A AMOSTRA
base = read.csv2("dados_salarios.csv", stringsAsFactors = TRUE)

set.seed(15032006)
base2 = base[sample(nrow(base), 220),]

# 2. salario_USD - MEDIA MEDIANA E PERCENTIL 
media <- mean(base2$salario_USD, na.rm = TRUE)
mediana <- median(base2$salario_USD, na.rm = TRUE)

percentis <- quantile(
  base2$salario_USD,
  probs = c(0.05, 0.25, 0.75, 0.95),
  na.rm = TRUE
)

minimo <- min(base2$salario_USD, na.rm = TRUE)
maximo <- max(base2$salario_USD, na.rm = TRUE)

# Criando a tabela resultado de forma organizada
resultado <- list(
  media = media,
  mediana = mediana,
  p5 = percentis[1],
  p25 = percentis[2],
  p75 = percentis[3],
  p95 = percentis[4],
  minimo = minimo,
  maximo = maximo
)

resultado

# 3. Selario_USD para as categorias (2020,2021,2022)
resumo_ano <- base2 |>
  group_by(ano) |>
  summarise(
    media = mean(salario_USD, na.rm = TRUE),
    mediana = median(salario_USD, na.rm = TRUE),
    .groups = "drop"
  )

resumo_ano

# 4. juntar SE + EX
resumo_exp <- base2 |>
  mutate(
    experiencia = ifelse(experiencia %in% c("SE", "EX"), "SE_EX", experiencia)
  ) |>
  group_by(experiencia) |>
  summarise(
    media = mean(salario_USD, na.rm = TRUE),
    mediana = median(salario_USD, na.rm = TRUE),
    .groups = "drop"
  )

resumo_exp

# 5. Gráfico de densidade da variável salario_USD
# Criar nova variável juntando SE + EX
base_plot <- base2 |>
  mutate(
    experiencia2 = ifelse(experiencia %in% c("SE", "EX"), "SE_EX", experiencia)
  )

# Calcular média e mediana por grupo
estatisticas <- base_plot |>
  group_by(experiencia2) |>
  summarise(
    media = mean(salario_USD, na.rm = TRUE),
    mediana = median(salario_USD, na.rm = TRUE),
    .groups = "drop"
  )

# Gráfico de densidade
ggplot(base_plot, aes(x = salario_USD, fill = experiencia2)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~experiencia2, scales = "free") +
  geom_vline(
    data = estatisticas,
    aes(xintercept = media),
    color = "blue",
    linetype = "dashed"
  ) +
  geom_vline(
    data = estatisticas,
    aes(xintercept = mediana),
    color = "red"
  ) +
  coord_cartesian(xlim = c(0, 300000)) + coord_cartesian(xlim = c(0, 300000))

# *** ADENDO *** 
# Utilizei limiar do eixo X para melhorar a visualização do gráfico pois existem valores muito
# altos, caso eu utiliza-se a função "geom_destiny()" achataria o gráfico, tornando
# inviável. 

# ==============================================================================
# valores claros do eixo X: 0   250000   500000   750000   1250000
# ==============================================================================
  theme_minimal()
