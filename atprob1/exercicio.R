
# = = = = = = = = = = = = = = = = = = = = = =  
# EXERCÍCIO 01: Análise de dados: salario.csv
# Autor: Guilherme Ricci Machado Villela
# = = = = = = = = = = = = = = = = = = = = = =

# 1) CLASSIFICACÃO DE VARIÁVEIS
#
# sexo              -> Qualitativa nominal
# idade             -> Quantitativa discreta
# tempo_experiencia -> Quantitativa discreta
# tamanho_empresa   -> Qualitativa nominal
# trabalho_remoto   -> Qualitativa nominal
# salario           -> Quantitativa contínua

# 2) LEITURA DA BASE DE DADOS salarios.csv

library(dplyr)
library(ggplot2)
library(readr)
library(scales)

base = read.csv2("salarios.csv")
set.seed(15032006) 
base1 = base[sample(nrow(base), 400),]

# 3) GRÁFICO TAMANHO_EMPRESA
base1 |>
  count(tamanho_empresa) |>
  mutate(percentual = n / sum(n)) |>
  ggplot(aes(x = tamanho_empresa, y = percentual)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Distribuição do Tamanho das Empresas",
    x = "Tamanho da Empresa",
    y = "Percentual"
  ) +
  theme_minimal()

# 4 - GRÁFICO PARA ANALISE TAMANHO_EMPRESA E TRABALHO_REMOTO
base1 |>
  count(tamanho_empresa, trabalho_remoto) |>
  group_by(tamanho_empresa) |>
  mutate(percentual = n / sum(n)) |>
  ggplot(aes(x = tamanho_empresa, y = percentual, fill = trabalho_remoto)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relação entre Tamanho da Empresa e Trabalho Remoto",
    x = "Tamanho da Empresa",
    y = "Percentual",
    fill = "Trabalho Remoto"
  ) +
  theme_minimal()

#5) GRÁFICO DE DENSIDADE
base1 |>
  ggplot(aes(x = salario)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(
    title = "Distribuição de Densidade do Salário",
    x = "Salário",
    y = "Densidade"
  ) +
  theme_minimal()

#6) GRÁFICO DE DENSIDADE PARA A VARIAVEL SALARIO
base1 |>
  ggplot(aes(x = salario, fill = trabalho_remoto)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Densidade do Salário por Trabalho Remoto",
    x = "Salário",
    y = "Densidade",
    fill = "Trabalho Remoto"
  ) +
  theme_minimal()
