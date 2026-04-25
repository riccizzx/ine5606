# ==========================================================
# EXERCÍCIO 03: Análise de dados: dados_alunos.csv
# Autor: Guilherme Ricci Machado Villela
# ==========================================================

library(dplyr)
library(ggplot2)

# Amostra
base = read.csv2("dados_alunos.csv")
set.seed(15032006)
base1 = base[sample(nrow(base), 90),]

# 2.mudando as variaveis escola e sexo
base1$Sexo <- factor(
  base1$Sexo,
  levels = c(1, 2),
  labels = c("Masculino", "Feminino")
)

base1$Escola <- factor(
  base1$Escola,
  levels = c(1, 2),
  labels = c("Publica", "Particular")
)