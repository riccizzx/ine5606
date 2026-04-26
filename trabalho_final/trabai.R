# =========================
# 1. Carregar dados
# =========================
df <- read.csv("ipnfo_location_sample.csv", stringsAsFactors = FALSE)

# =========================
# 2. Função: IP -> número
# =========================
ip_to_numeric <- function(ip) {
  parts <- as.numeric(unlist(strsplit(ip, "\\.")))
  return(parts[1]*256^3 + parts[2]*256^2 + parts[3]*256 + parts[4])
}

# =========================
# 3. Converter IPs
# =========================
df$start_num <- sapply(df$start_ip, ip_to_numeric)
df$end_num   <- sapply(df$end_ip, ip_to_numeric)

# =========================
# 4. Variável principal (VOCÊ)
# =========================
df$qtd_ips <- df$end_num - df$start_num + 1

# =========================
# PARTICIPANTE 1 (VOCÊ) - qtd_ips
# =========================

# Estatísticas
summary(df$qtd_ips)
mean(df$qtd_ips)
median(df$qtd_ips)
sd(df$qtd_ips)

# Histograma
hist(df$qtd_ips,
     main="Distribuição da Quantidade de IPs por Bloco",
     xlab="Quantidade de IPs")

# Boxplot (detectar outliers)
boxplot(df$qtd_ips,
        main="Boxplot da Quantidade de IPs")

# =========================
# PARTICIPANTE 2 - country_name
# =========================

freq_pais <- table(df$country_name)

barplot(freq_pais,
        las=2,
        main="Frequência de IPs por País",
        ylab="Quantidade de registros")

# =========================
# PARTICIPANTE 3 - city
# =========================

freq_cidade <- sort(table(df$city), decreasing=TRUE)

top10_cidades <- head(freq_cidade, 10)

barplot(top10_cidades,
        las=2,
        main="Top 10 Cidades com Mais Registros",
        ylab="Quantidade")

# =========================
# PARTICIPANTE 4 - radius
# =========================

# Estatísticas
summary(df$radius)

# Histograma
hist(df$radius,
     main="Distribuição do Raio de Precisão",
     xlab="Radius")

# Boxplot por país (simples mas forte)
boxplot(radius ~ country_name, data=df,
        las=2,
        main="Radius por País")