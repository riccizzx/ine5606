
# Guilherme Ricci, Mathias Petry, Pedro Cabral, Pedro Araujo

""
  # parte do SCRIPT PARA ANALISE DAS VARIAVEIS IPs
""

df <- read.csv("ipinfo_location_sample.csv", stringsAsFactors = FALSE)

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
# 4. Calcular quantidade de IPs
# =========================
df$qtd_ips <- df$end_num - df$start_num + 1

# =========================
# 5. Análise descritiva (geral)
# =========================
summary(df$qtd_ips)
mean(df$qtd_ips)
median(df$qtd_ips)
sd(df$qtd_ips)

# =========================
# 6. Agregação por país
# =========================
ips_por_pais <- aggregate(qtd_ips ~ country_name, data=df, sum)

# Ordenar do maior para o menor
ips_por_pais <- ips_por_pais[order(-ips_por_pais$qtd_ips), ]

# =========================
# 7. Agregação por cidade
# =========================
ips_por_cidade <- aggregate(qtd_ips ~ city, data=df, sum)
ips_por_cidade <- ips_por_cidade[order(-ips_por_cidade$qtd_ips), ]

# =========================
# 8. Gráfico - País
# =========================
barplot(ips_por_pais$qtd_ips,
        names.arg = ips_por_pais$country_name,
        las = 2,
        main = "Concentração de IPs por País",
        ylab = "Quantidade de IPs")

# =========================
# 9. Gráfico - Top 10 cidades
# =========================
top10_cidades <- head(ips_por_cidade, 10)

barplot(top10_cidades$qtd_ips,
        names.arg = top10_cidades$city,
        las = 2,
        main = "Top 10 Cidades com Maior Concentração de IPs",
        ylab = "Quantidade de IPs")

# =========================
# 11. Identificar maiores concentrações
# =========================
head(ips_por_pais, 5)
head(ips_por_cidade, 5)
