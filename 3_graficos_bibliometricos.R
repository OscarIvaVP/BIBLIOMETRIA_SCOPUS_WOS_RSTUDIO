# =============================================================================
# BIBLIOMETRIC ANALYSIS - DESDE CSV/TXT
# Fuente: bibliometrix_data.csv (generado por exportar_rdata_a_csv.R)
# 13 figuras — identico a graficos_bibliometrix_avanzado.R
# =============================================================================

library(bibliometrix)
library(ggplot2)
library(dplyr)
library(wordcloud)   # install.packages("wordcloud") si no lo tienes

BG  <- "white"
PAL <- c("#2C3E6B","#C0392B","#27AE60","#E67E22","#8E44AD",
         "#16A085","#2980B9","#F39C12","#D35400","#1ABC9C",
         "#7F8C8D","#2ECC71","#E74C3C","#9B59B6","#F1C40F")

theme_paper <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title    = element_text(face="bold", size=base_size+2,
                                   color="#2C3E6B", hjust=0.5, margin=margin(b=8)),
      plot.subtitle = element_text(size=base_size-1, color="#555555",
                                   hjust=0.5, margin=margin(b=12)),
      plot.caption  = element_text(size=base_size-3, color="#888888",
                                   hjust=1, face="italic"),
      axis.title    = element_text(face="bold", color="#2C3E6B", size=base_size-1),
      axis.text     = element_text(color="#333333", size=base_size-2),
      panel.grid.major = element_line(color="#E8E8E8", linewidth=0.4),
      panel.grid.minor = element_blank(),
      panel.border  = element_rect(color="#CCCCCC", fill=NA, linewidth=0.5),
      legend.title  = element_text(face="bold", size=base_size-1),
      legend.text   = element_text(size=base_size-2),
      legend.position = "bottom",
      plot.background = element_rect(fill=BG, color=NA),
      plot.margin   = margin(15,15,15,15)
    )
}

# =============================================================================
# CARGAR DATOS DESDE CSV
# =============================================================================
# Puedes cambiar "bibliometrix_data.csv" por "bibliometrix_data.txt"
# ambos usan el mismo formato (tab-separated)

ARCHIVO_DATOS <- "output/wos_scopus_consolidado.txt"   # <-- cambiar aqui si usas .txt

cat("\n>>> Cargando datos desde CSV...\n")
M <- convert2df(ARCHIVO_DATOS, dbsource = "wos", format = "plaintext")

# Asegurar que las columnas numericas se lean correctamente
if ("PY" %in% names(M)) M$PY <- as.integer(M$PY)
if ("TC" %in% names(M)) M$TC <- as.integer(M$TC)
if ("NR" %in% names(M)) M$NR <- as.integer(M$NR)

# Restaurar clase bibliometrixDB si existe el archivo de atributos
if (file.exists("bibliometrix_attrs.rds")) {
  attrs <- readRDS("bibliometrix_attrs.rds")
  class(M) <- attrs$class
}

# Asegurar que AU_CO existe
if (!"AU_CO" %in% names(M))
  M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

res <- biblioAnalysis(M, sep = ";")
cat(sprintf("Registros: %d | Periodo: %d-%d\n",
            nrow(M), min(M$PY, na.rm=TRUE), max(M$PY, na.rm=TRUE)))

# Crear carpeta de salida para figuras
DIR_FIGS <- "figuras"
if (!dir.exists(DIR_FIGS)) dir.create(DIR_FIGS)

summary(res, k = 10, pause = FALSE)

# =============================================================================
# FIG 01 — PRODUCCION CIENTIFICA ANUAL
# =============================================================================

cat("\n>>> [01/13] Produccion cientifica anual...\n")
while(!is.null(dev.list())) dev.off()

prod_anual <- as.data.frame(table(M$PY)) %>%
  rename(Year = Var1, Articles = Freq) %>%
  mutate(Year = as.integer(as.character(Year))) %>%
  filter(!is.na(Year))

p01 <- ggplot(prod_anual, aes(x = Year, y = Articles)) +
  geom_col(fill = "#2C3E6B", alpha = 0.85, width = 0.7) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              color = "#C0392B", fill = "#C0392B", alpha = 0.15,
              linewidth = 1.2, span = 0.6) +
  geom_text(aes(label = Articles), vjust = -0.5, size = 3,
            color = "#333333", fontface = "bold") +
  scale_x_continuous(breaks = seq(min(prod_anual$Year),
                                  max(prod_anual$Year), by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Annual Scientific Production",
    subtitle = paste0("n = ", nrow(M), " documents · ",
                      min(prod_anual$Year), "–", max(prod_anual$Year)),
    x = "Year", y = "Number of Articles",
    caption = "Source: WoS + Scopus | Bibliometric analysis"
  ) +
  theme_paper()

print(p01)
ggsave(file.path(DIR_FIGS, "fig01_annual_production.png"), p01, width=12, height=6, dpi=300, bg=BG)
cat("    Guardado: figuras/fig01_annual_production.png\n")

# =============================================================================
# FIG 02 — MAPA MUNDIAL DE PRODUCCION POR PAIS
# =============================================================================

cat("\n>>> [02/13] Mapa mundial...\n")
while(!is.null(dev.list())) dev.off()

if (is.data.frame(res$Countries)) {
  country_prod <- res$Countries
  names(country_prod) <- c("Country", "Articles")
  country_prod$Country <- tolower(trimws(as.character(country_prod$Country)))
} else {
  country_prod <- data.frame(
    Country  = tolower(trimws(names(res$Countries))),
    Articles = as.integer(res$Countries)
  )
}

country_prod <- country_prod %>%
  mutate(Country = case_when(
    Country == "usa"           ~ "united states of america",
    Country == "united states" ~ "united states of america",
    Country == "south korea"   ~ "south korea",
    Country == "iran"          ~ "iran",
    Country == "russia"        ~ "russia",
    TRUE ~ Country
  ))

world <- map_data("world") %>%
  mutate(region = tolower(region)) %>%
  mutate(region = case_when(
    region == "usa"  ~ "united states of america",
    region == "uk"   ~ "united kingdom",
    TRUE ~ region
  ))

map_data_plot <- world %>%
  left_join(country_prod, by = c("region" = "Country"))

p02 <- ggplot(map_data_plot, aes(x = long, y = lat, group = group, fill = Articles)) +
  geom_polygon(color = "white", linewidth = 0.2) +
  scale_fill_gradientn(
    colors   = c("#EEF2FF","#93A8D8","#2C3E6B"),
    na.value = "#F0F0F0",
    name     = "Articles",
    trans    = "sqrt",
    breaks   = c(1, 5, 15, 30, 60, 90)
  ) +
  coord_fixed(1.3, xlim = c(-170,180), ylim = c(-55,85)) +
  labs(
    title    = "Global Scientific Production by Country",
    subtitle = "Corresponding author affiliation",
    caption  = "Source: WoS + Scopus | Bibliometric analysis"
  ) +
  theme_paper() +
  theme(
    axis.title = element_blank(), axis.text  = element_blank(),
    panel.grid = element_blank(), panel.border = element_blank(),
    legend.key.width = unit(2, "cm")
  )

print(p02)
ggsave(file.path(DIR_FIGS, "fig02_world_map.png"), p02, width=14, height=7, dpi=300, bg=BG)
cat("    Guardado: figuras/fig02_world_map.png\n")

# =============================================================================
# FIG 03 — WORDCLOUD DE KEYWORDS
# =============================================================================

cat("\n>>> [03/13] Wordcloud...\n")
while(!is.null(dev.list())) dev.off()

kw_raw  <- paste(M$DE[!is.na(M$DE)], collapse = ";")
kw_list <- unlist(strsplit(kw_raw, ";"))
kw_list <- tolower(trimws(kw_list))
kw_list <- kw_list[nchar(kw_list) > 2 & kw_list != "na"]

kw_freq <- as.data.frame(table(kw_list), stringsAsFactors = FALSE)
names(kw_freq) <- c("word", "freq")
kw_freq <- kw_freq[order(-kw_freq$freq), ]
kw_freq <- kw_freq[kw_freq$freq >= 2, ]
kw_freq <- head(kw_freq, 80)

pal_wc <- colorRampPalette(c("#2C3E6B","#2980B9","#27AE60","#E67E22","#C0392B"))(8)

par(mar = c(2,2,3,2), bg = BG)
set.seed(42)
wordcloud(words=kw_freq$word, freq=kw_freq$freq, min.freq=2, max.words=80,
          random.order=FALSE, rot.per=0.25, colors=pal_wc, scale=c(4,0.4))
title("Author Keywords Frequency", cex.main=1.5, col.main="#2C3E6B")

png(file.path(DIR_FIGS, "fig03_wordcloud.png"), width=3000, height=2400, res=300, bg=BG)
par(mar = c(2,2,3,2), bg = BG)
set.seed(42)
wordcloud(words=kw_freq$word, freq=kw_freq$freq, min.freq=2, max.words=80,
          random.order=FALSE, rot.per=0.25, colors=pal_wc, scale=c(4,0.4))
title("Author Keywords Frequency", cex.main=1.5, col.main="#2C3E6B")
dev.off()
cat("    Guardado: figuras/fig03_wordcloud.png\n")

# =============================================================================
# FIG 04 — TREND TOPICS (EVOLUCION TEMPORAL DE KEYWORDS)
# =============================================================================

cat("\n>>> [04/13] Trend topics...\n")
while(!is.null(dev.list())) dev.off()

trend <- fieldByYear(M, field="DE", timespan=c(2014,2025),
                     min.freq=5, n.items=5, graph=FALSE)

p04 <- trend$graph +
  labs(
    title    = "Author Keywords Trend Over Time",
    subtitle = "Top emerging keywords by period · min. frequency = 5",
    caption  = "Source: WoS + Scopus | Bibliometric analysis"
  ) +
  theme_paper() +
  theme(legend.position = "right")

print(p04)
ggsave(file.path(DIR_FIGS, "fig04_trend_topics.png"), p04, width=14, height=7, dpi=300, bg=BG)
cat("    Guardado: figuras/fig04_trend_topics.png\n")

# =============================================================================
# FIG 05 — RED DE CO-AUTORIA ENTRE AUTORES
# =============================================================================

cat("\n>>> [05/13] Red co-autoria autores...\n")
while(!is.null(dev.list())) dev.off()

Net_AU <- biblioNetwork(M, analysis="collaboration",
                        network="authors", sep=";")

networkPlot(Net_AU, n=25, Title="Author Collaboration Network (Top 25)",
            type="fruchterman", size=TRUE, edgesize=3, labelsize=0.8)

png(file.path(DIR_FIGS, "fig05_coauthorship_authors.png"), width=3000, height=3000, res=300, bg=BG)
networkPlot(Net_AU, n=25, Title="Author Collaboration Network (Top 25)",
            type="fruchterman", size=TRUE, edgesize=3, labelsize=0.8)
dev.off()
cat("    Guardado: figuras/fig05_coauthorship_authors.png\n")

# =============================================================================
# FIG 06 — RED DE CO-AUTORIA ENTRE INSTITUCIONES
# =============================================================================

cat("\n>>> [06/13] Red co-autoria instituciones...\n")
while(!is.null(dev.list())) dev.off()

Net_UN <- biblioNetwork(M, analysis="collaboration",
                        network="universities", sep=";")

networkPlot(Net_UN, n=20, Title="University Collaboration Network (Top 20)",
            type="fruchterman", size=TRUE, edgesize=3, labelsize=0.7)

png(file.path(DIR_FIGS, "fig06_coauthorship_institutions.png"), width=3000, height=3000, res=300, bg=BG)
networkPlot(Net_UN, n=20, Title="University Collaboration Network (Top 20)",
            type="fruchterman", size=TRUE, edgesize=3, labelsize=0.7)
dev.off()
cat("    Guardado: figuras/fig06_coauthorship_institutions.png\n")

# =============================================================================
# FIG 07 — LEY DE LOTKA
# =============================================================================

cat("\n>>> [07/13] Ley de Lotka...\n")
while(!is.null(dev.list())) dev.off()

au_prod  <- as.data.frame(table(res$Authors))
names(au_prod) <- c("Author", "n_articles")

lotka_obs <- as.data.frame(table(au_prod$n_articles))
names(lotka_obs) <- c("n_articles", "n_authors")
lotka_obs$n_articles <- as.integer(as.character(lotka_obs$n_articles))
lotka_obs$prop_obs   <- lotka_obs$n_authors / sum(lotka_obs$n_authors)

fit  <- lm(log(prop_obs) ~ log(n_articles),
           data = lotka_obs[lotka_obs$n_articles <= 10, ])
beta <- -coef(fit)[2]
C    <- exp(coef(fit)[1])

lotka_obs$prop_teo <- C / (lotka_obs$n_articles ^ beta)
lotka_obs$prop_teo <- lotka_obs$prop_teo / sum(lotka_obs$prop_teo)
lotka_plot <- lotka_obs[lotka_obs$n_articles <= 10, ]

p07 <- ggplot(lotka_plot, aes(x = n_articles)) +
  geom_col(aes(y = prop_obs), fill="#2C3E6B", alpha=0.8, width=0.6) +
  geom_line(aes(y = prop_teo, color="Lotka's Law"),
            linewidth=1.3, linetype="dashed") +
  geom_point(aes(y = prop_teo, color="Lotka's Law"), size=3) +
  scale_color_manual(name="", values=c("Lotka's Law"="#C0392B")) +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  annotate("text",
           x     = max(lotka_plot$n_articles) * 0.65,
           y     = max(lotka_plot$prop_obs) * 0.85,
           label = sprintf("β = %.2f\nC = %.4f", beta, C),
           hjust=0, size=4, color="#333333", fontface="italic") +
  labs(
    title    = "Lotka's Law — Author Productivity",
    subtitle = "Observed vs. theoretical distribution of scientific output",
    x="Number of articles", y="Proportion of authors",
    caption="Source: WoS + Scopus | Bibliometric analysis"
  ) +
  theme_paper() +
  theme(legend.position = c(0.8, 0.8))

print(p07)
ggsave(file.path(DIR_FIGS, "fig07_lotka_law.png"), p07, width=10, height=6, dpi=300, bg=BG)
cat("    Guardado: figuras/fig07_lotka_law.png\n")

# =============================================================================
# FIG 08 — LEY DE BRADFORD
# =============================================================================

cat("\n>>> [08/13] Ley de Bradford...\n")
while(!is.null(dev.list())) dev.off()

so_freq <- as.data.frame(table(M$SO), stringsAsFactors=FALSE)
names(so_freq) <- c("Journal","Articles")
so_freq <- so_freq[order(-so_freq$Articles), ]
so_freq$rank     <- seq_len(nrow(so_freq))
so_freq$cum_art  <- cumsum(so_freq$Articles)
so_freq$cum_pct  <- so_freq$cum_art / sum(so_freq$Articles)
so_freq$log_rank <- log10(so_freq$rank)

total_art <- sum(so_freq$Articles)
zone_cut1 <- so_freq$rank[which(so_freq$cum_art >= total_art * 1/3)[1]]
zone_cut2 <- so_freq$rank[which(so_freq$cum_art >= total_art * 2/3)[1]]

so_freq$Zone <- case_when(
  so_freq$rank <= zone_cut1 ~ "Zone 1 (Core)",
  so_freq$rank <= zone_cut2 ~ "Zone 2",
  TRUE                      ~ "Zone 3 (Periphery)"
)

top30 <- head(so_freq, 30)
top30$Journal <- factor(top30$Journal, levels=rev(top30$Journal))

p08a <- ggplot(top30, aes(x=Articles, y=Journal, fill=Zone)) +
  geom_col(alpha=0.85) +
  scale_fill_manual(values=c(
    "Zone 1 (Core)"      = "#2C3E6B",
    "Zone 2"             = "#2980B9",
    "Zone 3 (Periphery)" = "#93A8D8"
  )) +
  scale_x_continuous(expand=expansion(mult=c(0,0.1))) +
  labs(
    title    = "Bradford's Law — Core Journals",
    subtitle = sprintf("Zone 1: %d journals · Zone 2: journals %d–%d · Zone 3: rest",
                       zone_cut1, zone_cut1+1, zone_cut2),
    x="Number of Articles", y=NULL, fill="Bradford Zone",
    caption="Source: WoS + Scopus | Bibliometric analysis"
  ) +
  theme_paper()

p08b <- ggplot(so_freq, aes(x=log_rank, y=cum_pct)) +
  geom_line(color="#2C3E6B", linewidth=1.2) +
  geom_vline(xintercept=log10(zone_cut1), color="#C0392B",
             linetype="dashed", linewidth=0.8) +
  geom_vline(xintercept=log10(zone_cut2), color="#E67E22",
             linetype="dashed", linewidth=0.8) +
  annotate("text", x=log10(zone_cut1)+0.05, y=0.1,
           label="Zone 1|2", color="#C0392B", size=3.5, hjust=0) +
  annotate("text", x=log10(zone_cut2)+0.05, y=0.1,
           label="Zone 2|3", color="#E67E22", size=3.5, hjust=0) +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(
    title    = "Bradford's Law — Cumulative Production",
    subtitle = "Cumulative % of articles by log(journal rank)",
    x="log10(Journal Rank)", y="Cumulative Articles (%)",
    caption="Source: WoS + Scopus | Bibliometric analysis"
  ) +
  theme_paper()

print(p08a)
print(p08b)
ggsave(file.path(DIR_FIGS, "fig08a_bradford_journals.png"), p08a, width=10, height=10, dpi=300, bg=BG)
ggsave(file.path(DIR_FIGS, "fig08b_bradford_curve.png"),   p08b, width=10, height=6,  dpi=300, bg=BG)
cat("    Guardado: figuras/fig08a_bradford_journals.png\n")
cat("    Guardado: figuras/fig08b_bradford_curve.png\n")

# =============================================================================
# FIG 09 — MAPA TEMATICO
# =============================================================================

cat("\n>>> [09/13] Mapa tematico...\n")
while(!is.null(dev.list())) dev.off()

Map <- thematicMap(M, field="DE", n=250, minfreq=3)
plot(Map$map)
ggsave(file.path(DIR_FIGS, "fig09_thematic_map.png"), plot=Map$map, width=10, height=8, dpi=300, bg=BG)
cat("    Guardado: figuras/fig09_thematic_map.png\n")

# =============================================================================
# FIG 10 — RED DE CO-OCURRENCIA DE KEYWORDS
# =============================================================================

cat("\n>>> [10/13] Red co-ocurrencia keywords...\n")
while(!is.null(dev.list())) dev.off()

Net_KW <- biblioNetwork(M, analysis="co-occurrences",
                        network="author_keywords", sep=";")

networkPlot(Net_KW, n=30, Title="Keyword Co-occurrence Network",
            type="fruchterman", size=TRUE, edgesize=5, labelsize=1)

png(file.path(DIR_FIGS, "fig10_keyword_cooccurrence.png"), width=3000, height=3000, res=300, bg=BG)
networkPlot(Net_KW, n=30, Title="Keyword Co-occurrence Network",
            type="fruchterman", size=TRUE, edgesize=5, labelsize=1)
dev.off()
cat("    Guardado: figuras/fig10_keyword_cooccurrence.png\n")

# =============================================================================
# FIG 11 — RED DE COLABORACION ENTRE PAISES
# =============================================================================

cat("\n>>> [11/13] Red colaboracion paises...\n")
while(!is.null(dev.list())) dev.off()

Net_CO <- biblioNetwork(M, analysis="collaboration",
                        network="countries", sep=";")

networkPlot(Net_CO, n=20, Title="International Collaboration Network",
            type="circle", size=TRUE, edgesize=5, labelsize=1)

png(file.path(DIR_FIGS, "fig11_country_collaboration.png"), width=3000, height=3000, res=300, bg=BG)
networkPlot(Net_CO, n=20, Title="International Collaboration Network",
            type="circle", size=TRUE, edgesize=5, labelsize=1)
dev.off()
cat("    Guardado: figuras/fig11_country_collaboration.png\n")

# =============================================================================
# FIG 12 — PRODUCCION DE AUTORES EN EL TIEMPO
# =============================================================================

cat("\n>>> [12/13] Produccion autores en el tiempo...\n")
while(!is.null(dev.list())) dev.off()

apot <- authorProdOverTime(M, k=15, graph=FALSE)

p12 <- ggplot(apot$dfAU,
              aes(x=year, y=Author, size=freq, color=Author)) +
  geom_point(alpha=0.75, shape=16) +
  geom_line(aes(group=Author), linewidth=0.3, color="#7F8C8D", alpha=0.4) +
  scale_size_continuous(name="Articles", range=c(2,12)) +
  scale_color_manual(values=colorRampPalette(PAL)(15), guide="none") +
  scale_x_continuous(breaks=scales::pretty_breaks(n=8)) +
  labs(
    title    = "Author Scientific Production Over Time",
    subtitle = "Top 15 most productive authors",
    x="Year", y=NULL,
    caption="Source: WoS + Scopus | Bibliometric analysis"
  ) +
  theme_paper() +
  theme(legend.position="right")

print(p12)
ggsave(file.path(DIR_FIGS, "fig12_author_production.png"), p12, width=12, height=7, dpi=300, bg=BG)
cat("    Guardado: figuras/fig12_author_production.png\n")

# =============================================================================
# FIG 13 — THREE-FIELDS PLOT / SANKEY
# =============================================================================

cat("\n>>> [13/13] Three-fields Sankey...\n")
while(!is.null(dev.list())) dev.off()

sankey <- threeFieldsPlot(M, fields=c("AU_CO","AU","DE"), n=c(15,15,15))
print(sankey)   # visualizar en Viewer

# threeFieldsPlot devuelve un widget HTML (networkD3), no un grafico base de R.
# png() no puede capturarlo. Se guarda via htmlwidgets + webshot2.
if (!requireNamespace("htmlwidgets", quietly=TRUE)) install.packages("htmlwidgets")
if (!requireNamespace("webshot2",    quietly=TRUE)) install.packages("webshot2")

tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(sankey, tmp_html, selfcontained = TRUE)
webshot2::webshot(tmp_html,
                  file   = file.path(DIR_FIGS, "fig13_sankey.png"),
                  vwidth = 1800, vheight = 1350,
                  zoom   = 2)
unlink(tmp_html)
cat("    Guardado: figuras/fig13_sankey.png\n")

# =============================================================================
# FIN
# =============================================================================

while(!is.null(dev.list())) dev.off()
cat("\n======================================\n")
cat("  ANALISIS COMPLETADO — 14 figuras\n")
cat("  Fuente: ", ARCHIVO_DATOS, "\n")
cat("======================================\n")