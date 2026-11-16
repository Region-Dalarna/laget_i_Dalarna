# ============================================================
# Regional konjunkturklocka — Dalarna
# stl-baserad säsongrensning (ingen X-13), tre indikatorer
# Fordon: robust STL + vinsorisering (bonus malus-spikar)
# ============================================================
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(mFilter)

# ------------------------------------------------------------
# berakna_konjunkturklocka()
#   df         : tibble med kolumnerna `period` (chr) + `value` (dbl), stigande i tid
#   frekvens   : "manad" eller "kvartal"
#   sasongrensa: TRUE -> stl()-säsongrensning; FALSE -> redan rensad
#   tecken     : +1 normalt, -1 för negativt korrelerade serier (konkurser)
#   start      : c(år, period) för ts()
#   namn       : etikett
#   robust     : TRUE -> stl(robust = TRUE), nedviktar extremvärden
#   winsor     : NULL = av; annars kapa säsongrensad serie till ±winsor sd
# Returnerar: tibble(tid, x, y, indikator)
# ------------------------------------------------------------
berakna_konjunkturklocka <- function(df, frekvens, sasongrensa,
                                     tecken = 1, start, namn,
                                     robust = FALSE, winsor = NULL) {
  
  freq_n <- if (frekvens == "manad") 12 else 4
  lambda <- if (frekvens == "manad") 1000000 else 50000
  
  serie <- ts(df$value, start = start, frequency = freq_n)
  
  # Säsongrensa med stl() (loess-baserad, ingen extern binär)
  if (sasongrensa) {
    stl_fit <- stl(serie, s.window = "periodic", robust = robust)
    serie   <- serie - stl_fit$time.series[, "seasonal"]
  }
  
  # Vinsorisering: kapa extremvärden (t.ex. bonus malus) till ±winsor sd
  if (!is.null(winsor)) {
    m <- mean(serie); s <- sd(serie)
    serie <- pmin(pmax(serie, m - winsor * s), m + winsor * s)
    serie <- ts(serie, start = start, frequency = freq_n)
  }
  
  # Lång trend via HP, cykel = serie - trend
  trend <- as.numeric(hpfilter(serie, freq = lambda, type = "lambda")$trend)
  gap   <- as.numeric(serie) - trend
  
  # Standardisera (medelvärde 0, sd 1), teckenbyte, omvänd x-axel
  y_std <- (gap - mean(gap)) / sd(gap)
  y_std <- tecken * y_std
  x_chg <- c(NA, diff(y_std))
  
  tibble(tid = df$period, y = y_std, x = -x_chg, indikator = namn) %>%
    na.omit()
}

# ============================================================
# Hämta och förbered indikatorerna
# ============================================================

# --- D: Fordon (månad, 2006->) ---
fordon_df <- pxweb2r::pxweb2_get_data(
  table = "TAB3277",
  query = list(Region = "20", Drivmedel = "*", ContentsCode = "*", Tid = "*"),
  quiet = TRUE) %>%
  group_by(månad) %>% summarise(value = sum(value), .groups = "drop") %>%
  arrange(månad) %>% transmute(period = månad, value) 

# --- F: AKU sysselsatta (kvartal, 2005->, ej säsongrensad) ---
aku_df <- pxweb2r::pxweb2_get_data(
  table = "TAB2923",
  query = list(
    Region            = "20",
    Arbetskraftstillh = "SYS",
    Kon               = "1+2",
    ContentsCode      = "AM0401UZ",
    Tid               = "*"
  ), quiet = TRUE) %>%
  arrange(kvartal) %>% filter(kvartal >= "2005K1") %>%
  transmute(period = kvartal, value)

# --- Konkurser (månad, 1996->, NEGATIV korr) ---
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_lan_variabel_konk_1996_tva.R")
konkurser_df <- hamta_konkurser_manad_lan_variabel_tva(
  region_vekt = "20",
  variabel_klartext = "Antal konkurser",
  manad_klartext = "*") %>%
  rename(value = last_col()) %>%
  arrange(månad) %>% transmute(period = månad, value)

# ============================================================
# Kör indikatorerna genom funktionen
# (Nystartade företag borttagen: i princip acyklisk, r ≈ -0,20 mot AKU)
# ============================================================
klocka <- bind_rows(
  berakna_konjunkturklocka(fordon_df,    "manad",   sasongrensa = TRUE,
                           tecken = +1, start = c(2006, 1),
                           namn = "Nyreg. fordon",
                           robust = TRUE, winsor = 3),       # robust + vinsor
  berakna_konjunkturklocka(aku_df,       "kvartal", sasongrensa = TRUE,
                           tecken = +1, start = c(2005, 1),
                           namn = "Sysselsättning (AKU)"),
  berakna_konjunkturklocka(konkurser_df, "manad",   sasongrensa = TRUE,
                           tecken = -1, start = c(1996, 1),
                           namn = "Konkurser")
)

# ============================================================
# DIAGNOSTIK — standardiserade cykler över tid (kör FÖRST)
# ============================================================
klocka_ts <- klocka %>%
  mutate(
    ar   = as.integer(str_sub(tid, 1, 4)),
    sub  = as.integer(str_extract(tid, "(?<=[MK])\\d+")),
    frek = if_else(str_detect(tid, "K"), 4L, 12L),
    tidspunkt = ar + (sub - 1) / frek
  )

ggplot(klocka_ts, aes(tidspunkt, y)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line() +
  facet_wrap(~indikator, scales = "free_x") +
  labs(x = NULL, y = "Standardiserad konjunkturcykel") +
  theme_minimal()

# ============================================================
# FÄRDIG FIGUR — regional konjunkturklocka, senaste läget
# ============================================================

# Senaste perioden per indikator
senaste <- klocka %>%
  group_by(indikator) %>%
  slice_tail(n = 1) %>%
  ungroup()

# --- Bygg en dynamisk "per period"-text till bildtexten ---
# (t.ex. "Nyreg. fordon 2026M08, Konkurser 2026M09, Sysselsättning (AKU) 2026K2")
perioder_txt <- senaste %>%
  arrange(indikator) %>%
  mutate(rad = paste0(indikator, " ", tid)) %>%
  dplyr::pull(rad) %>%
  paste(collapse = ", ")

bildtext <- paste0(
  "Avser senaste tillgängliga period per indikator: ", perioder_txt, ".\n",
  "Metod: förenklad variant av SCB:s konjunkturklocka. Kortsiktig avvikelse ",
  "från långsiktig trend (HP-filter, λ = 50 000/1 000 000), standardiserad ",
  "(medelvärde 0). Säsongrensning med STL. Konkurser är negativt korrelerad ",
  "med konjunkturen och har vänts om. Fordonsserien har justerats för ",
  "extremvärden kopplade till regeländringar (bonus malus). Konkursserien är ",
  "volatil på regional månadsbasis och bör tolkas med försiktighet.\n",
  "Källa: SCB (AKU, fordon) och Tillväxtanalys (konkurser). ",
  "Bearbetning: Samhällsanalys, Region Dalarna."
)

# --- Klockans konstanter ---
colors <- c("#F4A582", "#D6604D", "#B8E0B8", "#4D9A4D")  # Återh/Rec/Avm/Exp
axis_limit <- 3
mdx <- axis_limit; mdy <- axis_limit

# --- Figur ---
p <- ggplot() +
  # Kvadranter
  annotate("rect", xmin = -mdx, xmax = 0, ymin = -mdy, ymax = 0, fill = colors[1]) +
  annotate("rect", xmin = 0, xmax = mdx, ymin = -mdy, ymax = 0, fill = colors[2]) +
  annotate("rect", xmin = 0, xmax = mdx, ymin = 0, ymax = mdy, fill = colors[3]) +
  annotate("rect", xmin = -mdx, xmax = 0, ymin = 0, ymax = mdy, fill = colors[4]) +
  # Mittlinjer
  annotate("segment", x = 0, xend = 0, y = -mdy, yend = mdy, colour = "white", linewidth = 0.4) +
  annotate("segment", x = -mdx, xend = mdx, y = 0, yend = 0, colour = "white", linewidth = 0.4) +
  # Fasetiketter i hörnen
  annotate("text", x = -mdx + 0.1, y = mdy - 0.1, label = "Expansion",    hjust = 0, vjust = 1, fontface = "bold", colour = "grey20") +
  annotate("text", x =  mdx - 0.1, y = mdy - 0.1, label = "Avmattning",   hjust = 1, vjust = 1, fontface = "bold", colour = "grey20") +
  annotate("text", x =  mdx - 0.1, y = -mdy + 0.1, label = "Recession",   hjust = 1, vjust = 0, fontface = "bold", colour = "grey20") +
  annotate("text", x = -mdx + 0.1, y = -mdy + 0.1, label = "Återhämtning",hjust = 0, vjust = 0, fontface = "bold", colour = "grey20") +
  # Axeltext på kanterna
  annotate("text", x = 0, y = mdy,  label = "Över trend",  vjust = -0.6, size = 3.3, colour = "grey30") +
  annotate("text", x = 0, y = -mdy, label = "Under trend", vjust = 1.6,  size = 3.3, colour = "grey30") +
  annotate("text", x = -mdx, y = 0, label = "Ökande",   angle = 90,  vjust = -0.6, size = 3.3, colour = "grey30") +
  annotate("text", x =  mdx, y = 0, label = "Minskande", angle = 270, vjust = -0.6, size = 3.3, colour = "grey30") +
  # Indikatorpunkter
  geom_point(data = senaste, aes(x, y, fill = indikator),
             size = 5, shape = 21, colour = "white", stroke = 1) +
  scale_fill_brewer(palette = "Set1") +
  coord_fixed(ratio = 1, xlim = c(-axis_limit, axis_limit),
              ylim = c(-axis_limit, axis_limit), clip = "off") +
  labs(
    title    = "Konjunkturläge i Dalarna",
    subtitle = "Förenklad konjunkturklocka — tre regionala indikatorer",
    fill     = NULL,
    caption  = bildtext
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, colour = "grey30", margin = margin(b = 8)),
    plot.caption  = element_text(size = 7.5, colour = "grey40", hjust = 0,
                                 margin = margin(t = 10)),
    legend.position = "right",
    plot.margin   = margin(15, 15, 15, 15)
  )

print(p)

# --- Spara (justera mått/format efter rapportens mall) ---
# ggsave("konjunkturklocka_dalarna.png", p, width = 9, height = 7, dpi = 300, bg = "white")