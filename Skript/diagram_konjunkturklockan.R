# ============================================================
# SCB:s konjunkturklocka — förenklad version för BNP kvartal
# "Vår" version (enkel HP) jämförd med SCB:s publicerade värden
# ============================================================
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(mFilter)

if (!requireNamespace("pxweb2r", quietly = TRUE)) {
  remotes::install_github("FaluPeppe/pxweb2r")
}

# ============================================================
# BLOCK A — SCB:s egna klockvärden för BNP kvartal (valideringsmål)
#           från TAB5161 (NR0001, Konjunkturklockan)
# ============================================================
scb_raw <- pxweb2r::pxweb2_get_data(
  table = "TAB5161",
  query = list(
    FinansiellIndikator = "020",   # BNP kvartal
    ContentsCode        = "*",     # Konjunkturläge + Förändring
    Tid                 = "*"
  ), quiet = TRUE)

# tid-koderna är månadsstämplade för en kvartalsserie; behåll rader med värde.
# Kvartalsvärdet antas ligga på kvartalets sista månad (M03/M06/M09/M12).
scb_clock <- scb_raw %>%
  mutate(matt = case_when(
    str_detect(tabellinnehåll, "Konjunkturläge") ~ "y",      # avvikelse från trend
    str_detect(tabellinnehåll, "Förändring")     ~ "x_raw"   # förändring föreg. period
  )) %>%
  select(tid_m = månad, matt, value) %>%
  pivot_wider(names_from = matt, values_from = value) %>%
  filter(!is.na(y), str_sub(tid_m, 6, 7) %in% c("03", "06", "09", "12")) %>%
  mutate(
    # mappa 2000M03 -> 2000K1 osv.
    ar      = str_sub(tid_m, 1, 4),
    kv      = recode(str_sub(tid_m, 6, 7),
                     "03" = "1", "06" = "2", "09" = "3", "12" = "4"),
    tid     = paste0(ar, "K", kv),
    x       = -x_raw,        # SCB vänder vågräta axeln (medurs)
    source  = "SCB"
  ) %>%
  select(tid, x, y, source)

# ============================================================
# BLOCK B — "Vår" version, från säsongrensad real BNP (id 4553)
# ============================================================
bnp_raw <- pxweb2r::pxweb2_get_data(
  table = "TAB4553",                    # säsongrensad försörjningsbalans
  query = list(
    Anvandningstyp = "BNPM",         # BNP till marknadspris
    ContentsCode   = "NR0103CE",     # säsongrensad, fasta priser ref 2025
    Tid            = "*"
  ), quiet = TRUE)

bnp_q <- bnp_raw %>%
  arrange(kvartal) %>%
  filter(kvartal >= "2000K1")

gdp_sa <- ts(bnp_q$value, start = c(2000, 1), frequency = 4)

# Enkel HP (lambda = 50 000 för kvartal) på den redan säsongrensade serien
trend  <- as.numeric(hpfilter(gdp_sa, freq = 50000, type = "lambda")$trend)
gap    <- as.numeric(gdp_sa) - trend

y_std  <- (gap - mean(gap)) / sd(gap)   # standardiserad avvikelse från trend
x_chg  <- c(NA, diff(y_std))            # förändring från föregående kvartal

our_clock <- tibble(
  tid    = bnp_q$kvartal,
  y      = y_std,
  x      = -x_chg,                      # vänd axeln (medurs)
  source = "Vår"
) %>% na.omit()

# ============================================================
# DIAGNOSTIK — SCB vs vår, senaste 8 kvartalen, sida vid sida
# ============================================================
comparison <- our_clock %>%
  transmute(tid, var_x = x, var_y = y) %>%
  inner_join(
    scb_clock %>% transmute(tid, scb_x = x, scb_y = y),
    by = "tid"
  ) %>%
  mutate(dy = var_y - scb_y, dx = var_x - scb_x) %>%
  slice_tail(n = 8)

print(comparison, n = 8)

# ============================================================
# BLOCK C — rita klockan med senaste kvartalet för båda
# ============================================================
latest <- bind_rows(
  scb_clock %>% slice_tail(n = 1),
  our_clock %>% slice_tail(n = 1) %>% select(tid, x, y, source)
)

# --- Klockans konstanter ---
colors <- c(
  "#F4A582",  # [1] Återhämtning – ljus orange (under trend, stigande)
  "#D6604D",  # [2] Recession    – röd (under trend, fallande)
  "#B8E0B8",  # [3] Avmattning   – ljusgrön (över trend, fallande)
  "#4D9A4D"   # [4] Expansion    – grön (över trend, stigande)
)
axis_limit     <- 5
max_distance_x <- axis_limit
max_distance_y <- axis_limit
middle_top_x    <- 0
middle_bottom_x <- 0
middle_left_y   <- 0
middle_right_y  <- 0

# --- Klockan ---
p <- ggplot() +
  annotate("rect", xmin = -max_distance_x, xmax = 0, ymin = -max_distance_y, ymax = 0, fill = colors[1]) +
  annotate("rect", xmin = 0, xmax = max_distance_x, ymin = -max_distance_y, ymax = 0, fill = colors[2]) +
  annotate("rect", xmin = 0, xmax = max_distance_x, ymin = 0, ymax = max_distance_y, fill = colors[3]) +
  annotate("rect", xmin = -max_distance_x, xmax = 0, ymin = 0, ymax = max_distance_y, fill = colors[4]) +
  annotate("text", x = -max_distance_x, y = max_distance_y,  label = "Expansion",    hjust = 0, vjust = 1) +
  annotate("text", x =  max_distance_x, y = max_distance_y,  label = "Avmattning",   hjust = 1, vjust = 1) +
  annotate("text", x =  max_distance_x, y = -max_distance_y, label = "Recession",    hjust = 1, vjust = 0) +
  annotate("text", x = -max_distance_x, y = -max_distance_y, label = "Återhämtning", hjust = 0, vjust = 0) +
  annotate("text", x = middle_top_x,    y = max_distance_y,  label = "Över trend",   vjust = -0.5) +
  annotate("text", x = middle_bottom_x, y = -max_distance_y, label = "Under trend",  vjust = 1.5) +
  annotate("text", x = -max_distance_x, y = middle_left_y,   label = "Ökande",   hjust = 0.5, angle = 90) +
  annotate("text", x =  max_distance_x, y = middle_right_y,  label = "Minskande",hjust = 0.5, angle = 270) +
  geom_point(data = latest, aes(x, y, colour = source), size = 4) +
  scale_colour_manual(values = c("SCB" = "#1f4e79", "Vår" = "#c00000")) +
  coord_fixed(ratio = 1, xlim = c(-axis_limit, axis_limit), ylim = c(-axis_limit, axis_limit)) +
  labs(colour = NULL) +
  theme_void()

print(p)

# Does our RAW gap, re-standardised to match SCB's spread, line up better?
chk <- our_clock %>%
  transmute(tid, var_y = y) %>%
  inner_join(
    scb_clock %>% transmute(tid, scb_y = y),
    by = "tid"
  )

cor(chk$var_y, chk$scb_y, use = "complete.obs")
coef(lm(scb_y ~ var_y, data = chk))
