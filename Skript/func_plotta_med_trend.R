library(dplyr)
library(ggplot2)
library(rlang)

# ============================================================
# plotta_med_trend()
#   Ritar en tidsserie med en utjämnad loess-trend ovanpå, i
#   Region Dalarnas figurstil.
#
#   data       : data frame med en tids- och en värdekolumn
#   tidskol    : tidskolumn (oquoterad). Hanterar Date, "YYYY-MM",
#                "YYYYMmm" (2025M06) och "YYYYQn"/"YYYYKn" (2011Q1/2011K1)
#   vardekol   : värdekolumn (oquoterad)
#   titel      : diagramrubrik
#   y_titel    : y-axelns etikett
#   span       : loess-fönstrets bredd (lägre = följer kortare rörelser)
#   fran_noll  : TRUE -> y-axeln börjar på 0
#   x_format   : "ar" (årsetiketter), "manad" ("YYYY-MM"), "kvartal" ("YYYYKQ")
#   intervall  : visa var N:te etikett i vald x_format-enhet (första+sista tas alltid med)
#   gles_slut  : TRUE -> ta bort intervall-etiketter som ligger för nära sista
#                observationen, så att den framtvingade sista etiketten får plats
#   enhet_txt  : namn på den observerade serien (legend)
#   farg_data  : färg på observerade serien
#   farg_trend : färg på trendlinjen
#   kalla_txt  : källtext (hamnar i caption)
# ============================================================
plotta_med_trend <- function(data, tidskol, vardekol,
                             titel = NULL, y_titel = NULL,
                             span = 0.3, fran_noll = FALSE,
                             x_format = c("ar", "manad", "kvartal"),
                             intervall = 1,
                             gles_slut = FALSE,
                             enhet_txt = "Observerat värde",
                             farg_data  = rddiagram::diagramfarger("rus_sex")[4],
                             farg_trend = "black",
                             kalla_txt = "Källa: ...\nBearbetning: Samhällsanalys, Region Dalarna") {
  
  tidskol  <- enquo(tidskol)
  vardekol <- enquo(vardekol)
  x_format <- match.arg(x_format)
  
  # --- Förbered data: gör om tid till Date (flera format hanteras) ---
  df <- data %>%
    transmute(
      datum = {
        rawtid <- !!tidskol
        if (inherits(rawtid, "Date")) {
          rawtid
        } else {
          s <- as.character(rawtid)
          if (all(grepl("^[0-9]{4}[QK][1-4]$", s))) {
            ar <- as.integer(substr(s, 1, 4))
            kv <- as.integer(substr(s, 6, 6))
            as.Date(paste0(ar, "-", (kv - 1) * 3 + 1, "-01"))
          } else if (all(grepl("^[0-9]{4}M[0-9]{2}$", s))) {
            as.Date(paste0(substr(s, 1, 4), "-", substr(s, 6, 7), "-01"))
          } else {
            as.Date(paste0(s, "-01"))
          }
        }
      },
      value = !!vardekol
    ) %>%
    arrange(datum)
  
  # --- 1. Kandidat-brytpunkter för valt format ---
  if (x_format == "ar") {
    ar       <- sort(unique(as.integer(format(df$datum, "%Y"))))
    kandidat <- as.Date(paste0(ar, "-01-01"))
    etikett_format <- "%Y"
  } else if (x_format == "manad") {
    kandidat <- seq(min(df$datum), max(df$datum), by = "month")
    etikett_format <- "%Y-%m"
  } else {  # kvartal
    alla_man <- seq(min(df$datum), max(df$datum), by = "month")
    kandidat <- alla_man[as.integer(format(alla_man, "%m")) %in% c(1, 4, 7, 10)]
    etikett_format <- NULL
  }
  
  # --- 2. Behåll var N:te, tvinga in första och sista ---
  valda  <- kandidat[seq(1, length(kandidat), by = intervall)]
  forsta <- kandidat[1]
  sista  <- kandidat[length(kandidat)]
  
  # Ta ev. bort intervall-brytpunkter som ligger för nära slutet (< ett intervall-steg)
  if (gles_slut && length(kandidat) > 1) {
    index_sista <- length(kandidat)
    for_nara <- vapply(valda, function(b) {
      idx <- which(kandidat == b)
      (index_sista - idx) < intervall && (index_sista - idx) > 0
    }, logical(1))
    valda <- valda[!for_nara]
  }
  
  brytpunkter <- sort(unique(c(forsta, valda, sista)))
  
  if (length(brytpunkter) > 40)
    warning("Många x-etiketter (", length(brytpunkter),
            ") — överväg högre 'intervall'.")
  
  # --- Etiketter: kvartal byggs som "YYYYKQ", annars via date-format ---
  kvartalsetikett <- function(d) {
    paste0(format(d, "%Y"), "K", (as.integer(format(d, "%m")) - 1) %/% 3 + 1)
  }
  etikett_fn <- if (x_format == "kvartal") kvartalsetikett
  else function(d) format(d, etikett_format)
  
  # --- Axlar ---
  x_scale <- scale_x_date(
    breaks = brytpunkter, labels = etikett_fn,
    limits = c(min(df$datum), max(df$datum)),
    expand = expansion(mult = c(0.01, 0.01))
  )
  
  y_scale <- if (fran_noll) {
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
  } else {
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)))
  }
  
  # --- Figur ---
  ggplot(df, aes(datum, value)) +
    geom_line(aes(colour = enhet_txt), linewidth = 0.6) +
    geom_smooth(aes(colour = "Utjämnad trend"),
                se = FALSE, method = "loess", span = span, linewidth = 0.8) +
    x_scale +
    y_scale +
    scale_colour_manual(values = setNames(
      c(farg_data, farg_trend),
      c(enhet_txt, "Utjämnad trend")
    )) +
    labs(
      title = titel, x = NULL, y = y_titel, colour = NULL,
      caption = paste0(
        kalla_txt, "\n",
        "Observera: den utjämnade trenden är preliminär i seriens slut ",
        "och kan ändras när ny data tillkommer."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey75", linewidth = 0.5),
      panel.grid.minor.y = element_line(colour = "grey90", linewidth = 0.3),
      plot.title    = element_text(size = 20, hjust = 0.5),
      legend.position = "bottom",
      axis.text.x   = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title.y  = element_text(size = 10),
      axis.text     = element_text(),
      axis.ticks    = element_blank(),
      plot.caption = element_text(size = 11, face = "italic", hjust = 0,
                                  margin = margin(t = 10)),
      plot.caption.position = "plot",
      plot.margin   = margin(15, 20, 10, 15)
    )
}

# --- Ditt anrop (nystartade, kvartal) med glesare slut ---
# plotta_med_trend(nystartade_df, period, value,
#                  titel = "Nystartade företag i Dalarna per kvartal",
#                  y_titel = "Antal", enhet_txt = "Nystartade",
#                  span = 0.3, fran_noll = TRUE,
#                  x_format = "kvartal", intervall = 4, gles_slut = TRUE,
#                  kalla_txt = "Källa: Tillväxtanalys\nBearbetning: Samhällsanalys, Region Dalarna")