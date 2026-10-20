# ============================================================
# Regional konjunkturklocka för Dalarna
# ------------------------------------------------------------
# Skapar två figurer:
#   1. En förenklad "konjunkturklocka" (efter SCB:s modell) som visar
#      var i konjunkturcykeln två regionala indikatorer befinner sig:
#      sysselsättning (AKU) och nyregistrerade fordon.
#   2. Ett separat linjediagram över antal konkurser i Dalarna.
#
# KONJUNKTURKLOCKAN
#   Varje indikator placeras i ett koordinatsystem med fyra kvadranter.
#   - Lodrät axel: avvikelse från långsiktig trend (över/under trend).
#   - Vågrät axel: förändring sedan föregående period (ökande/minskande),
#     OMVÄND skala så att punkterna rör sig medurs genom cykeln:
#     Expansion -> Avmattning -> Recession -> Återhämtning.
#   Beräkningen per indikator (funktionen berakna_konjunkturklocka):
#     a) säsongrensning med STL (ingen extern X-13 krävs),
#     b) långsiktig trend skattas med HP-filter,
#     c) cykeln = serie minus trend, standardiserad (medelvärde 0).
#   Negativt korrelerade serier kan vändas om (argumentet `tecken`), och
#   serier med extremvärden kan vinsoriseras (argumentet `winsor`) –
#   fordonsserien justeras t.ex. för bonus malus-relaterade toppar.
#
# VAL AV INDIKATORER
#   SCB:s nationella klocka bygger på 14 indikatorer. På regional nivå
#   finns långt färre serier med tillräcklig frekvens och längd. Efter
#   test behölls sysselsättning (AKU) och nyregistrerade fordon, som båda
#   är stabila. Konkurser är för brusiga på regional nivå för att ge en
#   tillförlitlig punkt i klockan (positionen hoppar mellan kvadranter
#   när en enskild period läggs till/tas bort) och visas därför i ett
#   eget diagram istället. Nystartade företag uteslöts (i princip
#   okorrelerad med konjunkturen).
#
# VIKTIGT ATT VETA
#   - Det SENASTE läget är preliminärt. HP-filtret är känsligt i seriens
#     slut och punkterna kan flytta sig när ny data tillkommer – precis
#     som i SCB:s egen klocka, som revideras varje månad.
#   - Detta är en FÖRENKLAD variant. SCB använder en tvåstegsmetod
#     (Henderson- följt av HP-filter); vår enklare enstegs-HP ger en
#     något större amplitud men samma faser och vändpunkter.
#
# ANVÄNDNING
#   figurer <- skapa_konjunkturfigurer()                      # konkurser/kvartal
#   figurer <- skapa_konjunkturfigurer(konkurser_frekvens = "manad")
#   figurer <- skapa_konjunkturfigurer(spara_figur = TRUE)    # sparar PNG
#   figurer$klocka ; figurer$konkurser                        # hämta figurerna
#
# DATAKÄLLOR
#   SCB (AKU: TAB2923, fordon: TAB3277) via pxweb2r.
#   Tillväxtanalys (konkurser) via Region Dalarnas hamta_data-skript.
# ============================================================
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(mFilter)

# ------------------------------------------------------------
# Hjälpfunktion: en indikators klockkoordinater
# ------------------------------------------------------------
berakna_konjunkturklocka <- function(df, frekvens, sasongrensa,
                                     tecken = 1, start, namn,
                                     robust = FALSE, winsor = NULL) {
  freq_n <- if (frekvens == "manad") 12 else 4
  lambda <- if (frekvens == "manad") 1000000 else 50000
  
  serie <- ts(df$value, start = start, frequency = freq_n)
  
  if (sasongrensa) {
    stl_fit <- stl(serie, s.window = "periodic", robust = robust)
    serie   <- serie - stl_fit$time.series[, "seasonal"]
  }
  if (!is.null(winsor)) {
    m <- mean(serie); s <- sd(serie)
    serie <- pmin(pmax(serie, m - winsor * s), m + winsor * s)
    serie <- ts(serie, start = start, frequency = freq_n)
  }
  
  trend <- as.numeric(hpfilter(serie, freq = lambda, type = "lambda")$trend)
  gap   <- as.numeric(serie) - trend
  y_std <- (gap - mean(gap)) / sd(gap)
  y_std <- tecken * y_std
  x_chg <- c(NA, diff(y_std))
  
  tibble(tid = df$period, y = y_std, x = -x_chg, indikator = namn) %>% na.omit()
}

# ------------------------------------------------------------
# HUVUDFUNKTION
#   region            : läns-kod (SCB), default "20" = Dalarna
#   konkurser_frekvens: "kvartal" (default) eller "manad" för konkursdiagrammet
#   axis_limit        : klockans halva axelintervall
# Returnerar en lista: $klocka, $konkurser (ggplot) + $klocka_data, $konkurser_data
# ------------------------------------------------------------
skapa_konjunkturfigurer <- function(region = "20",
                                    konkurser_frekvens = "kvartal", # Finns även manad
                                    axis_limit = 3,
                                    spara_figur = FALSE,
                                    sokvag = ".",
                                    returnera_df_rmarkdown = FALSE) {
  
  konkurser_frekvens <- match.arg(konkurser_frekvens, c("kvartal", "manad"))
  
  # ---- Hämta data -------------------------------------------------------
  fordon_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3277",
    query = list(Region = region, Drivmedel = "*", ContentsCode = "*", Tid = "*"),
    quiet = TRUE) %>%
    group_by(månad) %>% summarise(value = sum(value), .groups = "drop") %>%
    arrange(månad) %>% transmute(period = månad, value)
  
  aku_df <- pxweb2r::pxweb2_get_data(
    table = "TAB2923",
    query = list(Region = region, Arbetskraftstillh = "SYS", Kon = "1+2",
                 ContentsCode = "AM0401UZ", Tid = "*"),
    quiet = TRUE) %>%
    arrange(kvartal) %>% filter(kvartal >= "2005K1") %>%
    transmute(period = kvartal, value)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_lan_variabel_konk_1996_tva.R")
  konkurser_manad <- hamta_konkurser_manad_lan_variabel_tva(
    region_vekt = region,
    variabel_klartext = "Antal konkurser",
    manad_klartext = "*") %>%
    rename(value = last_col()) %>%
    arrange(månad) %>% transmute(period = månad, value)
  
  # Konkurser: valbar frekvens för diagrammet
  if (konkurser_frekvens == "kvartal") {
    konkurser_df <- konkurser_manad %>%
      mutate(
        ar     = as.integer(str_sub(period, 1, 4)),
        man_nr = as.integer(str_sub(period, 6, 7)),
        kv_nr  = (man_nr - 1) %/% 3 + 1,
        per_q  = paste0(ar, "K", kv_nr)
      ) %>%
      group_by(per_q) %>%
      summarise(value = sum(value), n_man = n(), .groups = "drop") %>%
      filter(n_man == 3) %>%
      transmute(period = per_q, value) %>%
      arrange(period)
  } else {
    konkurser_df <- konkurser_manad
  }
  
  # ---- Klockan: AKU + fordon -------------------------------------------
  klocka_data <- bind_rows(
    berakna_konjunkturklocka(fordon_df, "manad",   sasongrensa = TRUE,
                             tecken = +1, start = c(2006, 1),
                             namn = "Nyreg. fordon", robust = TRUE, winsor = 3),
    berakna_konjunkturklocka(aku_df,    "kvartal", sasongrensa = TRUE,
                             tecken = +1, start = c(2005, 1),
                             namn = "Sysselsättning (AKU)")
  )
  
  if(returnera_df_rmarkdown == TRUE){
    assign("konjunkturklockan_df", klocka_data, envir = .GlobalEnv)
  }
  
  senaste <- klocka_data %>% group_by(indikator) %>% slice_tail(n = 1) %>% ungroup()
  
  perioder_txt <- senaste %>%
    arrange(indikator) %>%
    mutate(rad = paste0(indikator, " ", tid)) %>%
    dplyr::pull(rad) %>% paste(collapse = ", ")
  
  bildtext <- paste0(
    "Avser senaste tillgängliga period per indikator: ", perioder_txt, ".\n",
    "Metod: förenklad variant av SCB:s konjunkturklocka. Kortsiktig avvikelse ",
    "från långsiktig trend (HP-filter), standardiserad (medelvärde 0). ",
    "Säsongrensning med STL. Fordonsserien justerad för extremvärden kopplade ",
    "till regeländringar (bonus malus). Det senaste läget är preliminärt och ",
    "kan ändras när ny data tillkommer.\n",
    "Källa: SCB (AKU, fordon). Bearbetning: Samhällsanalys, Region Dalarna."
  )
  
  colors <- c("#F4A582", "#D6604D", "#B8E0B8", "#4D9A4D")  # Återh/Rec/Avm/Exp
  mdx <- axis_limit; mdy <- axis_limit
  
  klocka <- ggplot() +
    annotate("rect", xmin = -mdx, xmax = 0, ymin = -mdy, ymax = 0, fill = colors[1]) +
    annotate("rect", xmin = 0, xmax = mdx, ymin = -mdy, ymax = 0, fill = colors[2]) +
    annotate("rect", xmin = 0, xmax = mdx, ymin = 0, ymax = mdy, fill = colors[3]) +
    annotate("rect", xmin = -mdx, xmax = 0, ymin = 0, ymax = mdy, fill = colors[4]) +
    annotate("segment", x = 0, xend = 0, y = -mdy, yend = mdy, colour = "white", linewidth = 0.4) +
    annotate("segment", x = -mdx, xend = mdx, y = 0, yend = 0, colour = "white", linewidth = 0.4) +
    annotate("text", x = -mdx + 0.1, y = mdy - 0.1, label = "Expansion",    hjust = 0, vjust = 1, fontface = "bold", colour = "grey20") +
    annotate("text", x =  mdx - 0.1, y = mdy - 0.1, label = "Avmattning",   hjust = 1, vjust = 1, fontface = "bold", colour = "grey20") +
    annotate("text", x =  mdx - 0.1, y = -mdy + 0.1, label = "Recession",   hjust = 1, vjust = 0, fontface = "bold", colour = "grey20") +
    annotate("text", x = -mdx + 0.1, y = -mdy + 0.1, label = "Återhämtning",hjust = 0, vjust = 0, fontface = "bold", colour = "grey20") +
    annotate("text", x = 0, y = mdy,  label = "Över trend",  vjust = -0.6, size = 3.3, colour = "grey30") +
    annotate("text", x = 0, y = -mdy, label = "Under trend", vjust = 1.6,  size = 3.3, colour = "grey30") +
    annotate("text", x = -mdx, y = 0, label = "Ökande",   angle = 90,  vjust = -0.6, size = 3.3, colour = "grey30") +
    annotate("text", x =  mdx, y = 0, label = "Minskande", angle = 270, vjust = -0.6, size = 3.3, colour = "grey30") +
    geom_point(data = senaste, aes(x, y, fill = indikator),
               size = 5, shape = 21, colour = "white", stroke = 1) +
    scale_fill_brewer(palette = "Set1") +
    coord_fixed(ratio = 1, xlim = c(-axis_limit, axis_limit),
                ylim = c(-axis_limit, axis_limit), clip = "off") +
    labs(title = "Konjunkturläge i Dalarna",
         subtitle = "Förenklad konjunkturklocka — två regionala indikatorer",
         fill = NULL, caption = bildtext) +
    theme_void(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11, colour = "grey30", margin = margin(b = 8)),
      plot.caption  = element_text(size = 7.5, colour = "grey40", hjust = 0, margin = margin(t = 10)),
      legend.position = "right",
      plot.margin   = margin(15, 15, 15, 15)
    )
  
  # ---- Konkursdiagrammet -----------------------------------------------
  
  if (konkurser_frekvens == "kvartal") {
    konkurser_plot_df <- konkurser_df %>%
      mutate(
        ar    = as.integer(str_sub(period, 1, 4)),
        kv    = as.integer(str_sub(period, 6, 6)),
        datum = as.Date(paste0(ar, "-", (kv - 1) * 3 + 1, "-01"))
      )
    frek_txt <- "Per kvartal"
  } else {
    konkurser_plot_df <- konkurser_df %>%
      mutate(
        ar    = as.integer(str_sub(period, 1, 4)),
        man   = as.integer(str_sub(period, 6, 7)),
        datum = as.Date(paste0(ar, "-", man, "-01"))
      )
    frek_txt <- "Per månad"
  }
  
  if(returnera_df_rmarkdown == TRUE){
    assign("konkurser_tidsserie_df", konkurser_plot_df, envir = .GlobalEnv)
  }
  
  # Bygg x-axelns brytpunkter: alltid med första och sista året,
  # plus jämnt fördelade år däremellan. Fungerar för både månad och kvartal
  # eftersom datum redan är i Date-format.
  ar_min <- as.integer(format(min(konkurser_plot_df$datum), "%Y"))
  ar_max <- as.integer(format(max(konkurser_plot_df$datum), "%Y"))
  
  # jämnt fördelade "mellanår" (ca vart 4:e år), sedan tvinga in ändarna
  mellan_ar <- seq(ar_min, ar_max, by = 4)
  alla_ar   <- sort(unique(c(ar_min, mellan_ar, ar_max)))
  brytpunkter <- as.Date(paste0(alla_ar, "-01-01"))
  
  konkurser <- ggplot(konkurser_plot_df, aes(datum, value)) +
    geom_line(aes(colour = "Antal konkurser"), linewidth = 0.6) +
    geom_smooth(aes(colour = "Utjämnad trend"),
                se = FALSE, method = "loess", span = 0.3, linewidth = 0.8) +
    scale_x_date(breaks = brytpunkter, date_labels = "%Y",
                 limits = c(min(konkurser_plot_df$datum),
                            max(konkurser_plot_df$datum)),
                 expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_colour_manual(values = c("Antal konkurser" = rddiagram::diagramfarger("rus_sex")[4],
                                   "Utjämnad trend"   = "black")) +
    labs(
      title = paste0("Antal konkurser i Dalarna ", tolower(frek_txt)),
      x = NULL, y = "Antal",
      colour = NULL,
      caption = paste0(
        "Källa: Tillväxtanalys\n",
        "Bearbetning: Samhällsanalys, Region Dalarna\n",
        "Observera: den utjämnade trenden är preliminär i seriens slut ",
        "och kan ändras när ny data tillkommer."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      # Endast vågräta stödlinjer, inga lodräta, inga minor-linjer
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.major.y = element_line(colour = "grey75", linewidth = 0.5),
      # Titel
      plot.title   = element_text(face = "bold", size = 15, hjust = 0.5),
      # Legend under grafen
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.width = unit(1.2, "cm"),
      # Källa/bearbetning nere till vänster, kursiv
      plot.caption = element_text(size = 8, colour = "grey30", hjust = 0,
                                  face = "italic", margin = margin(t = 10)),
      plot.caption.position = "plot",
      # Axlar
      axis.title.y = element_text(size = 10, colour = "grey30"),
      axis.text    = element_text(colour = "grey30"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.ticks   = element_blank(),
      plot.margin  = margin(15, 20, 10, 15)
    )
  
  if (spara_figur) {
    # senaste observationens period, t.ex. "2026K2" eller "2026M09"
    sista_period <- konkurser_df %>% slice_tail(n = 1) %>% pull(period)
    
    ggsave(file.path(sokvag, "konjunkturklocka_dalarna.png"), klocka,
           width = 9, height = 7, dpi = 300, bg = "white")
    
    konk_fil <- paste0("konkurser_dalarna_", konkurser_frekvens,
                       "_", sista_period, ".png")
    ggsave(file.path(sokvag, konk_fil), konkurser,
           width = 8, height = 4.5, dpi = 300, bg = "white")
  }

  # ---- Returnera båda figurerna + data ----------------------------------
  list(klocka = klocka, konkurser = konkurser)
}
