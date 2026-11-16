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

# ============================================================
# skapa_konjunkturfigurer()
#   Bygger konjunkturklockan (AKU + fordon) och, valfritt, ett
#   diagnostikdiagram över de standardiserade cyklerna över tid.
#
#   region          : läns-kod (SCB), default "20" = Dalarna
#   axis_limit      : klockans halva axelintervall
#   visa_diagnostik : TRUE -> returnera även facetdiagrammet över cyklerna
#   spara_figur     : TRUE -> spara PNG
#   sokvag          : katalog för sparade filer
# Returnerar en lista: $klocka (+ $diagnostik om visa_diagnostik), $klocka_data
# ============================================================
skapa_konjunkturfigurer <- function(region = "20",
                                    axis_limit = 3,
                                    visa_diagnostik = FALSE,
                                    spara_figur = FALSE,
                                    sokvag = ".",
                                    returnera_df_rmarkdown = FALSE) {
  
  # ---- Hämta data (endast klockans indikatorer) ------------------------
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
  
  # ---- Klockans koordinater --------------------------------------------
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
  
  # ---- Resultatlista ----------------------------------------------------
  resultat <- list(klocka = klocka)
  
  # ---- Diagnostikdiagram (valfritt) ------------------------------------
  if (visa_diagnostik) {
    klocka_ts <- klocka_data %>%
      mutate(
        ar   = as.integer(str_sub(tid, 1, 4)),
        sub  = as.integer(str_extract(tid, "(?<=[MK])\\d+")),
        frek = if_else(str_detect(tid, "K"), 4L, 12L),
        tidspunkt = ar + (sub - 1) / frek
      )
    
    diagnostik <- ggplot(klocka_ts, aes(tidspunkt, y)) +
      geom_hline(yintercept = 0, colour = "grey70") +
      geom_line() +
      facet_wrap(~indikator, scales = "free_x") +
      labs(x = NULL, y = "Standardiserad konjunkturcykel") +
      theme_minimal(base_size = 12)
    
    resultat$diagnostik <- diagnostik
  }
  
  # ---- Spara (valfritt) -------------------------------------------------
  if (spara_figur) {
    ggsave(file.path(sokvag, "konjunkturklocka_dalarna.png"), klocka,
           width = 9, height = 7, dpi = 300, bg = "white")
    if (visa_diagnostik)
      ggsave(file.path(sokvag, "konjunkturklocka_diagnostik.png"), resultat$diagnostik,
             width = 10, height = 4, dpi = 300, bg = "white")
  }
  
  resultat
}

# ============================================================
# Användning
# ============================================================
# fig <- skapa_konjunkturfigurer()                         # bara klockan
# fig <- skapa_konjunkturfigurer(visa_diagnostik = TRUE)   # klocka + diagnostik
# fig$klocka ; fig$diagnostik
