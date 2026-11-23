# Skript som hämtar data och skapar figurer/variabler som används för att skapa markdown-rapporten. Det finns två alternativ för skriptet:

# 1: Kör skriptet utan att uppdatera data - sätt variabeln uppdatera_data till FALSE. Då läses den senast sparade versionen av R-studio global environment in.
# Detta är ett bra alternativ om man enbart vill ändra text eller liknande, men inte uppdatera data.

# 2: Uppdatera data - sätt variabeln uppdatera_data till FALSE. Då uppdateras data, alla figurer skapas på nytt och en ny enviroment sparas.
# Tar längre tid (ett par minuter) och medför en risk att text inte längre är aktuell då figurer har ändrats.

if (!require("pacman")) install.packages("pacman")
p_load(here,
       tidyverse)

# OBS! Ska sättas till FALSE när skriptet går i produktion - men kan stängas av genom att sätta till TRUE för att se att alla skript fungerar som de ska
# skriptet är till för att hantera rcurl-fel och inte vanliga fel som ju inte blir bättre av att man försöker flera gånger. =)
hoppa_over_felhantering = TRUE

# Skall data uppdateras? Annars läses data in från en sparad global environment-fil och rapporten knittas baserat på senast sparade data.
uppdatera_data = TRUE

if(uppdatera_data == TRUE){
  
  cat("Hämtning av data påbörjad\n\n")
  start_time <- Sys.time()
  
  if (!requireNamespace("rddiagramkatalog", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagramkatalog")
  }
  
  Output_mapp = here("Figurer","/")
  spara_figur = FALSE
  
  ######################################################
  ### Uppdateras automatiskt vid körning av skript   ###
  ######################################################
  
  # BNP - 1 diagram
  gg_BNP <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_BNP_forandring_SCB(
      skriv_diagramfil = spara_figur,
      utmapp = Output_mapp,
      returnera_data = TRUE,
      returnera_figur = TRUE,
      startar = "2000",
      visa_etikett = 16,
      x_axis_lutning = 90)
  }, hoppa_over = hoppa_over_felhantering)

  BNP_senaste_kvartal <- BNP_df %>% filter(ar>"2015") %>% filter(användning =="- BNP till marknadspris") %>% filter(ar_kvartal==last(ar_kvartal)) %>% .$kvartal
  BNP_senaste_ar <- BNP_df %>% filter(ar>"2015") %>% filter(användning =="- BNP till marknadspris") %>% filter(ar_kvartal==last(ar_kvartal)) %>% .$ar
  BNP_senaste_varde <- gsub("\\.",",",BNP_df %>% filter(ar>"2015") %>% filter(användning =="- BNP till marknadspris") %>% filter(ar_kvartal==last(ar_kvartal)) %>% .$Sasongsransad_forandring)
  
  # Prognoser BNP - ej PXweb
  gg_BNP_prognos <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_ekonomiska_prognoser_olika_progn_institut_ki(
      vald_variabel = "BNP",
      utmapp  = Output_mapp,
      diagram_capt = "Källa: Konjunkturinstitutet.\nBearbetning: Samhällsanalys, Region Dalarna.\nObservera att prognoserna är gjorda vid olika tidpunkter, vilket bör beaktas vid jämförelse mellan prognosinstitut.",
      valda_prognos_ar = "+1",
      x_axis_lutning = 45,
      manual_y_axis_title = "procent",
      skriv_diagramfil = spara_figur)
  }, hoppa_over = hoppa_over_felhantering)
  
  # Konjunkturbarometern - 2 diagram - ej PXweb
  gg_konjB <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_konjunkturbarometern_konj(
      skriv_diagramfil = spara_figur,
      utmapp = Output_mapp,
      diagram_capt = "Källa: Konjunkturinstitutet.\nBearbetning: Samhällsanalys, Region Dalarna.",
      antal_etiketter_barometern = 48, # Intervall mellan visade etiketter (i månader)
      legend_byrow = TRUE,
      antal_etiketter_bransch = 48, # Intervall mellan visade etiketter (i månader)
      startvarde_bransch = "1996-07",
      returnera_data = TRUE,
      returnera_figur = TRUE)
  }, hoppa_over = hoppa_over_felhantering)

  konjukturbarometern_senaste_manad <- last(barometern_df %>% filter(Indikator=="Barometerindikatorn")) %>% .$manad_long
  konjukturbarometern_senaste_ar <- last(barometern_df %>% filter(Indikator=="Barometerindikatorn")) %>% .$ar
  konjukturbarometern_senaste_varde <- gsub("\\.",",",last(barometern_df %>% filter(Indikator=="Barometerindikatorn")) %>% .$varde)
  
  konj_hushall_senaste <- gsub("\\.",",",last(barometern_df$varde[barometern_df$Indikator=="Konfidensindikator hushåll"])) 
  konj_bransch_positiv <- tolower(gsub( " .*$", "", barometern_df %>%filter(Indikator!="Konfidensindikator hushåll",Indikator!="Barometerindikatorn",Period==last(Period)) %>%  filter(varde==max(varde)) %>% .$Indikator))
  konj_bransch_positiv_varde <- gsub("\\.",",",barometern_df %>%filter(Indikator!="Konfidensindikator hushåll",Indikator!="Barometerindikatorn",Period==last(Period)) %>%  filter(varde==max(varde)) %>% .$varde)
  konj_bransch_negativ <- tolower(gsub( " .*$", "", barometern_df %>%filter(Indikator!="Konfidensindikator hushåll",Indikator!="Barometerindikatorn",Period==last(Period)) %>%  filter(varde==min(varde)) %>% .$Indikator))
  konj_bransch_negativ_varde <- gsub("\\.",",",barometern_df %>%filter(Indikator!="Konfidensindikator hushåll",Indikator!="Barometerindikatorn",Period==last(Period)) %>%  filter(varde==min(varde)) %>% .$varde)
  
  konj_bygg_senaste_varde <- gsub("\\.",",",barometern_df %>%filter(Indikator=="Byggverksamhet (SNI 41-43)",Period==last(Period)) %>% .$varde)
  
  # KPI - 1 diagram
  gg_infl <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_inflation_scb(
      skriv_diagramfil = spara_figur,
      antal_etiketter = 24,
      utmapp = Output_mapp,
      returnera_data = TRUE,
      returnera_figur = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  inflation_max_manad_ar <- paste(KPI_df %>%filter(Period>"1993-01") %>% filter(KPIF==max(KPIF,na.rm=TRUE)) %>% select(manad_long,ar),collapse = " ")
  inflation_max_varde <- gsub("\\.",",",KPI_df %>%filter(Period>"1993-01") %>% filter(KPIF==max(KPIF,na.rm=TRUE)) %>% .$KPIF)
  
  inflation_senaste_manad_ar <- paste(KPI_df %>% filter(månad==last(månad)) %>% select(manad_long,ar),collapse = " ")
  inflation_senaste_varde <- gsub("\\.",",",KPI_df %>% filter(månad==last(månad)) %>% .$KPIF)
  
  # Prognoser inflation - ej PXweb
  gg_infl_prognos <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_ekonomiska_prognoser_olika_progn_institut_ki(
      vald_variabel = "KPI med fast bostadsränta (KPIF), årsgenomsnitt",
      utmapp = Output_mapp,
      valda_prognos_ar = "+1",
      diagram_capt = "Källa: Konjunkturinstitutet.\nBearbetning: Samhällsanalys, Region Dalarna.\nObservera att prognoserna är gjorda vid olika tidpunkter, vilket bör beaktas vid jämförelse mellan prognosinstitut.",
      x_axis_lutning = 45,
      manual_y_axis_title = "procent",
      skriv_diagramfil = spara_figur)
  }, hoppa_over = hoppa_over_felhantering)
  
  # Styrränta - prognos - används för tillfället inte
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_ek_prognoser_fran_prognosinstitut_ki.R")
  prognos_styrranta_df <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {hamta_ek_prognoser_fran_prognosinstitut_ki() %>% 
      filter(variabel == "Styrränta, vid årets slut, procent**",
             prognos_for_ar == min(prognos_for_ar))
  }, hoppa_over = hoppa_over_felhantering)
  
  prognos_styrranta_ar <- unique(prognos_styrranta_df$prognos_for_ar)
  prognos_styrranta_antal_institut <- length(prognos_styrranta_df$Prognosinstitut)
  
  vanligaste <- prognos_styrranta_df %>%
    filter(!is.na(varde)) %>%             # drop missing values
    count(varde, name = "n", sort = TRUE) %>%
    slice_max(n, n = 1, with_ties = TRUE) # keep all modes if tied
  
  vanligaste_antal <- vanligaste$n
  vanligaste_varde <- gsub(".", ",", vanligaste$varde, fixed = TRUE)
  
  # Styrränta - prognos
  prognos_BNP_df <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {hamta_ek_prognoser_fran_prognosinstitut_ki() %>% 
      filter(variabel == "BNP")
  }, hoppa_over = hoppa_over_felhantering)
  
  prognos_BNP_2026 <- gsub(".", ",", round(mean(prognos_BNP_df %>% filter(prognos_for_ar == "2026") %>% .$varde),1), fixed = TRUE)
  prognos_BNP_2026_antal <- length(unique(prognos_BNP_df %>% filter(prognos_for_ar == "2026") %>% .$Prognosinstitut))
  prognos_BNP_2027<- gsub(".", ",", round(mean(prognos_BNP_df %>% filter(prognos_for_ar == "2027") %>% .$varde),1), fixed = TRUE)
  prognos_BNP_2027_antal <- length(unique(prognos_BNP_df %>% filter(prognos_for_ar == "2027") %>% .$Prognosinstitut))
  prognos_BNP_2028 <- gsub(".", ",", round(mean(prognos_BNP_df %>% filter(prognos_for_ar == "2028") %>% .$varde),1), fixed = TRUE)
  prognos_BNP_2028_antal <- length(unique(prognos_BNP_df %>% filter(prognos_for_ar == "2028") %>% .$Prognosinstitut))
  
  
  # Småhuspriser - 2 diagram
  # gg_smahuspriser <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
  #   rddiagramkatalog::diagram_smahuspriser(
  #     skriv_diagramfil = spara_figur,
  #     utmapp = Output_mapp,
  #     stodlinjer_avrunda_fem = FALSE,
  #     returnera_data = TRUE,
  #     returnera_figur = TRUE)
  # }, hoppa_over = hoppa_over_felhantering)
  
  source(here("Skript","diagram_smahuspriser_SCB.R"), encoding="UTF-8")
  gg_smahuspriser <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {diagram_smahuspriser(spara_figur = spara_figur, 
                                                                                                 output_mapp = Output_mapp,
                                                                                                 stodlinjer_avrunda_fem = FALSE,
                                                                                                 returnera_data = TRUE, 
                                                                                                 returnera_figur = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  priser_max_manad_ar <- paste(priser_df %>% filter(ar>2009,region == "Dalarna") %>%  filter(Medelpris==max(Medelpris)) %>% select(manad_long,ar),collapse = " ")
  priser_max_varde <- round(priser_df %>% filter(ar>2009,region == "Dalarna") %>%  filter(Medelpris==max(Medelpris)) %>% select(Medelpris) %>% .$Medelpris/1000,1)
  priser_senaste_manad_ar <- paste(priser_df %>% filter(Period==last(Period),region == "Dalarna") %>% select(manad_long,ar),collapse = " ")
  priser_senaste_varde <- gsub("\\.",",",(round(priser_df %>% filter(Period==last(Period),region == "Dalarna") %>% select(Medelpris)/1000,1)))
  priser_senaste_max_region <- paste(priser_df %>%filter(Period==max(Period)) %>% filter(Medelpris==max(Medelpris)) %>% .$region,collapse = " ")
  priser_senaste_max_varde <- gsub("\\.",",", round(priser_df %>%filter(Period==max(Period)) %>% filter(Medelpris==max(Medelpris)) %>% select(Medelpris)/1000,1))
  
  
  # Byggande - 2 figurer
  gg_nybygg_bygglov <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_nybyggnation_bygglov_scb(
      skriv_diagramfil = spara_figur,
      utmapp = Output_mapp,
      hustyp_klartext = "småhus",
      returnera_data = TRUE,
      returnera_figur = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  Bygglov_df <- Bygglov_df %>% 
    mutate(kvartal=case_when(
      kvartal=="K1" ~ "kvartal ett",
      kvartal=="K2" ~ "kvartal två",
      kvartal=="K3" ~ "kvartal tre",
      kvartal=="K4" ~ "kvartal fyra",
      TRUE ~ kvartal
    ))
  
  bygglov_smahus_max_manad_ar <- paste(Bygglov_df %>%filter(ar>"2019",hustyp=="småhus") %>% filter(Antal==max(Antal)) %>% select(kvartal,ar),collapse = " ")
  bygglov_smahus_max_varde <- Bygglov_df %>%filter(ar>"2019",hustyp=="småhus") %>% filter(Antal==max(Antal)) %>% .$Antal
  
  bygglov_smahus_senaste_manad_ar <- paste(Bygglov_df %>%filter(hustyp=="småhus")  %>% select(kvartal,ar) %>% last(),collapse = " ")
  bygglov_smahus_senaste_varde <- Bygglov_df %>%filter(hustyp=="småhus") %>% select(Antal) %>% last() %>% .$Antal
  #bygglov_flerbostadshus_senaste_varde <-Bygglov_df %>%filter(ar>"2009",hustyp=="flerbostadshus exkl. specialbostäder") %>% .$Antal %>% last()
  
  Husbyggande_df <- Husbyggande_df %>% 
    mutate(kvartal=case_when(
      kvartal=="K1" ~ "kvartal ett",
      kvartal=="K2" ~ "kvartal två",
      kvartal=="K3" ~ "kvartal tre",
      kvartal=="K4" ~ "kvartal fyra",
      TRUE ~ kvartal
    ))
  
  husbyggande_smahus_max_manad_ar <- paste(Husbyggande_df %>%filter(ar>"2019",hustyp=="småhus") %>% filter(varde==max(varde)) %>% select(kvartal,ar),collapse = " ")
  husbyggande_smahus_max_varde <- Husbyggande_df %>%filter(ar>"2019",hustyp=="småhus") %>% filter(varde==max(varde)) %>% .$varde
  husbyggande_smahus_senaste_manad_ar <- paste(Husbyggande_df %>%filter(hustyp=="småhus")  %>% select(kvartal,ar) %>% last(),collapse = " ")
  husbyggande_smahus_senaste_varde <- Husbyggande_df %>%filter(hustyp=="småhus") %>% select(varde) %>% last() %>% .$varde
  # husbyggande_flerbostadshus_senaste_varde <- Husbyggande_df %>%filter(hustyp=="flerbostadshus") %>% .$varde %>%  last()
  
  # Konkurser månadsbasis med trend
  source(here("Skript","func_plotta_med_trend.R"), encoding="UTF-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_lan_variabel_konk_1996_tva.R")
  
  konkurser_df <- hamta_konkurser_manad_lan_variabel_tva(
    region_vekt = "20", 
    variabel_klartext = "Antal konkurser",
    manad_klartext = "*") |>  
    rename(tid = månad,
           antal = matches("konkurser", ignore.case = TRUE)) |>
    mutate(år = str_sub(tid, 1, 4),
           månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
           manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B")) 
  
  
  gg_konkurser <-  plotta_med_trend(konkurser_df, tid, antal,
                                    titel = "Antal konkurser i Dalarna per månad",
                                    y_titel = "Antal",
                                    enhet_txt = "Konkurser",
                                    span = 0.3,            # slätare trend för en redan jämn serie
                                    fran_noll = TRUE,
                                    x_format = "manad", intervall = 48,   # etikett var 6:e månad
                                    kalla_txt = "Källa: Tillväxtverket\nBearbetning: Samhällsanalys, Region Dalarna")
  
  konkurser_senaste_manad <- last(konkurser_df$manad_long)
  konkurser_senaste_ar <- last(konkurser_df$år)
  konkurser_senaste_antal <- konkurser_df %>% filter(år==last(år),manad_long == last(manad_long)) %>% group_by(region,år) %>% .$antal %>% sum()
  
  konkurser_hittils_i_ar <- konkurser_df %>% filter(år==last(år)) %>% group_by(region,år) %>% .$antal %>% sum()
  manader_hittils_i_ar <- konkurser_df %>% filter(år==last(år)) %>% .$månad
  konkurser_motsvarande_foregaende_ar <- konkurser_df %>% filter(år==(as.numeric(last(år))-1),månad %in%manader_hittils_i_ar) %>% group_by(region,år) %>% .$antal %>% sum()
  
  konkurser_foregaende_ar <- as.character(as.numeric(last(konkurser_df$år))-1)
  konkurser_foregaende_ar_antal <- konkurser_df  %>% filter(år==(as.numeric(last(år))-1),manad_long == last(manad_long)) %>% group_by(region,år) %>% .$antal %>% sum()
  
  # Konkurser - årsbasis
  gg_konkurser_ar <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_konkurser_ar_tva(
      skriv_diagramfil = spara_figur,
      variabel_klartext = "Antal konkurser",
      utmapp = Output_mapp,
      returnera_data = TRUE,
      returnera_figur = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  # Nyregistrerade personbilar 2 figurer
  fordon_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3277",
    query = list(
      Region       = "20",
      Drivmedel    = NA,      # alla drivmedel -> summeras till total nedan
      ContentsCode = "*",
      Tid          = "*"
    ), quiet = TRUE) |>
    rename(tid = månad,
           antal = value) |>
    mutate(år = str_sub(tid, 1, 4),
           månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
           manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B")) 
  
  gg_fordon <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {plotta_med_trend(fordon_df, tid, antal,
                                                                                       titel = "Nyregistrerade personbilar i Dalarna per månad",
                                                                                       y_titel = "Antal",
                                                                                       enhet_txt = "Personbilar",
                                                                                       span = 0.3,
                                                                                       fran_noll = TRUE,
                                                                                       x_format = "manad", intervall = 24, gles_slut = TRUE,
                                                                                       kalla_txt = "Källa: SCB (Trafikanalys)\nBearbetning: Samhällsanalys, Region Dalarna")
  }, hoppa_over = hoppa_over_felhantering)
  
  fordon_senaste_manad <- last(fordon_df$manad_long)
  fordon_senaste_ar <- last(fordon_df$år)
  fordon_senaste_antal <- fordon_df %>% filter(år==last(år),manad_long == last(manad_long)) %>% group_by(region,år) %>% .$antal %>% sum()
  
  fordon_hittils_i_ar <- fordon_df %>% filter(år==last(år)) %>% group_by(region,år) %>% .$antal %>% sum()
  manader_hittils_i_ar <- fordon_df %>% filter(år==last(år)) %>% .$månad
  fordon_motsvarande_foregaende_ar <- fordon_df %>% filter(år==(as.numeric(last(år))-1),månad %in%manader_hittils_i_ar) %>% group_by(region,år) %>% .$antal %>% sum()
  
  fordon_foregaende_ar <- as.character(as.numeric(last(fordon_df$år))-1)
  fordon_foregaende_ar_antal <- fordon_df  %>% filter(år==(as.numeric(last(år))-1),manad_long == last(manad_long)) %>% group_by(region,år) %>% .$antal %>% sum()
  
  # Tar ut data för elbilar
  alla_tid   <- sort(unique(fordon_df$tid))
  sista      <- alla_tid[length(alla_tid)]
  for_12man  <- alla_tid[length(alla_tid) - 12]
  
  fordon_df_el <- pxweb2r::pxweb2_get_data(
    table = "TAB3277",
    query = list(
      Region       = "20",
      Drivmedel    = "*",      # alla drivmedel -> summeras till total nedan
      ContentsCode = "*",
      Tid          = c(for_12man,sista)
    ), quiet = TRUE) %>%
    group_by(månad) %>%
    summarise(
      totalt     = sum(value),
      el   = sum(value[drivmedel %in% c("el")]),
      andel      = el / totalt,
      .groups = "drop"
    ) %>%
    mutate(andel_proc = round(100 * andel, 0))
  
  # Nyregistrerade personbilar - årsbasis
  gg_fordon_ar <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_nyreg_bil_ar_scb(
      region_vekt = "20",
      skriv_diagramfil = spara_figur,
      utmapp = Output_mapp,
      returnera_data = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  
  # Nystartade företag - 2 figurer
  gg_nystartade <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_nystartade_tva(
      skriv_diagramfil = spara_figur,
      utmapp = Output_mapp,
      returnera_data = TRUE,
      returnera_figur = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  nystartade_max_ar <- nystartade_df %>% filter(antal == max(antal)) %>% .$ar
  nystartade_max_kvartal <- nystartade_df %>% filter(antal == max(antal)) %>% .$kvartal_namn
  nystartade_max_varde <- nystartade_df %>% filter(antal == max(antal)) %>% .$antal
  
  nystartade_senaste_ar <- nystartade_df %>% filter(tid == last(tid)) %>% .$ar
  nystartade_senaste_kvartal <- nystartade_df %>% filter(tid == last(tid)) %>% .$kvartal_namn
  nystartade_senaste_varde <- nystartade_df %>% filter(tid == last(tid)) %>% .$antal
  
  nystartade_jmf <- nystartade_df %>% filter(tid == last(tid)) %>% .$antal - last(nystartade_df %>% filter(tid != last(tid)) %>% .$antal)
  
  # Arbetslöshet län - 1 figur
  gg_arbetsloshet_lan <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_arbetsmarknadsstatus_senastear_scb(
      region_vekt = hamtaAllaLan(),
      skriv_diagramfil  = spara_figur, 
      utmapp = Output_mapp,
      diag_arbetskraftsdeltagande = FALSE, 
      diag_sysselsattningsgrad = FALSE, 
      returnera_data = TRUE, 
      returnera_figur = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  arbetsloshet_ar_senaste <- unique(arbetsmarknadsstatus %>% filter(region=="Dalarna") %>% .$ar)
  arbetsloshet_manad_senaste <- unique(arbetsmarknadsstatus %>% filter(region=="Dalarna") %>% .$manad_long)
  arbetsloshet_dalarna_senaste <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region=="Dalarna") %>% .$varde)
  arbetsloshet_Sverige_senaste <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region=="Sverige") %>% .$varde)
  arbetsloshet_gavleborg_senaste <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region=="Gävleborg") %>% .$varde)
  arbetsloshet_lan_min <- arbetsmarknadsstatus %>% filter(varde==min(varde)) %>% dplyr::pull(region) %>% rdverktyg::list_komma_och()
  arbetsloshet_lan_min_varde <- min(arbetsmarknadsstatus$varde) %>% formatC(format = "f", digits = 1) %>%  str_replace("\\.", "\\,")
  
  # Arbetslöshet förändring jämfört med samma månad föregående år - 1 figur Inte uppdaterad ännu
  gg_arb_ett_ar <- rddiagramkatalog::diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb(utmapp = Output_mapp,
                                                                                    dela_upp_utrikes = FALSE,
                                                                                    returnera_data = TRUE)
  
  
  storst_okning_arb_lan <- forandring_arbetsloshet_df %>% filter(födelseregion == "totalt") %>% filter(diff == max(diff)) %>% .$region
  storst_okning_arb_varde <- forandring_arbetsloshet_df %>% filter(födelseregion == "totalt") %>% filter(diff == max(diff)) %>% .$diff %>% gsub("\\.",",",.)
  
  okning_arb_Dalarna_varde <- round(forandring_arbetsloshet_df %>% filter(region == "Dalarna", födelseregion == "totalt") %>% .$diff,1) %>% gsub("\\.",",",.)
  
  gg_arb_ett_ar_kommun <- rddiagramkatalog::diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb(region_vekt = "20",
                                                                                           utmapp = Output_mapp,
                                                                                           dela_upp_utrikes = FALSE,
                                                                                           returnera_data = TRUE)
  
  storst_okning_arb_kommun <- forandring_arbetsloshet_df %>% filter(födelseregion == "totalt") %>% filter(diff == max(diff)) %>% .$region
  storst_okning_arb_kommun_varde <- forandring_arbetsloshet_df %>% filter(födelseregion == "totalt") %>% filter(diff == max(diff)) %>% .$diff %>% gsub("\\.",",",.)
  
  storst_minskning_arb_kommun <- forandring_arbetsloshet_df %>% filter(födelseregion == "totalt") %>% filter(diff == min(diff)) %>% .$region
  storst_minskning_arb_kommun_varde <- forandring_arbetsloshet_df %>% filter(födelseregion == "totalt") %>% filter(diff == min(diff)) %>% .$diff %>% gsub("\\.",",",.)
  
  # Arbetslöshet tidsserie - 1 figur
  gg_arbetsloshet_tidsserie <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_arbetsmarknadsstatus_tidsserie_scb(
      skriv_diagramfil = spara_figur, 
      utmapp = Output_mapp,
      returnera_data = TRUE,
      legend_rader = 1,
      marginal_yaxis_facet = c(0.02,0.02),
      diag_fargvekt  = rddiagram::diagramfarger("rus_sex"),
      diagram_facet = TRUE,
      tid_koder = "2021:9999",
      #start_ar = "2021",
      returnera_figur = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  arbetsloshet_tidsserie_ar <-  unique(last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$ar))
  arbetsloshet_tidsserie_manad <- unique(last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$manad_long))
  
  # Totalt
  arbetsloshet_tidserie_Dalarna_totalt_max_ar <- arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="totalt") %>% filter(varde==max(varde)) %>%  .$ar %>% .[1]
  arbetsloshet_tidserie_Dalarna_totalt_max_manad <- arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="totalt") %>% filter(varde==max(varde)) %>%  .$manad_long %>% .[1]
  arbetsloshet_tidserie_Dalarna_totalt_max_varde <- gsub("\\.",",", arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="totalt") %>% filter(varde==max(varde)) %>%  .$varde %>% .[1])
  
  arbetsloshet_tidserie_Dalarna_totalt_senaste_varde <- gsub("\\.",",",last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="totalt") %>% .$varde))
  
  # Inrikes/utrikes födda
  arbetsloshet_tidserie_Dalarna_inrikes_varde <- gsub("\\.",",",last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="inrikes född") %>% .$varde))
  arbetsloshet_tidserie_Dalarna_utrikes_varde <- gsub("\\.",",",last(arbetsmarknadsstatus_tidsserie %>% filter(region=="Dalarna",födelseregion=="utrikes född") %>% .$varde))
  
  
  # Hämta antal arbetslösa (till texten enbart)
  arbstatus_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = "20",
      Kon = "totalt",
      Alder = unique(arbetsmarknadsstatus_tidsserie$ålder),
      Fodelseregion = unique(arbetsmarknadsstatus_tidsserie$födelseregion),
      ContentsCode = "antal arbetslösa",
      Tid = "9999"
    )) %>% 
    rename(varde = value,
           regionkod = region_kod,
           variabel = tabellinnehåll) %>% 
    manader_bearbeta_scbtabeller()
  
  arblosa_utrikes <- format(plyr::round_any(arbstatus_df %>% filter(födelseregion == "utrikes född") %>% .$varde,10),big.mark = " ")
  arblosa_inrikes <- format(plyr::round_any(arbstatus_df %>% filter(födelseregion == "inrikes född") %>% .$varde,10),big.mark = " ")
  arblosa_manad <- unique(arbstatus_df$månad)
  arblosa_ar <- unique(arbstatus_df$år)
  arblosa_alder <- unique(arbstatus_df$ålder)
  
  # Arbetslöshet kommun - Karta, ej diagramskript (ännu) - upp med nya PXweb
  source(here("Skript","arbetsloshet_kommun_ny.R"), encoding="UTF-8")
  rdverktyg::funktion_upprepa_forsok_om_fel( function() {hamta_data_arbetsloshet(vald_region="20",
                                                                                 spara_data = TRUE,
                                                                                 output_mapp_excel = here("Data"))
  }, hoppa_over = hoppa_over_felhantering)
  
  
  # Ekonomiskt bistånd - 2 figurer
  gg_ek_stod <- rdverktyg::funktion_upprepa_forsok_om_fel( function() {
    rddiagramkatalog::diag_ek_stod_bakgrund_scb(
      utmapp = Output_mapp,
      skriv_diagramfil = spara_figur,
      stodlinjer_avrunda_fem = FALSE,
      returnera_data = TRUE)
  }, hoppa_over = hoppa_over_felhantering)
  
  ek_stod_manad_ar_forsta <- first(ekonomiskt_stod_df$månad_år)
  ek_stod_manad_ar_sista <- last(ekonomiskt_stod_df$månad_år)
  
  ek_stod_totalt_sista <- format(plyr::round_any(ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="totalt") %>% .$antal,100),big.mark = " ")
  
  ek_stod_skillnad_forsta <- plyr::round_any(ekonomiskt_stod_df %>% filter(månad_år==first(månad_år)) %>% filter(födelseregion=="utrikes född") %>% .$antal - ekonomiskt_stod_df %>% filter(månad_år==first(månad_år)) %>% filter(födelseregion=="inrikes född") %>% .$antal,10)
  ek_stod_skillnad_senaste <- abs(plyr::round_any(ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="utrikes född") %>% .$antal - ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="inrikes född") %>% .$antal,10))

  # Sparar global environment i R. Detta för att man skall slippa hämta data varje gång
  save.image(file = "G:/skript/projekt/environments/samhallsekonomiska_laget.RData")
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat(sprintf("Hämtning av data klar: Det tog %.2f sekunder.", elapsed_time))
  cat("\n\n")
  
  
}else{
  load("G:/skript/projekt/environments/samhallsekonomiska_laget.RData")
}