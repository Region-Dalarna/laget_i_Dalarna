# Skript som hämtar data och skapar figurer/variabler som används för att skapa markdown-rapporten. Det finns två alternativ för skriptet:

# 1: Kör skriptet utan att uppdatera data - sätt variabeln uppdatera_data till FALSE. Då läses den senast sparade versionen av R-studio global environment in.
# Detta är ett bra alternativ om man enbart vill ändra text eller liknande, men inte uppdatera data.

# 2: Uppdatera data - sätt variabeln uppdatera_data till FALSE. Då uppdateras data, alla figurer skapas på nytt och en ny enviroment sparas.
# Tar längre tid (ett par minuter) och medför en risk att text inte längre är aktuell då figurer har ändrats.

if (!require("pacman")) install.packages("pacman")
p_load(here,
       tidyverse)

# Skall data uppdateras? Annars läses data in från en sparad global environment-fil och rapporten knittas baserat på senast sparade data.
uppdatera_data = TRUE

if(uppdatera_data == TRUE){

  cat("Hämtning av data påbörjad\n\n")
  start_time <- Sys.time()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  Output_mapp = here("Figurer","/")
  spara_figur = FALSE
  
  ######################################################
  ### Uppdateras automatiskt vid körning av skript   ###
  ######################################################
  
  # BNP - 1 diagram
  source(here("Skript","diagram_BNP_forandring_SCB.R"), encoding="UTF-8")
  gg_BNP <- diagram_BNP_SCB(spara_figur = spara_figur, 
                            output_mapp = Output_mapp,
                            returnera_data = TRUE, 
                            returnera_figur = TRUE)
  
  BNP_senaste_kvartal <- BNP_df %>% filter(ar>"2015") %>% filter(användning =="- BNP till marknadspris") %>% filter(ar_kvartal==last(ar_kvartal)) %>% .$kvartal
  BNP_senaste_ar <- BNP_df %>% filter(ar>"2015") %>% filter(användning =="- BNP till marknadspris") %>% filter(ar_kvartal==last(ar_kvartal)) %>% .$ar
  BNP_senaste_varde <- gsub("\\.",",",BNP_df %>% filter(ar>"2015") %>% filter(användning =="- BNP till marknadspris") %>% filter(ar_kvartal==last(ar_kvartal)) %>% .$Sasongsransad_forandring)
  
  # Konjunkturbarometern - 2 diagram
  source(here("Skript","diagram_konjunkturbarometern_konj.R"), encoding="UTF-8")
  gg_konjB <- diagram_konjunkturbarometern(spara_figur = spara_figur, 
                                           output_mapp = Output_mapp,
                                           antal_etiketter_barometern = 24, # Intervall mellan visade etiketter (i månader)
                                           antal_etiketter_bransch = 24, # Intervall mellan visade etiketter (i månader)
                                           returnera_data = TRUE, 
                                           returnera_figur = TRUE)
  
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
  source(here("Skript","diagram_inflation_SCB.R"), encoding="UTF-8")
  gg_infl <- diagram_inflation_SCB(spara_figur = spara_figur, 
                                   antal_etiketter = 36,
                                   output_mapp = Output_mapp,
                                   returnera_data = TRUE, 
                                   returnera_figur = TRUE)
  
  inflation_max_manad_ar <- paste(KPI_df %>%filter(Period>"1993-01") %>% filter(KPIF==max(KPIF,na.rm=TRUE)) %>% select(manad_long,ar),collapse = " ")
  inflation_max_varde <- gsub("\\.",",",KPI_df %>%filter(Period>"1993-01") %>% filter(KPIF==max(KPIF,na.rm=TRUE)) %>% .$KPIF)
  
  inflation_senaste_manad_ar <- paste(KPI_df %>% filter(månad==last(månad)) %>% select(manad_long,ar),collapse = " ")
  inflation_senaste_varde <- gsub("\\.",",",KPI_df %>% filter(månad==last(månad)) %>% .$KPIF)
  
  # Styrränta - prognos
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_ek_prognoser_fran_prognosinstitut_ki.R")
  prognos_styrranta_df <- hamta_ek_prognoser_fran_prognosinstitut_ki() %>% 
    filter(variabel == "Styrränta, vid årets slut, procent**",
           prognos_for_ar == min(prognos_for_ar))
  
  prognos_styrranta_ar <- unique(prognos_styrranta_df$prognos_for_ar)
  prognos_styrranta_antal_institut <- length(prognos_styrranta_df$Prognosinstitut)
  
  vanligaste <- prognos_styrranta_df %>%
    filter(!is.na(varde)) %>%             # drop missing values
      count(varde, name = "n", sort = TRUE) %>%
        slice_max(n, n = 1, with_ties = TRUE) # keep all modes if tied
  
  vanligaste_antal <- vanligaste$n
  vanligaste_varde <- gsub(".", ",", vanligaste$varde, fixed = TRUE)
  
  # Styrränta - prognos
  prognos_BNP_df <- hamta_ek_prognoser_fran_prognosinstitut_ki() %>% 
    filter(variabel == "BNP")
  
  prognos_BNP_2026 <- gsub(".", ",", round(mean(prognos_BNP_df %>% filter(prognos_for_ar == "2026") %>% .$varde),1), fixed = TRUE)
  prognos_BNP_2026_antal <- length(unique(prognos_BNP_df %>% filter(prognos_for_ar == "2026") %>% .$Prognosinstitut))
  prognos_BNP_2027<- gsub(".", ",", round(mean(prognos_BNP_df %>% filter(prognos_for_ar == "2027") %>% .$varde),1), fixed = TRUE)
  prognos_BNP_2027_antal <- length(unique(prognos_BNP_df %>% filter(prognos_for_ar == "2027") %>% .$Prognosinstitut))
  prognos_BNP_2028 <- gsub(".", ",", round(mean(prognos_BNP_df %>% filter(prognos_for_ar == "2028") %>% .$varde),1), fixed = TRUE)
  prognos_BNP_2028_antal <- length(unique(prognos_BNP_df %>% filter(prognos_for_ar == "2028") %>% .$Prognosinstitut))
  
  
  # Småhuspriser - 2 diagram
  source(here("Skript","diagram_smahuspriser_SCB.R"), encoding="UTF-8")
  gg_smahuspriser <- diagram_smahuspriser(spara_figur = spara_figur, 
                                          output_mapp = Output_mapp,
                                          returnera_data = TRUE, 
                                          returnera_figur = TRUE)
  
  priser_max_manad_ar <- paste(priser_df %>% filter(Medelpris==(max(priser_df %>% filter(ar>2009,region%in%c("Dalarna")) %>% select(Medelpris)))) %>% select(manad_long,ar),collapse = " ")
  priser_max_varde <- round(priser_df %>% filter(Medelpris==(max(priser_df %>% filter(ar>2009,region%in%c("Dalarna")) %>% select(Medelpris)))) %>% .$Medelpris/1000,1)
  priser_senaste_manad_ar <- paste(priser_df %>% filter(Period==last(Period),region=="Dalarna") %>% select(manad_long,ar),collapse = " ")
  priser_senaste_varde <- gsub("\\.",",",(round(priser_df %>% filter(Period==last(Period),region=="Dalarna") %>% select(Medelpris)/1000,1)))
  priser_senaste_max_region <- paste(priser_df %>%filter(Period==max(Period)) %>% filter(Medelpris==max(Medelpris)) %>% .$region,collapse = " ")
  priser_senaste_max_varde <- gsub("\\.",",", round(priser_df %>%filter(Period==max(Period)) %>% filter(Medelpris==max(Medelpris)) %>% select(Medelpris)/1000,1))
  
  
  # Byggande - 2 figurer
  source(here("Skript","diagram_nybygg_bygglov_SCB.R"), encoding="UTF-8")
  gg_nybygg_bygglov <- diagram_nybyggnation_bygglov(spara_figur = spara_figur, 
                                                    output_mapp = Output_mapp,
                                                    returnera_data = TRUE, 
                                                    returnera_figur = TRUE)
  
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
  bygglov_flerbostadshus_senaste_varde <-Bygglov_df %>%filter(ar>"2009",hustyp=="flerbostadshus exkl. specialbostäder") %>% .$Antal %>% last()
  
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
  husbyggande_flerbostadshus_senaste_varde <- Husbyggande_df %>%filter(hustyp=="flerbostadshus") %>% .$varde %>%  last()
  
  # # Konkurser - 2 figurer - SCB har inte längre data för konkurser, så vi använder Tillväxtanalys data istället (se längre ned)
  # source(here("Skript","diagram_konkurser_SCB.R"), encoding="UTF-8")
  # gg_konkurser <- diagram_konkurser_SCB(spara_figur = spara_figur, 
  #                                       output_mapp = Output_mapp,
  #                                       returnera_data = TRUE, 
  #                                       returnera_figur = TRUE)
  
  # Konkurser - 1 figur - enbart län
  source(here("Skript","diagram_konkurser_tillvaxtanalys_korrekt.R"), encoding="UTF-8")
  gg_konkurser <- diagram_konkurser_TVA(spara_figur = spara_figur,
                                        variabel_klartext = "Antal anställda berörda av konkurser",
                                        output_mapp = Output_mapp,
                                        returnera_data = TRUE,
                                        returnera_figur = TRUE)
  
  konkurser_senaste_manad <- first(konkurser_df$manad_long)
  konkurser_senaste_ar <- first(konkurser_df$år)
  konkurser_senaste_antal <- konkurser_df %>% filter(år==first(år),manad_long == first(manad_long)) %>% group_by(region,år) %>% .$antal %>% sum()
  
  konkurser_hittils_i_ar <- konkurser_df %>% filter(år==first(år)) %>% group_by(region,år) %>% .$antal %>% sum()
  manader_hittils_i_ar <- konkurser_df %>% filter(år==first(år)) %>% .$månad
  konkurser_motsvarande_foregaende_ar <- konkurser_df %>% filter(år==(as.numeric(first(år))-1),månad %in%manader_hittils_i_ar) %>% group_by(region,år) %>% .$antal %>% sum()
  
  konkurser_foregaende_ar <- as.character(as.numeric(first(konkurser_df$år))-1)
  konkurser_foregaende_ar_antal <- konkurser_df  %>% filter(år==(as.numeric(first(år))-1),manad_long == first(manad_long)) %>% group_by(region,år) %>% .$antal %>% sum()
  
  # # Konkurser - 1 figur - tre största branscher
  # source(here("Skript","diagram_konkurser_tillvaxtanalys_bransch.R"), encoding="UTF-8")
  # gg_konkurser_bransch <- diagram_konkurser_bransch_TVA(antal_branscher = 3,
  #                                                       output_mapp = Output_mapp,
  #                                                       spara_figur = spara_figur,
  #                                                       returnera_dataframe_global_environment = TRUE)
  # 
  # konk_bransch_ar <- unique(konkurser_bransch_df$ar)
  # konk_bransch_manad <- unique(konkurser_bransch_df$manad_namn)
  # storsta_bransch <- konkurser_bransch_df %>% 
  #   filter(antal == max(antal)) %>% 
  #   .$bransch
  # storsta_bransch_antal <- konkurser_bransch_df %>% 
  #   filter(antal == max(antal)) %>% 
  #   .$antal
  # nast_storsta_bransch <- konkurser_bransch_df %>% 
  #   filter(antal == sort(antal,decreasing = TRUE)[2]) %>% 
  #   .$bransch
  # nast_storsta_bransch_antal <- konkurser_bransch_df %>%
  #   filter(antal == sort(antal,decreasing = TRUE)[2]) %>% 
  #   .$antal
  
  # Avregistrerade företag - 1 figur
  source(here("Skript","diagram_avreg_ftg_Bolagsverket.R"), encoding="UTF-8")
  gg_avregistrerade <- diagram_avregistrerade(spara_figur = spara_figur, 
                                              output_mapp = Output_mapp,
                                              returnera_data = TRUE, 
                                              returnera_figur = TRUE)
  
  avregistrerade_senaste_ar <- max(antal_avregistreringar_df$ar)
  avregistrerade_senaste_manad <- tolower(last(antal_avregistreringar_df$manad))
  avregisterade_senaste_varde <- last(antal_avregistreringar_df$antal)
  avregistrerade_foregaende_ar_varde <- antal_avregistreringar_df %>% filter(manad == last(manad), ar == sort(unique(ar), decreasing = TRUE)[2]) %>% dplyr::pull(antal)
  
  # Nystartade företag - 1 figur
  source(here("Skript","diagram_nystartade_ftg_tillvaxtanalys_korrekt.R"), encoding="UTF-8")
  gg_nystartade <- diagram_nystartade(spara_figur = spara_figur, 
                                      output_mapp = Output_mapp,
                                      returnera_data = TRUE, 
                                      returnera_figur = TRUE)
  
  nystartade_max_ar <- nystartade_df %>% filter(antal == max(antal)) %>% .$ar
  nystartade_max_kvartal <- nystartade_df %>% filter(antal == max(antal)) %>% .$kvartal_namn
  nystartade_max_varde <- nystartade_df %>% filter(antal == max(antal)) %>% .$antal
  
  nystartade_senaste_ar <- nystartade_df %>% filter(tid == last(tid)) %>% .$ar
  nystartade_senaste_kvartal <- nystartade_df %>% filter(tid == last(tid)) %>% .$kvartal_namn
  nystartade_senaste_varde <- nystartade_df %>% filter(tid == last(tid)) %>% .$antal
  
  nystartade_jmf <- nystartade_df %>% filter(tid == last(tid)) %>% .$antal - last(nystartade_df %>% filter(tid != last(tid)) %>% .$antal)
  
  # Arbetslöshet län - 1 figur
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_senastear.R")
  gg_arbetsloshet_lan <- diagram_arbetsmarknadsstatus(region_vekt = hamtaAllaLan(),
                                                      spara_figur = spara_figur, 
                                                      output_mapp_figur = Output_mapp,
                                                      diag_arbetskraftsdeltagande = FALSE, 
                                                      diag_sysselsattningsgrad = FALSE, 
                                                      returnera_data = TRUE, 
                                                      returnera_figur = TRUE)
  
  arbetsloshet_ar_senaste <- unique(arbetsmarknadsstatus %>% filter(region=="Dalarna") %>% .$ar)
  arbetsloshet_manad_senaste <- unique(arbetsmarknadsstatus %>% filter(region=="Dalarna") %>% .$manad_long)
  arbetsloshet_dalarna_senaste <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region=="Dalarna") %>% .$varde)
  arbetsloshet_Sverige_senaste <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region=="Sverige") %>% .$varde)
  arbetsloshet_gavleborg_senaste <- gsub("\\.",",",arbetsmarknadsstatus %>% filter(region=="Gävleborg") %>% .$varde)
  arbetsloshet_lan_min <- arbetsmarknadsstatus %>% filter(varde==min(varde)) %>% dplyr::pull(region) %>% list_komma_och()
  arbetsloshet_lan_min_varde <- min(arbetsmarknadsstatus$varde) %>% str_replace("\\.", "\\,")
  
  # Arbetslöshet tidsserie - 1 figur
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_tidsserie_SCB.R")
  gg_arbetsloshet_tidsserie <- diagram_arbetsmarknadsstatus_tidsserie (spara_figur = spara_figur, 
                                                                       output_mapp_figur = Output_mapp,
                                                                       returnera_data = TRUE,
                                                                       marginal_yaxis_facet = c(0.02,0.02),
                                                                       diagram_facet = TRUE,
                                                                       returnera_figur = TRUE)
  
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
  arbstatus_df <- hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(region_vekt = "20",
                                                                                    alder_klartext = unique(arbetsmarknadsstatus_tidsserie$ålder),
                                                                                    kon_klartext = "totalt",
                                                                                    fodelseregion_klartext = unique(arbetsmarknadsstatus_tidsserie$födelseregion),
                                                                                    cont_klartext = "antal arbetslösa",
                                                                                    wide_om_en_contvar = FALSE,
                                                                                    tid_koder = "9999") %>% 
    manader_bearbeta_scbtabeller()
  
  arblosa_utrikes <- format(arbstatus_df %>% filter(födelseregion == "utrikes född") %>% .$varde,big.mark = " ")
  arblosa_inrikes <- format(arbstatus_df %>% filter(födelseregion == "inrikes född") %>% .$varde,big.mark = " ")
  arblosa_manad <- unique(arbstatus_df$månad)
  arblosa_ar <- unique(arbstatus_df$år)
  arblosa_alder <- unique(arbstatus_df$ålder)
  
  # Arbetslöshet kommun - Karta, ej diagramskript (ännu)
  source(here("Skript","arbetsloshet_kommun.R"), encoding="UTF-8")
  hamta_data_arbetsloshet(vald_region="20",
                          spara_data=TRUE,
                          output_mapp_excel = here("Data"))
  
  
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_ek_stod_bakgrund.R")
  gg_ek_stod <- diagram_ek_stod_bakgrund_SCB (output_mapp = Output_mapp,
                                              skriv_diagrambildfil = spara_figur,
                                              returnera_data_rmarkdown = TRUE)
  
  ek_stod_manad_ar_forsta <- first(ekonomiskt_stod_df$månad_år)
  ek_stod_manad_ar_sista <- last(ekonomiskt_stod_df$månad_år)
  
  ek_stod_totalt_sista <- format(ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="totalt") %>% .$antal,big.mark = " ")
  
  ek_stod_skillnad_forsta <- plyr::round_any(ekonomiskt_stod_df %>% filter(månad_år==first(månad_år)) %>% filter(födelseregion=="utrikes född") %>% .$antal - ekonomiskt_stod_df %>% filter(månad_år==first(månad_år)) %>% filter(födelseregion=="inrikes född") %>% .$antal,10)
  ek_stod_skillnad_senaste <- plyr::round_any(ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="utrikes född") %>% .$antal - ekonomiskt_stod_df %>% filter(månad_år==last(månad_år)) %>% filter(födelseregion=="inrikes född") %>% .$antal,10)
  
  # Enbart för data
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_huvink_region_huvudfot1m_kon_alder_fodelseregion_tid_ArbStatFoT1_scb.R")
  ekonomiskt_bistand_df<- hamta_bas_huvink_region_huvudfot1m_kon_alder_fodelseregion_tid_scb(region = "20",
                                                                                             huvudfot1m_klartext = "ekonomiskt stöd",
                                                                                             fodelseregion_klartext = "*",
                                                                                             cont_klartext = "antal totalt",
                                                                                             alder_klartext = "15-74 år",
                                                                                             tid_koder = "9999",
                                                                                             kon_klartext = c("*"))
  
  antal_kvinnor_stod_inrikes <- plyr::round_any(ekonomiskt_bistand_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>% .$`antal totalt`,10)
  antal_man_stod_inrikes <- plyr::round_any(ekonomiskt_bistand_df %>% filter(kön == "män",födelseregion == "inrikes född") %>% .$`antal totalt`,10)
  antal_kvinnor_stod_utrikes <- plyr::round_any(ekonomiskt_bistand_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>% .$`antal totalt`,10)
  antal_man_stod_utrikes <- plyr::round_any(ekonomiskt_bistand_df %>% filter(kön == "män",födelseregion == "utrikes född") %>% .$`antal totalt`,10)
  
  
  # # Ekonomiskt bistånd SCB - 1 figur
  # source(here("Skript","diagram_ek_bistand_SCB.R"), encoding="UTF-8")
  # gg_ek_bistand_SCB = diagram_ek_bistand_SCB(spara_figur = spara_figur, 
  #                                            output_mapp = Output_mapp,
  #                                            returnera_data = TRUE, 
  #                                            returnera_figur = TRUE)
  # 
  # # Ekonomiskt bistånd SCB bakgrund - 1 figur
  # source(here("Skript","diagram_ek_bistand_SCB_bakgrund.R"), encoding="UTF-8")
  # gg_ek_bistand_bakgrund_SCB = diagram_ek_bistand_bakgrund_SCB(spara_figur = spara_figur, 
  #                                                              output_mapp = Output_mapp,
  #                                                              returnera_data = TRUE, 
  #                                                              returnera_figur = TRUE)
  
  ###########################################
  ### Kräver manuell nedladdning av data #### - Tidigare datahämtning, ej relevant längre
  ###########################################
  
  ## OBS! Se respektive skript för mer information om hur data hämtas OBS!
  
  
  # Ekonomiskt bistånd - 1 figur
  # source(here("Skript","diagram_ek_bistand_Socialstyrelsen.R"), encoding="UTF-8")
  # gg_ek_bistand <- diagram_ek_bistand(spara_figur = spara_figur, 
  #                                     output_mapp = Output_mapp,
  #                                     returnera_data = TRUE, 
  #                                     returnera_figur = TRUE)

  # Sparar global environment i R. Detta för att man skall slippa hämta data varje gång
  save.image(file = "G:/skript/projekt/environments/samhallsekonomiska_laget.RData")
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat(sprintf("Hämtning av data klar: Det tog %.2f sekunder.", elapsed_time))
  cat("\n\n")
  
  
}else{
  load("G:/skript/projekt/environments/samhallsekonomiska_laget.RData")
}



