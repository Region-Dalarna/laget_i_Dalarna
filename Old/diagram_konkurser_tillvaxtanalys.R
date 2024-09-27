
#test = diagram_konkurser_TVA(spara_figur = FALSE)
diagram_konkurser_TVA <- function(region_vekt = "20",
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  foretagsform = "*",			 #  NA = tas inte med i uttaget,  Finns: "aktiebolag", "enskild firma", "handelsbolag m.m."
                                  tid = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2009M01" : "2024M03"
                                  spara_figur = TRUE, # Skall diagrammet sparas
                                  returnera_data = FALSE, # Skall data returneras
                                  returnera_figur = TRUE, # Skall diagram returneras
                                  valda_farger = diagramfarger("rus_sex"),
                                  valda_farger_lan = diagramfarger("rus_tre_fokus"),
                                  diag_tidsserie = TRUE,
                                  diag_bransch = TRUE){# Startvärde för diagrammet.Finns från 1980
  
  
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta__region_sni2007_foretagsform_tid_KonkurserAnst07_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  objektnamn <- c()
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/
  # Diagram för konkurser i Dalarnas län. Senaste och näst senaste år samt genomsnitt de fem åren dessförrinnan.
  # Uppdaterad senast 2024-08-21
  # SCB har inte längre data för konkurser, så vi använder Tillväxtanalys data istället
  
  konkurser_df <-  read.xlsx(here("Data","Tillvaxtananalys_Konkurser_2024_08_21.xlsx"),sheet=1,startRow = 3)
  
  # Diverse justeringar
  konkurser_df <- konkurser_df %>% 
    rename(tid = X1,
           region = X2,
           varde =`Antal.anställda.berörda.av.konkurser`) %>%
      mutate("regionkod" = "20",
             år = str_sub(tid, 1, 4),
             månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
             manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B"),
             år_månad = paste0(år, " - ", månad),
             månad_år = paste0(månad, " ", år)) 
  
    
  if(returnera_data == TRUE){
    assign("konkurser_df", konkurser_df, envir = .GlobalEnv)
  }
  
  diagram_capt = "Källa: Tillväxtanalys\nBearbetning: Samhällsanalys, Region Dalarna"
  
  senaste_ar <- max(konkurser_df$år)
  senaste_manad <- last(konkurser_df$månad)
  senaste_ar_num <- senaste_ar %>% as.numeric()
  
  # gruppera per månad för senaste året och för året innan det
  konkurser_jmfr <- konkurser_df %>% 
    filter(år %in% c(senaste_ar, as.character(as.numeric(senaste_ar) -1))) %>% 
    group_by(år, månad,manad_long, regionkod, region) %>%
    summarize(Antal_berorda = sum(varde)) %>% 
    ungroup()
  
  jmfr_varnamn <- paste0("genomsnitt år ", as.character((senaste_ar_num-6)), "-", as.character((as.numeric(senaste_ar)-2)))
  
  antal_ar <- length(as.character(c(senaste_ar_num-2):(senaste_ar_num -6)))
  # gruppera fem senaste år
  konkurser_fem_senaste <- konkurser_df %>% 
    filter(år %in% as.character(c((senaste_ar_num-2):(senaste_ar_num -6)))) %>% 
    group_by(månad,manad_long, regionkod, region) %>%
    summarize(Berorda_tot = sum(varde, na.rm = TRUE),
              Antal_berorda = Berorda_tot/antal_ar) %>% 
    ungroup() %>% 
    mutate(år = jmfr_varnamn)
  
  konkurser_jmfr <- konkurser_jmfr %>% 
    bind_rows(konkurser_fem_senaste) %>% 
    mutate(år = factor(år, levels = c(jmfr_varnamn, as.character(senaste_ar_num-1), senaste_ar)))
  
  # skapa månader med NA-värde för de månader som vi saknar värde för, så att
  # staplarna inte blir tjocka utan att vi får ett tomrum där istället
  konkurser_jmfr <- konkurser_jmfr %>%
    group_by(år, regionkod, region) %>%
    complete(månad)
  
  konkurser_jmfr <- konkurser_jmfr %>%
    mutate(manad_long = str_to_title(manad_long)) %>% 
    mutate(manad_long = factor(manad_long, levels = c("Januari", "Februari", "Mars", "April", "Maj", 
                                                      "Juni", "Juli", "Augusti", "September", "Oktober",
                                                      "November", "December")))
  
  
  diagram_titel <- str_wrap("Antal personer berörda av konkurser i Dalarnas län")
  diagramfilnamn <- paste0("konkurser_lan.png")
  objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = konkurser_jmfr ,
                              skickad_x_var = "manad_long", 
                              skickad_y_var = "Antal_berorda", 
                              skickad_x_grupp = "år",
                              berakna_index = FALSE,
                              manual_color = diagramfarger("rus_sex"),
                              lagga_till_punkter = TRUE,
                              diagram_titel = diagram_titel,
                              diagram_capt =  diagram_capt,
                              output_mapp = output_mapp,
                              stodlinjer_avrunda_fem = TRUE,
                              manual_y_axis_title = "Antal",
                              #x_axis_visa_var_xe_etikett = 1,
                              filnamn_diagram = diagramfilnamn,
                              skriv_till_diagramfil =spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- objektnamn
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

