#test = diagram_nystartade(spara_figur = FALSE)
diagram_nystartade <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               foretagsform = "*",			 #  NA = tas inte med i uttaget,  Finns: "aktiebolag", "enskild firma", "handelsbolag m.m."
                               tid = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2009M01" : "2024M03"
                               spara_figur = TRUE, # Skall diagrammet sparas
                               returnera_data = FALSE, # Skall data returneras
                               returnera_figur = TRUE){# Finns från 2020
  
  # Diagram som skapar en figur för nystartade företag från Tillväxtanalys
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/
  # Uppdateras genom att hämta hem ny data. Kontrollerad 2024-05-08 - ingen ny data
  nystartade_df <-  read.xlsx(here("Data","Tillvaxtananalys_2024_04_10.xlsx"),startRow = 3)
  
  diagram_capt <- "Källa: Tillväxtanalys.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  # Diverse justeringar
  nystartade_df <- nystartade_df %>% 
    rename(ar_kvartal = X1,
           lan_nr = X2,
           antal =`antal.nystartade.företag`) %>% 
    mutate(ar = substr(ar_kvartal,1,4),
           kvartal = substr(ar_kvartal,5,6)) %>% 
    mutate(kvartal_namn = case_when(
      kvartal == "Q1" ~ "kvartal ett",
      kvartal == "Q2" ~ "kvartal två",
      kvartal == "Q3" ~ "kvartal tre",
      kvartal == "Q4" ~ "kvartal fyra")) %>% 
    mutate(grupp = "Nystartade företag")
  
  senaste_ar_nystartade = max(nystartade_df$ar)
  
  valda_år_nystartade = paste0("medelvärde ",min(nystartade_df$ar),"-",as.integer(senaste_ar_nystartade)-2)
  
  # Beräknar medelvärde för de fem år som föranleder näst senaste år
  Nystartade_medel <- nystartade_df %>%
    filter(ar %in% min(nystartade_df$ar):(as.integer(senaste_ar_nystartade)-2)) %>% 
    group_by(lan_nr,grupp,kvartal_namn,kvartal) %>% 
    summarize(antal = mean(antal)) %>% 
    mutate(ar = valda_år_nystartade)
  
  Nystartade_utskrift <- rbind(nystartade_df %>% select(-c(ar_kvartal)) %>%  filter(ar%in%(max(as.integer(ar))-1):max(ar)),Nystartade_medel)   
  
  if(returnera_data == TRUE){
    assign("nystartade_df", nystartade_df, envir = .GlobalEnv)
  }
  
  diagramtitel <- paste0("Antal nystartade företag per kvartal i Dalarna")
  diagramfilnamn <- paste0("nystartade.png")
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = Nystartade_utskrift %>% 
                                mutate(kvartal = factor(kvartal, levels = c("Q1","Q2","Q3","Q4"))),
                              skickad_x_var = "kvartal",
                              skickad_y_var = "antal",
                              skickad_x_grupp = "ar",
                              berakna_index = FALSE,
                              stodlinjer_avrunda_fem = TRUE,
                              manual_color = diagramfarger("rus_sex"),
                              diagram_titel = diagramtitel,
                              diagram_capt =  diagram_capt,
                              output_mapp = output_mapp,
                              manual_y_axis_title = "Antal nystartade företag",
                              legend_tabort = FALSE,
                              #x_axis_visa_var_xe_etikett = 4,
                              filnamn_diagram = diagramfilnamn,
                              skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- diagramfilnamn %>% str_remove(".png")
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

