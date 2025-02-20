diagram_konkurser_bransch_TVA <- function(region_vekt = hamtakommuner("20",F,F),			   # Val av region. Finns: Kommuner. I diagrammet sker en summering per län och bransch
                                          antal_branscher = 3, # Antal branscher att visa
                                          bransch_klartext = "*",
                                          juridisk_form_klartext = "*",
                                          output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",			# Var skall figuren sparas
                                          spara_figur = FALSE, # Skall diagrammet sparas
                                          returnera_dataframe_global_environment = FALSE, # Skall data returneras
                                          returnera_figur = TRUE, # Skall diagram returneras
                                          valda_farger = diagramfarger("rus_sex"),
                                          diag_tidsserie = TRUE){
          
  
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse,
         glue,
         openxlsx,
         stringr)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_kommun_bransch_juridisk_form_tva.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  
  diagram_capt = "Källa: Tillväxtanalys\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Diagrammet visar de tre branscher med flest anställda berörda av konkurser"
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/
  # Diagram för konkurser i Dalarnas län. Senaste och näst senaste år samt genomsnitt de fem åren dessförrinnan.
  # SCB har inte längre data för konkurser, så vi använder Tillväxtanalys data istället
  
  konkurser_df <- hamta_konkurser_region_bransch_juridisk_form_tva(
    region_vekt = region_vekt, 
    bransch_klartext = bransch_klartext,
    juridisk_form_klartext = juridisk_form_klartext,
    variabel_klartext = "*",
    tid_koder = "9999") %>% 
      mutate(bransch = trimws(str_replace_all(bransch, "[^A-Za-zåäöÅÄÖ ,]", "")),
             lan = hamtaregion_kod_namn(substr(regionkod,1,2)) %>% .$region %>% skapa_kortnamn_lan()) %>% 
        group_by(period,lan,bransch) %>% 
          summarize(antal = sum(`Anställda berörda av konkurser 2009M01- (tabell under utveckling)`)) %>% 
            ungroup()  
            
  
  # Slice topp 3
  konkurser_topp <- konkurser_df %>% 
    slice_max(antal, n = antal_branscher) %>% 
      mutate(manad = substr(period,6,7),
             ar = substr(period,1,4),
             manad_namn = case_when(manad == "01" ~ "januari",
                                    manad == "02" ~ "februari",
                                    manad == "03" ~ "mars",
                                    manad == "04" ~ "april",
                                    manad == "05" ~ "maj",
                                    manad == "06" ~ "juni",
                                    manad == "07" ~ "juli",
                                    manad == "08" ~ "augusti",
                                    manad == "09" ~ "september",
                                    manad == "10" ~ "oktober",
                                    manad == "11" ~ "november",
                                    manad == "12" ~ "december"))
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("konkurser_bransch_df", konkurser_topp, envir = .GlobalEnv)
  }

  diagram_titel <- paste0("Antal anställda berörda av konkurser i ",unique(konkurser_topp$lan), " i ",unique(konkurser_topp$manad_namn)," ",unique(konkurser_topp$ar))
  
  diagramfilnamn <- paste0("anstalla_berorda_konkurser_bransch_",unique(konkurser_topp$lan),".png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = konkurser_topp ,
                              skickad_x_var = "bransch", 
                              skickad_y_var = "antal", 
                              manual_color = valda_farger,
                              diagram_titel = diagram_titel,
                              diagram_capt =  diagram_capt,
                              output_mapp = output_mapp,
                              stodlinjer_avrunda_fem = TRUE,
                              x_axis_sort_value = TRUE,
                              diagram_liggande = TRUE,
                              manual_y_axis_title = "",
                              filnamn_diagram = diagramfilnamn,
                              skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- str_remove(diagramfilnamn, ".png")
  return(gg_list)


  if(returnera_figur == TRUE){
    return(diag)
  }
  
}

