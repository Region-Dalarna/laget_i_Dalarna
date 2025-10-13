
#test = diagram_avregistrerade(spara_figur = FALSE,startar=2021)
diagram_avregistrerade <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               foretagsform = "*",			 #  NA = tas inte med i uttaget,  Finns: "aktiebolag", "enskild firma", "handelsbolag m.m."
                               tid = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2009M01" : "2024M03"
                               spara_figur = TRUE, # Skall diagrammet sparas
                               returnera_data = FALSE, # Skall data returneras
                               diag_manad = TRUE, # Figur med data på månadsbasis
                               diag_ar = FALSE, # Figur med data på årsbasis
                               returnera_figur = TRUE,
                               startar = 2020){# Finns från 2020
  
  # Diagram som skapar en figur för avregistrerade företag från Bolagsverket
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_foretag_foreningar_handelser_bolagsverket.R")
  
  gg_list <- list()

  
  # hämta data från Bolagsverket och sortera efter variabeln armanad, där både år och månad är med som numeriska variabler
  antal_avreg_df <- hamta_foretag_foreningar_handelser(region_vekt = "20",
                                                       handelse_klartext = "Avslutade",
                                                       tid_koder = "2020:9999") %>%
    arrange(armanad)
  
  bolagsformer <- unique(antal_avreg_df$bolagsform) %>% list_komma_och() %>% tolower()
  
  # gruppera så att vi aggregerar alla företag som avregistrerats under samma månad och år
  antal_avregistreringar_df <- antal_avreg_df %>% 
    group_by(armanad, ar, manad, regionkod, region, handelse) %>% 
    summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") %>% 
    arrange(armanad) %>% 
    mutate(manad = manad %>% str_to_sentence(),
           manad = factor(manad, levels = unique(manad)),
           ar = ar %>% as.character())
  
  if(diag_manad == TRUE){
  # returnera dataframe till global environment om returnera_data = TRUE
  if(returnera_data == TRUE){
    assign("antal_avregistreringar_df", antal_avregistreringar_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- paste0("Källa: Bolagsverket\nBearbetning: Samhällsanalys, Region Dalarna\nDiagrambeskrivning: Antal avregistrerade ", bolagsformer)
  
  diagramtitel <- paste0("Antal avregistrerade företag i ", unique(antal_avregistreringar_df$region))
  diagramfilnamn <- paste0("avreg_manad.png")
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = antal_avregistreringar_df,
                                            skickad_x_var = "manad",
                                            skickad_y_var = "antal",
                                            skickad_x_grupp = "ar",
                                            berakna_index = FALSE,
                                            lagga_till_punkter = TRUE,
                                            manual_color = diagramfarger("rus_sex"),
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt,
                                            output_mapp = output_mapp,
                                            stodlinjer_avrunda_fem = TRUE,
                                            manual_y_axis_title = "",
                                            #visa_var_x_xlabel=1,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  
  if(diag_ar == TRUE){
    
    antal_avregistreringar_df_ar <- antal_avregistreringar_df %>%
      #mutate(ar = as.integer(ar)) %>%               # ensure numeric year
      group_by(ar, regionkod, region, handelse) %>%
      summarise(
        antal = sum(antal, na.rm = TRUE),            # total for the year
        n_månader = n_distinct(manad),               # how many months contributed
        .groups = "drop"
      ) %>%
      arrange(regionkod, region, handelse ,ar)
    
    # returnera dataframe till global environment om returnera_data = TRUE
    if(returnera_data == TRUE){
      assign("antal_avregistreringar_df_ar", antal_avregistreringar_df_ar, envir = .GlobalEnv)
    }
    
    diagram_capt <- paste0("Källa: Bolagsverket\nBearbetning: Samhällsanalys, Region Dalarna\nDiagrambeskrivning: Antal avregistrerade ", bolagsformer)
    
    diagramtitel <- paste0("Antal avregistrerade företag i ", unique(antal_avregistreringar_df$region))
    diagramfilnamn <- paste0("avreg_ar.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_avregistreringar_df_ar,
                                skickad_x_var = "ar",
                                skickad_y_var = "antal",
                                manual_color = diagramfarger("rus_sex"),
                                diagram_titel = diagramtitel,
                                diagram_capt =  diagram_capt,
                                manual_x_axis_text_vjust = 1,
                                x_axis_lutning = 0,
                                output_mapp = output_mapp,
                                stodlinjer_avrunda_fem = TRUE,
                                manual_y_axis_title = "",
                                filnamn_diagram = diagramfilnamn,
                                skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
}
