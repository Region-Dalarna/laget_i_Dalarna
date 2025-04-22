
diagram_ek_bistand_SCB <- function(region_vekt = "20",
                                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   tid = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2020M01" : "2024M11"
                                   alder_klartext = "15-74 år", #
                                   kon_klartext = "totalt", # Finns kvinnor, män, totalt
                                   spara_figur = TRUE, # Skall diagrammet sparas
                                   returnera_data = FALSE, # Skall data returneras?
                                   returnera_figur = TRUE){
  
  # Diagram som skapar en figur för ekonomisk bistånd. API från SCB
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_huvink_region_huvudfot1m_kon_alder_fodelseregion_tid_ArbStatFoT1_scb.R")
  
  ekonomiskt_bistand_df <- hamta_bas_huvink_region_huvudfot1m_kon_alder_fodelseregion_tid_scb(region = region_vekt,
                                                                                              tid = tid,
                                                                                              huvudfot1m_klartext = "ekonomiskt stöd",
                                                                                              fodelseregion_klartext = "totalt",
                                                                                              cont_klartext = "antal totalt",
                                                                                              alder_klartext = alder_klartext,
                                                                                              kon_klartext = kon_klartext) %>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  gg_list <- list()
  
  

  # Fixar lite med data
  ekonomiskt_bistand_df <- ekonomiskt_bistand_df %>% 
    rename(antal = `antal totalt`) %>% 
      manader_bearbeta_scbtabeller()
  
  if(returnera_data == TRUE){
    assign("ekonomiskt_bistand_SCB_df", ekonomiskt_bistand_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Antal individer som har ekonomiskt stöd som huvudsaklig inkomstkälla."
  diagram_titel <- paste0("Antal individer ",ekonomiskt_bistand_df$ålder, " med ekonomiskt bistånd i ",unique(ekonomiskt_bistand_df$region))
  diagramfilnamn <- paste0("ekonomiskt_bistand_individ_",unique(ekonomiskt_bistand_df$region),".png")
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = ekonomiskt_bistand_df,
                              skickad_x_var = "månad_år", 
                              skickad_y_var = "antal",
                              berakna_index = FALSE,
                              diagram_titel = diagram_titel,
                              manual_color = diagramfarger("rus_sex")[1],
                              diagram_capt =  diagram_capt,
                              output_mapp = output_mapp,
                              stodlinjer_avrunda_fem = TRUE,
                              manual_y_axis_title = "",
                              legend_tabort = TRUE,
                              x_axis_visa_var_xe_etikett = 6,
                              filnamn_diagram = diagramfilnamn,
                              skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- diagramfilnamn %>% str_remove(".png")
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

