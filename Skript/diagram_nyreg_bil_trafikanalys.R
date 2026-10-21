diagram_nyreg_bil_TV_ar <- function(region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
                                    output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",			# Var skall figuren sparas
                                    spara_figur = FALSE, # Skall diagrammet sparas
                                    returnera_data = FALSE # Skall data returneras
){
  
  # Skript som skapar ett diagram för nyregistrerade bilar på årsbasis. Används primärt i rapporten "Det samhällsekonomiska läget i Dalarna"
  
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("tidyverse")
  
  gg_list <- list()

  # Nyregistrerade personbilar, månad: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A/PersBilarDrivMedel/
  fordon_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3277",
    query = list(
      Region       = region_vekt,
      Drivmedel    = NA,      
      ContentsCode = "*",
      Tid          = "*"
    ), quiet = TRUE) |>
    dplyr::rename(tid = månad,
                  antal = value)
  
  # Bearbetar data på månadsbasis för att kunna lägga till senaste månad i caption nedan.
  fordon_df <- fordon_df %>%
    dplyr::mutate(år = str_sub(tid, 1, 4),
                 månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
                 manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B"),
                 år_månad = paste0(år, " - ", månad),
                 månad_år = paste0(månad, " ", år)) %>% 
    dplyr::select(-tid)
  
  senaste_ar <- max(fordon_df$år)
  senaste_manad <- last(fordon_df$manad_long)
  senaste_ar_num <- senaste_ar %>% as.numeric()
  
  # Grupperar på årsbasis
  fordon_df_ar <- fordon_df %>%
    dplyr::mutate(år = as.integer(år)) %>%               # ensure numeric year
    dplyr::group_by(region_kod, region, år) %>%
    dplyr::summarise(
      antal = sum(antal, na.rm = TRUE),               # how many months contributed
      .groups = "drop"
    ) %>%
    dplyr::arrange(region_kod, år)
  
  if(returnera_data == TRUE){
    assign("fordon_df_ar", fordon_df_ar, envir = .GlobalEnv)
  }
  
    diagram_titel <- paste0("Nyregistrerade personbilar i ", rdverktyg::skapa_kortnamn_lan(unique(fordon_df_ar$region)))
    diagramfilnamn <- paste0("antal_nyregistrerade_personbilar_ar_",rdverktyg::skapa_kortnamn_lan(unique(fordon_df_ar$region)),".png")
    diagram_capt <- paste0("Källa: SCB (Trafikanalys)\nBearbetning: Samhällsanalys, Region Dalarna\nData för ", senaste_ar, " till och med ",senaste_manad)
    
    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = fordon_df_ar %>% 
                                             dplyr::mutate(år = as.character(år)),
                                           skickad_x_var = "år", 
                                           skickad_y_var = "antal", 
                                           manual_color = diagramfarger("rus_sex"),
                                           diagram_titel = diagram_titel,
                                           diagram_capt =  diagram_capt,
                                           output_mapp = output_mapp,
                                           stodlinjer_avrunda_fem = TRUE,
                                           manual_x_axis_text_vjust = 1,
                                           manual_x_axis_text_hjust = 1,
                                           manual_y_axis_title = "Antal",
                                           filnamn_diagram = diagramfilnamn,
                                           skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
    names(gg_list) <- str_remove(diagramfilnamn, ".png")
    return(gg_list)
}


