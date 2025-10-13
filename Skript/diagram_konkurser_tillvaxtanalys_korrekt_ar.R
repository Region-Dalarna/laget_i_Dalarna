diagram_konkurser_TVA_ar <- function(region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
                                  variabel_klartext = "*",			 #  Finns: "Antal konkurser", "Antal anställda berörda av konkurser"
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",			# Var skall figuren sparas
                                  spara_figur = TRUE, # Skall diagrammet sparas
                                  returnera_data = FALSE, # Skall data returneras
                                  returnera_figur = TRUE # Skall diagram returneras
                                  ){
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse,
         glue,
         openxlsx,
         stringr)
  
  #source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_TVA.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_lan_variabel_konk_1996_tva.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  
  diagram_capt = "Källa: Tillväxtanalys\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/
  # Diagram för konkurser i Dalarnas län. Senaste och näst senaste år samt genomsnitt de fem åren dessförrinnan.
  # SCB har inte längre data för konkurser, så vi använder Tillväxtanalys data istället
  
  konkurser_df <- hamta_konkurser_manad_lan_variabel_tva(
    region_vekt = region_vekt, 
    variabel_klartext = variabel_klartext,
    manad_klartext = "*")
  
  # Diverse justeringar
  konkurser_df <- konkurser_df %>% 
    rename(tid = månad,
           antal = `Konkurser och anställda berörda av konkurs 1996-`) %>% 
    #separate(region, into = c("regionkod", "region"), sep = " ", remove = TRUE,extra = "merge") %>% 
    mutate(år = str_sub(tid, 1, 4),
           månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
           manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B"),
           år_månad = paste0(år, " - ", månad),
           månad_år = paste0(månad, " ", år)) %>% 
    select(-tid)
  
  senaste_ar <- max(konkurser_df$år)
  senaste_manad <- last(konkurser_df$manad_long)
  senaste_ar_num <- senaste_ar %>% as.numeric()

  # gruppera per månad för senaste året och för året innan det
  konkurser_df_ar <- konkurser_df %>%
    mutate(år = as.integer(år)) %>%               # ensure numeric year
    group_by(regionkod, region, variabel, år) %>%
    summarise(
      antal = sum(antal, na.rm = TRUE),            # total for the year
      n_månader = n_distinct(månad),               # how many months contributed
      .groups = "drop"
    ) %>%
    arrange(regionkod, variabel, år)
  
  if(returnera_data == TRUE){
    assign("konkurser_df_ar", konkurser_df_ar, envir = .GlobalEnv)
  }
  

  skapa_diagram <- function(df,vald_variabel){
    df <- df %>% 
      filter(variabel == vald_variabel)
    
    diagram_titel <- paste0(unique(df$variabel), " i ", skapa_kortnamn_lan(unique(df$region)))
    if(unique(df$variabel) == "Antal anställda berörda av konkurser"){
      diagramfilnamn <- paste0("anstalla_berorda_konkurser_ar",skapa_kortnamn_lan(unique(df$region)),".png")
    }else{
      diagramfilnamn <- paste0("anstal_konkurser_ar",skapa_kortnamn_lan(unique(df$region)),".png")
    }
    
    diagram_capt <- paste0("Källa: Tillväxtanalys\nBearbetning: Samhällsanalys, Region Dalarna\nData för ", senaste_ar, " till och med ",senaste_manad)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                   mutate(år = as.character(år)),
                                skickad_x_var = "år", 
                                skickad_y_var = "antal", 
                                #skickad_x_grupp = "år",
                                manual_color = diagramfarger("rus_sex"),
                                diagram_titel = diagram_titel,
                                diagram_capt =  diagram_capt,
                                output_mapp = output_mapp,
                                stodlinjer_avrunda_fem = TRUE,
                                manual_x_axis_text_vjust = 1,
                                manual_x_axis_text_hjust = 1,
                                manual_y_axis_title = "Antal",
                                #x_axis_visa_var_xe_etikett = 1,
                                filnamn_diagram = diagramfilnamn,
                                skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
    names(gg_list) <- str_remove(diagramfilnamn, ".png")
    return(gg_list)
  }
  
  diag <- map(unique(konkurser_df_ar$variabel), ~skapa_diagram(konkurser_df_ar,.x)) %>% purrr::flatten()
  
  
  if(returnera_figur == TRUE){
    return(diag)
  }
  
}

