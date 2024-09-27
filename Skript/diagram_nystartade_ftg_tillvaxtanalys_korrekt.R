#test = diagram_nystartade(spara_figur = FALSE)
diagram_nystartade <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               region_klartext = "20 Dalarnas län",			 #  Finns: "01 Stockholms län", "03 Uppsala län", "04 Södermanlands län", "05 Östergötlands län", "06 Jönköpings län", "07 Kronobergs län", "08 Kalmars län", "09 Gotlands län", "10 Blekinges län", "12 Skåne län", "13 Hallands län", "14 Västra Götalands län", "17 Värmlands län", "18 Örebro län", "19 Västmanlands län", "20 Dalarnas län", "21 Gävleborgs län", "22 Västernorrlands län", "23 Jämtlands län", "24 Västerbottens län", "25 Norrbottens län"
                               spara_figur = TRUE, # Skall diagrammet sparas
                               returnera_data = FALSE, # Skall data returneras
                               returnera_figur = TRUE){# Finns från 2020
  
  # Diagram som skapar en figur för nystartade företag från Tillväxtanalys
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse,
         glue,
         openxlsx,
         stringr)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_nystartade_ftg_kvartal_lan_TVA.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  
  nystartade_df <- hamta_nystartadeforetag_kvartal_lan_TVA(
    län_klartext = region_klartext , 
    variabel_klartext = "*",
    kvartal_klartext = "*")
  
  diagram_capt <- "Källa: Tillväxtanalys.\nBearbetning: Samhällsanalys, Region Dalarna."
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/

  #nystartade_df <-  read.xlsx(here("Data","Tillvaxtananalys_2024_04_10.xlsx"),startRow = 3)
  
  # Diverse justeringar
  nystartade_df <- nystartade_df %>% 
    rename(tid = kvartal,
           antal = `Nystartade företag 2011-`) %>%
      separate(län, into = c("regionkod", "region"), sep = " ", remove = TRUE,extra = "merge") %>% 
        mutate(ar = substr(tid,1,4),
               kvartal = substr(tid,5,6)) %>% 
          mutate(kvartal_namn = case_when(
            kvartal == "Q1" ~ "kvartal ett",
            kvartal == "Q2" ~ "kvartal två",
            kvartal == "Q3" ~ "kvartal tre",
            kvartal == "Q4" ~ "kvartal fyra")) 
  
  if(returnera_data == TRUE){
    assign("nystartade_df", nystartade_df, envir = .GlobalEnv)
  }
  
  diagram_titel <- paste0("Antal nystartade företag per kvartal i ", skapa_kortnamn_lan(unique(nystartade_df$region)))
  diagramfilnamn <- paste0("nystartade_ftg_",skapa_kortnamn_lan(unique(nystartade_df$region)),".png")
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = nystartade_df,
                              skickad_x_var = "tid",
                              skickad_y_var = "antal",
                              skickad_x_grupp = "variabel",
                              stodlinjer_avrunda_fem = TRUE,
                              manual_color = c(diagramfarger("rus_sex")[1],"#000000"),
                              diagram_titel = diagram_titel,
                              diagram_capt =  diagram_capt,
                              inkludera_sista_vardet_var_xe_etikett = FALSE,
                              output_mapp = "output_mapp",
                              manual_y_axis_title = "Antal",
                              legend_tabort = TRUE,
                              x_axis_visa_var_xe_etikett = 4,
                              filnamn_diagram = diagramfilnamn,
                              skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- diagramfilnamn %>% str_remove(".png")
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

