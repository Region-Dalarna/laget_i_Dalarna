
#test = diagram_ek_bistand(spara_figur = FALSE)
diagram_ek_bistand <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  foretagsform = "*",			 #  NA = tas inte med i uttaget,  Finns: "aktiebolag", "enskild firma", "handelsbolag m.m."
                                  tid = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2009M01" : "2024M03"
                                  spara_figur = TRUE, # Skall diagrammet sparas
                                  returnera_data = FALSE, # Skall data returneras
                                  returnera_figur = TRUE,
                                  startar = 2018){# Finns även tidigare, men konstigheter i data (Borlänge saknas) gör att 2018 bör sättas som första startår
  
  # Diagram som skapar en figur för ekonomisk bistånd.Ej API och bara för Dalarna
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         openxlsx)

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()
  
  # Källa: https://sdb.socialstyrelsen.se/if_ekb_manad/resultat.aspx
  # Uppdateras genom att hämta en ny version av data. Hämtad senaste 2024-06-17
  ekonomiskt_bistand_df <-  read.xlsx(here("Data","Socialstyrelsen 2024_08_21.xlsx"),startRow = 2) %>% 
    filter(År >= startar ,Antal.hushåll > 0)
 
  
  diagram_capt = "Källa: Socialstyrelsen\nBearbetning: Samhällsanalys, Region Dalarna"
  
  # Numrerar månadsnamn
  ekonomiskt_bistand_df <- ekonomiskt_bistand_df %>% 
    mutate(manad = case_when(
      Månad == "Januari" ~ "01",
      Månad == "Februari" ~ "02",
      Månad == "Mars" ~ "03",
      Månad == "April" ~ "04",
      Månad == "Maj" ~ "05",
      Månad == "Juni" ~ "06",
      Månad == "Juli" ~ "07",
      Månad == "Augusti" ~ "08",
      Månad == "September" ~ "09",
      Månad == "Oktober" ~ "10",
      Månad == "November" ~ "11",
      Månad == "December" ~ "12",
    ))
  
  # Fixar lite med data
  ekonomiskt_bistand_df <- ekonomiskt_bistand_df %>% 
    mutate(ar_manad = paste0(År,"-",manad),
           antal_hushall = as.numeric(Antal.hushåll),
           variabel = "Antal hushåll" ) %>%
      select(-Antal.hushåll)
  
  if(returnera_data == TRUE){
    assign("ekonomiskt_bistand_df", ekonomiskt_bistand_df, envir = .GlobalEnv)
  }

  diagram_titel <- paste0("Antal hushåll med ekonomiskt bistånd i Dalarna ",tolower(substr(first(ekonomiskt_bistand_df$Månad),1,3))," ",first(ekonomiskt_bistand_df$År)," till ",tolower(substr(last(ekonomiskt_bistand_df$Månad),1,3))," ",last(ekonomiskt_bistand_df$År))
  diagram_titel = str_wrap(diagram_titel,40)
  diagramfilnamn <- paste0("ekonomiskt_bistand.png")
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = ekonomiskt_bistand_df,
                                       skickad_x_var = "ar_manad", 
                                       skickad_y_var = "antal_hushall",
                                       skickad_x_grupp = "variabel",
                                       berakna_index=FALSE,
                                       manual_color = c(diagramfarger("rus_sex")[1],"#000000"),
                                       diagram_titel = diagram_titel,
                                       diagram_capt =  diagram_capt,
                                       inkludera_sista_vardet_var_xe_etikett = FALSE,
                                       output_mapp = output_mapp,
                                       stodlinjer_avrunda_fem = TRUE,
                                       manual_y_axis_title = "",
                                       legend_tabort = TRUE,
                                       x_axis_visa_var_xe_etikett = 12,
                                       filnamn_diagram = diagramfilnamn,
                                       skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- diagramfilnamn %>% str_remove(".png")
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

