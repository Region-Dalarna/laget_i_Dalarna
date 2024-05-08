
#test = diagram_avregistrerade(spara_figur = FALSE,startar=2021)
diagram_avregistrerade <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               foretagsform = "*",			 #  NA = tas inte med i uttaget,  Finns: "aktiebolag", "enskild firma", "handelsbolag m.m."
                               tid = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2009M01" : "2024M03"
                               spara_figur = TRUE, # Skall diagrammet sparas
                               returnera_data = FALSE, # Skall data returneras
                               returnera_figur = TRUE,
                               startar = 2020){# Finns från 2020
  
  # Diagram som skapar en figur för avregistrerade företag från Bolagsverket
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()
  
  # Källa: https://foretagsinfo.bolagsverket.se/sok-foretagsinformation-web/statistik
  # Uppdateras genom att hämta en ny version av data. Hämtad senaste 2024-03-15
  antal_avregistreringar_df <- read.csv("C:/Users/frkjon/Projekt/laget_i_Dalarna/Data/Bolagsverket_2024_05_08.csv",encoding="Latin1") %>% 
    pivot_longer(4:ncol(.),names_to = "variabel",values_to = "value") %>% 
      rename(År=Ã.r,Månad=MÃ.nad) %>% 
        group_by(År,Månad) %>% 
          summarize(antal = sum(value)) %>% 
            mutate(Månad = ifelse(nchar(as.character(Månad)) == 1,paste0("0",Månad),Månad),
                   ar_manad = paste0(År,"-",Månad)) %>% 
              filter(År >= startar)
  
  diagram_capt = "Källa: Socialstyrelsen\nBearbetning: Samhällsanalys, Region Dalarna"
  
  # Numrerar månadsnamn
  antal_avregistreringar_df <- antal_avregistreringar_df %>% 
    mutate(manad_long = case_when(
      Månad == "01" ~ "Januari",
      Månad == "02" ~ "Februari",
      Månad == "03" ~ "Mars",
      Månad == "04" ~ "April",
      Månad == "05" ~ "Maj",
      Månad == "06" ~ "Juni",
      Månad == "07" ~ "Juli",
      Månad == "08" ~ "Augusti",
      Månad == "09" ~ "September",
      Månad == "10" ~ "Oktober",
      Månad == "11" ~ "November",
      Månad == "12" ~ "December",
    ))
  
  if(returnera_data == TRUE){
    assign("antal_avregistreringar_df", antal_avregistreringar_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: Bolagsverket.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  diagramtitel <- paste0("Antal avregistrerade företag i Dalarna")
  diagramfilnamn <- paste0("avreg_manad.png")
  
  antal_avregistreringar_df$manad_long = factor(antal_avregistreringar_df$manad_long,levels=c("Januari","Februari","Mars","April","Maj","Juni","Juli","Augusti","September","Oktober","November","December"))
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = antal_avregistreringar_df %>%
                                              mutate(År = as.character(År)),
                                            skickad_x_var = "manad_long",
                                            skickad_y_var = "antal",
                                            skickad_x_grupp = "År",
                                            berakna_index = FALSE,
                                            lagga_till_punkter = TRUE,
                                            manual_color = diagramfarger("rus_sex"),
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt,
                                            output_mapp = output_mapp,
                                            stodlinjer_avrunda_fem = TRUE,
                                            manual_y_axis_title = "Antal",
                                            #visa_var_x_xlabel=1,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- diagramfilnamn %>% str_remove(".png")
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

