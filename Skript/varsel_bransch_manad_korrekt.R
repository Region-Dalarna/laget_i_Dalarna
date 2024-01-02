# Skript som hämtar data för varsel på bransh och län genom webscraping
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,rvest,readxl)

# Laddar in de funktioner som används för att skapa diagram
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

options(dplyr.summarise.inform = FALSE)

#test=hamta_varsel_bransch_ar(output_mapp="G:/skript/projekt/laget_i_Dalarna/Data/",spara_till_excel=FALSE)
hamta_varsel_bransch_manad <- function(vald_region = "20",
                                       filnamn="varsel_bransch_senastear.xlsx",
                                       output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/") {
  
  # Hämtar namn på vald region
  region_namn <- hamtaregion_kod_namn(vald_region)[2]
  
  # Manuell uppdatering av år
  yr <- "2023"
  
  #Hämta och spara fil lokalt
  td = tempdir()              # skapa temporär mapp
  varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
  
  # Tidigare
  # download.file(paste0("https://arbetsformedlingen.se",
  #                      jsonlite::read_json("https://arbetsformedlingen.se/rest/analysportalen/va/sitevision")[[3]]$properties$link),
  #               destfile = varsel_fil, mode = "wb") 
  
  download.file(paste0("https://",
                       jsonlite::read_json("https://arbetsformedlingen.se/rest/analysportalen/va/sitevision")[[3]]$properties$link),
                destfile = varsel_fil, mode = "wb") 
  
  fliknamn <- excel_sheets(varsel_fil) %>%                # hämta alla fliknamn ur Excelfilen
    .[. != "Summa"]                            # ta inte med fliken "Summa"
  
  # Skapar df. Av någon anledning blir SNI-koden omväxlande NA för två variabler, så jag skapar en variabel där dessa slå ihop.
  varsel_df <-  map_dfr(fliknamn, ~ read_xlsx(varsel_fil, sheet = .x, skip = 4, n_max = 19, na = "-") %>% 
                          mutate(Månad = .x)) %>% 
    mutate(SNIKod = ifelse(!(is.na(`SNI-kod`)),`SNI-kod`,SNI0kod))
  
  # Pivoterar data och gör andra justeringar.
  varsel_df <- varsel_df %>%  
    select(-c("Inga uppgifter")) %>%
      relocate(SNIKod, .before = 1) %>%
        select(-c("SNI-kod", "SNI0kod")) %>% 
          pivot_longer(-c("SNIKod", "Näringsgren","Månad"), names_to = "Region", values_to = "Antal_varslade") %>% 
            na.omit() %>% 
              filter(Antal_varslade > 0) %>% 
                mutate(Månad = factor(Månad, levels = fliknamn, ordered = TRUE),    # Gör om månad till factor-variabel som är "ordered" = kan sorteras i ordning, dvs. vi kan använda max och min-värden (sorteras utifrån flikordningen i Excelfilen från AF)
                       År = yr) %>%                                                 # Lägg till aktuellt år (hämtas från filnamnet på Excelfilen från AF)
                  relocate(År, .before = 1) %>% 
                    relocate(Månad, .after = År) %>% 
                      relocate(Region, .after = Månad) %>% 
                        group_by(År, Månad, Region) %>% 
                          mutate(tot_region = sum(Antal_varslade)) %>% 
                            ungroup()
  
  # # Fokuserar enbart på ett län och senaste år
  varsel_df <- varsel_df %>%
    filter(Månad == max(Månad),
            Region %in% region_namn)
  
  write.xlsx(varsel_df,paste0(output_mapp,filnamn))
    
  }

  
