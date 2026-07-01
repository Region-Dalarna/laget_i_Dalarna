# Hämtar data för arbetslöshet från SCB:s hemsida och delar upp i grupper (för att använda i karta)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,
               tidyverse,
               openxlsx)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_pxweb2.R")

#hamta_data_arbetsloshet()
hamta_data_arbetsloshet <- function(vald_region = "20",
                                    alder_vekt = "20-64",
                                    spara_data = TRUE,
                                    output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"){
  
  region_vekt <- hamtakommuner(vald_region,tamedriket = FALSE,tamedlan = FALSE)          # hämta alla kommuner om det är en 
  
  # Hämtar data med den nya versionen av pxweb
  arblosa_df <- pxweb2_hamta_data(
    tabell = "TAB6260",
    query = list(
      Region = region_vekt,
      Kon = "totalt",
      Alder = alder_vekt,
      Fodelseregion = "totalt",
      ContentsCode = "arbetslöshet",
      Tid = "9999"
    ))

  arblosa_df <- arblosa_df %>% 
    mutate(grupp = case_when(
      value >= 0 & value < 2 ~ "0-2",
      value >= 2 & value < 4 ~ "2-4",
      value >= 4 & value < 6 ~ "4-6",
      value >= 6 ~ "6+"
    ))
  
  if (spara_data==TRUE){
    flik_lista=lst("Arbetsloshet kommun" = arblosa_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,"/arbetsloshet_kommun.xlsx"))
  }
  
}


