# Hämtar data för BNP från användsningssidan från SCB.
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__NR__NR0103__NR0103B/NR0103ENS2010T10SKv/table/tableViewLayout1/

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,
               tidyverse,
               openxlsx)

# Läser in funktioner
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#hamta_data_kpi()
hamta_data_BNP <- function(spara_data = TRUE,
                           output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"){
  
  url_adress <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NR/NR0103/NR0103B/NR0103ENS2010T10SKv"
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Förändring i KPI på årsbasis."
  
  # =============================================== API-uttag ===============================================
  # Exempel med tabell som innehåller medelålder
  px_uttag <- pxweb_get(url = url_adress,
                        query = list(ContentsCode = c('*'),
                                     Anvandningstyp = c('*'),
                                     Tid = c('*')))
  
  BNP_df <-  as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text") %>%  
    mutate(ar=substr(kvartal,1,4),
          ar_kvartal = kvartal,
          kvartal=substr(kvartal,5,6)) %>% 
      rename("Sasongsransad_forandring"=`Säsongrensad, volymförändring föregående period, procent`) %>% 
        mutate(kvartal=ifelse(kvartal=="K1","kvartal ett",
                              ifelse(kvartal=="K2","kvartal två",
                                     ifelse(kvartal=="K3","kvartal tre","kvartal fyra"))))

  if (spara_data==TRUE){
    flik_lista=lst("BNP"= BNP_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,"/BNP.xlsx"))
  }

}



