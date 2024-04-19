# Hämtar data för nybyggande och bygglov från SCB
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BO__BO0101__BO0101G/LghHustypKv/table/tableViewLayout1/
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BO__BO0101__BO0101G/LghHustypKv/
# Info om statistiken: https://www.scb.se/hitta-statistik/statistik-efter-amne/boende-byggande-och-bebyggelse/bostadsbyggande-och-ombyggnad/bygglov-nybyggnad-och-ombyggnad/pong/statistiknyhet/paborjad-nybyggnation-av-bostadslagenheter-1a-kvartalet-2023-preliminara-uppgifter/

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,tidyverse,openxlsx)

# Läser in funktioner
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#hamta_data_nybyggnation()
hamta_data_nybyggnation <- function(vald_region="20",
                                   spara_data=TRUE,
                                   output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"){
  
  url_nybygg <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0101/BO0101C/LagenhetNyKv16"
  url_bygglov <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0101/BO0101G/LghHustypKv"
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Förändring i KPI på årsbasis."
  
  # =============================================== API-uttag ===============================================
  
  # Nybyggnation
  px_uttag_nybygg <- pxweb_get(url = url_nybygg,
                        query = list(Region=vald_region,
                                     Hustyp="*",
                                     ContentsCode = c('*'),
                                     Tid = c('*')))
  
  Husbyggande_df <-  as.data.frame(px_uttag_nybygg, column.name.type = "text", variable.value.type = "text")  %>%  
     mutate(ar=substr(kvartal,1,4),
            ar_kvartal = kvartal,
            kvartal=substr(kvartal,5,6)) %>% 
      rename("Fardigstallda"=`Färdigställda lägenheter i nybyggda hus`,
             "Paborjade"= `Påbörjade lägenheter i nybyggda hus`)
 
  px_uttag_bygglov <- pxweb_get(url = url_bygglov,
                               query = list(Region=vald_region,
                                            Hustyp ="*",
                                            ContentsCode = c('*'),
                                            Tid = c('*'))) 
  
  # Bygglov
  Bygglov_df <-  as.data.frame(px_uttag_bygglov, column.name.type = "text", variable.value.type = "text")  %>%  
    mutate(ar = substr(kvartal,1,4),
           ar_kvartal = kvartal,
           kvartal=substr(kvartal,5,6)) %>% 
    rename("Antal" = `Bygglov för nybyggnad, lägenheter`)
  

  if (spara_data==TRUE){
    flik_lista=lst("Nybyggnation"= Husbyggande_df,"Bygglov" = Bygglov_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,"/byggande.xlsx"))
  }
  
  # diagramtitel <- paste0("Nybyggnation i Dalarna")
  # diagramfilnamn <- paste0("nybyggnation.png")
  # 
  # smahuspriser_linje <- SkapaLinjeDiagram(skickad_df = Husbyggande_df %>% 
  #                                           filter(kvartal==last(kvartal))%>% 
  #                                             filter(ar>"2009"),
  #                                         skickad_x_var = "ar", 
  #                                         skickad_y_var = "Paborjade",
  #                                         skickad_x_grupp = "hustyp",
  #                                         berakna_index=FALSE,
  #                                         manual_color = c(diagramfarger("rus_sex")),
  #                                         diagram_titel = diagramtitel,
  #                                         diagram_capt =  diagram_capt,
  #                                         output_mapp = output_mapp_excel,
  #                                         manual_y_axis_title = "Antal",
  #                                         visa_var_x_xlabel=1,
  #                                         filnamn_diagram = diagramfilnamn,
  #                                         skriv_till_diagramfil = FALSE)
  # 
  # diagramtitel <- paste0("Antal godkända bygglov i Dalarna")
  # diagramfilnamn <- paste0("bygglov.png")
  # 
  # smahuspriser_linje <- SkapaLinjeDiagram(skickad_df = Bygglov_df %>% 
  #                                           filter(kvartal==last(kvartal))%>% 
  #                                             filter(ar>"2009") %>% 
  #                                               filter(hustyp%in%c("småhus","flerbostadshus exkl. specialbostäder")),
  #                                         skickad_x_var = "ar", 
  #                                         skickad_y_var = "Antal",
  #                                         skickad_x_grupp = "hustyp",
  #                                         berakna_index=FALSE,
  #                                         manual_color = c(diagramfarger("rus_sex")),
  #                                         diagram_titel = diagramtitel,
  #                                         diagram_capt =  diagram_capt,
  #                                         output_mapp = output_mapp_excel,
  #                                         manual_y_axis_title = "Antal",
  #                                         visa_var_x_xlabel=1,
  #                                         filnamn_diagram = diagramfilnamn,
  #                                         skriv_till_diagramfil = FALSE)
  
}



