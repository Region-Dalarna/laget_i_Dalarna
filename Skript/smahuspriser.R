# Skript som hämtar data för småhuspriser från statistikdatabasen och skriver ut två diagram om användaren vill
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BO__BO0501__BO0501B/SmahusT2M/
pacman::p_load(pxweb,tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=hamta_data_smahuspriser(skapa_figur=TRUE)
hamta_data_smahuspriser <- function(region_vekt="20",
                                    jmf_vekt="00", # Vilken region vill vi jämföra utvecklingen med
                                    output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                    output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                    skapa_figur=FALSE,
                                    spara_data=FALSE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Noteringen om månad avser i själva verket en tremånadersperiod, där noteringen avser den sista månaden i perioden."
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0501/BO0501B/SmahusT2M"
  
  varlista <- list("Region"=hamtaAllaLan(),
                   "ContentsCode"=c("BO0501AJ"),
                   "Tid"="*")
  
  px_data <- pxweb_get(url = url1,query = varlista)
  
  priser_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>%   
    mutate(ar=substr(månad,1,4),
           manad_long=format(as.Date(paste(ar, str_sub(månad, 6,7),"1", sep = "-")), "%B"),
           Period=paste(ar, str_sub(månad, 6,7),sep = "-")) %>% 
      select(-månad) %>% 
        rename("Medelpris"=`Medelpris, 1000-tals kronor`)
  
  ValdGeografi <- skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region,byt_ut_riket_mot_sverige = TRUE)
  jmf_geografi <- skapa_kortnamn_lan(hamtaregion_kod_namn(jmf_vekt)$region,byt_ut_riket_mot_sverige = TRUE) 
  
  priser_df$region <- skapa_kortnamn_lan(priser_df$region,byt_ut_riket_mot_sverige = TRUE)
  
  if (spara_data==TRUE){
    flik_lista=lst("Smahuspriser"=priser_df,)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,"/smahuspriser.xlsx"))
  }

  # Skapar ett linjediagram för förändring i priser. Blir något fel när man skapar index via funktionen, 
  # så jag gör det på egen hand
  diagramtitel <- paste0("Förändring i priser på småhus")
  diagramfilnamn <- paste0("smahuspriser_tidserie.png")
  
  gg_obj <- SkapaLinjeDiagram(skickad_df =priser_df %>% 
                                filter(Period>2010,region%in%c(jmf_geografi,ValdGeografi))%>%
                                  group_by(region) %>% 
                                    mutate("Index (startvärde 100)"=(Medelpris/first(Medelpris))*100),
                               skickad_x_var = "Period", 
                               skickad_y_var = "Index (startvärde 100)", 
                               skickad_x_grupp = "region",
                               berakna_index=FALSE,
                               manual_color = diagramfarger("rus_sex"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               output_mapp = output_mapp_figur,
                               visa_var_x_xlabel=24,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skapa_figur)
  
  gg_list <- c(gg_list,lst("smahuspriser_tidsserie" = gg_obj))
  
  diagramtitel <- paste0("Småhuspriser i Sveriges län, ",last(priser_df$manad_long)," ",max(priser_df$ar))
  diagramfilnamn <- paste0("smahuspriser_lan.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df =priser_df %>%
                                filter(Period==max(Period)) %>% 
                                         mutate(fokus=ifelse(region==ValdGeografi,1,
                                                             ifelse(region==jmf_geografi,2,0))),
                               skickad_x_var = "region", 
                               skickad_y_var = "Medelpris",
                               x_var_fokus = "fokus",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("rus_tre_fokus")[1:3],
                               diagram_titel = diagramtitel,
                               x_axis_sort_value = TRUE,
                               diagram_capt =  diagram_capt,
                               diagram_facet = FALSE,
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skapa_figur)
  
  gg_list <- c(gg_list,lst("smahuspriser_lan" = gg_obj))

  return(gg_list)
}


