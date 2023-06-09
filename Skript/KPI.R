# Hämtar data för KPIF (Konsumentprisindex med fast ränta) från SCB. Denna senare används som grund för Riksbankens mål för penningpolitiken (2 procent).

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,tidyverse,openxlsx)

# Läser in funktioner
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#hamta_data_kpi()
hamta_data_kpi <- function(spara_data=TRUE,
                           output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"){
  
  url_adress <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/PR/PR0101/PR0101G/KPIF"
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Förändring i KPI på årsbasis."

  # =============================================== API-uttag ===============================================
  # Exempel med tabell som innehåller medelålder
  px_uttag <- pxweb_get(url = url_adress,
                        query = list(ContentsCode = c('*'),
                                     Tid = c('*')))
  
  KPI_df <-  as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text") %>% 
    mutate(ar=substr(månad,1,4),
           manad_long=format(as.Date(paste(ar, str_sub(månad, 6,7),"1", sep = "-")), "%B"),
           Period=paste(ar, str_sub(månad, 6,7),sep = "-")) %>% 
            rename("KPIF" = `KPIF, 12-månadsförändring, 1987=100`)
  
 #  KPI_df<-KPI_df %>% 
 #    mutate("variabel"="Inflation")
 #  
 #  # skapar dataframe vars syfte är att skapa en svart linje i diagrammet
 #  KPI_df_mal<- KPI_df %>%
 #    select(KPIF,manad_long,Period,variabel) %>%
 #      mutate("KPIF"=2,
 #             variabel = "Riksbankens inflationsmål")   
 # 
 # KPI_utskrift <- rbind(KPI_df %>% select(KPIF,manad_long,Period,variabel),KPI_df_mal)


  if (spara_data==TRUE){
    flik_lista=lst("KPI"= KPI_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,"/KPI.xlsx"))
  }
  
  # 
  # diagramtitel <- paste0("Inflation (årsförändring i KPIF) i Sverige i ",last(KPI_utskrift$manad_long)," varje år")
  # diagramfilnamn <- paste0("inflation.png")
  # 
  # smahuspriser_linje <- SkapaLinjeDiagram(skickad_df = KPI_utskrift %>%
  #                                           filter(manad_long==last(manad_long),Period>"1993-01"),
  #                                         skickad_x_var = "Period",
  #                                         skickad_y_var = "KPIF, 12-månadsförändring, 1987=100",
  #                                         skickad_x_grupp = "variabel",
  #                                         berakna_index=FALSE,
  #                                         manual_color = c(diagramfarger("rus_sex")[1],"#000000"),
  #                                         diagram_titel = diagramtitel,
  #                                         diagram_capt =  diagram_capt,
  #                                         output_mapp = output_mapp_excel,
  #                                         manual_y_axis_title = "procent",
  #                                         visa_var_x_xlabel=1,
  #                                         filnamn_diagram = diagramfilnamn,
  #                                         skriv_till_diagramfil = FALSE)
  
  
}



