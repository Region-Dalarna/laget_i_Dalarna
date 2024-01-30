# Skript som hämtar data för barometerindikatorn och om användaren vill skapar ett diagram
# Data är dels för barometerindikatorn generellt, dels för senaste år på branschnivå
# Skriptet kräver att användaren använder keyring och har sparat sitt lösenord under rd.

pacman::p_load(pxweb,httr,tidyverse,keyring,openxlsx)

# Laddar in de funktioner som används för att ta hem data ochskapa diagram
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=hamta_data_konjunkturbarometern(skapa_figur=TRUE)
hamta_data_konjunkturbarometern <- function(output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                            output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                            skapa_figur=FALSE,
                                            spara_data=FALSE){
          
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Konjunkturinstitutet.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: En indikator över 100 motsvarar en starkare ekonomi än normalt och värden över 110 en mycket starkare ekonomi än normalt.\nEn indikator under 100 respektive under 90 visar en svagare respektive mycket svagare ekonomi än normalt."

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  objektnamn <- c()
  
  #==========================================================================================================  
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.info()[["user"]], password = key_get("rd","frkjon")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://statistik.konj.se/PXWeb/api/v1/sv/KonjBar/indikatorer/Indikatorm.px"
  
  # Variabelnamn
  # "Indikator"=c("KIFI") Barometerindikatorn generellt
  # "Indikator"=c("BTVI") Tillverkningsindustrin
  # "Indikator"=c("BBYG") Byggindustri
  # "Indikator"=c("BDHAN") Handel
  # "Indikator"=c("BTJA") Tjänstesektorn
  # "Indikator"=c("bhus") Hushållen
  
  # Ta hem data för barometerindikatorn generellt för alla år
  
  varlista <- list("Indikator"=c("KIFI","BTVI","BBYG","BDHAN","BTJA","bhus"),
                   "Period"="*")
  
  px_data <- pxweb_get(url = url1,query = varlista)
  
  barometern_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>% 
    filter(!(is.na(`Barometerindikatorn och andra indikatorer`)))
  
  # # skapar dataframe vars syfte är att skapa en svart linje i diagrammet
  # barometern_jmf_df<- barometern_df %>%
  #   mutate("Barometerindikatorn och andra indikatorer"=100,
  #          "Indikator"="Normalt läge i ekonomin")
  # 
  # barometern_df<-rbind(barometern_df,barometern_jmf_df)
  # 
  # Skapar år och månad som är lättare att förstå
  barometern_df<-barometern_df %>% 
    mutate(ar=substr(Period,1,4),
           manad_long=format(as.Date(paste(ar, str_sub(Period, 6,7),"1", sep = "-")), "%B"),
           Period=paste(ar, str_sub(Period, 6,7),sep = "-"))

  
  # # Ta hem data på branschnivå för senaste år enbart
  # varlista <- list("Indikator"=c("BTVI","BBYG","BDHAN","BTJA","bhus"),
  #                  "Period"=max(hamta_giltiga_varden_fran_tabell(url1, "Period")))
  # 
  # #"Period"=max(hamta_giltiga_varden_fran_tabell(url1, "Period")))
  # 
  # px_data <- pxweb_get(url = url1,query = varlista)
  # 
  # barometern_bransch_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")%>%
  #   mutate(ar=substr(Period,1,4),
  #          manad_long=format(as.Date(paste(ar, str_sub(Period, 6,7),"1", sep = "-")), "%B"),
  #          Period=paste(ar, str_sub(Period, 6,7),sep = "-"))
  
  if (spara_data==TRUE){
    flik_lista=lst("Alla"=barometern_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,"/konjunkturbarometern.xlsx"))
  }
  
  if(skapa_figur==TRUE){
    
    # skapar dataframe vars syfte är att skapa en svart linje i diagrammet
    barometern_jmf_df<- barometern_df %>%
      filter(Indikator=="Barometerindikatorn") %>% 
        mutate("Barometerindikatorn och andra indikatorer"=100,
               "Indikator"="Normalt läge i ekonomin")
    
   barometern_gen_utskrift<-rbind(barometern_df %>% filter(Indikator=="Barometerindikatorn"),barometern_jmf_df)
    
    diagramtitel <- paste0("Företagen och hushållens syn på ekonomin i Sverige")
    diagramfilnamn <- paste0("barometern.png")
    objektnamn <- c(objektnamn,"barometern")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = barometern_gen_utskrift,
                                skickad_x_var = "Period", 
                                skickad_y_var = "Barometerindikatorn och andra indikatorer",
                                skickad_x_grupp ="Indikator",
                                berakna_index=FALSE,
                                manual_color = c(diagramfarger("rus_sex")[1],"#000000"),
                                diagram_titel = diagramtitel,
                                diagram_capt =  diagram_capt,
                                output_mapp = output_mapp_figur,
                                visa_var_x_xlabel=24,
                                filnamn_diagram = diagramfilnamn,
                                skriv_till_diagramfil = skapa_figur)
    
    gg_list[[i]] <-gg_obj
    i=i+1
    
    # skapar dataframe vars syfte är att skapa en svart linje i diagrammet. Väljer en av branscherna för att linjen inte skall upprepas
    barometern_jmf_df<- barometern_df %>%
      filter(Period>"2019-12",Indikator=="Tillverkningsindustri (SNI 10-33)") %>% 
        mutate("Barometerindikatorn och andra indikatorer"=100,
               "Indikator"="Normalt läge i ekonomin")
    
    barometern_bransch_utskrift<-rbind(barometern_df %>% filter(Indikator!="Barometerindikatorn",Period>"2019-12"),barometern_jmf_df)
    
    diagramtitel <- paste0("Företagen och hushållens syn på ekonomin i Sverige")
    diagramfilnamn <- paste0("barometern.png")
    objektnamn <- c(objektnamn,"barometern_bransch")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = barometern_bransch_utskrift,
                                skickad_x_var = "Period", 
                                skickad_y_var = "Barometerindikatorn och andra indikatorer",
                                skickad_x_grupp ="Indikator",
                                berakna_index=FALSE,
                                manual_color = c(diagramfarger("rus_sex")[1:3],"#000000",diagramfarger("rus_sex")[4:5]),
                                diagram_titel = diagramtitel,
                                diagram_capt =  diagram_capt,
                                output_mapp = output_mapp_figur,
                                visa_var_x_xlabel=12,
                                filnamn_diagram = diagramfilnamn,
                                skriv_till_diagramfil = skapa_figur)
    
    gg_list[[i]] <-gg_obj
    i=i+1
    # 
    # 
    # 
    # diagramtitel <- paste0("Företagen och hushållens syn på ekonomin i Sverige, ",unique(barometern_bransch_df$manad_long)," ",unique(barometern_bransch_df$ar))
    # diagramfilnamn <- paste0("barometern_bransch.png")
    # objektnamn <- c(objektnamn,"barometern_bransch")
    # 
    # gg_obj <- SkapaStapelDiagram(skickad_df = barometern_bransch_df ,
    #                              skickad_x_var = "Indikator", 
    #                              skickad_y_var = "Barometerindikatorn och andra indikatorer",
    #                              manual_x_axis_text_vjust=1,
    #                              manual_x_axis_text_hjust=1,
    #                              manual_color = diagramfarger("rus_sex")[1],
    #                              diagram_titel = diagramtitel,
    #                              diagram_capt =  diagram_capt,
    #                              x_axis_sort_value = TRUE,
    #                              diagram_facet = FALSE,
    #                              output_mapp = output_mapp_figur,
    #                              filnamn_diagram = diagramfilnamn,
    #                              skriv_till_diagramfil = skapa_figur)
    # 
    # gg_list[[i]] <-gg_obj
    # i=i+1
    
    names(gg_list)<-c(objektnamn)
    return(gg_list)
  }
}


