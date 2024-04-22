#test = diagram_konjunkturbarometern(spara_figur=FALSE)
diagram_konjunkturbarometern <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         spara_figur = TRUE, # Skall diagrammet sparas
                                         returnera_data = FALSE, # Skall data returneras
                                         returnera_figur = TRUE,
                                         valda_farger = diagramfarger("rus_sex"),
                                         diag_barometern = TRUE,
                                         diag_bransch = TRUE,
                                         antal_etiketter_barometern = 24, # Intervall mellan visade etiketter (i månader)
                                         antal_etiketter_bransch = 12, # Intervall mellan visade etiketter (i månader)
                                         startvarde_barometern = "1996-07",# Startvärde för diagrammet.Finns från 1996-07
                                         startvarde_bransch = "2000-01"){# Startvärde för diagrammet (bransch).Finns från 1996-07
  
    # Skript som skapar två linjediagram för konjunkturbarometern, det ena generellt det andra uppdelat på bredare branschgrupper
    # Skapat av Jon Frank 2024-04-17
    
    if (!require("pacman")) install.packages("pacman")
    p_load(tidyverse)
    
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_konjunkturbarometern_indikator_period_konj.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
    
    diagram_capt <- "Källa: Konjunkturinstitutet.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: En indikator över 100 motsvarar en starkare ekonomi än normalt och värden över 110 en mycket starkare ekonomi än normalt.\nEn indikator under 100 respektive under 90 visar en svagare respektive mycket svagare ekonomi än normalt."
    gg_list <- list()
    objektnamn <- c()
  
    barometern_df <- hamta_konjunkturbarometern(indikator = c("Barometerindikatorn","Tillverkningsindustri (SNI 10-33)",
                                                                                    "Byggindustri (SNI 41-43)","Handel (SNI 45-47)",
                                                                                    "Tjänstesektorn (SNI 49-82 + 95-96)","Konfidensindikator hushåll"),
                                                period = "*")
    
    barometern_df <- barometern_df %>% 
      filter(!(is.na(`Barometerindikatorn och andra indikatorer`))) %>% 
        mutate(ar=substr(Period,1,4),
               manad_long=format(as.Date(paste(ar, str_sub(Period, 6,7),"1", sep = "-")), "%B"),
               Period=paste(ar, str_sub(Period, 6,7),sep = "-")) %>% 
          filter(Period >= min(startvarde_barometern,startvarde_bransch)) %>% 
            rename("varde" = `Barometerindikatorn och andra indikatorer`)
    
    if(returnera_data == TRUE){
      assign("barometern_df", barometern_df, envir = .GlobalEnv)
    }
      
    if(diag_barometern == TRUE){
      
      # skapar dataframe vars syfte är att skapa en svart linje i diagrammet
      barometern_jmf_df <- barometern_df %>%
        filter(Period >= startvarde_barometern,Indikator == "Barometerindikatorn") %>% 
          mutate(varde = 100,
                 "Indikator"="Normalt läge i ekonomin")
        
      barometern_gen_utskrift <- rbind(barometern_df %>% filter(Period >= startvarde_barometern,Indikator == "Barometerindikatorn"),barometern_jmf_df)
      
      diagramtitel <- paste0("Barometerindikatorns utveckling i Sverige mellan ",first(barometern_gen_utskrift$manad_long)," ",first(barometern_gen_utskrift$ar)," och ",last(barometern_gen_utskrift$manad_long)," ",last(barometern_gen_utskrift$ar))
      diagramtitel = str_wrap(diagramtitel,45)
      diagramfilnamn <- paste0("barometern.png")
      objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
        
      gg_obj <- SkapaLinjeDiagram(skickad_df = barometern_gen_utskrift,
                                  skickad_x_var = "Period", 
                                  skickad_y_var = "varde",
                                  skickad_x_grupp ="Indikator",
                                  berakna_index=FALSE,
                                  manual_color = c(valda_farger[1],"#000000"),
                                  stodlinjer_avrunda_fem = TRUE,
                                  manual_y_axis_title="Indikatorvärde",
                                  diagram_titel = diagramtitel,
                                  diagram_capt =  diagram_capt,
                                  output_mapp = output_mapp,
                                  x_axis_visa_var_xe_etikett = antal_etiketter_barometern,
                                  filnamn_diagram = diagramfilnamn,
                                  skriv_till_diagramfil = spara_figur)
      
      gg_list <- c(gg_list, list(gg_obj))
    
      }
      
    if(diag_bransch == TRUE){  
    
      # Skapar en dataframe vars syfte är att rita en svart linje i diagrammet
      barometern_jmf_df<- barometern_df %>%
        filter(Period >= startvarde_bransch,Indikator=="Tillverkningsindustri (SNI 10-33)") %>% 
        mutate(varde = 100,
               "Indikator" = "Normalt läge i ekonomin")
      
      barometern_bransch_utskrift<-rbind(barometern_df %>% filter(Indikator!="Barometerindikatorn",Period >= startvarde_bransch),barometern_jmf_df)
      
      
      barometern_bransch_utskrift <-  barometern_bransch_utskrift %>% 
        mutate(Indikator = case_when(barometern_bransch_utskrift$Indikator == "Byggindustri (SNI 41-43)" ~ "Byggindustri",
                                     barometern_bransch_utskrift$Indikator == "Handel (SNI 45-47)" ~ "Handel",
                                     barometern_bransch_utskrift$Indikator == "Tillverkningsindustri (SNI 10-33)" ~ "Tillverkningsindustri",
                                     barometern_bransch_utskrift$Indikator == "Tjänstesektorn (SNI 49-82 + 95-96)" ~ "Tjänstesektorn",
                                     barometern_bransch_utskrift$Indikator == "Konfidensindikator hushåll" ~ "Hushåll",
                                     .default = barometern_bransch_utskrift$Indikator))
      
      barometern_bransch_utskrift <- barometern_bransch_utskrift %>% 
        mutate(Indikator = factor(Indikator, levels = c("Byggindustri", "Handel", "Tillverkningsindustri",
                                                        "Tjänstesektorn", "Hushåll", 
                                                        "Normalt läge i ekonomin")))
      
      diagramtitel <- paste0("Barometerindikatorns utveckling i Sverige mellan januari 2020 och ",last(barometern_bransch_utskrift$manad_long)," ",last(barometern_bransch_utskrift$ar))
      diagramtitel = str_wrap(diagramtitel,45)
      diagramfilnamn <- paste0("barometern_bransch.png")
      objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
      
      gg_obj <- SkapaLinjeDiagram(skickad_df = barometern_bransch_utskrift ,
                                        skickad_x_var = "Period", 
                                        skickad_y_var = "varde",
                                        skickad_x_grupp ="Indikator",
                                        berakna_index=FALSE,
                                        manual_color = c(valda_farger[1:5],"#000000"),
                                        manual_y_axis_title="Indikatorvärde",
                                        stodlinjer_avrunda_fem = TRUE,
                                        diagram_titel = diagramtitel,
                                        diagram_capt =  diagram_capt,
                                        output_mapp = output_mapp,
                                        #x_axis_storlek = 7,
                                        x_axis_visa_var_xe_etikett = antal_etiketter_bransch,
                                        filnamn_diagram = diagramfilnamn,
                                        skriv_till_diagramfil = spara_figur)
      
      gg_list <- c(gg_list, list(gg_obj))
    
    }
    
  names(gg_list) <- c(objektnamn)
  
  if(returnera_figur==TRUE) return(gg_list)
    
  
}



