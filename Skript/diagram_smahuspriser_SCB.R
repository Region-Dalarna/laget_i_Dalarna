#test  = diagram_smahuspriser(region_vekt = "20",spara_figur = FALSE)
diagram_smahuspriser <- function(region_vekt = "20",
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            spara_figur = TRUE, # Skall diagrammet sparas
                            returnera_data = FALSE, # Skall data returneras
                            returnera_figur = TRUE, # Skall diagram returneras
                            valda_farger = diagramfarger("rus_sex"),
                            valda_farger_lan = diagramfarger("rus_tre_fokus"),
                            diag_tidsserie = TRUE,
                            diag_lan_jmf = TRUE,
                            startar = "2010"){# Startvärde för diagrammet.Finns från 1997

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_smahuspriser_region_tid_SmahusT2M_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()
  objektnamn = c()
  
  smahuspriser_df <- hamta_smahuspriser_region_tid_scb(
    region_vekt = hamtaAllaLan(),			# Val av region.
    cont_klartext = "Medelpris, 1000-tals kronor",			 #  Finns: "Antal köpta småhus", "Medelpris, 1000-tals kronor", "Köpeskillingskoefficient (K/T-tal)"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1997M04", "1997M05", "1997M06", "1997M07", "1997M08", "1997M09", "1997M10", "1997M11", "1997M12", "1998M01", "1998M02", "1998M03", "1998M04", "1998M05", "1998M06", "1998M07", "1998M08", "1998M09", "1998M10", "1998M11", "1998M12", "1999M01", "1999M02", "1999M03", "1999M04", "1999M05", "1999M06", "1999M07", "1999M08", "1999M09", "1999M10", "1999M11", "1999M12", "2000M01", "2000M02", "2000M03", "2000M04", "2000M05", "2000M06", "2000M07", "2000M08", "2000M09", "2000M10", "2000M11", "2000M12", "2001M01", "2001M02", "2001M03", "2001M04", "2001M05", "2001M06", "2001M07", "2001M08", "2001M09", "2001M10", "2001M11", "2001M12", "2002M01", "2002M02", "2002M03", "2002M04", "2002M05", "2002M06", "2002M07", "2002M08", "2002M09", "2002M10", "2002M11", "2002M12", "2003M01", "2003M02", "2003M03", "2003M04", "2003M05", "2003M06", "2003M07", "2003M08", "2003M09", "2003M10", "2003M11", "2003M12", "2004M01", "2004M02", "2004M03", "2004M04", "2004M05", "2004M06", "2004M07", "2004M08", "2004M09", "2004M10", "2004M11", "2004M12", "2005M01", "2005M02", "2005M03", "2005M04", "2005M05", "2005M06", "2005M07", "2005M08", "2005M09", "2005M10", "2005M11", "2005M12", "2006M01", "2006M02", "2006M03", "2006M04", "2006M05", "2006M06", "2006M07", "2006M08", "2006M09", "2006M10", "2006M11", "2006M12", "2007M01", "2007M02", "2007M03", "2007M04", "2007M05", "2007M06", "2007M07", "2007M08", "2007M09", "2007M10", "2007M11", "2007M12", "2008M01", "2008M02", "2008M03", "2008M04", "2008M05", "2008M06", "2008M07", "2008M08", "2008M09", "2008M10", "2008M11", "2008M12", "2009M01", "2009M02", "2009M03", "2009M04", "2009M05", "2009M06", "2009M07", "2009M08", "2009M09", "2009M10", "2009M11", "2009M12", "2010M01", "2010M02", "2010M03", "2010M04", "2010M05", "2010M06", "2010M07", "2010M08", "2010M09", "2010M10", "2010M11", "2010M12", "2011M01", "2011M02", "2011M03", "2011M04", "2011M05", "2011M06", "2011M07", "2011M08", "2011M09", "2011M10", "2011M11", "2011M12", "2012M01", "2012M02", "2012M03", "2012M04", "2012M05", "2012M06", "2012M07", "2012M08", "2012M09", "2012M10", "2012M11", "2012M12", "2013M01", "2013M02", "2013M03", "2013M04", "2013M05", "2013M06", "2013M07", "2013M08", "2013M09", "2013M10", "2013M11", "2013M12", "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01", "2024M02", "2024M03"
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "smahuspriser.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
    
  )
  
  priser_df <- smahuspriser_df %>%   
    mutate(ar=substr(månad,1,4),
           manad_long=format(as.Date(paste(ar, str_sub(månad, 6,7),"1", sep = "-")), "%B"),
           Period=paste(ar, str_sub(månad, 6,7),sep = "-")) %>% 
      select(-månad) %>% 
        rename("Medelpris"=`Medelpris, 1000-tals kronor`) %>% 
          mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE)) %>% 
            filter(ar>=startar)
  
  if(returnera_data == TRUE){
    assign("priser_df", priser_df, envir = .GlobalEnv)
  }
  
  if(diag_tidsserie==TRUE){
  
    # Skapar ett linjediagram för förändring i priser. Blir något fel när man skapar index via funktionen, 
    # så jag gör det på egen hand
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    
    jmf_vekt="00"
    ValdGeografi <- skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region,byt_ut_riket_mot_sverige = TRUE)
    jmf_geografi <- skapa_kortnamn_lan(hamtaregion_kod_namn(jmf_vekt)$region,byt_ut_riket_mot_sverige = TRUE) 
    
    diagramtitel <- paste0("Förändring i priser på småhus mellan ",first(priser_df$manad_long)," ",first(priser_df$ar)," och ",last(priser_df$manad_long)," ",last(priser_df$ar))
    diagramtitel = str_wrap(diagramtitel,40)
    diagramfilnamn <- paste0("smahuspriser_tidserie.png")
    objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = priser_df %>% 
                                              filter(ar >= startar,region%in%c(jmf_geografi,ValdGeografi))%>%
                                              group_by(region)%>% 
                                              mutate("Index (startvärde 100)"=(Medelpris/first(Medelpris))*100),
                                            skickad_x_var = "Period", 
                                            skickad_y_var = "Index (startvärde 100)", 
                                            skickad_x_grupp = "region",
                                            berakna_index=FALSE,
                                            stodlinjer_avrunda_fem = TRUE,
                                            manual_color = valda_farger,
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt,
                                            output_mapp = output_mapp,
                                            x_axis_visa_var_xe_etikett = 12,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if(diag_lan_jmf == TRUE){
    diagramtitel <- paste0("Småhuspriser i ",last(priser_df$manad_long)," ",max(priser_df$ar))
    diagramfilnamn <- paste0("smahuspriser_lan.png")
    objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = priser_df %>%
                                             filter(Period == last(Period)) %>% 
                                             mutate(fokus=ifelse(region == ValdGeografi,1,
                                                                 ifelse(region == jmf_geografi,2,0))),
                                           skickad_x_var = "region", 
                                           skickad_y_var = "Medelpris",
                                           x_var_fokus = "fokus",
                                           manual_x_axis_text_vjust=1,
                                           manual_x_axis_text_hjust=1,
                                           stodlinjer_avrunda_fem = TRUE,
                                           manual_color = valda_farger_lan,
                                           diagram_titel = diagramtitel,
                                           manual_y_axis_title = "Medelpris (tkr)",
                                           x_axis_sort_value = TRUE,
                                           diagram_capt =  diagram_capt,
                                           output_mapp = output_mapp,
                                           filnamn_diagram = diagramfilnamn,
                                           skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- objektnamn
  
  if(returnera_figur==TRUE) return(gg_list)

}

