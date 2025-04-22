#test  = diagram_nybyggnation_bygglov(region_vekt = "20",spara_figur = FALSE,returnera_data = TRUE)
diagram_salda_smahus <- function(region_vekt = "20",
                                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         spara_figur = FALSE, # Skall diagrammet sparas
                                         returnera_data = FALSE, # Skall data returneras
                                         returnera_figur = TRUE, # Skall diagram returneras
                                         valda_farger = diagramfarger("rus_sex"),
                                         startar = "2000",# Startvärde för diagrammet. Finns från 1998
                                         ){
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  source(here("Skript","diagram_ek_bistand_SCB_bakgrund.R"), encoding="UTF-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Gäller för nybyggnad."
  gg_list <- list()

  salda_smahus_df <- hamta_salda_smahus_region_tid_scb(region_vekt = "20",			   # Val av region. Finns: "00", "0010", "0020", "0030", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "RIKS1", "RIKS2", "RIKS3", "RIKS4", "RIKS5", "RIKS6", "RIKS7", "RIKS8"
                                                       cont_klartext = "Antal",			 #  Finns: "Antal", "Köpeskilling, medelvärde i tkr", "Bas-/taxeringsvärde, medelvärde i tkr", "Köpeskillingskoefficient"
                                                       tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1998K1", "1998K2", "1998K3", "1998K4", "1999K1", "1999K2", "1999K3", "1999K4", "2000K1", "2000K2", "2000K3", "2000K4", "2001K1", "2001K2", "2001K3", "2001K4", "2002K1", "2002K2", "2002K3", "2002K4", "2003K1", "2003K2", "2003K3", "2003K4", "2004K1", "2004K2", "2004K3", "2004K4", "2005K1", "2005K2", "2005K3", "2005K4", "2006K1", "2006K2", "2006K3", "2006K4", "2007K1", "2007K2", "2007K3", "2007K4", "2008K1", "2008K2", "2008K3", "2008K4", "2009K1", "2009K2", "2009K3", "2009K4", "2010K1", "2010K2", "2010K3", "2010K4", "2011K1", "2011K2", "2011K3", "2011K4", "2012K1", "2012K2", "2012K3", "2012K4", "2013K1", "2013K2", "2013K3", "2013K4", "2014K1", "2014K2", "2014K3", "2014K4", "2015K1", "2015K2", "2015K3", "2015K4", "2016K1", "2016K2", "2016K3", "2016K4", "2017K1", "2017K2", "2017K3", "2017K4", "2018K1", "2018K2", "2018K3", "2018K4", "2019K1", "2019K2", "2019K3", "2019K4", "2020K1", "2020K2", "2020K3", "2020K4", "2021K1", "2021K2", "2021K3", "2021K4", "2022K1", "2022K2", "2022K3", "2022K4", "2023K1", "2023K2", "2023K3", "2023K4", "2024K1", "2024K2", "2024K3", "2024K4"
                                                       long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
                                                       wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
                                                       output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
                                                       excel_filnamn = "salda_smahus.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
                                                       returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
    
  ) %>% 
    mutate(ar=substr(kvartal,1,4),
           ar_kvartal = kvartal,
           kvartal=substr(kvartal,5,6)) %>% 
    filter(ar>=startar) 
  
  if(returnera_data == TRUE){
    assign("salda_smahus_df", salda_smahus_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCBs öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  diagramtitel <- glue("Antal sålda småhus i {unique(salda_smahus_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()}")
  #diagramtitel <- glue("{unique(nybyggnation_df$variabel)} i {unique(nybyggnation_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()}")
  diagramfil <- glue("antal_salda_smahus_{unique(salda_smahus_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()}.png")

  gg_obj <- SkapaStapelDiagram(skickad_df = salda_smahus_df %>% 
                                filter(kvartal == last(kvartal)),
                              skickad_x_var = "ar_kvartal",
                              skickad_y_var = "Antal",
                              stodlinjer_avrunda_fem = TRUE,
                              manual_color = valda_farger,
                              diagram_titel = diagramtitel,
                              manual_x_axis_text_vjust = 1,
                              manual_x_axis_text_hjust = 1,
                              diagram_capt =  diagram_capt,
                              output_mapp = output_mapp,
                              manual_y_axis_title = "Antal",
                              x_axis_visa_var_xe_etikett = 1,
                              filnamn_diagram = diagramfil,
                              skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
    
  
   names(gg_list) <- diagramfil %>% str_remove(".png")
  
  if(returnera_figur==TRUE) return(gg_list)
  
}

