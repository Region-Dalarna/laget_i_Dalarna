#test = diagram_BNP_SCB(spara_figur = FALSE,startar="1980")
diagram_BNP_SCB <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  spara_figur = TRUE, # Skall diagrammet sparas
                                  returnera_data = FALSE, # Skall data returneras
                                  returnera_figur = TRUE, # Skall diagram returneras
                                  valda_farger = diagramfarger("rus_sex"),
                                  startar = "2016"){# Startvärde för diagrammet.Finns från 1980

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_BNP_anvandningstyp_tid_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()
  
  BNP_df <- hamta_BNP_anvandningstyp_tid_scb(
    anvandningstyp_klartext = "- BNP till marknadspris",			 #  Finns: "- BNP till marknadspris", "- Import av varor och tjänster", "-- import av varor", "-- import av tjänster", "- Konsumtionsutgifter", "-- hushållens konsumtionsutgifter; inkl icke-vinstdrivande organisationer", "--- de egentliga hushållens konsumtion", "--- hushållens icke vinstdrivande organisationers konsumtion", "-- offentliga konsumtionsutgifter", "--- statliga myndigheters konsumtionsutgifter", "--- kommunala myndigheters konsumtionsutgifter", "---- primärkommunernas konsumtionsutgifter", "---- regionernas konsumtionsutgifter", "- Bruttoinvesteringar", "-- fast bruttoinvestering", "-- lagerinvesteringar inkl. värdeföremål", "--- lagerinvesteringar", "--- investering i värdeföremål", "- Export av varor och tjänster", "-- export av varor", "-- export av tjänster", "- Inhemsk slutlig användning exkl. lagerinvesteringar", "- Inhemsk slutlig användning", "Summa slutlig användning exkl. lagerinvesteringar", "Summa slutlig användning"
    cont_klartext = "Säsongrensad, volymförändring föregående period, procent",			 #  Finns: "Säsongrensad, löpande priser, mnkr", "Säsongrensad, fasta priser referensår 2022, mnkr", "Säsongrensad, volymförändring föregående period, procent"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "1980K1", "1980K2", "1980K3", "1980K4", "1981K1", "1981K2", "1981K3", "1981K4", "1982K1", "1982K2", "1982K3", "1982K4", "1983K1", "1983K2", "1983K3", "1983K4", "1984K1", "1984K2", "1984K3", "1984K4", "1985K1", "1985K2", "1985K3", "1985K4", "1986K1", "1986K2", "1986K3", "1986K4", "1987K1", "1987K2", "1987K3", "1987K4", "1988K1", "1988K2", "1988K3", "1988K4", "1989K1", "1989K2", "1989K3", "1989K4", "1990K1", "1990K2", "1990K3", "1990K4", "1991K1", "1991K2", "1991K3", "1991K4", "1992K1", "1992K2", "1992K3", "1992K4", "1993K1", "1993K2", "1993K3", "1993K4", "1994K1", "1994K2", "1994K3", "1994K4", "1995K1", "1995K2", "1995K3", "1995K4", "1996K1", "1996K2", "1996K3", "1996K4", "1997K1", "1997K2", "1997K3", "1997K4", "1998K1", "1998K2", "1998K3", "1998K4", "1999K1", "1999K2", "1999K3", "1999K4", "2000K1", "2000K2", "2000K3", "2000K4", "2001K1", "2001K2", "2001K3", "2001K4", "2002K1", "2002K2", "2002K3", "2002K4", "2003K1", "2003K2", "2003K3", "2003K4", "2004K1", "2004K2", "2004K3", "2004K4", "2005K1", "2005K2", "2005K3", "2005K4", "2006K1", "2006K2", "2006K3", "2006K4", "2007K1", "2007K2", "2007K3", "2007K4", "2008K1", "2008K2", "2008K3", "2008K4", "2009K1", "2009K2", "2009K3", "2009K4", "2010K1", "2010K2", "2010K3", "2010K4", "2011K1", "2011K2", "2011K3", "2011K4", "2012K1", "2012K2", "2012K3", "2012K4", "2013K1", "2013K2", "2013K3", "2013K4", "2014K1", "2014K2", "2014K3", "2014K4", "2015K1", "2015K2", "2015K3", "2015K4", "2016K1", "2016K2", "2016K3", "2016K4", "2017K1", "2017K2", "2017K3", "2017K4", "2018K1", "2018K2", "2018K3", "2018K4", "2019K1", "2019K2", "2019K3", "2019K4", "2020K1", "2020K2", "2020K3", "2020K4", "2021K1", "2021K2", "2021K3", "2021K4", "2022K1", "2022K2", "2022K3", "2022K4", "2023K1", "2023K2", "2023K3", "2023K4"
    long_format = FALSE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "BNP.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  )
  
  BNP_df <- BNP_df %>%  
    mutate(ar=substr(kvartal,1,4),
           ar_kvartal = kvartal,
           kvartal=substr(kvartal,5,6)) %>% 
      rename("Sasongsransad_forandring"=`Säsongrensad, volymförändring föregående period, procent`) %>% 
        mutate(kvartal=ifelse(kvartal=="K1","kvartal ett",
                              ifelse(kvartal=="K2","kvartal två",
                                     ifelse(kvartal=="K3","kvartal tre","kvartal fyra"))))
  
  if(returnera_data == TRUE){
    assign("BNP_df", BNP_df, envir = .GlobalEnv)
  }
  
  
  diagram_capt <- "Källa: SCBs öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  diagramtitel <- paste0("Procentuell förändring i Sveriges BNP jämfört med föregående kvartal")
  diagramtitel <- str_wrap(diagramtitel,40)
  diagramfilnamn <- paste0("BNP.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = BNP_df %>% 
                                     filter(ar>=startar) ,
                                   skickad_x_var = "ar_kvartal", 
                                   skickad_y_var = "Sasongsransad_forandring",
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   stodlinjer_avrunda_fem = TRUE,
                                   manual_color = valda_farger,
                                   diagram_titel = diagramtitel,
                                   x_axis_sort_value = FALSE,
                                   manual_y_axis_title = "procent",
                                   x_axis_visa_var_xe_etikett = 8,
                                   diagram_capt =  diagram_capt,
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- diagramfilnamn %>% str_remove(".png")
  
  if(returnera_figur==TRUE) return(gg_list)

}

