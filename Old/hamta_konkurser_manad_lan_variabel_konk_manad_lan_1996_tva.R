hamta_konkurser_manad_lan_variabel_tva <- function(
			manad_klartext = "*",			 #  Finns: "1996M01", "1996M02", "1996M03", "1996M04", "1996M05", "1996M06", "1996M07", "1996M08", "1996M09", "1996M10", "1996M11", "1996M12", "1997M01", "1997M02", "1997M03", "1997M04", "1997M05", "1997M06", "1997M07", "1997M08", "1997M09", "1997M10", "1997M11", "1997M12", "1998M01", "1998M02", "1998M03", "1998M04", "1998M05", "1998M06", "1998M07", "1998M08", "1998M09", "1998M10", "1998M11", "1998M12", "1999M01", "1999M02", "1999M03", "1999M04", "1999M05", "1999M06", "1999M07", "1999M08", "1999M09", "1999M10", "1999M11", "1999M12", "2000M01", "2000M02", "2000M03", "2000M04", "2000M05", "2000M06", "2000M07", "2000M08", "2000M09", "2000M10", "2000M11", "2000M12", "2001M01", "2001M02", "2001M03", "2001M04", "2001M05", "2001M06", "2001M07", "2001M08", "2001M09", "2001M10", "2001M11", "2001M12", "2002M01", "2002M02", "2002M03", "2002M04", "2002M05", "2002M06", "2002M07", "2002M08", "2002M09", "2002M10", "2002M11", "2002M12", "2003M01", "2003M02", "2003M03", "2003M04", "2003M05", "2003M06", "2003M07", "2003M08", "2003M09", "2003M10", "2003M11", "2003M12", "2004M01", "2004M02", "2004M03", "2004M04", "2004M05", "2004M06", "2004M07", "2004M08", "2004M09", "2004M10", "2004M11", "2004M12", "2005M01", "2005M02", "2005M03", "2005M04", "2005M05", "2005M06", "2005M07", "2005M08", "2005M09", "2005M10", "2005M11", "2005M12", "2006M01", "2006M02", "2006M03", "2006M04", "2006M05", "2006M06", "2006M07", "2006M08", "2006M09", "2006M10", "2006M11", "2006M12", "2007M01", "2007M02", "2007M03", "2007M04", "2007M05", "2007M06", "2007M07", "2007M08", "2007M09", "2007M10", "2007M11", "2007M12", "2008M01", "2008M02", "2008M03", "2008M04", "2008M05", "2008M06", "2008M07", "2008M08", "2008M09", "2008M10", "2008M11", "2008M12", "2009M01", "2009M02", "2009M03", "2009M04", "2009M05", "2009M06", "2009M07", "2009M08", "2009M09", "2009M10", "2009M11", "2009M12", "2010M01", "2010M02", "2010M03", "2010M04", "2010M05", "2010M06", "2010M07", "2010M08", "2010M09", "2010M10", "2010M11", "2010M12", "2011M01", "2011M02", "2011M03", "2011M04", "2011M05", "2011M06", "2011M07", "2011M08", "2011M09", "2011M10", "2011M11", "2011M12", "2012M01", "2012M02", "2012M03", "2012M04", "2012M05", "2012M06", "2012M07", "2012M08", "2012M09", "2012M10", "2012M11", "2012M12", "2013M01", "2013M02", "2013M03", "2013M04", "2013M05", "2013M06", "2013M07", "2013M08", "2013M09", "2013M10", "2013M11", "2013M12", "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01", "2024M02", "2024M03", "2024M04", "2024M05", "2024M06", "2024M07", "2024M08", "2024M09"
			region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
			variabel_klartext = "*",			 #  Finns: "Antal konkurser", "Antal anställda berörda av konkurser"
			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
			excel_filnamn = "konkurser.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
){

  # ====================================================================================================
  #
  # Funktion för att hämta data från Tillväxtanalys API med hjälp av pxweb-paketet
  # Automatgenererat av en funktion i R som skrivits av Peter Möller, Region Dalarna
  #
  # Skapad av: frkjon den 16 oktober 2024
  # Senast uppdaterad: 16 oktober 2024
  #
  # url till tabellens API: https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/konk_manad_lan_1996.px
  #
  # ====================================================================================================

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
    			tidyverse,
    			writexl)

  # Behändiga funktioner som används i skriptet
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

  # Url till SCB:s databas
  url_uttag <- "https://statistik.tillvaxtanalys.se:443:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/konk_manad_lan_1996.px"
  px_meta <- pxweb_get(url_uttag)

  varlist_koder <- pxvarlist(px_meta)$koder
  varlist_bada <- pxvarlist(px_meta)

  # Gör om från klartext till kod som databasen förstår
  variabel_vekt <- hamta_kod_med_klartext(px_meta, variabel_klartext, skickad_fran_variabel = "variabel")

  # Hantera region-koder när regionkoderna ligger tillsammans med klartext i samma kolumn, och det är löpnummer istället för koder för län och kommuner
  region_vekt <- hamta_regionkod_med_knas_regionkod(px_meta, region_vekt, "län")           # konvertera korrekta läns- och kommunkoder till de löpnummer som används i denna databas

  # Hantera tid-koder
  px_meta$variables <- sortera_px_variabler(px_meta$variables, sorterings_vars = "månad", sortera_pa_kod = FALSE)        # sortera om månader så att de kommer i kronologisk ordning
  manad_klartext <- manad_klartext %>%           # ersätt "9999" med senaste månad
     str_replace_all("9999", hamta_giltiga_varden_fran_tabell(px_meta, "månad", klartext = TRUE) %>% max())
  giltiga_manader <- hamta_giltiga_varden_fran_tabell(px_meta, "månad")

  if (all(manad_klartext == "*")) {
      tid_vekt <- giltiga_manader
  } else {
     tid_vekt <- map(manad_klartext, function(period) {
        if (str_detect(period, ":")){     # kontrollera om det finns ett kolon = intervall
           intervall <- map_chr(str_split(period, ":") %>% unlist(), ~ hamta_kod_med_klartext(px_meta, .x, "månad"))
           retur_txt <- giltiga_manader[which(giltiga_manader == intervall[1]):which(giltiga_manader == intervall[2])]
        } else retur_txt <- hamta_kod_med_klartext(px_meta, period, "månad")
     }) %>% unlist()
     index_period <- map_lgl(px_meta$variables, ~ .x$text == "månad")          # hitta platsen i px_meta$variables där variabeln "månad" finns
     period_varden <- px_meta$variables[[which(index_period)]]$values         # läs in alla värden för variabeln "månad"
    tid_vekt <- tid_vekt[match(period_varden[period_varden %in% tid_vekt], tid_vekt)]        # sortera om tid_vekt utifrån ordningen i px_meta (som vi sorterade ovan) 
   }

  # query-lista till pxweb-uttag
  varlista <- list(
  	"månad" = tid_vekt,
  	"län" = region_vekt,
  	"variabel" = variabel_vekt)

  # Hämta data med varlista
  px_uttag <- pxweb_get(url = url_uttag, query = varlista)

  var_vektor <- NA
  var_vektor_klartext <- NA

  # gör om pxweb-uttaget till en dataframe
  px_df <- as.data.frame(px_uttag)
  px_df <- px_df %>% region_kolumn_splitta_kod_klartext("län")
  if (!all(is.na(var_vektor))) {
      # om man vill ha med koder också för variabler utöver klartext så läggs de på här (om det finns värden i var_vektor)
      px_df <- px_df %>%
            cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(any_of(var_vektor)))

      # kolumnerna med koder läggs framför motsvarande kolumner med klartext
      for (varflytt_index in 1:length(var_vektor)) {
        px_df <- px_df %>%
            relocate(any_of(names(var_vektor)[varflytt_index]), .before = any_of(var_vektor_klartext[varflytt_index]))
      }
  }

  # Om användaren vill spara data till en Excel-fil
  if (!is.na(output_mapp) & !is.na(excel_filnamn)){
    write.xlsx(px_df, paste0(output_mapp, excel_filnamn))
  }

  # Returnera data som en dataframe om användern valt det
  if (returnera_df) return(px_df)
} # slut hämta data-funktion
