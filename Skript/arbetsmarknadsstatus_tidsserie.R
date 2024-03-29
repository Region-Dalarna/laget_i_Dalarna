## =================================================================================================================
# Skript som laddar hem data för arbetsmarknadsstatus från SCB och skriver ut 3 diagram (arbetslöshet, arbetskraftsdeltagande, sysselsättningsgrad)
# Källa  
# =================================================================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,tidyverse,openxlsx)

# Laddar in de funktioner som används för att skapa diagram
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- diag_arbetsmarknadsstatus(skapa_fil = FALSE)

diag_arbetsmarknadsstatus <-function(region_vekt = "20", 
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     skapa_fil = TRUE,
                                     diag_arbetslosthet = TRUE,
                                     diag_arbetskraftsdeltagande = TRUE,
                                     diag_sysselsattningsgrad = TRUE){
  
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/AM/AM0210/AM0210A/ArbStatusM"
  url3 <- paste0(url1, url2)
  
  # Om man vill ta reda vilka variabler som finns i tabellen. 
  # pxvarlist(url3)                        # för att se alla variabler för en tabell
  # pxvardelist(url3, "contentscode")      # för att se alla värden för vald variabel
  
  # Tar bara ut data för de variabler vi vill ha:
  cont_code=c()
  if(diag_arbetslosthet==TRUE) cont_code=c(cont_code, "000006II")
  if(diag_arbetskraftsdeltagande == TRUE) cont_code=c(cont_code,"000006IJ")
  if(diag_sysselsattningsgrad == TRUE) cont_code=c(cont_code,"000006IK")
  
  # Variabler som skall tas ut
  varlista <-  list(#"Region"=c("00",region_vekt),
                    "Region"=c(hamtaAllaLan()),
                    "Kon"=c("1+2"),
                    "Alder"=c("20-64"),
                    "Fodelseregion"=c("tot"),
                    "ContentsCode"=cont_code,
                    "Tid"="*")
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame och tar med regionkod
  arbetsmarknadsstatus_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% 
    mutate(ar=substr(månad,1,4),
           manad_long=format(as.Date(paste(ar, str_sub(månad, 6,7),"1", sep = "-")), "%B"),
           Period=paste(ar, str_sub(månad, 6,7),sep = "-")) %>% 
      select(-månad)
  
  arbetsmarknadsstatus_df <- arbetsmarknadsstatus_df %>% 
    rename(regionkod = Region)%>%
      relocate(regionkod, .before = region)
  
  # Tar bort län i länsnamnet
  
  arbetsmarknadsstatus_df$region <-skapa_kortnamn_lan(arbetsmarknadsstatus_df$region)
  
  openxlsx::write.xlsx(arbetsmarknadsstatus_df,paste0(output_mapp,"/arbetsmarknadsstatus_tidsserie.xlsx"))
  
}

