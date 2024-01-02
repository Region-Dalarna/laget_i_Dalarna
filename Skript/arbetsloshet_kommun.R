# Hämtar data för konkurser från SCBs hemsida
pacman::p_load(pxweb,tidyverse,openxlsx)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#hamta_data_arbetsloshet()
hamta_data_arbetsloshet <- function(vald_region="20",
                                    spara_data=TRUE,
                                    output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"){
  
  # url till tabellen i SCB:s statistikdatabas
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0210A/ArbStatusM"
  region_vekt <- hamtakommuner(vald_region)          # hämta alla kommuner om det är en 
  alder_vekt="20-64"
  # variabler som vi vill ha med i uttaget
  varlista <- list(
    Region = region_vekt,
    Alder = alder_vekt,
    Kon = '*',
    Fodelseregion = '*',
    ContentsCode = '*',
    Tid = '*'
  )
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(Region)) %>% 
    rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  
  # ta med de variabler vi vill använda, filtrera samt gruppera dem och beräkna andel utrikes födda
  arblosa_bakgr <- px_df %>% 
    filter(födelseregion == "totalt",
           kön == "totalt") %>%  
    group_by(månad, regionkod, region, ålder) %>%
    ungroup()
  
  # Skapar grupper baserat på arbetslöshet
  arblosa_bakgr <- arblosa_bakgr %>% 
    mutate(grupp = ifelse(between(arbetslöshet,0,0.999),"0-1",
                          ifelse(between(arbetslöshet,1,1.999),"1-2",
                                 ifelse(between(arbetslöshet,2,2.999),"2-3",
                                        ifelse(between(arbetslöshet,3,3.999),"3-4",
                                               ifelse(between(arbetslöshet,4,4.999),"4-5",
                                                      ifelse(between(arbetslöshet,5,5.9999),"5-6",
                                                             ifelse(between(arbetslöshet,6,6.9999),"6-7","7+"))))))))
  
  if (spara_data==TRUE){
    flik_lista=lst("Arbetsloshet kommun"= arblosa_bakgr)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,"/arbetsloshet_kommun.xlsx"))
  }

  
}


