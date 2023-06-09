# Hämtar data för konkurser från SCBs hemsida (företag och berörda personer)
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__NV__NV1401__NV1401A/KonkurserForet07/
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__NV__NV1401__NV1401A/KonkurserAnst07/

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,tidyverse,openxlsx)


#hamta_data_konkurser()
hamta_data_konkurser <- function(vald_region="20",
                                 spara_data=TRUE,
                                 output_mapp_excel = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"){

  url_lista=c("https://api.scb.se/OV0104/v1/doris/sv/ssd/NV/NV1401/NV1401A/KonkurserForet07",
              "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/NV/NV1401/NV1401A/KonkurserAnst07")

  # =============================================== API-uttag ===============================================

  flik_lista=lst()
  i=1
  # Använder en loop för att skriva hämta två df:s och lägga dessa i en lista.
  while(i <= length(url_lista)){
    px_uttag <- pxweb_get(url = url_lista[i],
                          query = list(
                            Region = vald_region,
                            SNI2007 = c('*'),
                            ContentsCode = c('*'),
                            Tid = c('*')))
  
    px_df <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region, SNI2007)) %>% rename(regionkod = regionkod.Region, SNIkod = regionkod.SNI2007) %>% relocate(regionkod, .before = region) %>% 
                relocate(SNIkod, .before = `näringsgren SNI 2007`) 
    
    flik_lista[[i]] <- px_df %>% 
      rename(tid = månad) %>% 
        mutate(år = str_sub(tid, 1, 4),
               månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
               manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B"),
               år_månad = paste0(år, " - ", månad),
               månad_år = paste0(månad, " ", år))
    
    flik_lista[[i]] <- flik_lista[[i]] %>%
      arrange(regionkod) %>%
        group_by(regionkod) %>%
          mutate(sort = row_number())

    flik_lista[[i]] <- flik_lista[[i]] %>%
      mutate(månad_år_sort = factor(månad_år))

    flik_lista[[i]]$månad_år_sort <- reorder(flik_lista[[i]]$månad_år_sort, flik_lista[[i]]$sort)
    
    i=i+1
  }
  
  # Namnger lista (ger fliknamn i Exceldokument)
  names(flik_lista) <- c("Antal konkurser","Antal berörda")
  
  flik_lista[["Antal berörda"]] <- flik_lista[["Antal berörda"]] %>% 
    rename("Antal_berorda" = `Anställda drabbade av konkurser`)
  
  if (spara_data==TRUE){
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_excel,"/konkurser_korrekt.xlsx"))
  }
  
}



