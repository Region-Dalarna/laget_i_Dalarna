# Skript som hämtar data för varsel på bransh genom webscraping och sedan sparar till Excel (om användaren vill)
# OBS!! FUNGERAR INTE SEDAN ARBETSFÖRMEDLINGEN HAR ÄNDRAT HEMSIDAN. FUNKAR VARKEN MED PETERS ELER EMANUELS SÄTT

pacman::p_load(tidyverse,openxlsx)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
options(dplyr.summarise.inform = FALSE)

#test=hamta_varsel_bransch_ar(output_mapp="G:/skript/projekt/laget_i_Dalarna/Data/",spara_till_excel=FALSE)
hamta_varsel_bransch_ar <- function(vald_region = c("20"),
                                    filnamn="varsel_bransch_senastear.xlsx",
                                    output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",
                                    spara_till_excel=FALSE
) {
  
  # skapa textsträng med kortnamn för län av vald(a) region(er)
  geografi <- hamtaregion_kod_namn(vald_region)$region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE)
  
  # ================================== nedladdning av fil ============================================
  
  # hämta webbsidan med tidigare statistik på Arbetsförmedlingen och spara som en vektor
  webbsida <- suppressWarnings(readLines("https://arbetsformedlingen.se/statistik/statistik-om-varsel"))
  
  varsel_index <- which(str_detect(webbsida, "Tillfällig"))   # sök alla rader där "varsel" finns med
  xlsx_index <- which(str_detect(webbsida, ".xlsx"))      # sök alla rader där ".xlsx" finns med
  
  fil_index <- varsel_index[varsel_index %in% xlsx_index]    # ta index där båda är med
  fil_strang <- webbsida[fil_index]                          # skapa sträng med det element där båda är med
  
  # i den strängen, ta ut startposition för alla "/download/" som hittar i strängen (det är sökvägar)
  start_sokvagar <- str_locate_all(fil_strang, "/download/")[[1]][,1]  
  
  # funktion för att ta ut fullständig url från de startpositioner vi hittade i raden ovan
  extrahera_sokvag <- function(strang, startpos) {
    
    nystrang <- str_sub(strang, startpos, nchar(strang))
    slutpos <- str_locate(nystrang, '\"')[[1]]-1
    
    retur_strang <- str_sub(nystrang, 1, slutpos)
    retur_strang <- paste0("https://arbetsformedlingen.se", retur_strang)
    return(retur_strang)
  }       
  
  # vi skapar en vektor med fullständiga sökvägar för samtliga excelfiler som finns på webbsidan
  af_urler <- start_sokvagar %>% map_chr(~ extrahera_sokvag(fil_strang, .x))
  
  # ta ut sökväg för varsel per län, dvs en sökväg som innehåller "varsel" och "lan" och "bransch"
  varsel_lan_url <- af_urler[str_detect(af_urler, "varsel") & str_detect(af_urler, "lan") & str_detect(af_urler, "bransch")]   
  
  # spara filen temporärt för att kunna extrahera fliknamn och kolla startrad
  
  td = tempdir()              # skapa temporär mapp
  varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
  
  download.file(varsel_lan_url, destfile = varsel_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
  fliknamn <- getSheetNames(varsel_fil)               # hämta alla fliknamn ur Excelfilen
  
  
  # skapar en tom dataframe
  varsel_tot<- data.frame(matrix(vector(), 0, 4,
                                 dimnames=list(c(), c("År", "Näringsgren","region" ,"antal"))),
                          stringsAsFactors=F)
  
  i=1
  
  while(i<=length(fliknamn)){
    flik <- fliknamn[[i]]
    
    if (flik=="Summa") break
    # Väljer startrad
    startrads_test_df <- read.xlsx(varsel_lan_url, sheet = flik, skipEmptyRows = FALSE)
    startrad <- which(!is.na(startrads_test_df[[1]]))[1]+1
    
    # läs in Excelfilen till en df direkt från url:en
    varsel_df <-read.xlsx(varsel_lan_url, sheet = flik, startRow = startrad) %>% 
      select(-1)
    
    # ===================================== slut på hämtning av fil =====================================
    
    # pivotera df:n så att det blir long-format och gör diverse filtreringar och bearbetningar av data
    varsel_df <- varsel_df %>% 
      pivot_longer(2:ncol(varsel_df), names_to = "region", values_to = "antal") %>%
      filter(region!="Inga.uppgifter",Näringsgren!="Summa",!(is.na(Näringsgren))) %>% 
      mutate(region = ifelse(region == "Riket", "Sverige", region),
             region=skapa_kortnamn_lan(sub("\\..*", "", region)),
             Månad=flik) %>% 
      filter(region%in%geografi)
    
    # Om varsel är na för alla branscher så saknas sannolikt data (dvs. data för månaden har inte kommit ännu) 
    if(all(is.na(varsel_df$antal))) break
    # Binder ihop varje år
    varsel_tot=rbind(varsel_tot,varsel_df)
    i=i+1
    
  }
  
  # Om användaren vill spara till Excel görs det här
  if(spara_till_excel==TRUE) write.xlsx(varsel_tot,paste0(output_mapp,filnamn))
  
  return(varsel_tot)
  
}

