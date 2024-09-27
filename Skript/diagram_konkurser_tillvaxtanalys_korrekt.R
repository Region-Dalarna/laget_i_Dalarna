diagram_konkurser_TVA <- function(region_klartext = "20 Dalarnas län", # Finns: "01 Stockholms län", "03 Uppsala län", "04 Södermanlands län", "05 Östergötlands län", "06 Jönköpings län", "07 Kronobergs län", "08 Kalmar län", "09 Gotlands län", "10 Blekinge län", "12 Skåne län", "13 Hallands län", "14 Västra Götalands län", "17 Värmlands län", "18 Örebro län", "19 Västmanlands län", "20 Dalarnas län", "21 Gävleborgs län", "22 Västernorrlands län", "23 Jämtlands län", "24 Västerbottens län", "25 Norrbottens län"
                                  variabel_klartext = "*",			 #  Finns: "Antal konkurser", "Antal anställda berörda av konkurser"
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",			# Var skall figuren sparas
                                  spara_figur = TRUE, # Skall diagrammet sparas
                                  returnera_data = FALSE, # Skall data returneras
                                  returnera_figur = TRUE, # Skall diagram returneras
                                  valda_farger = diagramfarger("rus_sex"),
                                  valda_farger_lan = diagramfarger("rus_tre_fokus"),
                                  diag_tidsserie = TRUE){
  
  
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse,
         glue,
         openxlsx,
         stringr)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_TVA.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  
  diagram_capt = "Källa: Tillväxtanalys\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/
  # Diagram för konkurser i Dalarnas län. Senaste och näst senaste år samt genomsnitt de fem åren dessförrinnan.
  # SCB har inte längre data för konkurser, så vi använder Tillväxtanalys data istället
  
  konkurser_df <- hamta_konkurser_TVA(
    region_klartext = region_klartext, 
    variabel_klartext = variabel_klartext,
    period_klartext = "*")
  
  # Diverse justeringar
  konkurser_df <- konkurser_df %>% 
    rename(tid = månad,
           antal = `Konkurser och anställda berörda av konkurs 1996-`) %>% 
      separate(län, into = c("regionkod", "region"), sep = " ", remove = TRUE,extra = "merge") %>% 
        mutate(år = str_sub(tid, 1, 4),
               månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
               manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B"),
               år_månad = paste0(år, " - ", månad),
               månad_år = paste0(månad, " ", år)) %>% 
          select(-tid)

  if(returnera_data == TRUE){
    assign("konkurser_df", konkurser_df, envir = .GlobalEnv)
  }
  
  senaste_ar <- max(konkurser_df$år)
  senaste_manad <- last(konkurser_df$månad)
  senaste_ar_num <- senaste_ar %>% as.numeric()
  
  # gruppera per månad för senaste året och för året innan det
  konkurser_jmfr <- konkurser_df %>% 
    filter(år %in% c(senaste_ar, as.character(as.numeric(senaste_ar) -1))) %>% 
      group_by(år, månad,manad_long, regionkod, region,variabel) %>%
        summarize(Antal_berorda = sum(antal)) %>% 
          ungroup()
  
  jmfr_varnamn <- paste0("genomsnitt år ", as.character((senaste_ar_num-6)), "-", as.character((as.numeric(senaste_ar)-2)))
  
  antal_ar <- length(as.character(c(senaste_ar_num-2):(senaste_ar_num -6)))
  # gruppera fem senaste år
  konkurser_fem_senaste <- konkurser_df %>% 
    filter(år %in% as.character(c((senaste_ar_num-2):(senaste_ar_num -6)))) %>% 
      group_by(månad,manad_long, regionkod, region,variabel) %>%
        summarize(antal_tot = sum(antal, na.rm = TRUE),
                  Antal_berorda = antal_tot/antal_ar) %>% 
          ungroup() %>% 
            mutate(år = jmfr_varnamn)
      
  konkurser_jmfr <- konkurser_jmfr %>% 
    bind_rows(konkurser_fem_senaste) %>% 
      mutate(år = factor(år, levels = c(jmfr_varnamn, as.character(senaste_ar_num-1), senaste_ar)))
  
  # skapa månader med NA-värde för de månader som vi saknar värde för, så att
  # staplarna inte blir tjocka utan att vi får ett tomrum där istället
  konkurser_jmfr <- konkurser_jmfr %>%
    group_by(år, regionkod, region) %>%
      complete(månad)
  
  konkurser_jmfr <- konkurser_jmfr %>%
    mutate(manad_long = str_to_title(manad_long)) %>% 
      mutate(manad_long = factor(manad_long, levels = c("Januari", "Februari", "Mars", "April", "Maj", 
                                                        "Juni", "Juli", "Augusti", "September", "Oktober",
                                                        "November", "December")))
  
  skapa_diagram <- function(df,vald_variabel){
    df <- df %>% 
      filter(variabel == vald_variabel)
    
    diagram_titel <- paste0(unique(df$variabel), " i ", skapa_kortnamn_lan(unique(df$region)))
    if(unique(df$variabel) == "Antal anställda berörda av konkurser"){
      diagramfilnamn <- paste0("anstalla_berorda_konkurser_",skapa_kortnamn_lan(unique(df$region)),".png")
    }else{
      diagramfilnamn <- paste0("anstal_konkurser_",skapa_kortnamn_lan(unique(df$region)),".png")
    }
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = df ,
                                skickad_x_var = "manad_long", 
                                skickad_y_var = "Antal_berorda", 
                                skickad_x_grupp = "år",
                                berakna_index = FALSE,
                                manual_color = diagramfarger("rus_sex"),
                                lagga_till_punkter = TRUE,
                                diagram_titel = diagram_titel,
                                diagram_capt =  diagram_capt,
                                output_mapp = output_mapp,
                                stodlinjer_avrunda_fem = TRUE,
                                manual_y_axis_title = "Antal",
                                #x_axis_visa_var_xe_etikett = 1,
                                filnamn_diagram = diagramfilnamn,
                                skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
    names(gg_list) <- str_remove(diagramfilnamn, ".png")
    return(gg_list)
  }
  
  diag <- map(unique(konkurser_jmfr$variabel), ~skapa_diagram(konkurser_jmfr,.x)) %>% flatten()
  
  
  if(returnera_figur == TRUE){
    return(diag)
  }
  
}

