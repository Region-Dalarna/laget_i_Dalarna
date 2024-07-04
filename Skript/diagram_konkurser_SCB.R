############### OBS Funkar inte efter längre (SCB publicerar inte uppdaterad data för konkurser) ################
#test = diagram_konkurser_SCB(spara_figur = FALSE)
diagram_konkurser_SCB <- function(region_vekt = "20",
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  foretagsform = "*",			 #  NA = tas inte med i uttaget,  Finns: "aktiebolag", "enskild firma", "handelsbolag m.m."
                                  tid = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2009M01" : "2024M03"
                                  spara_figur = TRUE, # Skall diagrammet sparas
                                  returnera_data = FALSE, # Skall data returneras
                                  returnera_figur = TRUE, # Skall diagram returneras
                                  valda_farger = diagramfarger("rus_sex"),
                                  valda_farger_lan = diagramfarger("rus_tre_fokus"),
                                  diag_tidsserie = TRUE,
                                  diag_bransch = TRUE){# Startvärde för diagrammet.Finns från 1980
  
  


  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta__region_sni2007_foretagsform_tid_KonkurserAnst07_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  objektnamn <- c()
  gg_list <- list()
  
  konkurser_df <- hamta__region_sni2007_foretagsform_tid_scb(
    region_vekt = region_vekt,			# Val av region.
    sni2007_klartext = "*",			 
    foretagsform_klartext = foretagsform,			 #  NA = tas inte med i uttaget,  Finns: "aktiebolag", "enskild firma", "handelsbolag m.m."
    cont_klartext = "*",			 #  Finns: "Anställda drabbade av konkurser"
    tid_koder = "*",			 # 
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = ".xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
    
  ) %>% 
    mutate(`näringsgren SNI 2007` = case_when(
      `näringsgren SNI 2007` == "el-, gas- och värmeverk; vatten- och reningsverk; anläggningar för avfallshantering, återvinning och sanering" ~ "el-, gas- och värmeverk m.m.",
      `näringsgren SNI 2007` == "uthyrningsfirmor, arbetsförmedlingar, rekryteringsföretag, personaluthyrningsföretag o.d." ~ "uthyrningsfirmor, arbetsförmedlingar m.m.",
      `näringsgren SNI 2007` == "andra serviceföretag och företag för personliga tjänster, civila myndigheter och försvaret" ~ "andra serviceföretag m.m.",
      `näringsgren SNI 2007` == "övrig tillverkningsindustri, reparationsverkstäder och installationsföretag" ~ "övrig tillverkningsindustri m.m.",
      .default = `näringsgren SNI 2007`
    ))  %>% 
    rename(tid = månad,
           "varde" = `Anställda drabbade av konkurser`) %>% 
    mutate(år = str_sub(tid, 1, 4),
           månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
           manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B"),
           år_månad = paste0(år, " - ", månad),
           månad_år = paste0(månad, " ", år))
  
  konkurser_df <- konkurser_df %>%
    arrange(regionkod) %>%
    group_by(regionkod) %>%
    mutate(sort = row_number())
  
  konkurser_df <- konkurser_df %>%
    mutate(månad_år_sort = factor(månad_år))
  
  konkurser_df$månad_år_sort <- reorder(konkurser_df$månad_år_sort, konkurser_df$sort)

  if(diag_tidsserie == TRUE){
    
    if(returnera_data == TRUE){
      assign("konkurser_df", konkurser_df, envir = .GlobalEnv)
    }
    
    diagram_capt = "Källa: Näringsverksamhet, Konkurser och ackord, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    
    senaste_ar <- max(konkurser_df$år)
    senaste_manad <- last(konkurser_df$manad_long)
    senaste_ar_num <- senaste_ar %>% as.numeric()
    
    # gruppera per månad för senaste året och för året innan det
    konkurser_jmfr <- konkurser_df %>% 
      filter(år %in% c(senaste_ar, as.character(as.numeric(senaste_ar) -1))) %>% 
      group_by(år, månad,manad_long, regionkod, region) %>%
      summarize(Antal_berorda = sum(varde)) %>% 
      ungroup()

    jmfr_varnamn <- paste0("genomsnitt år ", as.character((senaste_ar_num-6)), "-", as.character((as.numeric(senaste_ar)-2)))
    
    antal_ar <- length(as.character(c(senaste_ar_num-2):(senaste_ar_num -6)))
    # gruppera fem senaste år
    konkurser_fem_senaste <- konkurser_df %>% 
      filter(år %in% as.character(c((senaste_ar_num-2):(senaste_ar_num -6)))) %>% 
      group_by(månad,manad_long, regionkod, region) %>%
      summarize(Berorda_tot = sum(varde, na.rm = TRUE),
                Antal_berorda = Berorda_tot/antal_ar) %>% 
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
    
  
    diagram_titel <- str_wrap("Antal personer berörda av konkurser i Dalarnas län")
    diagramfilnamn <- paste0("konkurser_lan.png")
    objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = konkurser_jmfr ,
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
                                       skriv_till_diagramfil =spara_figur)
  
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if(diag_bransch == TRUE){
    
    senaste_ar <- last(unique(konkurser_df$månad_år))
    # ett_ar_sedan <- nth(unique(konkurser_df$månad_år),length(unique(konkurser_df$månad_år))-12)
    
    # Väljer ut de branscher som har fler än noll antal påverkade av konkurs. Mutate ändrar till stor bokstav.
    konkurser_bransch_10 <- konkurser_df %>%
      rename("bransch"= `näringsgren SNI 2007`) %>%
      filter(månad_år%in%senaste_ar) %>%
      slice_max(varde,n=10) %>%
      filter(varde>0) %>% 
      mutate(bransch = paste0(toupper(substr(bransch, 1, 1)), substr(bransch, 2, nchar(bransch))))
    
    if(returnera_data == TRUE){
      assign("konkurser_branscher_df", konkurser_bransch_10, envir = .GlobalEnv)
    }
    
    konkurser_bransch_10$bransch <- str_wrap(konkurser_bransch_10$bransch,40)
    
    diagram_titel <- paste0("Antal personer berörda av konkurser i ",unique(konkurser_bransch_10$region)," ",unique(konkurser_bransch_10$månad_år))
    diagramfilnamn <- paste0("konkurser_bransch.png")
    objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = konkurser_bransch_10 ,
                                            skickad_x_var = "bransch",
                                            skickad_y_var = "varde",
                                            diagram_titel = diagram_titel,
                                            diagram_capt = diagram_capt,
                                            manual_x_axis_text_vjust = 1,
                                            manual_x_axis_text_hjust = 1,
                                            #diagram_facet = jmfr_riket,
                                            manual_y_axis_title = "Antal berörda av konkurser",
                                            facet_grp = "region",
                                            facet_legend_bottom = TRUE,
                                            manual_color = diagramfarger("rus_sex")[1],
                                            stodlinjer_avrunda_fem = TRUE,
                                            x_axis_lutning = 0,
                                            output_mapp = output_mapp,
                                            x_axis_sort_value = TRUE,
                                            diagram_liggande = TRUE,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_figur)

  gg_list <- c(gg_list, list(gg_obj))
  }
  names(gg_list) <- objektnamn
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }

}

