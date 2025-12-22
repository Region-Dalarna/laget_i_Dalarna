#test <- diagram_konkurser_TVA(spara_figur = FALSE)
diagram_konkurser_TVA <- function(region_vekt = "20",			   # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
                                  variabel_klartext = "*",			 #  Finns: "Antal konkurser", "Antal anställda berörda av konkurser"
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",			# Var skall figuren sparas
                                  spara_figur = TRUE, # Skall diagrammet sparas
                                  returnera_data = FALSE, # Skall data returneras
                                  returnera_figur = TRUE# Skall diagram returneras
                                  ){
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse,
         glue,
         openxlsx,
         stringr)
  
  #source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_TVA.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_konkurser_manad_lan_variabel_konk_1996_tva.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  
  diagram_capt = "Källa: Tillväxtanalys\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()
  
  # Källa: https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/
  # Diagram för konkurser i Dalarnas län. Senaste och näst senaste år samt genomsnitt de fem åren dessförrinnan.
  # SCB har inte längre data för konkurser, så vi använder Tillväxtanalys data istället
  
  konkurser_df <- hamta_konkurser_manad_lan_variabel_tva(
    region_vekt = region_vekt, 
    variabel_klartext = variabel_klartext,
    manad_klartext = "*")
  
  # # Diverse justeringar
  # konkurser_df <- konkurser_df %>% 
  #   rename(tid = månad,
  #          antal = `Konkurser och anställda berörda av konkurs 1996-`) %>% 
  #     separate(region, into = c("regionkod", "region"), sep = " ", remove = TRUE,extra = "merge") %>% 
  #       mutate(år = str_sub(tid, 1, 4),
  #              månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%b"),
  #              manad_long=format(as.Date(paste(år, str_sub(tid, 6,7),"1", sep = "-")), "%B"),
  #              år_månad = paste0(år, " - ", månad),
  #              månad_år = paste0(månad, " ", år)) %>% 
  #         select(-tid)
  
  # Diverse justeringar
  konkurser_df <- konkurser_df %>% 
    rename(tid = månad,
           antal = matches("konkurser", ignore.case = TRUE)) %>% 
    #separate(region, into = c("regionkod", "region"), sep = " ", remove = TRUE,extra = "merge") %>% 
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
    filter(år %in% c(senaste_ar, as.character(as.numeric(senaste_ar) -1),as.character(as.numeric(senaste_ar) -2))) %>% 
      group_by(år, månad,manad_long, regionkod, region,variabel) %>%
        summarize(Antal_berorda = sum(antal)) %>% 
          ungroup()
  
  jmfr_varnamn <- paste0("genomsnitt år ", as.character((senaste_ar_num-7)), "-", as.character((as.numeric(senaste_ar)-3)))
  
  antal_ar <- length(as.character(c(senaste_ar_num-3):(senaste_ar_num -7)))
  # gruppera fem senaste år
  konkurser_fem_senaste <- konkurser_df %>% 
    filter(år %in% as.character(c((senaste_ar_num-3):(senaste_ar_num -7)))) %>% 
      group_by(månad,manad_long, regionkod, region,variabel) %>%
        summarize(antal_tot = sum(antal, na.rm = TRUE),
                  Antal_berorda = antal_tot/antal_ar) %>% 
          ungroup() %>% 
            mutate(år = jmfr_varnamn)
      
  konkurser_jmfr <- konkurser_jmfr %>% 
    bind_rows(konkurser_fem_senaste) %>% 
      mutate(år = factor(år, levels = c(jmfr_varnamn,as.character(senaste_ar_num-2) ,as.character(senaste_ar_num-1), senaste_ar)))
  
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
  
  diag <- map(unique(konkurser_jmfr$variabel), ~skapa_diagram(konkurser_jmfr,.x)) %>% purrr::flatten()
  
  
  if(returnera_figur == TRUE){
    return(diag)
  }
  
}

