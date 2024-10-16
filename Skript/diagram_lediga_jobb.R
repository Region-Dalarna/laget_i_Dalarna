if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       glue)

source("C:/Users/frkjon/Projekt/laget_i_Dalarna/Skript/hamta_lediga_jobb_region_sektor_tid_RegionIndE1K_RegionIndE1KN_scb.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")

diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
output_mapp <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"
visa_dataetiketter <- FALSE
gg_list <- list()

lediga_jobb_df <- hamta_lediga_jobb_region_sektor_tid_scb(
  region_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
  sektor_klartext = "*",			 #  Finns: "offentlig sektor", "privat sektor", "totalt", "hela ekonomin", "näringslivet och hushållens icke-vinstdrivande organisationer", "offentlig förvaltning"
  cont_klartext = "*",			 #  Finns: "Lediga jobb", "Felmarginal ±", "Lediga jobb, totalt", "Lediga jobb, totalt, osäkerhetsmarginal", "Lediga jobb med omgående tillträde", "Lediga jobb med omgående tillträde, osäkerhetsmargnial"
  tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006K1", "2006K2", "2006K3", "2006K4", "2007K1", "2007K2", "2007K3", "2007K4", "2008K1", "2008K2", "2008K3", "2008K4", "2009K1", "2009K2", "2009K3", "2009K4", "2010K1", "2010K2", "2010K3", "2010K4", "2011K1", "2011K2", "2011K3", "2011K4", "2012K1", "2012K2", "2012K3", "2012K4", "2013K1", "2013K2", "2013K3", "2013K4", "2014K1", "2014K2", "2014K3", "2014K4", "2015K1", "2015K2", "2015K3", "2015K4", "2016K1", "2016K2", "2016K3", "2016K4", "2017K1", "2017K2", "2017K3", "2017K4", "2018K1", "2018K2", "2018K3", "2018K4", "2019K1", "2019K2", "2019K3", "2019K4", "2020K1", "2020K2", "2020K3", "2020K4", "2021K1", "2021K2", "2021K3", "2021K4", "2022K1", "2022K2", "2022K3", "2022K4", "2023K1", "2023K2", "2023K3", "2023K4", "2024K1", "2024K2"
  long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  excel_filnamn = "lediga_jobb.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  
) %>% 
  filter(variabel%in%c("Lediga jobb", "Lediga jobb, totalt")) %>%
  mutate(sektor = case_when(sektor == "offentlig förvaltning" ~ "offentlig sektor",
                            sektor == "näringslivet och hushållens icke-vinstdrivande organisationer" ~ "privat sektor",
                            sektor == "hela ekonomin" ~ "totalt",
                            TRUE ~ sektor),
         variabel = ifelse(variabel == "Lediga jobb, totalt", "Lediga jobb", variabel),) 


# om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
region_start <- unique(lediga_jobb_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
region_txt <- ar_alla_kommuner_i_ett_lan(unique(lediga_jobb_df$regionkod), returnera_text = TRUE, returtext = region_start)
region_txt <- ar_alla_lan_i_sverige(unique(lediga_jobb_df$regionkod), returnera_text = TRUE, returtext = region_txt)
regionfil_txt <- region_txt
region_txt <- paste0(" i ", region_txt)
regionkod_txt <- if (region_start == region_txt) unique(lediga_jobb_df$regionkod) %>% paste0(collapse = "_") else region_txt

diagramtitel <- glue("Lediga jobb {region_txt}")
diagramfil <- glue("lediga_jobb_{regionfil_txt}_ar{min(lediga_jobb_df$kvartal)}_{max(lediga_jobb_df$kvartal)}.png") %>% str_replace_all("__", "_")

if ("variabel" %in% names(lediga_jobb_df)) {
  if (length(unique(lediga_jobb_df$variabel)) > 6) chart_df <- lediga_jobb_df %>% filter(variabel == unique(lediga_jobb_df$variabel)[1]) else chart_df <- lediga_jobb_df
} else chart_df <- lediga_jobb_df

gg_obj <- SkapaStapelDiagram(skickad_df = chart_df %>% 
                               filter(substr(kvartal,5,6)=="K2",
                                      sektor != "totalt"),
                             skickad_x_var = "kvartal" ,
                             skickad_y_var = if ("varde" %in% names(chart_df)) "varde" else "Lediga jobb",
                             skickad_x_grupp = if ("sektor" %in% names(chart_df) & length(unique(chart_df$sektor)) > 1) "sektor" else NA,
                             x_axis_sort_value = FALSE,
                             diagram_titel = diagramtitel,
                             diagram_capt = diagram_capt,
                             stodlinjer_avrunda_fem = TRUE,
                             filnamn_diagram = diagramfil,
                             dataetiketter = visa_dataetiketter,
                             geom_position_stack = TRUE,
                             manual_y_axis_title = "",
                             manual_x_axis_text_vjust = 1,
                             manual_x_axis_text_hjust = 1,
                             manual_color = if ("sektor" %in% names(chart_df) & length(unique(chart_df$sektor)) > 1) diagramfarger("rus_sex") else diagramfarger("rus_sex")[1],
                             output_mapp = output_mapp,
                             diagram_facet = FALSE,
                             facet_grp = NA,
                             facet_scale = "free",
)

gg_list <- c(gg_list, list(gg_obj))
names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")

