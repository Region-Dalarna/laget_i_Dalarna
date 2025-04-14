# Uppdaterar data som används i rapporten "Läget i Dalarna" och kör markdown-filen som skapar rapporten.
# Notera att viss data uppdateras automatiskt, medan annan måste laddas hem från diverse källor (som finns i skripten).
# För mer info om källor för data som behöver hämtas, se avsnittet mot slutet av skriptet.

if (!require("pacman")) install.packages("pacman")
p_load(here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

Output_mapp = here("Figurer","/")
spara_figur = FALSE

######################################################
### Uppdateras automatiskt vid körning av skript   ###
######################################################

# BNP - 1 diagram
source(here("Skript","diagram_BNP_forandring_SCB.R"), encoding="UTF-8")
gg_BNP <- diagram_BNP_SCB(spara_figur = spara_figur, 
                          output_mapp = Output_mapp,
                          returnera_data = TRUE, 
                          returnera_figur = TRUE)

# Konjunkturbarometern - 2 diagram
source(here("Skript","diagram_konjunkturbarometern_konj.R"), encoding="UTF-8")
gg_konjB <- diagram_konjunkturbarometern(spara_figur = spara_figur, 
                                         output_mapp = Output_mapp,
                                         antal_etiketter_barometern = 24, # Intervall mellan visade etiketter (i månader)
                                         antal_etiketter_bransch = 24, # Intervall mellan visade etiketter (i månader)
                                         returnera_data = TRUE, 
                                         returnera_figur = TRUE)

# KPI - 1 diagram
source(here("Skript","diagram_inflation_SCB.R"), encoding="UTF-8")
gg_infl <- diagram_inflation_SCB(spara_figur = spara_figur, 
                                 antal_etiketter = 36,
                                 output_mapp = Output_mapp,
                                 returnera_data = TRUE, 
                                 returnera_figur = TRUE)

# Småhuspriser - 2 diagram
source(here("Skript","diagram_smahuspriser_SCB.R"), encoding="UTF-8")
gg_smahuspriser <- diagram_smahuspriser(spara_figur = spara_figur, 
                                        output_mapp = Output_mapp,
                                        returnera_data = TRUE, 
                                        returnera_figur = TRUE)

# Byggande - 2 figurer
source(here("Skript","diagram_nybygg_bygglov_SCB.R"), encoding="UTF-8")
gg_nybygg_bygglov <- diagram_nybyggnation_bygglov(spara_figur = spara_figur, 
                                                  output_mapp = Output_mapp,
                                                  returnera_data = TRUE, 
                                                  returnera_figur = TRUE)

# # Konkurser - 2 figurer - SCB har inte längre data för konkurser, så vi använder Tillväxtanalys data istället (se längre ned)
# source(here("Skript","diagram_konkurser_SCB.R"), encoding="UTF-8")
# gg_konkurser <- diagram_konkurser_SCB(spara_figur = spara_figur, 
#                                       output_mapp = Output_mapp,
#                                       returnera_data = TRUE, 
#                                       returnera_figur = TRUE)

# Konkurser - 1 figur - enbart län
source(here("Skript","diagram_konkurser_tillvaxtanalys_korrekt.R"), encoding="UTF-8")
gg_konkurser <- diagram_konkurser_TVA(spara_figur = spara_figur,
                                      variabel_klartext = "Antal anställda berörda av konkurser",
                                      output_mapp = Output_mapp,
                                      returnera_data = TRUE,
                                      returnera_figur = TRUE)

# Konkurser - 1 figur - tre största branscher
source(here("Skript","diagram_konkurser_tillvaxtanalys_bransch.R"), encoding="UTF-8")
gg_konkurser_bransch <- diagram_konkurser_bransch_TVA(antal_branscher = 3,
                                                      output_mapp = Output_mapp,
                                                      spara_figur = spara_figur,
                                                      returnera_dataframe_global_environment = TRUE)

konk_bransch_ar <- unique(konkurser_bransch_df$ar)
konk_bransch_manad <- unique(konkurser_bransch_df$manad_namn)
storsta_bransch <- konkurser_bransch_df %>% 
  filter(antal == max(antal)) %>% 
  .$bransch
storsta_bransch_antal <- konkurser_bransch_df %>% 
  filter(antal == max(antal)) %>% 
  .$antal
nast_storsta_bransch <- konkurser_bransch_df %>% 
  filter(antal == sort(antal,decreasing = TRUE)[2]) %>% 
  .$bransch
nast_storsta_bransch_antal <- konkurser_bransch_df %>%
  filter(antal == sort(antal,decreasing = TRUE)[2]) %>% 
  .$antal

# Nystartade företag - 1 figur
source(here("Skript","diagram_nystartade_ftg_tillvaxtanalys_korrekt.R"), encoding="UTF-8")
gg_nystartade <- diagram_nystartade(spara_figur = spara_figur, 
                                    output_mapp = Output_mapp,
                                    returnera_data = TRUE, 
                                    returnera_figur = TRUE)


# Arbetslöshet län - 1 figur
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_senastear.R")
gg_arbetsloshet_lan <- diagram_arbetsmarknadsstatus(region_vekt = hamtaAllaLan(),
                                                    spara_figur = spara_figur, 
                                                    output_mapp_figur = Output_mapp,
                                                    diag_arbetskraftsdeltagande = FALSE, 
                                                    diag_sysselsattningsgrad = FALSE, 
                                                    returnera_data = TRUE, 
                                                    returnera_figur = TRUE)

# Arbetslöshet tidsserie - 1 figur
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_tidsserie_SCB.R")
gg_arbetsloshet_tidsserie <- diagram_arbetsmarknadsstatus_tidsserie (spara_figur = spara_figur, 
                                                                     output_mapp_figur = Output_mapp,
                                                                     returnera_data = TRUE,
                                                                     marginal_yaxis_facet = c(0.02,0.02),
                                                                     diagram_facet = TRUE,
                                                                     returnera_figur = TRUE)

# Hämta antal arbetslösa (till texten enbart)
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
arbstatus_df <- hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(region_vekt = "20",
                                                                                  alder_klartext = unique(arbetsmarknadsstatus_tidsserie$ålder),
                                                                                  kon_klartext = "totalt",
                                                                                  fodelseregion_klartext = unique(arbetsmarknadsstatus_tidsserie$födelseregion),
                                                                                  cont_klartext = "antal arbetslösa",
                                                                                  wide_om_en_contvar = FALSE,
                                                                                  tid_koder = "9999") %>% 
  manader_bearbeta_scbtabeller()

arblosa_utrikes <- format(arbstatus_df %>% filter(födelseregion == "utrikes född") %>% .$varde,big.mark = " ")
arblosa_inrikes <- format(arbstatus_df %>% filter(födelseregion == "inrikes född") %>% .$varde,big.mark = " ")
arblosa_manad <- unique(arbstatus_df$månad)
arblosa_ar <- unique(arbstatus_df$år)
arblosa_alder <- unique(arbstatus_df$ålder)

# Arbetslöshet kommun - Karta, ej diagramskript (ännu)
source(here("Skript","arbetsloshet_kommun.R"), encoding="UTF-8")
hamta_data_arbetsloshet(vald_region="20",
                        spara_data=TRUE,
                        output_mapp_excel = here("Data"))

# Avregistrerade företag - 1 figur
source(here("Skript","diagram_avreg_ftg_Bolagsverket.R"), encoding="UTF-8")
gg_avregistrerade <- diagram_avregistrerade(spara_figur = spara_figur, 
                                            output_mapp = Output_mapp,
                                            returnera_data = TRUE, 
                                            returnera_figur = TRUE)

# Ekonomiskt bistånd SCB - 1 figur
source(here("Skript","diagram_ek_bistand_SCB.R"), encoding="UTF-8")
gg_ek_bistand_SCB = diagram_ek_bistand_SCB(spara_figur = spara_figur, 
                                           output_mapp = Output_mapp,
                                           returnera_data = TRUE, 
                                           returnera_figur = TRUE)

# Ekonomiskt bistånd SCB bakgrund - 1 figur
source(here("Skript","diagram_ek_bistand_SCB_bakgrund.R"), encoding="UTF-8")
gg_ek_bistand_bakgrund_SCB = diagram_ek_bistand_bakgrund_SCB(spara_figur = spara_figur, 
                                                             output_mapp = Output_mapp,
                                                             returnera_data = TRUE, 
                                                             returnera_figur = TRUE)

###########################################
### Kräver manuell nedladdning av data #### - Tidigare datahämtning, ej relevant längre
###########################################

## OBS! Se respektive skript för mer information om hur data hämtas OBS!


# Ekonomiskt bistånd - 1 figur
# source(here("Skript","diagram_ek_bistand_Socialstyrelsen.R"), encoding="UTF-8")
# gg_ek_bistand <- diagram_ek_bistand(spara_figur = spara_figur, 
#                                     output_mapp = Output_mapp,
#                                     returnera_data = TRUE, 
#                                     returnera_figur = TRUE)

###########################################
###   "Knittar" Rmarkdown-filen        ####
###########################################


# rmarkdown::render(
#   input = 'laget_i_Dalarna.Rmd',
#   output_file = 'laget_i_Dalarna.html',
#   envir = parent.frame()
# )

