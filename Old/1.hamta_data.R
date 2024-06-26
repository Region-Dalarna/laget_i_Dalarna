# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

Output_mapp = here("Data","/")

# Varsel på månadsbasis
# source("G:/skript/hamta_data/func_hamta_statistik_AF_varsel_manad.R", encoding="UTF-8")
# varsel_manad <- hamta_varsel_per_manad(vald_region = "20",
#                                        filnamn="varsel_manad.xlsx",
#                                        output_mapp = Output_mapp,
#                                        start_tid="1992-01",
#                                        spara_till_excel=TRUE) 

# # Varsel på årsbasis uppdelat på bransch
# source("G:/skript/hamta_data/func_hamta_statistik_AF_varsel_bransch_ar.R", encoding="UTF-8")
# varsel_ar_bransch <- hamta_varsel_bransch_ar(vald_region = "20",
#                                            filnamn="varsel_bransch.xlsx",
#                                            output_mapp = Output_mapp,
#                                            spara_till_excel=TRUE) 

# Varsel på månadsbasis uppdelat på bransch - ANVÄNDS INTE LÄNGRE. BRISTANDE KVALITET
# source(here("Skript","varsel_bransch_manad_korrekt.R"), encoding="UTF-8")
# hamta_varsel_bransch_manad(vald_region = "20",
#                            filnamn="varsel_bransch_manad.xlsx",
#                            output_mapp = Output_mapp)

# Konjunkturbarometern - KVAR
source(here("Skript","barometerindikatorn.R"), encoding="UTF-8")
hamta_data_konjunkturbarometern(output_mapp_excel = here("Data"),
                                output_mapp_figur = here("Output","/"),
                                skapa_figur=FALSE,
                                spara_data=TRUE)

# BNP - KLAR
source(here("Skript","BNP.R"), encoding="UTF-8")
hamta_data_BNP(spara_data=TRUE,
               output_mapp_excel = here("Data"))

# KPI - KLAR
source(here("Skript","KPI.R"), encoding="UTF-8")
hamta_data_kpi(spara_data=TRUE,
               output_mapp_excel = here("Data"))

# Småhuspriser - KLAR
source(here("Skript","smahuspriser.R"), encoding="UTF-8")
hamta_data_smahuspriser(output_mapp_excel = here("Data"),
                        output_mapp_figur = here("Output","/"),
                        skapa_figur=FALSE,
                        spara_data=TRUE)
# Konkurser - KLAR
source(here("Skript","konkurser.R"), encoding="UTF-8")
hamta_data_konkurser(vald_region="20",
                     spara_data=TRUE,
                     output_mapp_excel = here("Data"))

# # Arbetslöshet
# source("G:/skript/diagram/diag_bas_syss_arblosa_inr_utr_fodda_manad.R", encoding="UTF-8")
# diag_bas_arblosa_inr_utr_fodda_manad(vald_region="20",
#                                      skriv_excelfil=TRUE,
#                                      skriv_diagram = FALSE,
#                                      output_mapp = here("Data","/"))

# # Arbetslöshet län
# source(here("Skript","arbetsmarknadsstatus_lan.R"), encoding="UTF-8")
# diag_arbetsmarknadsstatus(output_mapp = here("Data"),
#                           skapa_fil = TRUE,
#                           diag_arbetslosthet = TRUE,
#                           diag_arbetskraftsdeltagande = FALSE,
#                           diag_sysselsattningsgrad = FALSE)

# Arbetslöshet tidsserie - Kvar
source(here("Skript","arbetsmarknadsstatus_tidsserie.R"), encoding="UTF-8")
diag_arbetsmarknadsstatus(region_vekt="20",
                          output_mapp = here("Data"),
                          skapa_fil = TRUE,
                          diag_arbetslosthet = TRUE,
                          diag_arbetskraftsdeltagande = FALSE,
                          diag_sysselsattningsgrad = FALSE)

# Arbetslöshet kommun - Kvar
source(here("Skript","arbetsloshet_kommun.R"), encoding="UTF-8")
hamta_data_arbetsloshet(vald_region="20",
                        spara_data=TRUE,
                        output_mapp_excel = here("Data"))



# Byggande - KLAR
source(here("Skript","Byggande.R"), encoding="UTF-8")
hamta_data_nybyggnation(vald_region="20",
                        spara_data=TRUE,
                        output_mapp_excel = here("Data"))

