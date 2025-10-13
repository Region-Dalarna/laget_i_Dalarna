# test <- hamta_flytt_regso_region_bakgrund_tid_scb(region_vekt = "*",
#                                                   bakgrund_klartext = c("män","kvinnor"),
#                                                   cont_klartext = "Inrikes flyttnetto, procent")
# 
# source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_ek_prognoser_olika_prognosinstitut_ki.R")
# 
# test <- diag_ekonomiska_prognoser_olika_progn_institut_ki(valda_prognos_ar = "+3",
#                                                           skriv_diagramfil = TRUE,
#                                                           output_mapp = Output_mapp)

if (!require("pacman")) install.packages("pacman")
p_load(here,
       tidyverse)


source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
# Regso/Deso

# Befolkning
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_deso_region_alder_kon_tid_FolkmDesoAldKon_scb.R")
test <- hamta_bef_deso_regso(kon_klartext = "totalt",tid_vekt = "*")

df_out <- test%>%
  group_by(regionkod, region, omrade, kommunkod, kommun, ålder, kön) %>%
  arrange(år, .by_group = TRUE) %>%
  mutate(Antal_1jan = lag(Antal)) %>%
  ungroup()

# Flyttnetto
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_data_flyttnetto_mm_regso.R")
flyttar <- hamta_flytt_regso_region_bakgrund_tid_scb(region_vekt = "20",
                                                  bakgrund_klartext = "män och kvinnor",
                                                  cont_klartext = "Inrikes flyttnetto, procent")

# Kombinerar de båda för att beräkna flyttnetto (antal)
df_bada <- df_out %>% 
  filter(år %in% unique(flyttar$år),
         !(is.na(Antal_1jan))) %>% 
  left_join(flyttar %>% select(-variabel), by = c("regionkod","år","region")) %>% 
    mutate(`Inrikes flyttnetto, antal` = round(`Antal_1jan` * `Inrikes flyttnetto, procent` / 100,0))


# Län
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
bef_lan_kommun <- hamta_bef_folkmangd_alder_kon_ar_scb(kon_klartext = NA)

bef_lan_kommun_lag <- bef_lan_kommun %>%
  group_by(regionkod, region, ) %>%
  arrange(år, .by_group = TRUE) %>%
  mutate(Folkmängd_1jan = lag(Folkmängd)) %>%
  ungroup()

# Flyttnetto
source(here("Skript","hamta_flyttnetto_procent_region_bakgrund_tid_IntGr2Lan_scb.R"), encoding="UTF-8")
flyttar_lan_reg <- hamta_flyttnetto_procent_region_bakgrund_tid_scb(bakgrund_klartext = "män och kvinnor",
                                                                    cont_klartext = "Inrikes flyttnetto, procent")

# Kombinerar de båda för att beräkna flyttnetto (antal)
df_bada_lan <- bef_lan_kommun_lag %>% 
  filter(år %in% unique(flyttar_kom_reg$år),
         !(is.na(Folkmängd_1jan))) %>% 
  left_join(flyttar_lan_reg %>% select(-variabel), by = c("regionkod","år","region")) %>% 
  mutate(`Inrikes flyttnetto, antal` = round(`Folkmängd_1jan` * `Inrikes flyttnetto, procent` / 100,0))

# Kommun
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
bef_lan_kommun <- hamta_bef_folkmangd_alder_kon_ar_scb(hamtakommuner(tamedlan=FALSE,tamedriket=FALSE),kon_klartext = NA)

bef_kommun_lag <- bef_lan_kommun %>%
  group_by(regionkod, region, ) %>%
  arrange(år, .by_group = TRUE) %>%
  mutate(Folkmängd_1jan = lag(Folkmängd)) %>%
  ungroup()

# Flyttnetto
source(here("Skript","hamta_flyttnetto_procent_region_bakgrund_tid_IntGr2Kom_scb.R"), encoding="UTF-8")
flyttar_kom_reg <- hamta_flyttnetto_procent_region_bakgrund_tid_scb(region_vekt = hamtakommuner(tamedlan=FALSE,tamedriket=FALSE),
                                                                    bakgrund_klartext = "män och kvinnor",
                                                                    cont_klartext = "Inrikes flyttnetto, procent")

# Kombinerar de båda för att beräkna flyttnetto (antal)
df_bada_kom <- bef_kommun_lag %>% 
  filter(år %in% unique(flyttar_kom_reg$år),
         !(is.na(Folkmängd_1jan))) %>% 
  left_join(flyttar_kom_reg %>% select(-variabel), by = c("regionkod","år","region")) %>% 
  mutate(`Inrikes flyttnetto, antal` = round(`Folkmängd_1jan` * `Inrikes flyttnetto, procent` / 100,0))



