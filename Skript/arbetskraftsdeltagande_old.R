if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       glue)

source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_arbetskraftsdeltagande_region_utbildngrupp_kon_tid_RegionInd19U1b_19U1bN1_scb.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")

diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel av befolkningen 20-64 år som antingen är förvärvsarbetande eller inskrivna på arbetsförmedlingen "
output_mapp <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"
visa_dataetiketter <- FALSE
gg_list <- list()

arbetskraftsdeltagande_df <- hamta_arbetskraftsdeltagande_region_utbildngrupp_kon_tid_scb(
  region_vekt = "20",			# Val av region. Finns: "00", "FA00", "FA01", "FA02", "FA03", "FA04", "FA05", "FA06", "FA07", "FA08", "FA09", "FA10", "FA11", "FA12", "FA13", "FA14", "FA15", "FA16", "FA17", "FA18", "FA19", "FA20", "FA21", "FA22", "FA23", "FA24", "FA25", "FA26", "FA27", "FA28", "FA29", "FA30", "FA31", "FA32", "FA33", "FA34", "FA35", "FA36", "FA37", "FA38", "FA39", "FA40", "FA41", "FA42", "FA43", "FA44", "FA45", "FA46", "FA47", "FA48", "FA49", "FA50", "FA51", "FA52", "FA53", "FA54", "FA55", "FA56", "FA57", "FA58", "FA59", "FA60", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
  utbildngrupp_klartext = NA,			 #  NA = tas inte med i uttaget,  Finns: "samtliga utbildningsnivåer", "samtliga utbildningsinriktningar", "allmän utbildning", "samtliga utbildningsgrupper", "folkskoleutbildning och motsvarande utbildning", "grundskoleutbildning och motsvarande utbildning", "samhällsvetenskaplig el. humanistisk utbildning, gymnasial", "naturvetenskaplig utbildning, gymnasial nivå ", "teknisk utbildning, gymnasial nivå", "pedagogisk utbildning, gymnasial nivå ", "minst 30 hp inom pedagogik och lärarutbildning, ej examen ", "förskollärarutbildning ", "fritidspedagogutbildning ", "lärarutbildning för grundskolans tidigare år ", "lärarutbildning grsk senare år och gymn., allmänna + praktiskt-estet.", "speciallärar- och specialpedagogutbildning ", "yrkeslärarutbildning ", "övrig utbildning inom pedagogik / lärarutbildning, eftergymnasial", "estetisk utbildning, gymnasial nivå ", "utbildning inom humaniora och konst, gymnasial nivå ", "minst 30 hp inom humaniora och konst, ej examen ", "humanistisk utbildning, eftergymnasial nivå (minst 3 år) ", "konstnärlig utbildning, eftergymnasial nivå ", "utbildning inom medieproduktion, eftergymnasial nivå ", "teologisk utbildning, eftergymnasial nivå (minst 3 år) ", "övrig utbildning inom humaniora och konst, eftergymnasial nivå ", "ekonomisk utbildning, gymnasial nivå ", "handel- och administrationsutbildning, gymnasial nivå ", "minst 30 hp i samhällsvetenskap, juridik, handel, admin., ej examen ", "biblioteks- och informationsvetensk. högskoleutbildning (minst 3 år) ", "ekonomutbildning, högskoleutbildning (minst 3 år) ", "personal- och beteendevetarutbildning, högskoleutb. (minst 3 år) ", "juristutbildning ", "journalistik och medievetenskaplig utbildning, eftergymnasial nivå ", "psykologutbildning ", "samhällsvetar- och förvaltningsutb. högskoleutbildning (minst 3 år) ", "övrig utb. i samhällsvetenskap, juridik, handel, admin., eftergymnasial", "yrkesinriktad utb. inom naturvetenskap, matematik, data, gymnasial nivå ", "minst 30 hp inom naturvetenskap, matematik, data, ej examen ", "biologutbildning, högskoleutbildning (minst 3 år) ", "datautbildning, eftergymnasial nivå ", "fysikerutbildning, högskoleutbildning (minst 3 år) ", "geovetenskaplig utbildning, högskoleutbildning (minst 3 år) ", "kemistutbildning, högskoleutbildning (minst 3 år) ", "matematiker-, statistiker-, datavetenskaplig högskoleutb. (minst 3 år) ", "övrig naturvetenskaplig högskoleutbildning (minst 3 år) ", "övrig utbildning inom naturvetenskap, matematik, data, eftergymnasial", "gymnasieingenjörsutbildning", "byggutbildning, gymnasial nivå ", "data-, el- och energiteknisk utbildning, gymnasial nivå ", "fordonsutbildning, gymnasial nivå ", "industriutbildning, gymnasial nivå ", "vvs- och fastighetsutbildning, gymnasial nivå ", "övrig utbildning inom teknik och tillverkning, gymnasial nivå ", "minst 180 högskolepoäng inom teknik och tillverkning, ej examen", "30-179 högskolepoäng inom teknik och tillverkning, ej examen", "arkitektutbildning ", "civilingenjörsutbildning; industriell ekonomi", "civilingenjörsutbildning; väg- och vatten, byggnadsteknik, lantmäteri", "civilingenjörsutbildning; maskinteknik, fordons- och farkostteknik", "civilingenjörsutbildning; teknisk fysik, elektro- och datateknik", "civilingenjörsutbildning; kemi- och bioteknik, material- och geoteknik", "civilingenjörsutbildning; övrig/okänd inriktning", "högskoleingenjörsutb.; väg- och vatten, byggnadsteknik, lantmäteri", "högsk.ing.utb; maskinteknik, fordons- farkostteknik, industriell ekon.", "högskoleingenjörsutbildning; teknisk fysik, elektro- och datateknik", "högskoleingenjörsutb.; kemi- och bioteknik, material- och geoteknik", "högskoleingenjörsutbildning; övrig/okänd inriktning", "teknikutbildning, yrkeshögskolan", "övrig utbildning inom teknik och tillverkning, eftergymnasial nivå ", "naturbruksutbildning, gymnasial nivå ", "minst 30 hp inom lant- och skogsbruk, djursjukvård, ej examen ", "agronom- och hortonomutbildning ", "skogsvetenskaplig utbildning, högskoleutbildning (minst 3 år) ", "veterinärutbildning ", "övrig utb. inom lant- och skogsbruk, djursjukvård, eftergymnasial", "barn- och fritidsutbildning, gymnasial nivå ", "vård- och omsorgsutb.; övrig gymn. utb. i hälso- och sjukvård", "tandsköterskeutbildning ", "minst 30 hp inom hälso- och sjukvård, social omsorg, ej examen ", "apotekarutbildning ", "arbetsterapeututbildning ", "biomedicinsk analytikerutbildning ", "fritidsledarutbildning, eftergymnasial nivå ", "läkarutbildning (exkl. disputerade som saknar läkarexamen) ", "receptarieutbildning ", "sjukgymnast-/fysioterapeututbildning ", "barnmorskeutbildning ", "sjuksköterskeutbildning, grundutbildning ", "social omsorgsutbildning, eftergymnasial nivå ", "socionomutbildning ", "specialistsjuksköterskeutbildning", "tandhygienistutbildning ", "tandläkarutbildning ", "övrig utb. inom hälso- och sjukvård, social omsorg, eftergymnasial", "restaurang- och livsmedelsutbildning, gymnasial nivå ", "transportutbildning, gymnasial nivå ", "övrig utbildning inom tjänsteområdet, gymnasial nivå ", "minst 30 hp inom tjänsteområdet, ej examen ", "polisutbildning ", "transportutbildning, eftergymnasial nivå ", "övrig utbildning inom tjänsteområdet, eftergymnasial nivå ", "gymnasial utbildning, ospecificerad ", "eftergymnasial utbildning, ospecificerad ", "okänd utbildning ", "pedagogik och lärarutbildning", "förgymnasial utbildning", "humaniora och konst", "gymnasial utbildning", "samhällsvetenskap, juridik, handel, administration", "eftergymnasial utbildning, mindre än 3 år", "naturvetenskap, matematik och data", "teknik och tillverkning", "eftergymnasial utbildning, 3 år eller mer", "lant- och skogsbruk samt djursjukvård", "okänd utbildningsnivå", "hälso- och sjukvård samt social omsorg", "tjänster", "okänd utbildningsinriktning", "medicinsk sekreterarutbildning", "YH-utbildning i företagsekonomi, handel, administration", "data och IT-utbildning, eftergymnasial (minst 3 år)", "data och IT utbildning, eftergymnasial (kortare än 3år)", "fysik- och  matematikutbildning, eftergymnasial nivå (minst 3år)", "läkarutbildning, med specialistkompetens", "röntgensjuksköterskeutbildning", "specialistsjuksköterskeutbildning; anestesi-, intensiv-, operations- och ambulanssjukvård", "specialistsjuksköterskeutbildning; barn och ungdom", "specialistsjuksköterskeutbildning; distriktssköterska", "specialistsjuksköterskeutbildning; psykiatrisk vård", "specialistsjuksköterskeutbildning; övriga inriktningar", "hotell- och turismutbildning, gymnasial"
  kon_klartext = c("män", "kvinnor"),			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
  cont_klartext = "*",			 #  Finns: "I arbetskraften", "Inte i arbetskraften", "Totalt antal personer"
  tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
  long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  excel_filnamn = "arbetskraftsdeltagande.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  
)
# Calculate the percentage of the population in the labor force using pivots
arbetskraftsdeltagande_andel <- arbetskraftsdeltagande_df %>% pivot_wider(names_from = variabel, values_from = varde) %>% mutate(arbetskraftsdeltagande = `I arbetskraften` / `Totalt antal personer` * 100) %>% select(-`I arbetskraften`, -`Inte i arbetskraften`, -`Totalt antal personer`) %>% pivot_longer(cols = starts_with("arbetskraftsdeltagande"), names_to = "variabel", values_to = "andel")

# om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
region_start <- unique(arbetskraftsdeltagande_andel$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
region_txt <- ar_alla_kommuner_i_ett_lan(unique(arbetskraftsdeltagande_df$regionkod), returnera_text = TRUE, returtext = region_start)
region_txt <- ar_alla_lan_i_sverige(unique(arbetskraftsdeltagande_df$regionkod), returnera_text = TRUE, returtext = region_txt)
regionfil_txt <- region_txt
region_txt <- paste0(" i ", region_txt)
regionkod_txt <- if (region_start == region_txt) unique(arbetskraftsdeltagande_df$regionkod) %>% paste0(collapse = "_") else region_txt

diagramtitel <- glue("Arbetskraftsdeltagande (20-64 år) i {region_txt}")
diagramfil <- glue("arbetskraftsdeltagande_{regionfil_txt}.png") %>% str_replace_all("__", "_")

# if ("variabel" %in% names(arbetskraftsdeltagande_df)) {
#    if (length(unique(arbetskraftsdeltagande_df$variabel)) > 6) chart_df <- arbetskraftsdeltagande_df %>% filter(variabel == unique(arbetskraftsdeltagande_df$variabel)[1]) else chart_df <- arbetskraftsdeltagande_df
# } else chart_df <- arbetskraftsdeltagande_df

gg_obj <- SkapaStapelDiagram(skickad_df = arbetskraftsdeltagande_andel,
                             skickad_x_var = "år",
                             skickad_y_var = "andel",
                             skickad_x_grupp = if ("kön" %in% names(arbetskraftsdeltagande_andel) & length(unique(arbetskraftsdeltagande_andel$kön)) > 1) "kön" else NA,
                             x_axis_sort_value = FALSE,
                             diagram_titel = diagramtitel,
                             skriv_till_diagramfil = FALSE,
                             diagram_capt = diagram_capt,
                             stodlinjer_avrunda_fem = TRUE,
                             filnamn_diagram = diagramfil,
                             manual_y_axis_title = "procent",
                             manual_x_axis_text_vjust = 1,
                             manual_x_axis_text_hjust = 1,
                             manual_color = if ("kön" %in% names(arbetskraftsdeltagande_andel) & length(unique(arbetskraftsdeltagande_andel$kön)) > 1) diagramfarger("kon") else diagramfarger("rus_sex")[1],
                             output_mapp = "output_mapp",
                             diagram_facet = FALSE,
                             facet_grp = NA,
                             facet_scale = "free",
)

gg_list <- c(gg_list, list(gg_obj))
names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")

