# Skript som hämtar data till "Fakta om Dalarna"
# En del saker behöver fortfarande uppdateras manuellt, exempelvis länkar och text under fliken demografi (rörande hur mycket befolkningen har minskat)

# Kontrollerar att paket som behövs finns installerade (funktioner anropas med namespace, paket::funktion)
paket <- c("dplyr", "stringr", "purrr", "tibble", "readxl", "writexl", "here", "rKolada", "remotes")
saknas <- paket[!paket %in% rownames(installed.packages())]
if (length(saknas) > 0) install.packages(saknas)

if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")

if (!requireNamespace("rdverktyg", quietly = TRUE)) {
  remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
}

# Output-mapp
output_mapp <- here::here("Output")
if (!dir.exists(output_mapp)) dir.create(output_mapp, recursive = TRUE)

# Tom lista som fylls på flik för flik
fakta_lista <- list()

# Hjälpfunktion: gör om namngivna värden till en tabell med variabel och varde
skapa_flik <- function(...) {
  v <- list(...)
  tibble::tibble(
    variabel = names(v),
    varde = purrr::map_chr(v, ~ paste(as.character(.x), collapse = ", "))
  )
}

# Flik 1: Invånarantal i Dalarna
# Base data
beffor_df_historik <- pxweb2r::pxweb2_get_data(
  table = "TAB6471",
  query = list(
    Region = "20",
    Kon = "totalt, samtliga män och kvinnor",
    ContentsCode = "Folkmängden per månad",
    ålder = "totalt, samtliga åldrar",
    Tid = "9999"
  ))

# Add to list
befolkning_datum <- beffor_df_historik |> dplyr::pull(månad)
befolkning <- beffor_df_historik |> dplyr::pull(value)

fakta_lista[["Invånarantal"]] <- skapa_flik(
  befolkning_datum = befolkning_datum,
  befolkning = befolkning
)

# Fliken Dalarnas sex största arbetsgivare (och några slides till)

input_mapp <- "G:/Samhällsanalys/Statistik/Företagsstatistik/Största arbetsgivare/www_foretagsklimat_se/mail_fran_foretagsklimat/"

# Största privata arbetsgivare
files <- list.files(input_mapp, pattern = "samtliga", full.names = TRUE)

file_info <- file.info(files)
latest_file <- rownames(file_info)[which.max(file_info$mtime)]

år_extracted <- stringr::str_extract(latest_file, "\\d{4}")

flikar <- c("Län", "Kommun")

data_list <- list()

for (sheet in flikar) {
  df <- readxl::read_xlsx(latest_file, sheet = sheet, skip = 3) |>
    dplyr::select(1:3, 5) |>
    dplyr::rename(antal_anstallda = RedigeratAntalAnställda)
  
  names(df)[1] <- "regionkod"
  names(df)[2] <- "region_namn"
  
  # Delar upp i offentlig och privat verksamhet
  df <- df |>
    dplyr::mutate(
      sektor = dplyr::if_else(
        stringr::str_detect(FöretagsNamn, stringr::regex("region|kommun|församling|verket", ignore_case = TRUE)),
        "Offentlig",
        "Privat"
      ),
      år = år_extracted
    )
  
  # Fixar stora bokstäver för kommun och region
  df <- df |>
    dplyr::mutate(
      FöretagsNamn = dplyr::case_when(
        stringr::str_detect(FöretagsNamn, "^REGION ") ~ stringr::str_to_title(FöretagsNamn),
        stringr::str_detect(FöretagsNamn, " KOMMUN$") ~ stringr::str_to_title(FöretagsNamn),
        TRUE ~ FöretagsNamn
      )
    )
  
  data_list[[sheet]] <- df
}

arbgiv_alla <- dplyr::bind_rows(data_list, .id = "niva")
rm(df, data_list)

# The 6 biggest in dalarna - Titta direkt i filen istället. Är den som heter privata (inte samtliga)


# Step 1: get the single biggest employer per kommun, per sector
arbgiv_max <- arbgiv_alla |>
  dplyr::group_by(region_namn, sektor) |>
  dplyr::slice_max(antal_anstallda, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

# Step 2: split into private and public
privat <- arbgiv_max |>
  dplyr::filter(sektor == "Privat") |>
  dplyr::select(regionkod,
                Kommun = region_namn,
                `Arbetsgivare (privat)` = FöretagsNamn,
                `Antal anställda (privat)` = antal_anstallda,
                år)

offentlig <- arbgiv_max |>
  dplyr::filter(sektor == "Offentlig") |>
  dplyr::select(regionkod,
                Kommun = region_namn,
                `Arbetsgivare (offentlig)` = FöretagsNamn,
                `Antal anställda (offentlig)` = antal_anstallda)

# Step 3: join into the final table-ready df
arbgiv <- dplyr::left_join(privat, offentlig, by = c("regionkod", "Kommun")) |>
  dplyr::relocate(år, .before = Kommun) |>
    dplyr::filter(substr(regionkod, 1, 2) == "20") |> 
      dplyr::select(-regionkod)

arbgiv_rader <- arbgiv |>
  dplyr::mutate(
    offentlig_text = dplyr::if_else(
      stringr::str_detect(`Arbetsgivare (offentlig)`, "Kommun"),
      `Arbetsgivare (offentlig)`,
      paste0(Kommun, ": ", `Arbetsgivare (offentlig)`)
    ),
    varde = paste0(offentlig_text, " ", `Antal anställda (offentlig)`,
                   " (", `Arbetsgivare (privat)`, " ", `Antal anställda (privat)`, ")")
  ) |>
  dplyr::select(variabel = Kommun, varde)

fakta_lista[["Största arbetsgivare"]] <- dplyr::bind_rows(
  tibble::tibble(variabel = "år", varde = as.character(unique(arbgiv$år))),
  arbgiv_rader
)

# Flik 2 Export
# Base data
export <- rdpostgres::oppnadata_hamta("mikro_db", tabell = "export_tidsserie") |>
  dplyr::filter(ar == max(ar), region != "Riket")

# Add to list
export_varde_ar <- max(export$ar)

export_varde_Dalarna <- export |>
  dplyr::filter(region == "Dalarnas län") |>
  dplyr::mutate(exportvarde = exportvarde / 10^9) |>
  dplyr::pull(exportvarde)

export_placering <- export |>
  dplyr::mutate(placering = dplyr::min_rank(dplyr::desc(exportvarde))) |>
  dplyr::filter(region == "Dalarnas län") |>
  dplyr::pull(placering)

fakta_lista[["Export"]] <- skapa_flik(
  export_ar = export_varde_ar,
  export_varde_mdkr = round(export_varde_Dalarna, 1),
  export_placering = export_placering
)

# Flik 3: Utländskt ägande

# Base data
branschnyckel <- readxl::read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") |>
  dplyr::select(Avdelning, Grupp_kod, Branschgrupp) |>
  dplyr::distinct()

source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_utlandskt_agande_tid_agarland_lan_bransch_tva.R")
agande_df <- hamta_utlandskt_agande_tid_agarland_lan_bransch_tva(tid_koder = "9999",
                                                                 agarland_vekt = "*",
                                                                 region_vekt = "20",
                                                                 variabel_klartext = "Antal anställda") |>
  dplyr::rename(varde = dplyr::last_col()) |>
  dplyr::filter(!stringr::str_detect(bransch, "ospecificerad")) |>
  dplyr::mutate(branschbokstav = stringr::str_sub(bransch, 1, 1)) |>
  dplyr::left_join(branschnyckel, by = c("branschbokstav" = "Avdelning")) |>
  dplyr::group_by(år, region, Branschgrupp, variabel) |>
  dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
  dplyr::filter(!is.na(Branschgrupp))

# Add to list
utlandskt_agande_ar <- max(agande_df$år)
utlands_agande_totalt <- sum(agande_df$varde)
utlands_agande_andel_tillverkning <- agande_df |>
  dplyr::mutate(andel = varde / sum(varde) * 100) |>
  dplyr::filter(Branschgrupp == "Tillverkning och utvinning") |>
  dplyr::pull(andel)

fakta_lista[["Utländskt ägande"]] <- skapa_flik(
  utlandskt_agande_ar = utlandskt_agande_ar,
  utlandskt_agande_antal_anstallda = utlands_agande_totalt,
  utlandskt_agande_andel_tillverkning = round(utlands_agande_andel_tillverkning, 1)
)

# Fliken demografi
# En text om befolkningsminskning. Behöver inte ladda hem nya data

# Fliken kompetensförsörjning:
# Senare. Claude hjälpte mig med detta, men nog enkelt att skriva kod.

# Flik 4: Arbetslöshet

# Base data
arbetsmarknadsstatus_df <- pxweb2r::pxweb2_get_data(
  table = "TAB6260",
  query = list(
    Region = c("00", "20"),
    Kon = "totalt",
    Alder = "20–65 år",
    Fodelseregion = "totalt",
    ContentsCode = "arbetslöshet",
    Tid = "9999"
  ))

# Add to list
arbetsloshet_riket <- arbetsmarknadsstatus_df |> dplyr::filter(region == "Riket") |> dplyr::pull(value)
arbetsloshet_dalarna <- arbetsmarknadsstatus_df |> dplyr::filter(region == "Dalarnas län") |> dplyr::pull(value)
arbetsloshet_period <- unique(arbetsmarknadsstatus_df$månad)

fakta_lista[["Arbetslöshet"]] <- skapa_flik(
  arbetsloshet_period = arbetsloshet_period,
  arbetsloshet_riket = arbetsloshet_riket,
  arbetsloshet_dalarna = arbetsloshet_dalarna
)

# Flik 5: Infrastruktur

# Base data
bredband_df <- rdverktyg::hamta_kolada_df(kpi = "N07918", c(20, 00), valda_ar = c(1900:2100)) |>
  dplyr::filter(ar == max(ar))

# Add to list
fakta_lista[["Infrastruktur"]] <- skapa_flik(
  bredband_ar = unique(bredband_df$ar),
  bredband_dalarna = bredband_df |> dplyr::filter(region == "Dalarna") |> dplyr::pull(varde),
  bredband_riket = bredband_df |> dplyr::filter(region == "Riket") |> dplyr::pull(varde)
)

# Sparar till Excel, en flik per element i listan, med dagens datum i filnamnet
writexl::write_xlsx(
  fakta_lista,
  file.path(output_mapp, paste0("fakta_om_dalarna_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
)