library(dplyr)
library(readxl)
library(purrr)
library(dplyr)
library(rlang)
library(readr)
library(writexl)

library(tidyverse)
library(haven)
library(dplyr)


###=====================###
###Indlæser alle datasæt###
###=====================###
filer <- list.files(
  path = "Z:\\Data på Surveybanken\\Oprindelige datafiler!!!\\Datafiler_monadisk", #<-------- OBS. stinavn
  pattern = "\\.sav$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (fil in filer) {
  navn <- tools::file_path_sans_ext(basename(fil))
  assign(navn, read_sav(fil))
}




setwd("C:/Users/BO62RK/OneDrive - Aalborg Universitet/Desktop/Surveybanken/Monadisk fil") #<-------- OBS. stinavn

#Indlæser excelarket med overblik
sti <- "Spoergsmaalsformulering_og_skalering_v4_endelig.xlsx" #<----------- OBS. excelfil skal lægge i mappen fra working directory

#Laver objekt til harmonisering
harm <- read_excel(sti, sheet = "Spørgsmål_og_skala") |>
  filter(!is.na(Koncept), !is.na(Bølge), !is.na(Variabelnavn),
         is.na(Note) | Note != "Eksempelrække – slet/overskriv når du er i gang")

# Stopklods: samme rå-variabel koblet til flere koncepter i samme bølge
dupes <- harm |>
  distinct(Bølge, Variabelnavn, Koncept) |>
  count(Bølge, Variabelnavn) |>
  filter(n > 1)

if (nrow(dupes) > 0) {
  print(dupes)
  stop("Ret disse rækker i Excel-arket først – samme rå-variabel er koblet til flere koncepter.")
}

###================================###
###Omdøber variablene i datasættene###
###================================###
omdoeb_datasaet <- function(bolge_navn) {
  if (!exists(bolge_navn, envir = .GlobalEnv)) {
    warning(paste("Datasæt ikke fundet i environment:", bolge_navn))
    return(invisible(NULL))
  }
  df <- get(bolge_navn, envir = .GlobalEnv)
  mapping <- harm |> filter(Bølge == bolge_navn)
  
  # split split-ballot-rækker (fx "q17a + q17b") fra almindelige 1:1-rækker
  er_split <- grepl("\\+", mapping$Variabelnavn)
  split_mapping  <- mapping[er_split, ]
  simpel_mapping <- mapping[!er_split, ]
  
  # --- almindelige 1:1 omdøbninger ---
  fundet <- simpel_mapping$Variabelnavn %in% names(df)
  if (any(!fundet)) {
    warning(paste0(bolge_navn, ": findes ikke i data, springes over: ",
                   paste(simpel_mapping$Variabelnavn[!fundet], collapse = ", ")))
  }
  simpel_mapping <- simpel_mapping[fundet, ]
  
  intern_dup <- simpel_mapping |> count(Standardnavn) |> filter(n > 1)
  if (nrow(intern_dup) > 0) {
    warning(paste0(bolge_navn, ": flere rå-variabler mappet til samme standardnavn: ",
                   paste(intern_dup$Standardnavn, collapse = ", "), " - springes over"))
    simpel_mapping <- simpel_mapping |> filter(!Standardnavn %in% intern_dup$Standardnavn)
  }
  
  kolliderer <- intersect(simpel_mapping$Standardnavn, setdiff(names(df), simpel_mapping$Variabelnavn))
  if (length(kolliderer) > 0) {
    warning(paste0(bolge_navn, ": standardnavn kolliderer med eksisterende kolonne, springes over: ",
                   paste(kolliderer, collapse = ", ")))
    simpel_mapping <- simpel_mapping |> filter(!Standardnavn %in% kolliderer)
  }
  
  rename_vec <- setNames(simpel_mapping$Variabelnavn, simpel_mapping$Standardnavn)
  df <- df |> rename(!!!rename_vec)
  
  # --- split-ballot rækker: coalesce til én harmoniseret variabel ---
  for (i in seq_len(nrow(split_mapping))) {
    komponenter <- trimws(strsplit(split_mapping$Variabelnavn[i], "\\+")[[1]])
    nyt_navn <- split_mapping$Standardnavn[i]
    mangler <- setdiff(komponenter, names(df))
    if (length(mangler) > 0) {
      warning(paste0(bolge_navn, ": split-komponenter mangler i data, springes over: ",
                     paste(mangler, collapse = ", ")))
      next
    }
    df <- df |> mutate(!!nyt_navn := coalesce(!!!syms(komponenter)))
  }
  
  assign(bolge_navn, df, envir = .GlobalEnv)
}

alle_bolger <- unique(harm$Bølge)
walk(alle_bolger, omdoeb_datasaet)




###=======================================###
###Rekoder og normalisere svarkategorierne###
###=======================================###
library(dplyr)
library(purrr)
library(haven)

ikke_normaliser <- c("koen", "region", "stilling", "parti_sidst", "parti_naeste",
                     "sektor_offentlig_privat")

missing_regex <- regex("ved ikke|ikke svar|ønsker ikke|ikke besvaret|ikke relevant", ignore_case = TRUE)

ren_og_normaliser_variabel <- function(x) {
  if (!is.numeric(x) && !haven::is.labelled(x)) return(x)
  
  labs <- attr(x, "labels")
  x_num <- as.numeric(x)
  
  if (!is.null(labs)) {
    missing_koder <- labs[grepl(missing_regex, names(labs))]
    x_num[x_num %in% missing_koder] <- NA
  }
  
  rng <- range(x_num, na.rm = TRUE)
  if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] != rng[2]) {
    x_num <- (x_num - rng[1]) / (rng[2] - rng[1])
  }
  x_num
}

normaliser_alle <- function(bolge_navn) {
  if (!exists(bolge_navn, envir = .GlobalEnv)) return(invisible(NULL))
  df <- get(bolge_navn, envir = .GlobalEnv)
  std_navne <- unique(harm$Standardnavn[harm$Bølge == bolge_navn])
  std_navne <- setdiff(intersect(std_navne, names(df)), ikke_normaliser)
  df <- df |> mutate(across(all_of(std_navne), ren_og_normaliser_variabel))
  assign(bolge_navn, df, envir = .GlobalEnv)
}
walk(alle_bolger, normaliser_alle)

###Standardisering af ikke normaliserede variable
hent_kategorier <- function(bolge_navn, variabel) {
  if (!exists(bolge_navn, envir = .GlobalEnv)) return(NULL)
  df <- get(bolge_navn, envir = .GlobalEnv)
  if (!variabel %in% names(df)) return(NULL)
  labs <- attr(df[[variabel]], "labels")
  if (is.null(labs)) return(NULL)
  tibble::tibble(variabel = variabel, bolge = bolge_navn,
                 kode = as.numeric(labs), label = names(labs))
}

kategori_oversigt <- map_dfr(ikke_normaliser, function(v) {
  map_dfr(alle_bolger, hent_kategorier, variabel = v)
})

writexl::write_xlsx(kategori_oversigt, "kategori_oversigt.xlsx")

#Køn
konverter_koen <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if ("koen" %in% names(df)) {
    df$koen <- factor(as.numeric(df$koen), levels = 1:2, labels = c("Kvinde", "Mand"))
    assign(bolge_navn, df, envir = .GlobalEnv)
  }
}
walk(alle_bolger, konverter_koen)

# Køn -> dummy (0=Kvinde, 1=Mand)
lav_koen_dummy <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if (!"koen" %in% names(df)) return(invisible(NULL))
  df$koen <- ifelse(is.na(df$koen), NA_real_, as.numeric(df$koen == "Mand"))
  assign(bolge_navn, df, envir = .GlobalEnv)
}
walk(alle_bolger, lav_koen_dummy)

#Region
konverter_region <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if ("region" %in% names(df)) {
    df$region <- factor(as.numeric(df$region), levels = 1:5,
                        labels = c("Hovedstaden", "Sjælland", "Syddanmark", "Midtjylland", "Nordjylland"))
    assign(bolge_navn, df, envir = .GlobalEnv)
  }
}
walk(alle_bolger, konverter_region)

# Region -> 5 dummies
regioner <- c("Hovedstaden", "Sjælland", "Syddanmark", "Midtjylland", "Nordjylland")
lav_region_dummies <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if (!"region" %in% names(df)) return(invisible(NULL))
  for (reg in regioner) {
    nyt_navn <- paste0("region_", reg)
    df[[nyt_navn]] <- ifelse(is.na(df$region), NA_real_, as.numeric(df$region == reg))
  }
  assign(bolge_navn, df, envir = .GlobalEnv)
}
walk(alle_bolger, lav_region_dummies)

#Off. eller privat ansat
sektor_mapping <- tibble::tribble(
  ~bolge,     ~kode, ~ny_kode,
  "Corona0",  1,     2,          # Privat ansat
  "Corona0",  2,     1,          # Offentligt ansat
  "Corona0",  3,     3,          # Selvstændig -> Andet
  "Corona0",  4,     3,          # Andet
  "Corona0",  5,     NA_real_,
  "Tryg2007", 1,     NA_real_,   # ser ikke ud til at være samme spørgsmål
  "Tryg2007", 2,     NA_real_,
  "Tryg2007", 99,    NA_real_
)

rekod_sektor <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if (!"sektor_offentlig_privat" %in% names(df)) return(invisible(NULL))
  
  raa <- as.numeric(df$sektor_offentlig_privat)
  saerregler <- sektor_mapping |> filter(bolge == bolge_navn)
  
  if (nrow(saerregler) > 0) {
    ny <- raa
    for (i in seq_len(nrow(saerregler))) ny[raa == saerregler$kode[i]] <- saerregler$ny_kode[i]
  } else {
    ny <- ifelse(raa %in% 1:3, raa, NA_real_)  # standard: 1/2/3, resten (4/88/99) = ved ikke
  }
  
  df$sektor_offentlig_privat <- factor(ny, levels = 1:3,
                                       labels = c("Offentligt ansat", "Privat ansat", "Andet"))
  assign(bolge_navn, df, envir = .GlobalEnv)
}
walk(alle_bolger, rekod_sektor)

# Sektor -> dummy (0=Offentligt ansat, 1=Privat ansat, Andet + Ved ikke -> NA)
lav_sektor_dummy <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if (!"sektor_offentlig_privat" %in% names(df)) return(invisible(NULL))
  df$sektor_offentlig_privat <- dplyr::case_when(
    df$sektor_offentlig_privat == "Offentligt ansat" ~ 0,
    df$sektor_offentlig_privat == "Privat ansat"      ~ 1,
    TRUE ~ NA_real_   # dækker både "Andet" og de oprindelige "Ved ikke"/NA
  )
  assign(bolge_navn, df, envir = .GlobalEnv)
}
walk(alle_bolger, lav_sektor_dummy)

#Partier
library(dplyr)
library(purrr)
library(readxl)

parti_noegle <- read_excel("parti_kategorinoegle.xlsx",
                           sheet = "Sheet1") |>
  select(variabel, bolge, kode, Standardkategori)

rekod_parti_variabel <- function(bolge_navn, variabel) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if (!variabel %in% names(df)) return(invisible(NULL))
  
  opslag <- parti_noegle |> filter(variabel == !!variabel, bolge == bolge_navn)
  if (nrow(opslag) == 0) return(invisible(NULL))
  
  raa <- as.numeric(df[[variabel]])
  ny <- opslag$Standardkategori[match(raa, opslag$kode)]
  df[[variabel]] <- factor(ny, levels = sort(unique(parti_noegle$Standardkategori[!is.na(parti_noegle$Standardkategori)])))
  assign(bolge_navn, df, envir = .GlobalEnv)
}

alle_bolger_parti <- unique(parti_noegle$bolge)
walk(alle_bolger_parti, ~ rekod_parti_variabel(.x, "parti_sidst"))
walk(alle_bolger_parti, ~ rekod_parti_variabel(.x, "parti_naeste"))

alle_partier <- sort(unique(parti_noegle$Standardkategori[!is.na(parti_noegle$Standardkategori)]))

lav_parti_dummies <- function(bolge_navn, variabel) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if (!variabel %in% names(df)) return(invisible(NULL))
  
  for (parti in alle_partier) {
    nyt_navn <- paste0(variabel, "_", parti)
    df[[nyt_navn]] <- ifelse(is.na(df[[variabel]]), NA_real_,
                             as.numeric(df[[variabel]] == parti))
  }
  assign(bolge_navn, df, envir = .GlobalEnv)
}

walk(alle_bolger_parti, ~ lav_parti_dummies(.x, "parti_sidst"))
walk(alle_bolger_parti, ~ lav_parti_dummies(.x, "parti_naeste"))

#Stilling
library(readxl)
library(dplyr)
library(purrr)

stilling_noegle <- read_excel("stilling_kategorinoegle.xlsx") |>
  select(bolge, kode, Standardkategori_navn)

stilling_kategorier <- c("Ufaglaert_specialarbejder","Faglaert_arbejder","Lavere_funktionaer",
                         "Hoejere_funktionaer","Selvstaendig","Fleksjobber","Orlov","Ledig",
                         "Under_uddannelse","Foertidspensionist","Efterloen_pensionist",
                         "Fravaerende_sygdom","Ressourceforloeb")

lav_stilling_dummies <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if (!"stilling" %in% names(df)) return(invisible(NULL))
  
  opslag <- stilling_noegle |> filter(bolge == bolge_navn)
  if (nrow(opslag) == 0) return(invisible(NULL))
  
  raa <- as.numeric(df$stilling)
  kategori <- opslag$Standardkategori_navn[match(raa, opslag$kode)]
  
  for (kat in stilling_kategorier) {
    nyt_navn <- paste0("stilling_", kat)
    df[[nyt_navn]] <- ifelse(is.na(kategori), NA_real_, as.numeric(kategori == kat))
  }
  assign(bolge_navn, df, envir = .GlobalEnv)
}

alle_bolger_stilling <- unique(stilling_noegle$bolge)
walk(alle_bolger_stilling, lav_stilling_dummies)


###========================###
###Tilføjer kolonne med tid###
###========================###
library(readxl)
library(dplyr)
library(purrr)

tid <- read_excel("Spoergsmaalsformulering_og_skalering_v4_endelig.xlsx", sheet = "Tid")

tilfoej_tidspunkt <- function(bolge_navn) {
  if (!exists(bolge_navn, envir = .GlobalEnv)) return(invisible(NULL))
  df <- get(bolge_navn, envir = .GlobalEnv)
  info <- tid |> filter(Måling == bolge_navn)
  
  if (nrow(info) == 1) {
    df$tidspunkt <- as.Date(sprintf("%d-%02d-01", info$År, info$Måned))
    assign(bolge_navn, df, envir = .GlobalEnv)
  } else {
    warning(paste(bolge_navn, ": ingen tidsinfo fundet i Tid-arket"))
  }
}

walk(alle_bolger, tilfoej_tidspunkt)



###================================###
###Tjekker unique_key for paneldata###
###================================###
library(dplyr)
library(purrr)

# 1) Hvilke bølger har en unique_key-kolonne?
har_unique_key <- alle_bolger[map_lgl(alle_bolger, ~ exists(.x, envir = .GlobalEnv) &&
                                        "unique_key" %in% names(get(.x, envir = .GlobalEnv)))]

print(har_unique_key)
print(setdiff(alle_bolger, har_unique_key))   # dem uden unique_key

# 2) Format og entydighed inden for hver bølge
tjek_unique_key <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  ids <- df$unique_key
  tibble::tibble(
    bolge      = bolge_navn,
    n_raekker  = length(ids),
    n_unikke   = n_distinct(ids),
    type       = class(ids)[1],
    eksempel   = paste(head(unique(ids), 3), collapse = ", ")
  )
}
oversigt_key <- map_dfr(har_unique_key, tjek_unique_key)
print(oversigt_key, n = Inf)


###====================###
###Bygger respondent ID###
###====================###
byg_respondent_id <- function(bolge_navn) {
  df <- get(bolge_navn, envir = .GlobalEnv)
  if ("unique_key" %in% names(df)) {
    df$respondent_id <- as.character(df$unique_key)
    df$har_panel_id   <- TRUE
  } else {
    df$respondent_id <- paste0(bolge_navn, "_", seq_len(nrow(df)))
    df$har_panel_id   <- FALSE
  }
  assign(bolge_navn, df, envir = .GlobalEnv)
}
walk(alle_bolger, byg_respondent_id)


###=================###
###Bygger trend file###
###=================###
library(dplyr)
library(purrr)

standardnavne_vars <- setdiff(unique(harm$Standardnavn), "stilling")

fast_id_vars <- c("respondent_id", "har_panel_id", "boelge", "tidspunkt")

vaelg_haandterede_variable <- function(bolge_navn) {
  if (!exists(bolge_navn, envir = .GlobalEnv)) return(NULL)
  df <- get(bolge_navn, envir = .GlobalEnv)
  df$boelge <- bolge_navn
  
  df |>
    select(
      any_of(fast_id_vars),
      any_of(standardnavne_vars),
      starts_with("region_"),
      starts_with("stilling_"),
      starts_with("parti_sidst_"),
      starts_with("parti_naeste_")
    )
}

trend_fil <- map_dfr(alle_bolger, vaelg_haandterede_variable)

write_csv(trend_fil, "monadisk_fil.csv")

###==================###
###Laver paneldatasæt###
###==================###
panel_data <- trend_fil |>
  filter(har_panel_id) |>
  arrange(respondent_id, tidspunkt)

# Hvor mange unikke personer er der reelt paneldata for (dvs. optræder i 2+ bølger)?
panel_oversigt <- panel_data |>
  group_by(respondent_id) |>
  summarise(n_boelger = n_distinct(boelge), .groups = "drop")

table(panel_oversigt$n_boelger)

panel_ids_med_gentagelse <- panel_oversigt |>
  filter(n_boelger >= 2) |>
  pull(respondent_id)

panel_data_strict <- panel_data |>
  filter(respondent_id %in% panel_ids_med_gentagelse)

panel_data_final <- panel_data |>
  left_join(panel_oversigt, by = "respondent_id") |>
  filter(n_boelger >= 2) |>
  relocate(respondent_id, n_boelger, boelge, tidspunkt)

write_csv(panel_data_final, "panel_data.csv")

#EKSEMPEL
##panel_data_final |> filter(n_boelger >= 5)
