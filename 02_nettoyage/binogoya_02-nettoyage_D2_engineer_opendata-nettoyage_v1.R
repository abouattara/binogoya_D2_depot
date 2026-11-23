# =====================================================================
# 02 NETTOYAGE - OPENDATA : conversion numérique et contrôles
#
# Entrée : Data/interimdata/binogoya_01-collecte_D2_engineer_opendata-extrait_v1.rds
# Sortie : Data/cleandata/binogoya_02-nettoyage_D2_engineer_opendata-tables_v1.rds
#            liste nommée « à plat » : une table par fichier source (nom du fichier),
#            entrée de 04_analyse ; attribut "dossier" = dossier source de chaque table
#          Data/cleandata/binogoya_02-nettoyage_D2_engineer_opendata-controles_v1.xlsx
# =====================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("dplyr", "purrr", "writexl"))

library(dplyr)
library(purrr)

exiger(FIC$interim_opendata, "01_collecte/binogoya_01-collecte_D2_engineer_opendata-lecture_v1.R")
opendata_brut <- readRDS(FIC$interim_opendata)

# ---------------------------------------------------------------------
# 1. Conversion numérique (espaces, espaces insécables, virgule décimale)
# ---------------------------------------------------------------------
vers_nombre <- function(x) {
  txt <- gsub("[[:space:]\u00a0]", "", x)
  txt <- gsub(",", ".", txt)
  suppressWarnings(as.numeric(txt))
}

nettoyer_table <- function(t, garder_na = FALSE) {
  out <- t |> mutate(valeur = vers_nombre(valeur_txt)) |> select(-valeur_txt) |> relocate(valeur, .before = fichier)
  if (!garder_na) out <- filter(out, !is.na(valeur))
  arrange(out, annee)
}

propre <- map(opendata_brut, \(lst) map(lst, nettoyer_table))

# ---------------------------------------------------------------------
# 2. Contrôles (calculés sur les valeurs brutes)
# ---------------------------------------------------------------------
controles <- imap_dfr(opendata_brut, \(lst, dossier) imap_dfr(lst, \(t, nom) {
  v <- vers_nombre(t$valeur_txt)
  nn <- !is.na(t$valeur_txt) & is.na(v)
  tibble(
    dossier = dossier,
    table = nom,
    n_lignes_retenues = sum(!is.na(v)),
    annees = if (any(!is.na(t$annee))) paste(range(t$annee, na.rm = TRUE), collapse = "-") else NA_character_,
    n_non_numeriques = sum(nn),
    exemples_non_numeriques = paste(head(unique(t$valeur_txt[nn]), 5), collapse = " ; "),
    colonnes = paste(setdiff(names(t), c("annee", "valeur_txt", "fichier")), collapse = ", ")
  )
}))
print(controles, n = Inf, width = Inf)
if (any(controles$n_non_numeriques > 0))
  warning("Valeurs non numériques ignorées : voir ", basename(FIC$opendata_controles), call. = FALSE)

# ---------------------------------------------------------------------
# 3. Liste à plat (un nom = un fichier source) et sauvegarde
# ---------------------------------------------------------------------
tables <- do.call(c, unname(propre))
if (anyDuplicated(names(tables))) stop("Noms de fichiers OPENDATA en double : ",
                                       paste(names(tables)[duplicated(names(tables))], collapse = ", "))
attr(tables, "dossier") <- setNames(rep(names(propre), lengths(propre)), names(tables))

saveRDS(tables, FIC$opendata)
writexl::write_xlsx(list(controles = controles), FIC$opendata_controles)
message("OPENDATA nettoyé : ", length(tables), " tables -> ", FIC$opendata)
