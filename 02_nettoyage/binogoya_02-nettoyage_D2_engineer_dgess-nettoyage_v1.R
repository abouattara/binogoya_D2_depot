# =====================================================================
# 02 NETTOYAGE - DGESS : conversion numérique, harmonisation du sexe, contrôles
#
# Entrée : Data/interimdata/binogoya_01-collecte_D2_engineer_dgess-extrait_v1.rds
# Sortie : Data/cleandata/binogoya_02-nettoyage_D2_engineer_dgess-tables_v1.rds
#            liste (tables, catalogue) : entrée de 03_transformation et de 04_analyse
#          Data/cleandata/binogoya_02-nettoyage_D2_engineer_dgess-controles_v1.xlsx
# =====================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("dplyr", "purrr", "stringr", "stringi", "writexl"))

library(dplyr)
library(purrr)
library(stringr)
library(stringi)

exiger(FIC$interim_dgess, "01_collecte/binogoya_01-collecte_D2_engineer_dgess-extraction_v1.R")
res_brut <- readRDS(FIC$interim_dgess)

# ---------------------------------------------------------------------
# 1. Outils de nettoyage
# ---------------------------------------------------------------------
nettoyer_num <- function(x) {
  x <- str_remove_all(x, "[\\s\\u00A0\\u202F%]")
  suppressWarnings(as.numeric(str_replace(x, ",", ".")))
}

harmoniser_sexe <- function(x) {
  s <- str_squish(str_to_upper(stri_trans_general(x, "Latin-ASCII")))
  case_match(
    s,
    c("G", "GARCONS", "M", "H", "MASCULIN") ~ "G",
    c("F", "FILLES", "FEMININ")             ~ "F",
    c("T", "TOTAL", "ENSEMBLE")             ~ "T",
    .default = x
  )
}

# ---------------------------------------------------------------------
# 2. Nettoyage de chaque table
# ---------------------------------------------------------------------
res <- res_brut
res$tables <- map(res_brut$tables, \(t) {
  t <- t |> mutate(valeur = nettoyer_num(valeur_brute), .before = valeur_brute)
  if ("sexe" %in% names(t)) t <- t |> mutate(sexe = harmoniser_sexe(sexe))
  t
})

# ---------------------------------------------------------------------
# 3. Contrôles
# ---------------------------------------------------------------------
controles <- imap_dfr(res$tables, \(t, nom) tibble(
  table = nom,
  n_lignes = nrow(t),
  n_valeurs_manquantes = sum(is.na(t$valeur_brute)),
  n_non_numeriques = sum(!is.na(t$valeur_brute) & is.na(t$valeur)),
  exemples_non_numeriques = paste(head(unique(t$valeur_brute[!is.na(t$valeur_brute) & is.na(t$valeur)]), 5),
                                  collapse = " ; "),
  modalites_sexe = if ("sexe" %in% names(t)) paste(sort(unique(t$sexe)), collapse = ", ") else NA_character_,
  annees = paste(range(t$annee, na.rm = TRUE), collapse = "-")
))
print(controles, n = Inf, width = Inf)

sexe_inattendu <- controles |> filter(!is.na(modalites_sexe), str_detect(modalites_sexe, "[^GFT, ]"))
if (nrow(sexe_inattendu) > 0) warning("Modalités de sexe non harmonisées dans : ",
                                      paste(sexe_inattendu$table, collapse = ", "), call. = FALSE)

# ---------------------------------------------------------------------
# 4. Sauvegarde
# ---------------------------------------------------------------------
saveRDS(res, FIC$dgess)
writexl::write_xlsx(list(controles = controles, catalogue = res$catalogue), FIC$dgess_controles)
message("DGESS nettoyé -> ", FIC$dgess)
