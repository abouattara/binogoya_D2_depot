# =====================================================================
# 03 TRANSFORMATION - DGESS : tables d'analyse par thème
#
# Entrée : Data/cleandata/binogoya_02-nettoyage_D2_engineer_dgess-tables_v1.rds
#          (res$tables : A_region, B_statut, C_region, D_region_statut...)
# Sortie : Data/transformdata/binogoya_03-transformation_D2_engineer_dgess-tables-analyse_v1.rds
#          Data/transformdata/binogoya_03-transformation_D2_engineer_dgess-tables-analyse_v1.xlsx
#          tables_analyse = liste de tables longues, une par thème
# =====================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("dplyr", "tidyr", "stringr", "purrr", "stringi", "writexl"))

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(stringi)

exiger(FIC$dgess, "02_nettoyage/binogoya_02-nettoyage_D2_engineer_dgess-nettoyage_v1.R")
res <- readRDS(FIC$dgess)

# ---------------------------------------------------------------------
# T0. Paramètres
# ---------------------------------------------------------------------

# Colonnes standard produites par l'extraction (le reste = libellés : region, milieu, statut...)
cols_std <- c("type_tableau", "tableau", "titre", "annee_scolaire", "annee",
              "sexe", "valeur", "valeur_brute", "est_total", "feuille")

# Cycle déduit du nom de la feuille :
#   "PRESCOLAIRE" -> prescolaire ; "Feuil1" ... "Feuil13" -> primaire
cycle_de <- function(feuille) {
  f <- str_to_lower(stri_trans_general(feuille, "Latin-ASCII"))
  case_when(
    str_detect(f, "presco")      ~ "prescolaire",
    str_detect(f, "^feuil\\d+$") ~ "primaire",
    TRUE                         ~ NA_character_
  )
}

# Tableaux retenus : (cycle, numéro de tableau) -> table de sortie + indicateur
selection <- tribble(
  ~cycle,        ~tableau, ~table_sortie,         ~indicateur,   ~statut_analyse,
  # --- principaux ---
  "prescolaire", "1",      "effectifs_region",    "effectif",    "principal",
  "prescolaire", "3",      "effectifs_milieu",    "effectif",    "principal",
  "prescolaire", "5",      "tbs_region",          "tbs",         "principal",
  "prescolaire", "6",      "ips_region",          "ips_tbs",     "principal",
  "primaire",    "1",      "effectifs_region",    "effectif",    "principal",
  "primaire",    "3",      "effectifs_milieu",    "effectif",    "principal",
  "primaire",    "5",      "nouveaux_cp1_region", "nouveaux_cp1","principal",
  "primaire",    "6",      "tba_region",          "tba",         "principal",
  "primaire",    "7",      "tbs_region",          "tbs",         "principal",
  "primaire",    "10",     "ips_region",          "ips_tba",     "principal",
  "primaire",    "11",     "ips_region",          "ips_tbs",     "principal",
  # --- complémentaires (explicatifs) ---
  "prescolaire", "2",      "effectifs_statut",    "effectif",    "complementaire",
  "primaire",    "2",      "effectifs_statut",    "effectif",    "complementaire",
  "prescolaire", "7",      "offre_scolaire",      "offre_t7",    "complementaire",
  "prescolaire", "8",      "offre_scolaire",      "offre_t8",    "complementaire",
  "primaire",    "13",     "offre_scolaire",      "offre_t13",   "complementaire"
)

# ---------------------------------------------------------------------
# T1. Empiler toutes les tables extraites et ne garder que la sélection
# ---------------------------------------------------------------------
donnees <- imap(res$tables, \(t, nom) {
  labs <- setdiff(names(t), cols_std)
  t |> mutate(across(all_of(labs), as.character), table_source = nom)
}) |>
  bind_rows() |>
  mutate(cycle = cycle_de(feuille), tableau = as.character(tableau)) |>
  inner_join(selection, by = c("cycle", "tableau"))

# ---------------------------------------------------------------------
# T2. Contrôles
# ---------------------------------------------------------------------
# Repérage des feuilles -> cycle
print(res$catalogue |> distinct(feuille) |> mutate(cycle = cycle_de(feuille)))

# Tableaux demandés mais introuvables
manquants <- anti_join(selection, distinct(donnees, cycle, tableau), by = c("cycle", "tableau"))
if (nrow(manquants) > 0) {
  warning("Tableaux demandés introuvables :\n",
          paste0("  ", manquants$cycle, " T", manquants$tableau, collapse = "\n"),
          call. = FALSE)
}

# Vérification visuelle : le titre correspond-il à l'indicateur attendu ?
verif <- donnees |>
  group_by(cycle, tableau, table_sortie, indicateur) |>
  summarise(titre    = first(titre),
            n_lignes = n(),
            annees   = paste(range(annee, na.rm = TRUE), collapse = "-"),
            .groups  = "drop")
print(verif, n = Inf, width = Inf)

# ---------------------------------------------------------------------
# T3. Tables d'analyse (une par thème, colonnes vides retirées)
# ---------------------------------------------------------------------
tables_analyse <- donnees |>
  split(donnees$table_sortie) |>
  map(\(t) t |>
        select(-any_of(c("table_sortie", "table_source", "type_tableau",
                         "valeur_brute", "feuille", "statut_analyse"))) |>
        select(where(\(x) !all(is.na(x)))) |>
        relocate(cycle, indicateur, tableau, annee_scolaire, annee) |>
        relocate(valeur, titre, .after = last_col()))

names(tables_analyse)
# ex. "effectifs_region" "effectifs_milieu" "effectifs_statut" "tbs_region"
#     "tba_region" "nouveaux_cp1_region" "ips_region" "offre_scolaire"

# ---------------------------------------------------------------------
# T4. Export (un onglet par table + onglet de vérification)
# ---------------------------------------------------------------------
saveRDS(tables_analyse, FIC$dgess_analyse_rds)
writexl::write_xlsx(c(list(verification = verif), tables_analyse), FIC$dgess_analyse_xlsx)
message("Tables d'analyse DGESS -> ", FIC$dgess_analyse_rds)
