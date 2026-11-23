# =====================================================================
# 02 NETTOYAGE - CONASUR : typage, contrôle et correction des Pcodes
#
# Entrée : Data/interimdata/binogoya_01-collecte_D2_engineer_conasur-extrait_v1.rds
# Sortie : Data/cleandata/binogoya_02-nettoyage_D2_engineer_conasur-pdi_v1.rds
#            table propre, une ligne par ligne du fichier source (entrée de 04_analyse)
#          Data/cleandata/binogoya_02-nettoyage_D2_engineer_conasur-controles_v1.xlsx
# La transformation (agrégations, jointures) vient ensuite.
# =====================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("dplyr", "tidyr", "stringr", "purrr", "stringi", "writexl"))

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(stringi)

exiger(FIC$interim_conasur, "01_collecte/binogoya_01-collecte_D2_engineer_conasur-lecture_v1.R")
lecture <- readRDS(FIC$interim_conasur)

# ---------------------------------------------------------------------
# 0. Paramètres
# ---------------------------------------------------------------------
cols_nb <- c("hommes", "femmes", "enfants_moins_5", "enfants_plus_5",
             "total_enfants", "total_individus")

# Pcodes erronés dans idps4 (repérés en les comparant aux autres fichiers et à la hiérarchie
# région / province). pcode_correct = NA : correction à renseigner (Pcode officiel, HDX / OCHA) ;
# en attendant, les lignes sont marquées `pcode_a_verifier = TRUE` et laissées telles quelles.
corrections_pcodes <- tribble(
  ~commune,       ~pcode_faux, ~pcode_correct, ~commentaire,
  "Oula",         "BF540102",  "BF540308",     "Yatenga : Pcode d'Ouindigui (Loroum) utilisé par erreur ; code du fichier de 2020",
  "Baraboule",    "BF460301",  "BF560302",     "Soum (Sahel) : Pcode de Barani utilisé par erreur ; code du fichier de 2020",
  "Bartiebougou", "BF480302",  "BF520301",     "Komondjari (Est) : Pcode de Baskoure utilisé par erreur ; code du fichier de 2020",
  "Koundougou",   "BF540305",  NA_character_,  "Houet : Pcode de la série Yatenga (BF5403) ; Pcode officiel à renseigner"
)

# ---------------------------------------------------------------------
# 1. Outils
# ---------------------------------------------------------------------
# Libellé normalisé pour comparer : sans accents, casse, espaces, tirets, apostrophes
norm_lib <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> str_to_lower() |> str_replace_all("[^a-z0-9]+", "")
}

vers_nombre <- function(x) {
  x <- str_remove_all(as.character(x), "[\\s\\u00A0\\u202F]")
  x <- str_replace(x, ",", ".")
  x[x %in% c("", "-", "--", "NA", "N/A", "n/a", "ND", "nd")] <- NA
  suppressWarnings(as.numeric(x))
}

# Numéro de série Excel, ou texte jj/mm/aaaa, aaaa-mm-jj...
vers_date <- function(x) {
  x   <- str_squish(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  serie <- !is.na(x) & str_detect(x, "^\\d{5}(\\.\\d+)?$")
  out[serie] <- as.Date(as.numeric(x[serie]), origin = "1899-12-30")
  for (fmt in c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%d/%m/%y", "%Y/%m/%d")) {
    reste <- is.na(out) & !is.na(x)
    if (!any(reste)) break
    out[reste] <- suppressWarnings(as.Date(x[reste], format = fmt))
  }
  out
}

# Typage des effectifs et de la date
typer_idps4 <- function(d) {
  n_nonnum <- sum(map_int(cols_nb, \(cn) sum(!is.na(d[[cn]]) & is.na(vers_nombre(d[[cn]])))))
  d <- d |>
    mutate(across(all_of(cols_nb), \(x) round(vers_nombre(x), 6)),   # arrondi : supprime le bruit de calcul
           date_brute  = date_donnee,
           date_donnee = vers_date(date_donnee))
  list(donnees = d, n_nonnum = n_nonnum)
}

# ---------------------------------------------------------------------
# 2. Nettoyage et indicateurs de qualité par ligne
# ---------------------------------------------------------------------
nettoyer_idps4 <- function(d) {
  d |>
    mutate(
      across(c(admin1_pcode, admin2_pcode, admin3_pcode),
             \(x) str_to_upper(str_remove_all(x, "\\s+"))),
      admin3_valide = !is.na(admin3_pcode) & str_detect(admin3_pcode, "^[A-Z]{2}\\d{6}$"),
      # le Pcode commune doit commencer par le Pcode province, lui-même par le Pcode région
      incoh_hierarchie = coalesce(
        admin3_valide & ((!is.na(admin2_pcode) & str_sub(admin3_pcode, 1, 6) != admin2_pcode) |
                           (!is.na(admin1_pcode) & str_sub(admin3_pcode, 1, 4) != admin1_pcode)),
        FALSE),
      province_cle  = norm_lib(province),
      commune_cle   = norm_lib(commune),
      sans_effectif = if_all(all_of(cols_nb), is.na),
      negatif       = if_any(all_of(cols_nb), \(x) !is.na(x) & x < 0),
      incoh_enfants = !is.na(total_enfants) & !is.na(enfants_moins_5) & !is.na(enfants_plus_5) &
        total_enfants != enfants_moins_5 + enfants_plus_5,
      incoh_total   = !is.na(total_individus) & !is.na(hommes) & !is.na(femmes) & !is.na(total_enfants) &
        total_individus != hommes + femmes + total_enfants
    )
}

# Pcode province attendu pour chaque couple (région, province) = celui de la majorité des lignes.
# Une ligne dont le Pcode province diffère a un Pcode ou un libellé suspect. Diagnostic seulement.
verifier_pcodes <- function(d) {
  attendu <- d |>
    filter(admin3_valide, !coalesce(pcode_a_verifier, FALSE)) |>
    count(region_cle = norm_lib(region), province_cle, admin2_pcode) |>
    group_by(region_cle, province_cle) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(region_cle, province_cle, admin2_attendu = admin2_pcode)

  d |>
    mutate(region_cle = norm_lib(region)) |>
    left_join(attendu, by = c("region_cle", "province_cle")) |>
    mutate(pcode_incoherent = admin3_valide & !is.na(admin2_attendu) & admin2_pcode != admin2_attendu) |>
    select(-region_cle)
}

# Applique la table `corrections_pcodes` (clé : commune + Pcode erroné)
appliquer_corrections <- function(d, corr) {
  corr <- corr |>
    transmute(commune_cle = norm_lib(commune), admin3_pcode = pcode_faux,
              pcode_correct, en_table = TRUE)
  d |>
    mutate(admin3_pcode_original = admin3_pcode) |>
    left_join(corr, by = c("commune_cle", "admin3_pcode")) |>
    mutate(
      a_corriger       = coalesce(en_table, FALSE),
      pcode_corrige    = a_corriger & !is.na(pcode_correct),
      pcode_a_verifier = a_corriger & is.na(pcode_correct),
      admin3_pcode     = if_else(pcode_corrige, pcode_correct, admin3_pcode),
      admin2_pcode     = if_else(pcode_corrige, str_sub(pcode_correct, 1, 6), admin2_pcode),
      admin1_pcode     = if_else(pcode_corrige, str_sub(pcode_correct, 1, 4), admin1_pcode)
    ) |>
    select(-en_table, -pcode_correct, -a_corriger)
}

# Plusieurs lignes pour un même lieu et une même date sont normales (sites, localités...) :
# on les marque, on ne les supprime pas.
marquer_repetitions <- function(d) {
  d |>
    group_by(admin3_pcode, province_cle, date_donnee) |>
    mutate(n_lignes_lieu_date = n()) |>
    ungroup() |>
    mutate(repete_lieu_date = n_lignes_lieu_date > 1,
           doublon_exact = duplicated(across(all_of(c("region", "province", "commune",
                                                      "admin3_pcode", "date_donnee", cols_nb)))))
}

# ---------------------------------------------------------------------
# 3. Libellés géographiques : un libellé de référence par Pcode
# ---------------------------------------------------------------------
niveaux_geo <- tribble(
  ~pcode,         ~lib,
  "admin1_pcode", "region",
  "admin2_pcode", "province",
  "admin3_pcode", "commune"
)

# forme en casse mixte la plus fréquente (évite "KOMKI-IPALA") ; à défaut casse "Titre"
choisir_libelle <- function(x, freq) {
  mixte <- x != str_to_upper(x) & x != str_to_lower(x)
  i <- order(!mixte, -freq)[1]
  if (mixte[i]) x[i] else str_to_title(str_to_lower(x[i]))
}

construire_dico_geo <- function(d) {
  pmap_dfr(niveaux_geo, \(pcode, lib) {
    d |>
      filter(!is.na(.data[[pcode]]), !is.na(.data[[lib]])) |>
      group_by(across(all_of(c(pcode, lib)))) |>
      summarise(freq = n(), .groups = "drop") |>
      group_by(across(all_of(pcode))) |>
      summarise(libelle_std = choisir_libelle(.data[[lib]], freq),
                n_variantes = n(),
                n_variantes_reelles = n_distinct(norm_lib(.data[[lib]])),
                variantes   = paste(.data[[lib]], collapse = " | "),
                .groups = "drop") |>
      rename(pcode = all_of(pcode)) |>
      mutate(niveau = lib, .before = 1)
  })
}

harmoniser_geo <- function(d, dico) {
  for (k in seq_len(nrow(niveaux_geo))) {
    pc  <- niveaux_geo$pcode[k]
    lb  <- niveaux_geo$lib[k]
    ref <- dico |> filter(niveau == lb)
    m   <- setNames(ref$libelle_std, ref$pcode)
    d[[paste0(lb, "_std")]] <- coalesce(unname(m[d[[pc]]]), d[[lb]])
  }
  d
}

# ---------------------------------------------------------------------
# 4. Exécution
# ---------------------------------------------------------------------
typage <- typer_idps4(lecture$donnees)
d_brut <- typage$donnees

# Pcodes suspects AVANT correction (diagnostic)
d0 <- d_brut |> nettoyer_idps4() |> mutate(pcode_a_verifier = FALSE)
suspects_avant <- d0 |>
  verifier_pcodes() |>
  filter(pcode_incoherent) |>
  count(region, province, commune, admin3_pcode, admin2_attendu, sort = TRUE)

# Corrections, répétitions, libellés harmonisés
conasur <- d_brut |>
  nettoyer_idps4() |>
  appliquer_corrections(corrections_pcodes) |>
  marquer_repetitions()

dico_geo <- conasur |> filter(!pcode_a_verifier) |> construire_dico_geo()
conasur  <- conasur |>
  harmoniser_geo(dico_geo) |>
  select(where(\(x) !all(is.na(x))))      # retire les colonnes entièrement vides (source, pays, site...)

# ---------------------------------------------------------------------
# 5. Contrôles
# ---------------------------------------------------------------------
controles <- list(
  n_lignes          = nrow(conasur),
  colonnes_hors_modele = lecture$inconnues,
  cellules_non_numeriques = typage$n_nonnum,
  dates             = list(etendue = range(conasur$date_donnee, na.rm = TRUE),
                           n_invalides = sum(is.na(conasur$date_donnee)),
                           par_date = count(conasur, date_donnee)),
  manquants         = conasur |>
    summarise(across(everything(), \(x) sum(is.na(x)))) |>
    pivot_longer(everything(), names_to = "colonne", values_to = "n_manquants") |>
    mutate(pct = round(100 * n_manquants / nrow(conasur), 1)) |>
    arrange(desc(n_manquants)),
  pcode_corriges    = conasur |> filter(pcode_corrige) |>
    count(commune, admin3_pcode_original, admin3_pcode),
  pcode_a_verifier  = conasur |> filter(pcode_a_verifier) |>
    count(region, province, commune, admin3_pcode),
  pcode_suspects_apres = conasur |> mutate(pcode_a_verifier = FALSE) |> verifier_pcodes() |>
    filter(pcode_incoherent) |>
    count(region, province, commune, admin3_pcode, admin2_attendu, sort = TRUE),
  hierarchie        = conasur |> filter(incoh_hierarchie) |>
    count(region, province, commune, admin1_pcode, admin2_pcode, admin3_pcode, sort = TRUE),
  totaux_incoherents = conasur |> filter(incoh_enfants | incoh_total | sans_effectif | negatif) |>
    select(ligne_excel, region, province, commune, date_donnee, all_of(cols_nb)),
  repetitions       = conasur |> count(repete_lieu_date, doublon_exact),
  variantes_libelles = dico_geo |> filter(n_variantes_reelles > 1) |>
    arrange(niveau, desc(n_variantes_reelles))
)

# Résumé à lire en priorité
cat("\nLignes :", controles$n_lignes,
    "\nPériode :", format(controles$dates$etendue[1]), "->", format(controles$dates$etendue[2]),
    "\nDates distinctes :", nrow(controles$dates$par_date),
    "\nCellules non numériques :", controles$cellules_non_numeriques, "\n")

print(controles$manquants, n = 30)
print(controles$pcode_corriges)
print(controles$pcode_a_verifier)           # à renseigner dans corrections_pcodes
print(controles$pcode_suspects_apres)       # ce qui reste suspect après corrections
print(controles$hierarchie, n = 30)         # Pcode commune non cohérent avec Pcode province / région
print(controles$totaux_incoherents)         # totaux incohérents, manquants ou négatifs
print(controles$repetitions)                # lignes multiples par lieu + date / doublons exacts

# Colonnes hors modèle (colonne_17...) : à identifier avant de les ignorer
print(head(lecture$hors_modele, 15))

# ---------------------------------------------------------------------
# 6. Export
# ---------------------------------------------------------------------
saveRDS(conasur, FIC$conasur)
writexl::write_xlsx(
  list(donnees           = conasur,
       manquants         = controles$manquants,
       pcode_corriges    = controles$pcode_corriges,
       pcode_a_verifier  = controles$pcode_a_verifier,
       hierarchie        = controles$hierarchie,
       dictionnaire_geo  = dico_geo,
       hors_modele       = lecture$hors_modele),
  FIC$conasur_controles)
message("CONASUR nettoyé -> ", FIC$conasur)
