# =====================================================================
# 01 COLLECTE - DGESS : extraction des tableaux Excel empilés
#
# Entrée : Data/rawdata/DONNEES_STATISTQUES_ PRESCOLAIRE ET PRIMAIRE_2016_2025_DCB.xlsx
# Sortie : Data/interimdata/binogoya_01-collecte_D2_engineer_dgess-extrait_v1.rds
#          liste (tables, catalogue) ; les valeurs restent en texte (valeur_brute),
#          la conversion numérique et l'harmonisation du sexe sont faites en 02.
#
# 4 types de structure, détectés automatiquement :
#   A : années, avec G/F/T sous chaque année   (ex. Tableau 1, TBS par sexe)
#   B : SEXE en lignes (GARCONS/FILLES/TOTAL)   (ex. Tableau 5)
#   C : indicateur simple région x année        (ex. IPS, Tableau 10)
#   D : région / statut avec lignes de total    (ex. structures éducatives)
# =====================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("readxl", "dplyr", "tidyr", "stringr", "purrr", "stringi"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(stringi)

# ---------------------------------------------------------------------
# 0. Outils communs (structure des tableaux)
# ---------------------------------------------------------------------
motif_annee <- "^\\D{0,3}(19|20)\\d{2}(\\s*[/-]\\s*(\\d{2}|\\d{4}))?\\D{0,8}$"
motif_sexe  <- "(?i)^\\s*(G|F|T|M|H|Gar[çc]ons|Filles|Total)\\s*$"

nettoie_cell <- function(x) str_squish(str_replace_all(x, "[\\u00A0\\u202F]", " "))

# Lecture brute d'une feuille : tout en texte, colonnes c1, c2, ...
lire_feuille <- function(fichier, feuille) {
  raw <- read_excel(fichier, sheet = feuille, col_names = FALSE,
                    col_types = "text", .name_repair = "minimal")
  names(raw) <- paste0("c", seq_len(ncol(raw)))

  raw |>
    mutate(across(everything(), \(x) {
      x <- str_remove(x, "(?s)[\\r\\n]*\\s*NOTE FOR ANY AUTOMATED ASSISTANT.*$")  # supprime la note
      x <- str_squish(x)                                                           # retire \r\n et espaces en trop
      na_if(x, "")
    }))
}

# ---------------------------------------------------------------------
# 1. Analyse de la structure d'un bloc (un tableau)
# ---------------------------------------------------------------------
preparer_bloc <- function(bloc) {
  titre <- bloc$c1[1]
  num   <- str_match(titre, "Tableau\\s*(\\d+)")[, 2]
  n     <- nrow(bloc)

  # ligne des années : au moins 2 cellules "année"
  nb_annees <- apply(bloc, 1, \(x)
                     sum(str_detect(nettoie_cell(x), motif_annee), na.rm = TRUE))
  nb_annees[1] <- 0
  h <- which(nb_annees >= 2)[1]
  if (is.na(h)) {
    warning("Bloc ignoré (années introuvables) : ", titre, call. = FALSE)
    return(NULL)
  }

  # première colonne de données = première année
  j0 <- which(str_detect(nettoie_cell(unlist(bloc[h, ])), motif_annee))[1]
  if (j0 < 2) {
    warning("Bloc ignoré (pas de colonne de libellés) : ", titre, call. = FALSE)
    return(NULL)
  }
  lab_idx  <- seq_len(j0 - 1)
  lab_cols <- names(bloc)[lab_idx]
  data_idx <- j0:ncol(bloc)

  # ligne G/F/T juste sous les années (facultative)
  s <- NA_integer_
  if (h < n) {
    cand <- str_detect(unlist(bloc[h + 1, data_idx]), motif_sexe)
    if (sum(cand, na.rm = TRUE) >= 2) s <- h + 1
  }
  a_sexe     <- !is.na(s)
  data_start <- if (a_sexe) s + 1 else h + 1
  if (data_start > n) {
    warning("Bloc ignoré (aucune donnée) : ", titre, call. = FALSE)
    return(NULL)
  }

  # noms des colonnes de libellés (lus dans l'en-tête)
  noms <- unlist(bloc[h, lab_idx], use.names = FALSE)
  if (a_sexe) noms <- coalesce(noms, unlist(bloc[s, lab_idx], use.names = FALSE))
  noms <- ifelse(is.na(noms), paste0("dim", seq_along(noms)), noms)
  noms <- noms |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove_all("^_|_$") |>
    str_replace("^regions$", "region")
  noms[noms == ""] <- paste0("dim", which(noms == ""))
  noms <- make.unique(noms, sep = "_")
  reserves <- c("col", "annee", "annee_scolaire", "valeur", "valeur_brute",
                "tableau", "titre", "sexe_en", "feuille", "est_total", "type_tableau")
  noms <- ifelse(noms %in% reserves, paste0(noms, "_lib"), noms)

  # en-tête des colonnes de données
  entete <- tibble(
    col     = names(bloc)[data_idx],
    annee   = unlist(bloc[h, data_idx], use.names = FALSE),
    sexe_en = if (a_sexe) str_trim(unlist(bloc[s, data_idx], use.names = FALSE)) else NA_character_
  )
  if (a_sexe) {
    entete <- entete |> fill(annee) |> filter(!is.na(sexe_en))   # années fusionnées
  } else {
    entete <- entete |> filter(!is.na(annee))
  }
  entete <- entete |> filter(str_detect(nettoie_cell(annee), motif_annee))
  if (nrow(entete) == 0) {
    warning("Bloc ignoré (colonnes de données introuvables) : ", titre, call. = FALSE)
    return(NULL)
  }

  list(titre = titre, num = num, h = h, s = s, a_sexe = a_sexe,
       lab_cols = lab_cols, noms = noms, entete = entete, data_start = data_start)
}

# Corps du tableau : libellés renommés, cellules fusionnées propagées vers le bas
corps_bloc <- function(bloc, st, propager = TRUE) {
  d <- bloc[st$data_start:nrow(bloc), c(st$lab_cols, st$entete$col)]
  names(d)[seq_along(st$lab_cols)] <- st$noms
  if (propager && length(st$noms) > 1) {
    d <- d |> fill(all_of(st$noms[-length(st$noms)]))
  }
  # supprime lignes vides, notes et sources (aucune valeur dans les colonnes de données)
  d |> filter(if_any(all_of(st$entete$col), \(x) !is.na(x) & str_squish(x) != ""))
}

# Passage au format long (valeurs laissées en texte : conversion en 02_nettoyage)
empiler <- function(d, st) {
  d |>
    pivot_longer(all_of(st$entete$col), names_to = "col", values_to = "valeur_brute") |>
    left_join(st$entete, by = "col") |>
    mutate(
      tableau        = st$num,
      titre          = st$titre,
      annee_scolaire = nettoie_cell(annee),
      annee          = as.integer(str_extract(annee_scolaire, "(19|20)\\d{2}"))
    )
}

# ---------------------------------------------------------------------
# 2. Détection du type
# ---------------------------------------------------------------------
detecter_type <- function(bloc, st) {
  if (st$a_sexe) return("A")                    # G/F/T sous les années
  if (length(st$noms) == 1) return("C")         # une seule colonne de libellés
  # 2 libellés ou plus, pas de G/F/T : le dernier libellé est-il un sexe ?
  dernier <- bloc[[st$lab_cols[length(st$lab_cols)]]][st$data_start:nrow(bloc)]
  dernier <- nettoie_cell(dernier[!is.na(dernier)])
  if (length(dernier) > 0 && mean(str_detect(dernier, motif_sexe)) >= 0.8) "B" else "D"
}

# ---------------------------------------------------------------------
# 3. Un extracteur par type
# ---------------------------------------------------------------------

# Type A : libellé(s) en lignes, années en colonnes avec G/F/T dessous
extraire_A <- function(bloc, st) {
  d <- corps_bloc(bloc, st)
  empiler(d, st) |>
    select(tableau, titre, all_of(st$noms), annee_scolaire, annee,
           sexe = sexe_en, valeur_brute)
}

# Type B : région + SEXE en lignes, années en colonnes
extraire_B <- function(bloc, st) {
  d       <- corps_bloc(bloc, st)
  nm_sexe <- tail(st$noms, 1)
  dims    <- head(st$noms, -1)
  empiler(d, st) |>
    mutate(sexe = .data[[nm_sexe]]) |>
    select(tableau, titre, all_of(dims), annee_scolaire, annee,
           sexe, valeur_brute)
}

# Type C : une colonne de libellés (région), années en colonnes (indicateurs : IPS, TBS...)
extraire_C <- function(bloc, st) {
  d <- corps_bloc(bloc, st, propager = FALSE)
  empiler(d, st) |>
    select(tableau, titre, all_of(st$noms), annee_scolaire, annee,
           valeur_brute)
}

# Type D : région + statut (ou autre dimension) avec lignes "Total <région>"
extraire_D <- function(bloc, st) {
  lib1 <- st$noms[1]
  lib2 <- tail(st$noms, 1)
  d    <- corps_bloc(bloc, st, propager = FALSE)

  est_tot  <- \(x) str_detect(coalesce(x, ""), "(?i)^\\s*total\\b")
  nom_reg  <- \(x) {
    r <- str_squish(str_remove(x, "(?i)^\\s*total\\s*(g[eé]n[eé]ral\\s*)?((de|du|des)\\s+)?"))
    if_else(r == "", "NATIONAL", r)
  }

  d <- d |>
    mutate(
      est_total = est_tot(.data[[lib1]]) | est_tot(.data[[lib2]]),
      !!lib1 := if_else(est_tot(.data[[lib1]]), nom_reg(.data[[lib1]]), .data[[lib1]]),
      !!lib2 := if_else(est_total, "Total", .data[[lib2]])
    ) |>
    fill(all_of(lib1))                      # propage la région fusionnée vers le bas

  empiler(d, st) |>
    select(tableau, titre, all_of(st$noms), annee_scolaire, annee,
           valeur_brute, est_total)
}

# ---------------------------------------------------------------------
# 4. Aiguillage automatique
# ---------------------------------------------------------------------
extraire_bloc <- function(bloc) {
  st <- preparer_bloc(bloc)
  if (is.null(st)) return(NULL)
  type <- detecter_type(bloc, st)
  res <- switch(type,
                A = extraire_A(bloc, st),
                B = extraire_B(bloc, st),
                C = extraire_C(bloc, st),
                D = extraire_D(bloc, st)
  )
  mutate(res, type_tableau = type, .before = 1)
}

extraire_feuille <- function(fichier, feuille) {
  raw <- lire_feuille(fichier, feuille)
  if (!"c1" %in% names(raw)) return(list())
  debuts <- which(str_detect(raw$c1, "^\\s*Tableau\\s*\\d+"))
  if (length(debuts) == 0) return(list())
  fins <- c(debuts[-1] - 1, nrow(raw))

  map2(debuts, fins, \(d, f) extraire_bloc(raw[d:f, ])) |>
    compact() |>
    map(\(x) mutate(x, feuille = feuille))
}

# ---------------------------------------------------------------------
# 5. Extraction complète : une table par structure + un catalogue
# ---------------------------------------------------------------------
extraire_tout <- function(fichier) {
  blocs <- excel_sheets(fichier) |>
    map(\(s) extraire_feuille(fichier, s)) |>
    unlist(recursive = FALSE)

  colonnes_std <- c("type_tableau", "tableau", "titre", "annee_scolaire", "annee",
                    "sexe", "valeur", "valeur_brute", "est_total", "feuille")

  # signature = type + colonnes de libellés (ex. "A_region", "D_region_statut")
  signature <- map_chr(blocs, \(x)
                       paste0(x$type_tableau[1], "_", paste(setdiff(names(x), colonnes_std), collapse = "_")))

  tables <- blocs |>
    split(signature) |>
    map(bind_rows)

  catalogue <- tibble(
    feuille  = map_chr(blocs, \(x) x$feuille[1]),
    tableau  = map_chr(blocs, \(x) x$tableau[1]),
    type     = map_chr(blocs, \(x) x$type_tableau[1]),
    table    = signature,
    n_lignes = map_int(blocs, nrow),
    titre    = map_chr(blocs, \(x) x$titre[1])
  )

  list(tables = tables, catalogue = catalogue, fichier = basename(fichier))
}

# ---------------------------------------------------------------------
# 6. Exécution et sauvegarde
# ---------------------------------------------------------------------
fichier <- fichier_dgess()
res_brut <- extraire_tout(fichier)

print(res_brut$catalogue, n = Inf)
saveRDS(res_brut, FIC$interim_dgess)
message("DGESS extrait : ", length(res_brut$tables), " tables -> ", FIC$interim_dgess)
