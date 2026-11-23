# =============================================================================
# 01 COLLECTE - OPENDATA (INSD) : lecture des fichiers à en-tête multi-lignes
#
# Entrée : Data/rawdata/EFF_PAR_CYCLE/, NB_ECOLE_SALLE/, TAUX_SCO_6_16_COMMUNE/,
#          TAUX_SCO_REGION/ (tous les .xlsx de chaque dossier)
# Sortie : Data/interimdata/binogoya_01-collecte_D2_engineer_opendata-extrait_v1.rds
#          liste par dossier, puis par fichier ; valeurs laissées en texte (valeur_txt),
#          conversion numérique faite en 02_nettoyage.
#
# Structure attendue d'un fichier :
#   - colonne A, lignes d'en-tête : libellé de la ligne (Indicateur, Région,
#     Rubrique, Sexe, Scale, Units, ...)
#   - colonnes B, C, ... : une série par colonne (modalités dans l'en-tête)
#   - à partir de la 1re ligne dont la colonne A est une année : les données
# =============================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("readxl", "dplyr", "tidyr", "tibble", "purrr", "stringi"))

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# -- Utilitaire : transforme les libellés de la colonne A en noms de colonnes --
nettoyer_noms <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x[is.na(x) | x == ""] <- "champ"
  make.unique(x, sep = "_")
}

# -----------------------------------------------------------------------------
# lire_opendata() : lit UN fichier et renvoie une table LONGUE
# Résultat : annee | <une colonne par ligne d'en-tête> | valeur_txt | fichier
# -----------------------------------------------------------------------------
lire_opendata <- function(chemin, feuille = 1, verbose = TRUE) {

  # 1. Lecture brute, tout en texte (pas de devinette de types)
  brut <- suppressMessages(
    readxl::read_excel(chemin, sheet = feuille, col_names = FALSE, col_types = "text")
  )
  m <- as.matrix(brut)
  m[] <- trimws(m)
  m[!is.na(m) & m == ""] <- NA

  # 2. Repérer la 1re ligne de données : colonne A = année (ex. 1997)
  col_a     <- m[, 1]
  est_annee <- !is.na(col_a) & grepl("^(19|20)[0-9]{2}(\\.0+)?$", col_a)
  if (!any(est_annee)) stop("Aucune année trouvée en colonne A : ", basename(chemin))
  debut <- which(est_annee)[1]
  if (debut < 2) stop("Pas de lignes d'en-tête avant les données : ", basename(chemin))
  n_ent <- debut - 1

  noms_ent <- nettoyer_noms(col_a[seq_len(n_ent)])
  if (verbose) {
    message(basename(chemin), " : ", n_ent, " lignes d'en-tête -> ",
            paste(noms_ent, collapse = ", "))
  }

  # 3. Séparer en-tête et corps (on ignore les notes de bas de page)
  ent   <- m[seq_len(n_ent), -1, drop = FALSE]
  corps <- m[debut:nrow(m), , drop = FALSE]
  corps <- corps[est_annee[debut:nrow(m)], , drop = FALSE]

  # 4. Garder uniquement les colonnes qui contiennent quelque chose
  utiles <- which(colSums(!is.na(ent)) > 0 |
                    colSums(!is.na(corps[, -1, drop = FALSE])) > 0)

  # 5. Table des métadonnées : une ligne par colonne de données
  meta <- as_tibble(t(ent[, utiles, drop = FALSE]), .name_repair = "minimal")
  names(meta) <- noms_ent
  meta$.col <- utiles

  # 6. Table longue des valeurs (texte)
  vals <- corps[, utiles + 1, drop = FALSE]
  colnames(vals) <- paste0("v", utiles)

  long <- as_tibble(vals) %>%
    mutate(annee = as.integer(as.numeric(corps[, 1]))) %>%
    pivot_longer(starts_with("v"), names_to = ".col", values_to = "valeur_txt") %>%
    mutate(.col = as.integer(sub("^v", "", .col)))

  # 7. Jointure avec les métadonnées
  long %>%
    left_join(meta, by = ".col") %>%
    mutate(fichier = tools::file_path_sans_ext(basename(chemin))) %>%
    select(annee, all_of(noms_ent), valeur_txt, fichier) %>%
    arrange(annee)
}

# -----------------------------------------------------------------------------
# lire_dossier() : lit tous les .xlsx d'un répertoire
# Renvoie une LISTE nommée (une table par fichier), car les structures diffèrent.
# -----------------------------------------------------------------------------
lire_dossier <- function(dossier, motif = "\\.xlsx$", recursif = FALSE, ...) {
  fichiers <- list.files(dossier, pattern = motif, full.names = TRUE, recursive = recursif)
  fichiers <- fichiers[!grepl("^~\\$", basename(fichiers))]   # fichiers temporaires Excel
  if (length(fichiers) == 0) stop("Aucun fichier trouvé dans : ", dossier)

  res <- map(fichiers, function(f) lire_opendata(f, ...))
  names(res) <- tools::file_path_sans_ext(basename(fichiers))
  res
}

# =============================================================================
# Exécution : un élément par dossier source
# =============================================================================
opendata_brut <- imap(DOSSIERS_OPENDATA, \(dossier, nom) {
  if (!dir.exists(dossier)) { warning("Dossier OPENDATA absent : ", dossier, call. = FALSE); return(NULL) }
  message("--- ", nom)
  lire_dossier(dossier)
}) |> compact()

# Inventaire
iwalk(opendata_brut, \(lst, nom) cat("\n", nom, ":\n  ", paste(names(lst), collapse = "\n   "), "\n", sep = ""))

saveRDS(opendata_brut, FIC$interim_opendata)
message("OPENDATA extrait : ", sum(lengths(opendata_brut)), " fichiers -> ", FIC$interim_opendata)
