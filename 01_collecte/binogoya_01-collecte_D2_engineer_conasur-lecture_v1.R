# =====================================================================
# 01 COLLECTE - CONASUR : lecture de idps4.xlsx
#
# Pourquoi un seul fichier : idps4 contient tous les lieux et dates des autres fichiers
# (sommes identiques sur les clés communes). Les autres ne servaient qu'à le vérifier.
#
# Entrée : Data/rawdata/CONASUR/idps4.xlsx
# Sortie : Data/interimdata/binogoya_01-collecte_D2_engineer_conasur-extrait_v1.rds
#          liste (donnees, hors_modele, inconnues) ; colonnes renommées, valeurs en texte.
#          Typage, corrections de Pcodes et contrôles : 02_nettoyage.
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
# 0. Paramètres
# ---------------------------------------------------------------------
fichier <- FIC$conasur_brut

# nom nettoyé de l'en-tête Excel -> nom final
correspondance <- c(
  source                    = "source",
  pays_destination          = "pays",
  region_destination        = "region",
  province_destination      = "province",
  commune_destination       = "commune",
  ville_village_destination = "ville_village",
  site_camp                 = "site_camp",
  hommes                    = "hommes",
  femmes                    = "femmes",
  enfants_de_moins_de_5_ans = "enfants_moins_5",
  enfants_de_plus_de_5_ans  = "enfants_plus_5",
  total_enfants             = "total_enfants",
  total_individus           = "total_individus",
  admin_1_pcode             = "admin1_pcode",
  admin_2_pcode             = "admin2_pcode",
  admin_3_pcode             = "admin3_pcode",
  date_donnnee              = "date_donnee"
)

# ---------------------------------------------------------------------
# 1. Outils
# ---------------------------------------------------------------------
nettoyer_nom <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |> str_remove_all("^_+|_+$")
}

# ---------------------------------------------------------------------
# 2. Lecture : en-tête en ligne 1, ligne de balises HXL (#adm1+name...) ignorée
# ---------------------------------------------------------------------
lire_idps4 <- function(fichier, feuille = 1) {
  if (!file.exists(fichier)) stop("Fichier introuvable : ", fichier)

  brut <- suppressMessages(
    read_excel(fichier, sheet = feuille, col_names = FALSE,
               col_types = "text", .name_repair = "minimal")
  )
  m <- as.matrix(brut)
  m[] <- str_squish(m)
  m[!is.na(m) & m == ""] <- NA

  noms <- nettoyer_nom(coalesce(m[1, ], ""))
  noms[noms == ""] <- paste0("colonne_", which(noms == ""))
  noms <- make.unique(noms, sep = "_")

  corps <- m[-1, , drop = FALSE]
  colnames(corps) <- noms
  d <- as_tibble(corps) |> mutate(ligne_excel = row_number() + 1L)

  # lignes entièrement vides
  d <- d |> filter(if_any(-ligne_excel, \(x) !is.na(x)))

  # ligne(s) HXL : la majorité des cellules renseignées commence par "#"
  est_hxl <- d |>
    select(-ligne_excel) |>
    pmap_lgl(\(...) {
      v <- c(...)
      v <- v[!is.na(v)]
      length(v) > 0 && mean(str_starts(v, "#")) >= 0.5
    })
  if (any(est_hxl)) {
    message("Ligne(s) HXL ignorée(s) : ligne(s) Excel ", paste(d$ligne_excel[est_hxl], collapse = ", "))
  }
  d[!est_hxl, ]
}

# Noms finaux et mise de côté des colonnes hors modèle (sans typage)
renommer_idps4 <- function(d) {
  absentes <- setdiff(names(correspondance), names(d))
  if (length(absentes) > 0) {
    stop("Colonnes attendues absentes (noms nettoyés) : ", paste(absentes, collapse = ", "),
         "\nColonnes trouvées : ", paste(names(d), collapse = ", "))
  }
  d <- d |> rename(!!!set_names(names(correspondance), unname(correspondance)))

  # colonnes sans nom / hors modèle (colonne_17...) : écartées de la table, gardées à part
  inconnues   <- setdiff(names(d), c(unname(correspondance), "ligne_excel"))
  hors_modele <- d |>
    select(ligne_excel, any_of(c("commune", "date_donnee")), all_of(inconnues)) |>
    filter(if_any(all_of(inconnues), \(x) !is.na(x)))
  d <- d |> select(-all_of(inconnues))

  list(donnees = d, hors_modele = hors_modele, inconnues = inconnues)
}

# ---------------------------------------------------------------------
# 3. Exécution et sauvegarde
# ---------------------------------------------------------------------
lecture <- lire_idps4(fichier) |> renommer_idps4()

cat("\nLignes lues :", nrow(lecture$donnees),
    "\nColonnes hors modèle :", paste(lecture$inconnues, collapse = ", "), "\n")

saveRDS(lecture, FIC$interim_conasur)
message("CONASUR extrait -> ", FIC$interim_conasur)
