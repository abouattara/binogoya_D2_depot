# =============================================================================
# binogoya - Défi 2 : configuration commune (chemins, noms de fichiers, packages)
#
# Tous les scripts du projet commencent par :
#   source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
# Les chemins sont relatifs à la racine du projet (fichier binogoya_D2_depot.Rproj)
# grâce au package here : le projet fonctionne sur n'importe quel ordinateur,
# quel que soit le dossier où il est copié et le répertoire de travail courant.
#
# Circulation des données :
#   Data/rawdata        sources officielles (non modifiées)
#     -> 01_collecte    lecture et mise en forme, valeurs laissées en texte
#   Data/interimdata    tables extraites, non nettoyées
#     -> 02_nettoyage   conversion numérique, dates, sexe, Pcodes, contrôles
#   Data/cleandata      tables propres (entrée de l'analyse)
#     -> 03_transformation  sélection et mise en forme par thème
#   Data/transformdata  tables d'analyse par thème
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")

# --- Installation des packages manquants (appelée par chaque script)
installer <- function(pkgs) {
  manq <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(manq) > 0) {
    message("Installation des packages manquants : ", paste(manq, collapse = ", "))
    install.packages(manq, repos = "https://cloud.r-project.org")
  }
  invisible(TRUE)
}

# --- Identité du dépôt
EQUIPE  <- "binogoya"
DEFI    <- "D2"
VERSION <- "v1"

# --- Nom de fichier conforme : equipe_etape_defi_role_description_version.ext
nf <- function(etape, role, description, ext, version = VERSION) {
  etape <- sub("_", "-", etape)                       # 02_nettoyage -> 02-nettoyage
  paste0(paste(EQUIPE, etape, DEFI, role, description, version, sep = "_"), ".", ext)
}

# --- Dossiers
CH <- list(
  racine      = here::here(),
  raw         = here::here("Data", "rawdata"),
  raw_conasur = here::here("Data", "rawdata", "CONASUR"),
  shp         = here::here("Data", "shapefile"),
  shp_admin   = here::here("Data", "shapefile", "admin_boundaries"),
  shp_ecoles  = here::here("Data", "shapefile", "education_facilities"),
  interim     = here::here("Data", "interimdata"),
  clean       = here::here("Data", "cleandata"),
  transform   = here::here("Data", "transformdata"),
  sorties     = here::here("04_analyse", "sorties"),
  figures     = here::here("04_analyse", "sorties", "figures"),
  interp      = here::here("05_interpretation")
)
for (d in CH) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# --- Sous-dossiers OPENDATA (un dossier = une famille de fichiers)
DOSSIERS_OPENDATA <- c(
  EFF_PAR_CYCLE         = here::here("Data", "rawdata", "EFF_PAR_CYCLE"),
  NB_ECOLE_SALLE        = here::here("Data", "rawdata", "NB_ECOLE_SALLE"),
  TAUX_SCO_6_16_COMMUNE = here::here("Data", "rawdata", "TAUX_SCO_6_16_COMMUNE"),
  TAUX_SCO_REGION       = here::here("Data", "rawdata", "TAUX_SCO_REGION")
)

# --- Fichier source DGESS (trouvé par motif : le nom officiel contient des espaces)
fichier_dgess <- function() {
  f <- list.files(CH$raw, "PRESCOLAIRE.*PRIMAIRE.*\\.xlsx$", ignore.case = TRUE, full.names = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]
  if (length(f) == 0) stop("Fichier DGESS introuvable dans ", CH$raw,
                           " (nom attendu : DONNEES_STATISTQUES_ PRESCOLAIRE ET PRIMAIRE_2016_2025_DCB.xlsx)")
  f[1]
}

# --- Fichiers échangés entre étapes (chaque étape écrit, la suivante lit)
FIC <- list(
  # sources
  conasur_brut = file.path(CH$raw_conasur, "idps4.xlsx"),
  zip_admin    = file.path(CH$shp, "bfa_admin_boundaries.shp.zip"),
  zip_ecoles   = file.path(CH$shp, "hotosm_bfa_education_facilities_osm_shp.zip"),
  # 01 - collecte
  interim_dgess    = file.path(CH$interim, nf("01_collecte", "engineer", "dgess-extrait", "rds")),
  interim_opendata = file.path(CH$interim, nf("01_collecte", "engineer", "opendata-extrait", "rds")),
  interim_conasur  = file.path(CH$interim, nf("01_collecte", "engineer", "conasur-extrait", "rds")),
  interim_geo      = file.path(CH$interim, nf("01_collecte", "engineer", "geo-inventaire", "csv")),
  # 02 - nettoyage
  dgess             = file.path(CH$clean, nf("02_nettoyage", "engineer", "dgess-tables", "rds")),
  dgess_controles   = file.path(CH$clean, nf("02_nettoyage", "engineer", "dgess-controles", "xlsx")),
  opendata          = file.path(CH$clean, nf("02_nettoyage", "engineer", "opendata-tables", "rds")),
  opendata_controles = file.path(CH$clean, nf("02_nettoyage", "engineer", "opendata-controles", "xlsx")),
  conasur           = file.path(CH$clean, nf("02_nettoyage", "engineer", "conasur-pdi", "rds")),
  conasur_controles = file.path(CH$clean, nf("02_nettoyage", "engineer", "conasur-controles", "xlsx")),
  # 03 - transformation
  dgess_analyse_rds  = file.path(CH$transform, nf("03_transformation", "engineer", "dgess-tables-analyse", "rds")),
  dgess_analyse_xlsx = file.path(CH$transform, nf("03_transformation", "engineer", "dgess-tables-analyse", "xlsx")),
  geo_corresp_rds    = file.path(CH$transform, nf("03_transformation", "engineer", "geo-correspondance", "rds")),
  geo_corresp_xlsx   = file.path(CH$transform, nf("03_transformation", "engineer", "geo-correspondance", "xlsx")),
  conasur_geo        = file.path(CH$transform, nf("03_transformation", "engineer", "conasur-geo", "rds")),
  opendata_prov      = file.path(CH$transform, nf("03_transformation", "engineer", "opendata-provinces", "rds")),
  # table polygone -> province des données : écrite par la transformation géographique (03),
  # lue par l'analyse (04) et l'application (05) ; nom conservé pour ne pas modifier ces scripts
  corresp_prov_csv   = file.path(CH$shp, nf("04_analyse", "analyst", "correspondance-provinces", "csv")),
  # 04 - analyse
  resultats_rds  = file.path(CH$sorties, nf("04_analyse", "analyst", "resultats-ecarts", "rds")),
  resultats_xlsx = file.path(CH$sorties, nf("04_analyse", "analyst", "resultats-ecarts", "xlsx")),
  tableau_bord   = file.path(CH$sorties, nf("04_analyse", "analyst", "tableau-de-bord", "html")),
  # 05 - interprétation
  cache_shiny = file.path(CH$sorties, nf("05_interpretation", "analyst", "cache-shiny", "rds"))
)

# --- Chemin d'une figure : chemin_fig("01_tableau_ecarts_genre")
#     -> 04_analyse/sorties/figures/binogoya_04-analyse_D2_analyst_fig01-tableau-ecarts-genre_v1.png
chemin_fig <- function(nom) {
  file.path(CH$figures, nf("04_analyse", "analyst", paste0("fig", gsub("_", "-", nom)), "png"))
}

# --- Vérifie qu'un fichier d'entrée existe, sinon indique le script à lancer
exiger <- function(f, script) {
  if (!file.exists(f)) stop("Fichier introuvable : ", f, "\nLancez d'abord : ", script, call. = FALSE)
  invisible(f)
}