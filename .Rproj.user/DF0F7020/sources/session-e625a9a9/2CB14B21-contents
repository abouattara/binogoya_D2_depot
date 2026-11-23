# =============================================================================
# binogoya - Défi 2 : exécution de toute la chaîne, dans l'ordre
#
# Ouvrir binogoya_D2_depot.Rproj dans RStudio, puis :
#   source("binogoya_00-pipeline_D2_engineer_executer-tout_v1.R")
# Chaque script tourne dans un environnement séparé et lit les fichiers écrits
# par l'étape précédente : on peut aussi lancer un script seul, à condition que
# ses fichiers d'entrée existent.
# =============================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))

etapes <- c(
  # 01 - collecte
  "01_collecte/binogoya_01-collecte_D2_engineer_dgess-extraction_v1.R",
  "01_collecte/binogoya_01-collecte_D2_engineer_opendata-lecture_v1.R",
  "01_collecte/binogoya_01-collecte_D2_engineer_conasur-lecture_v1.R",
  "01_collecte/binogoya_01-collecte_D2_engineer_geo-extraction_v1.R",
  # 02 - nettoyage
  "02_nettoyage/binogoya_02-nettoyage_D2_engineer_dgess-nettoyage_v1.R",
  "02_nettoyage/binogoya_02-nettoyage_D2_engineer_opendata-nettoyage_v1.R",
  "02_nettoyage/binogoya_02-nettoyage_D2_engineer_conasur-nettoyage_v1.R",
  # 03 - transformation
  "03_transformation/binogoya_03-transformation_D2_engineer_dgess-tables-analyse_v1.R",
  "03_transformation/binogoya_03-transformation_D2_engineer_geo-correspondance_v1.R",
  # 04 - analyse
  "04_analyse/binogoya_04-analyse_D2_analyst_ecarts-education_v1.R",
  "04_analyse/binogoya_04-analyse_D2_analyst_parite-tendances_v1.R"
)

for (f in etapes) {
  message("\n==================== ", f, " ====================")
  debut <- Sys.time()
  source(here::here(f), local = new.env(), encoding = "UTF-8")
  message("Terminé en ", round(difftime(Sys.time(), debut, units = "secs")), " s")
}
# 05 - interprétation : rapport (HTML) ; l'application se lance à part
message("\n==================== 05_interpretation : rapport ====================")
installer("rmarkdown")
rmarkdown::render(here::here("05_interpretation", "binogoya_05-interpretation_D2_analyst_rapport-ecarts_v1.Rmd"),
                  envir = new.env(), quiet = TRUE)

message("\nChaîne terminée. Pour ouvrir l'application :\n",
        '  shiny::runApp(here::here("05_interpretation", "binogoya_05-interpretation_D2_analyst_app-shiny_v1.R"))')