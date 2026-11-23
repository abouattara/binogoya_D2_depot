# =====================================================================
# 01 COLLECTE - Fonds de carte : extraction des archives et inventaire
#
# Entrée : Data/shapefile/bfa_admin_boundaries.shp.zip (limites OCHA)
#          Data/shapefile/hotosm_bfa_education_facilities_osm_shp.zip (écoles OSM)
# Sortie : Data/shapefile/admin_boundaries/ et Data/shapefile/education_facilities/
#          Data/interimdata/binogoya_01-collecte_D2_engineer_geo-inventaire_v1.csv
# Les couches sont ensuite lues directement par l'analyse (04) et l'application (05).
# =====================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("sf"))

library(sf)

# --- Extraction
for (z in c(FIC$zip_admin, FIC$zip_ecoles)) exiger(z, "déposer l'archive dans Data/shapefile/")
unzip(FIC$zip_ecoles, exdir = CH$shp_ecoles)
unzip(FIC$zip_admin,  exdir = CH$shp_admin)

# --- Lister les .shp extraits
shp_ecoles <- list.files(CH$shp_ecoles, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
shp_admin  <- list.files(CH$shp_admin,  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
print(basename(shp_ecoles))
print(basename(shp_admin))

# --- Lecture de contrôle et inventaire (niveau, géométrie, CRS, colonnes)
inventaire <- do.call(rbind, lapply(c(shp_admin, shp_ecoles), \(p) {
  x <- st_read(p, quiet = TRUE)
  data.frame(
    fichier   = basename(p),
    dossier   = basename(dirname(p)),
    n_objets  = nrow(x),
    geometrie = paste(unique(as.character(st_geometry_type(x))), collapse = "/"),
    crs       = if (is.na(st_crs(x))) NA_character_ else st_crs(x)$input,
    colonnes  = paste(setdiff(tolower(names(x)), "geometry"), collapse = ", ")
  )
}))
print(inventaire[, c("fichier", "n_objets", "geometrie", "crs")])
write.csv(inventaire, FIC$interim_geo, row.names = FALSE, fileEncoding = "UTF-8")

# --- Aperçu rapide (seulement en session interactive)
if (interactive() && length(shp_admin) > 0 && length(shp_ecoles) > 0) {
  admin1 <- st_transform(st_read(shp_admin[1], quiet = TRUE), 4326)
  ecoles <- st_transform(st_read(shp_ecoles[1], quiet = TRUE), 4326)
  plot(st_geometry(admin1))
  plot(st_geometry(ecoles), add = TRUE, pch = 20, cex = 0.3, col = "red")
}
message("Inventaire des fonds de carte -> ", FIC$interim_geo)
