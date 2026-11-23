# =====================================================================
# 03 TRANSFORMATION - Géographie : correspondance données <-> polygones
#
# Problème : les données (CONASUR, OpenData) suivent l'ancien découpage
#   (13 régions, 45 provinces, Pcodes CONASUR) ; le fond de carte OCHA suit
#   le découpage 2025 (17 régions, 47 provinces), avec des Pcodes en partie
#   différents. Sans table de passage, des polygones restent gris, ou
#   reçoivent la valeur d'une autre province (même Pcode, autre province).
#
# Méthode : pont par les COMMUNES, unité la plus stable entre les découpages
#   1. chaque commune CONASUR est rattachée à une commune du fond de carte :
#      Pcode contrôlé par le nom, puis nom identique, puis nom proche ;
#   2. chaque polygone province reçoit l'ancienne province dont viennent la
#      majorité de ses communes ; à défaut le Pcode, puis le nom ;
#   3. la nomenclature 2025 (noms officiels des nouvelles provinces) et les
#      corrections manuelles (corrections_provinces, en G0) sont prioritaires ;
#   4. une province scindée en 2025 garde ses valeurs sur son polygone
#      principal ; les nouvelles provinces en héritent si HERITER_NOUVELLES
#      = TRUE (colonne valeur_heritee), sinon elles restent sans valeur.
#   Les Pcodes d'origine ne sont jamais écrasés : les Pcodes du fond de carte
#   sont AJOUTÉS (suffixe _shp).
#
# Entrées : Data/cleandata/binogoya_02-nettoyage_D2_engineer_conasur-pdi_v1.rds
#           Data/cleandata/binogoya_02-nettoyage_D2_engineer_opendata-tables_v1.rds
#           Data/shapefile/admin_boundaries/*.shp (01_collecte/...geo-extraction_v1.R)
# Sorties : Data/transformdata/
#   binogoya_03-transformation_D2_engineer_geo-correspondance_v1.rds
#     liste : provinces, communes, regions, opendata_noms, polygones (sf), controles
#   binogoya_03-transformation_D2_engineer_geo-correspondance_v1.xlsx (un onglet par table)
#   binogoya_03-transformation_D2_engineer_conasur-geo_v1.rds
#     CONASUR + adm3/adm2/adm1_pcode_shp (Pcodes du fond de carte)
#   binogoya_03-transformation_D2_engineer_opendata-provinces_v1.rds
#     $donnees : taux par province, ancien découpage (45 provinces, admin2_pcode)
#     $carte   : mêmes taux portés sur les polygones (adm2_pcode_shp, valeur_heritee)
#   Data/shapefile/binogoya_04-analyse_D2_analyst_correspondance-provinces_v1.csv
#     table polygone -> province des données, à l'endroit où l'analyse (04) et
#     l'application (05) la lisent déjà. Elle est RÉÉCRITE à chaque exécution :
#     les corrections se font dans corrections_provinces (G0), pas dans Excel.
# =====================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("sf", "dplyr", "tidyr", "stringr", "purrr", "stringi", "tibble", "readr", "writexl"))

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr); library(stringr)
  library(purrr); library(stringi); library(tibble)
})
sf_use_s2(FALSE)

if (is.null(FIC$geo_corresp_rds))
  stop("Entrées geo_* absentes de FIC : mettez à jour binogoya_00-config_D2_engineer_chemins_v1.R", call. = FALSE)

exiger(FIC$conasur,  "02_nettoyage/binogoya_02-nettoyage_D2_engineer_conasur-nettoyage_v1.R")
exiger(FIC$opendata, "02_nettoyage/binogoya_02-nettoyage_D2_engineer_opendata-nettoyage_v1.R")
conasur <- as_tibble(readRDS(FIC$conasur))
od      <- readRDS(FIC$opendata)

# ---------------------------------------------------------------------
# G0. Paramètres
# ---------------------------------------------------------------------
HERITER_NOUVELLES <- TRUE   # nouvelles provinces 2025 : valeurs de la province d'origine
SEUIL_NOM <- 0.25           # distance d'édition relative maximale pour un « nom proche »
PART_MIN  <- 0.5            # part minimale de communes pour attribuer une ancienne province

# Découpage 2025 (décrets des 2 juillet et 12 août 2025) : nom du polygone -> ancienne province.
#   Le Soum est scindé en Djelgodji (Djibo : Baraboulé, Diguel, Djibo, Kelbo, Nassoumbou,
#   Pobé-Mengao, Tongomayel) et Karo-Peli (Arbinda : Arbinda, Koutougou) ; la Tapoa en
#   Gobnangou (Diapaga) et Dyamongou (Kantchari). nouvelle = TRUE : province créée,
#   FALSE : ancienne province renommée (elle garde le rôle de polygone principal).
#   Les noms sont comparés après normalisation, avec une tolérance d'orthographe.
nomenclature_2025 <- tribble(
  ~province_shp,   ~province_data, ~nouvelle, ~changement,
  "Djelgodji",     "Soum",         FALSE,     "Soum renommé (Djibo), amputé d'Arbinda et Koutougou",
  "Karo-Peli",     "Soum",         TRUE,      "nouvelle province issue du Soum (Arbinda, Koutougou)",
  "Gobnangou",     "Tapoa",        FALSE,     "Tapoa renommée (Diapaga), amputée de la zone de Kantchari",
  "Dyamongou",     "Tapoa",        TRUE,      "nouvelle province issue de la Tapoa (Kantchari)",
  "Sandbondtenga", "Sanmatenga",   FALSE,     "changement de nom",
  "Bassitenga",    "Oubritenga",   FALSE,     "changement de nom",
  "Kossin",        "Kossi",        FALSE,     "changement de nom"
)

# Corrections de provinces (prioritaires sur tout le reste) : Pcode du polygone -> Pcode
# CONASUR de l'ancienne province. À remplir à partir de l'onglet provinces_a_verifier.
#   ex. adm2_pcode_shp = "BF5601", admin2_pcode = "BF5603"
corrections_provinces <- tibble(adm2_pcode_shp = character(), admin2_pcode = character(),
                                commentaire = character())

# Corrections manuelles de communes (prioritaires). Exemple :
#   commune_data = "Koundougou", admin3_pcode_data = "BF540305", adm3_pcode_shp = "BF..."
corrections_communes <- tibble(
  commune_data      = character(),
  admin3_pcode_data = character(),
  adm3_pcode_shp    = character(),
  commentaire       = character()
)

# Noms OpenData à forcer vers une province (Pcode CONASUR). Exemple :
#   nom_src = "Komandjari", admin2_pcode = "BF4802"
corrections_opendata <- tibble(nom_src = character(), admin2_pcode = character())

# ---------------------------------------------------------------------
# G1. Outils
# ---------------------------------------------------------------------
norm_lib <- function(x) {
  x |> as.character() |> stri_trans_general("Latin-ASCII") |> str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "")
}
# « Les Balé » = « Balé »
cle_province <- function(x) sub("^les(?=[a-z]{3,})", "", norm_lib(x), perl = TRUE)
# Clé région sur l'ancien découpage (identique à 04_analyse)
cle_region <- function(x) {
  k <- norm_lib(x)
  case_when(
    k %in% c("national", "burkinafaso", "totalgeneral", "bfa", "bf", "ensemble") ~ "national",
    k == "hautbassins" ~ "hautsbassins",
    k == "centresansouagadougou" ~ "centre",
    TRUE ~ k
  )
}

# Distance d'édition relative, élément par élément
d_paire <- function(a, b) {
  vapply(seq_along(a), \(i) {
    if (is.na(a[i]) || is.na(b[i])) return(NA_real_)
    drop(adist(a[i], b[i])) / max(nchar(a[i]), nchar(b[i]), 1)
  }, numeric(1))
}

# Rattache chaque clé à une clé de référence : nom identique (homonymes départagés
# par le contexte, ex. la province), puis nom proche s'il est unique et sous le seuil.
rapprocher <- function(cles, ref, ctx = NULL, ref_ctx = NULL, seuil = SEUIL_NOM) {
  n <- length(cles)
  j <- rep(NA_integer_, n); met <- rep(NA_character_, n); dist <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    k <- cles[i]
    if (is.na(k) || !nzchar(k) || length(ref) == 0) next
    ex <- which(ref == k)
    if (length(ex) > 1 && !is.null(ctx)) {
      ex2 <- ex[coalesce(ref_ctx[ex] == ctx[i], FALSE)]
      if (length(ex2) == 1) { j[i] <- ex2; met[i] <- "nom identique (homonyme départagé)"; dist[i] <- 0; next }
    }
    if (length(ex) == 1) { j[i] <- ex; met[i] <- "nom identique"; dist[i] <- 0; next }
    if (length(ex) > 1)  { met[i] <- "homonymes non départagés"; next }
    d <- drop(adist(k, ref)) / pmax(nchar(k), nchar(ref), 1)
    d[is.na(ref)] <- NA
    m <- suppressWarnings(min(d, na.rm = TRUE))
    if (is.finite(m) && m <= seuil && sum(d == m, na.rm = TRUE) == 1) {
      j[i] <- which(d == m)[1]; met[i] <- "nom proche (à vérifier)"; dist[i] <- m
    } else if (is.finite(m)) dist[i] <- m
  }
  tibble(j = j, methode = met, distance = dist)
}

# Couches de POLYGONES seulement (l'archive OCHA contient aussi lignes et points),
# noms de colonnes en minuscules, géométries réparées, WGS84
lire_couches <- function(dir) {
  f <- list.files(dir, "\\.shp$", recursive = TRUE, full.names = TRUE)
  map(f, \(p) tryCatch({
    x <- st_read(p, quiet = TRUE)
    if (!all(as.character(st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) return(NULL)
    d <- st_drop_geometry(x); names(d) <- make.unique(tolower(names(d)), sep = "_")
    x <- st_make_valid(st_sf(d, geometry = st_geometry(x)))
    if (is.na(st_crs(x))) st_set_crs(x, 4326) else st_transform(x, 4326)
  }, error = \(e) {
    warning("Couche illisible : ", basename(p), " (", conditionMessage(e), ")", call. = FALSE)
    NULL
  })) |>
    set_names(basename(f)) |> compact()
}
couche <- function(lst, avec, sans = character()) {
  for (x in lst) if (all(avec %in% names(x)) && !any(sans %in% names(x))) return(x)
  NULL
}
nom_col <- function(x, cands) intersect(cands, names(x))[1]

# ---------------------------------------------------------------------
# G2. Fond de carte : provinces, communes, régions (découpage 2025)
# ---------------------------------------------------------------------
couches <- lire_couches(CH$shp_admin)
if (length(couches) == 0)
  stop("Aucune couche de polygones dans ", CH$shp_admin,
       " : lancez 01_collecte/binogoya_01-collecte_D2_engineer_geo-extraction_v1.R", call. = FALSE)
sh_prov <- couche(couches, "adm2_pcode", sans = "adm3_pcode")
sh_com  <- couche(couches, "adm3_pcode")
if (is.null(sh_prov)) stop("Couche provinces (adm2_pcode sans adm3_pcode) introuvable.", call. = FALSE)

nc2  <- nom_col(sh_prov, c("adm2_fr", "adm2_name1", "adm2_name"))
nc1  <- nom_col(sh_prov, c("adm1_fr", "adm1_name", "adm1_name1"))
prov_shp <- tibble(
  adm2_pcode_shp = as.character(sh_prov$adm2_pcode),
  province_shp   = if (!is.na(nc2)) as.character(sh_prov[[nc2]]) else adm2_pcode_shp,
  adm1_pcode_shp = if ("adm1_pcode" %in% names(sh_prov)) as.character(sh_prov$adm1_pcode) else NA_character_,
  region_shp     = if (!is.na(nc1)) as.character(sh_prov[[nc1]]) else NA_character_
) |>
  mutate(province_cle = cle_province(province_shp))
if (anyDuplicated(prov_shp$adm2_pcode_shp))
  warning("Pcodes en double dans la couche provinces : ",
          paste(unique(prov_shp$adm2_pcode_shp[duplicated(prov_shp$adm2_pcode_shp)]), collapse = ", "), call. = FALSE)

com_shp <- NULL
if (!is.null(sh_com)) {
  # Province de chaque commune : colonne adm2_pcode, sinon par localisation
  if (!"adm2_pcode" %in% names(sh_com)) {
    pts <- suppressWarnings(st_point_on_surface(st_geometry(sh_com)))
    idx <- st_within(pts, sh_prov)
    sh_com$adm2_pcode <- sh_prov$adm2_pcode[vapply(idx, \(v) if (length(v)) v[1] else NA_integer_, integer(1))]
  }
  nc3 <- nom_col(sh_com, c("adm3_fr", "adm3_name1", "adm3_name"))
  com_shp <- tibble(
    adm3_pcode_shp = as.character(sh_com$adm3_pcode),
    commune_shp    = if (!is.na(nc3)) as.character(sh_com[[nc3]]) else NA_character_,
    adm2_pcode_shp = as.character(sh_com$adm2_pcode)
  ) |>
    mutate(commune_cle = norm_lib(commune_shp)) |>
    left_join(select(prov_shp, adm2_pcode_shp, province_shp, adm1_pcode_shp, region_shp,
                     province_cle_shp = province_cle), by = "adm2_pcode_shp")
} else warning("Couche communes (adm3) absente : appariement par Pcode et nom de province seulement.", call. = FALSE)

reg_shp <- prov_shp |> filter(!is.na(adm1_pcode_shp)) |> distinct(adm1_pcode_shp, region_shp)

message("Fond de carte : ", nrow(prov_shp), " provinces, ",
        if (is.null(com_shp)) 0 else nrow(com_shp), " communes, ", nrow(reg_shp), " régions")

# ---------------------------------------------------------------------
# G3. Référentiel des données (ancien découpage, Pcodes CONASUR)
# ---------------------------------------------------------------------
for (v in c("pcode_a_verifier", "incoh_hierarchie")) if (!v %in% names(conasur)) conasur[[v]] <- FALSE
if (!"commune_cle" %in% names(conasur)) conasur$commune_cle <- norm_lib(conasur$commune)

# Une ligne par commune des données (clé : Pcode + nom, car un Pcode faux peut être partagé)
com_dat <- conasur |>
  filter(!is.na(commune_cle) | !is.na(admin3_pcode)) |>
  mutate(pcode_fiable = !coalesce(pcode_a_verifier, FALSE) & !coalesce(incoh_hierarchie, FALSE)) |>
  count(admin3_pcode, commune_cle, commune = commune_std, admin2_pcode, province = province_std,
        admin1_pcode, region = region_std, pcode_fiable, name = "n_lignes") |>
  arrange(desc(pcode_fiable), desc(n_lignes)) |>
  distinct(admin3_pcode, commune_cle, .keep_all = TRUE) |>
  mutate(province_cle = cle_province(province))

# Une ligne par ancienne province (45)
ref_prov <- com_dat |>
  filter(pcode_fiable, !is.na(admin2_pcode)) |>
  count(admin2_pcode, province, admin1_pcode, region, wt = n_lignes) |>
  arrange(desc(n)) |>
  distinct(admin2_pcode, .keep_all = TRUE) |>
  select(-n) |>
  mutate(province_cle = cle_province(province))
ref_reg <- ref_prov |> distinct(admin1_pcode, region) |> distinct(admin1_pcode, .keep_all = TRUE)
message("Données : ", nrow(ref_prov), " provinces, ", nrow(ref_reg), " régions, ", nrow(com_dat), " communes")

# ---------------------------------------------------------------------
# G4. Communes : données -> fond de carte
# ---------------------------------------------------------------------
if (!is.null(com_shp)) {
  j_pc  <- match(com_dat$admin3_pcode, com_shp$adm3_pcode_shp)
  d_pc  <- d_paire(com_dat$commune_cle, com_shp$commune_cle[j_pc])
  ok_pc <- com_dat$pcode_fiable & !is.na(j_pc) & coalesce(d_pc <= SEUIL_NOM, FALSE)
  nm    <- rapprocher(com_dat$commune_cle, com_shp$commune_cle, com_dat$province_cle, com_shp$province_cle_shp)
  
  communes <- com_dat |>
    mutate(
      methode = case_when(
        ok_pc ~ "pcode",
        !is.na(nm$j) & !is.na(j_pc) ~ paste0(nm$methode, " (Pcode contredit par le nom)"),
        !is.na(nm$j) ~ nm$methode,
        !is.na(j_pc) & pcode_fiable ~ "pcode, nom différent (à vérifier)",
        !is.na(j_pc) ~ "pcode douteux, nom introuvable",
        TRUE ~ coalesce(nm$methode, "aucune correspondance")
      ),
      j = case_when(ok_pc ~ j_pc,
                    !is.na(nm$j) ~ nm$j,
                    pcode_fiable & !is.na(j_pc) ~ j_pc,
                    TRUE ~ NA_integer_),
      distance = if_else(ok_pc, d_pc, coalesce(nm$distance, d_pc))
    )
  
  # Corrections manuelles (prioritaires)
  if (nrow(corrections_communes) > 0) {
    cm <- corrections_communes |> mutate(commune_cle = norm_lib(commune_data))
    k  <- match(paste(communes$commune_cle, communes$admin3_pcode), paste(cm$commune_cle, cm$admin3_pcode_data))
    jm <- match(cm$adm3_pcode_shp[k], com_shp$adm3_pcode_shp)
    communes <- communes |>
      mutate(j = if_else(!is.na(jm), jm, j),
             methode = if_else(!is.na(jm), "correction manuelle", methode))
  }
  
  communes <- communes |>
    mutate(adm3_pcode_shp = com_shp$adm3_pcode_shp[j],
           commune_shp    = com_shp$commune_shp[j],
           adm2_pcode_shp = com_shp$adm2_pcode_shp[j],
           province_shp   = com_shp$province_shp[j],
           adm1_pcode_shp = com_shp$adm1_pcode_shp[j],
           region_shp     = com_shp$region_shp[j]) |>
    select(-j)
} else {
  communes <- com_dat |>
    mutate(methode = "pas de couche communes", distance = NA_real_,
           adm3_pcode_shp = NA_character_, commune_shp = NA_character_, adm2_pcode_shp = NA_character_,
           province_shp = NA_character_, adm1_pcode_shp = NA_character_, region_shp = NA_character_)
}

ok_com <- c("pcode", "nom identique", "nom identique (homonyme départagé)", "correction manuelle")
dbl3 <- communes$adm3_pcode_shp[duplicated(communes$adm3_pcode_shp) & !is.na(communes$adm3_pcode_shp)]
communes <- communes |>
  mutate(pcode_change = !is.na(adm3_pcode_shp) & coalesce(admin3_pcode != adm3_pcode_shp, TRUE),
         doublon_shp  = adm3_pcode_shp %in% dbl3,
         a_verifier   = !methode %in% ok_com | doublon_shp) |>
  relocate(admin3_pcode, commune, adm3_pcode_shp, commune_shp, methode, distance, a_verifier)

print(table(communes$methode))

# ---------------------------------------------------------------------
# G5. Provinces : polygone -> ancienne province
# ---------------------------------------------------------------------
# a. Vote des communes (seules les communes au Pcode fiable votent)
lien_com <- communes |> filter(pcode_fiable, !is.na(adm2_pcode_shp), !is.na(admin2_pcode))
votes <- lien_com |>
  count(adm2_pcode_shp, admin2_pcode, name = "n_com") |>
  group_by(adm2_pcode_shp) |>
  mutate(part_com = n_com / sum(n_com), n_com_apparies = sum(n_com), n_origines = n()) |>
  slice_max(n_com, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(adm2_pcode_shp, admin2_pcode_com = admin2_pcode, n_com_apparies, part_com, n_origines)
# part de chaque ancienne province qui tombe dans chaque polygone
repart <- lien_com |>
  count(admin2_pcode, adm2_pcode_shp, name = "n") |>
  group_by(admin2_pcode) |> mutate(part_origine = n / sum(n)) |> ungroup() |>
  select(admin2_pcode_data = admin2_pcode, adm2_pcode_shp, part_origine)
n_com_shp <- if (is.null(com_shp)) tibble(adm2_pcode_shp = character(), n_communes_shp = integer()) else
  count(com_shp, adm2_pcode_shp, name = "n_communes_shp")

# b. Pcode identique (contrôlé par le nom) et c. nom
prov <- prov_shp |>
  left_join(votes, by = "adm2_pcode_shp") |>
  left_join(n_com_shp, by = "adm2_pcode_shp")
j_pc <- match(prov$adm2_pcode_shp, ref_prov$admin2_pcode)
prov$admin2_pcode_pc <- ref_prov$admin2_pcode[j_pc]
prov$d_pc  <- d_paire(prov$province_cle, ref_prov$province_cle[j_pc])
prov$pc_ok <- !is.na(prov$admin2_pcode_pc) & coalesce(prov$d_pc <= SEUIL_NOM, FALSE)
nm <- rapprocher(prov$province_cle, ref_prov$province_cle)
prov$admin2_pcode_nom <- ref_prov$admin2_pcode[nm$j]
prov$methode_nom      <- nm$methode

# d. Nomenclature 2025 : nouveau nom -> ancienne province
nomen <- nomenclature_2025 |>
  mutate(cle_shp = cle_province(province_shp), cle_data = cle_province(province_data))
jn <- rapprocher(prov$province_cle, nomen$cle_shp, seuil = 0.2)$j
prov$admin2_pcode_nomen <- ref_prov$admin2_pcode[rapprocher(nomen$cle_data[jn], ref_prov$province_cle)$j]
prov$nomen_nouvelle     <- nomen$nouvelle[jn]
prov$nomen_note         <- nomen$changement[jn]
nomen_absents <- nomen |> filter(!cle_data %in% ref_prov$province_cle)
if (nrow(nomen_absents) > 0)
  warning("Nomenclature 2025 : anciennes provinces introuvables dans les données : ",
          paste(nomen_absents$province_data, collapse = ", "), call. = FALSE)

# e. Corrections manuelles (prioritaires)
prov$admin2_pcode_man <- corrections_provinces$admin2_pcode[
  match(prov$adm2_pcode_shp, corrections_provinces$adm2_pcode_shp)]

# Décision
cand <- cbind(prov$admin2_pcode_nomen, prov$admin2_pcode_com,
              ifelse(prov$pc_ok, prov$admin2_pcode_pc, NA), prov$admin2_pcode_nom)
prov$desaccord <- apply(cand, 1, \(v) length(unique(na.omit(v))) > 1)

prov <- prov |>
  mutate(
    par_communes = coalesce(part_com, 0) >= PART_MIN,
    admin2_pcode_data = case_when(
      !is.na(admin2_pcode_man)   ~ admin2_pcode_man,
      !is.na(admin2_pcode_nomen) ~ admin2_pcode_nomen,
      par_communes               ~ admin2_pcode_com,
      pc_ok                      ~ admin2_pcode_pc,
      !is.na(admin2_pcode_nom)   ~ admin2_pcode_nom,
      !is.na(admin2_pcode_pc)    ~ admin2_pcode_pc,
      TRUE                       ~ NA_character_),
    methode = case_when(
      !is.na(admin2_pcode_man)   ~ "correction manuelle",
      !is.na(admin2_pcode_nomen) ~ "nomenclature 2025",
      par_communes               ~ "communes",
      pc_ok                    ~ "pcode",
      !is.na(admin2_pcode_nom) ~ methode_nom,
      !is.na(admin2_pcode_pc)  ~ "pcode, nom différent",
      TRUE                     ~ "aucune correspondance"),
    province_data = ref_prov$province[match(admin2_pcode_data, ref_prov$admin2_pcode)],
    admin1_pcode_data = coalesce(ref_prov$admin1_pcode[match(admin2_pcode_data, ref_prov$admin2_pcode)],
                                 substr(admin2_pcode_data, 1, 4))
  ) |>
  left_join(repart, by = c("admin2_pcode_data", "adm2_pcode_shp"))

# Polygone principal de chaque ancienne province : nomenclature 2025 > même nom > même Pcode
# > plus grande part de communes
prov <- prov |>
  mutate(nom_egal = coalesce(province_cle == cle_province(province_data), FALSE),
         score = 20 * coalesce(!nomen_nouvelle, FALSE) - 20 * coalesce(nomen_nouvelle, FALSE) +
           10 * nom_egal + 5 * coalesce(adm2_pcode_shp == admin2_pcode_data, FALSE) +
           coalesce(part_origine, 0)) |>
  group_by(admin2_pcode_data) |>
  mutate(n_polygones = n(), principal = row_number() == which.max(score)) |>
  ungroup() |>
  mutate(
    n_polygones = if_else(is.na(admin2_pcode_data), NA_integer_, n_polygones),
    principal   = principal & !is.na(admin2_pcode_data),
    type_lien = case_when(
      is.na(admin2_pcode_data) ~ "sans correspondance",
      coalesce(nomen_nouvelle, FALSE) ~ "nouvelle province (découpage 2025)",
      n_polygones == 1 & coalesce(n_origines, 1L) <= 1 & adm2_pcode_shp == admin2_pcode_data ~ "identique",
      n_polygones == 1 & coalesce(n_origines, 1L) <= 1 ~ "recodée ou renommée",
      n_polygones == 1 ~ "redécoupée (communes de plusieurs provinces)",
      principal ~ "province d'origine (scindée en 2025)",
      TRUE ~ "nouvelle province (découpage 2025)"),
    nouvelle         = type_lien == "nouvelle province (découpage 2025)",
    valeur_heritee   = nouvelle & HERITER_NOUVELLES,
    admin2_pcode_res = if_else(nouvelle & !HERITER_NOUVELLES, NA_character_, admin2_pcode_data)
  )

# Ancienne région des polygones sans correspondance : celle des voisins de la même nouvelle région
for (k in which(is.na(prov$admin1_pcode_data) & !is.na(prov$adm1_pcode_shp))) {
  j <- which(prov$adm1_pcode_shp == prov$adm1_pcode_shp[k] & !is.na(prov$admin1_pcode_data))
  if (length(j)) prov$admin1_pcode_data[k] <- names(sort(table(prov$admin1_pcode_data[j]), decreasing = TRUE))[1]
}
prov$region_data <- ref_reg$region[match(prov$admin1_pcode_data, ref_reg$admin1_pcode)]

prov <- prov |>
  mutate(
    a_verifier = methode %in% c("nom proche (à vérifier)", "pcode, nom différent", "aucune correspondance",
                                "homonymes non départagés") |
      desaccord | (methode == "communes" & coalesce(part_com, 1) < 0.8) |
      (methode == "correction manuelle" & is.na(province_data)),
    note = case_when(
      methode == "aucune correspondance" ~ "à compléter dans corrections_provinces (paramètres G0)",
      methode == "correction manuelle" & is.na(province_data) ~ "Pcode de corrections_provinces absent des données",
      desaccord ~ "nomenclature, communes, Pcode et nom ne désignent pas la même province",
      nouvelle & valeur_heritee ~ paste0("valeurs de ", province_data, " (province d'origine) ; ",
                                         coalesce(nomen_note, "scission 2025")),
      nouvelle ~ "sans données propres : reste grise sur la carte",
      !is.na(nomen_note) ~ nomen_note,
      methode == "communes" & coalesce(part_com, 1) < 0.8 ~ "communes issues de plusieurs anciennes provinces",
      TRUE ~ "")
  )

provinces <- prov |>
  select(adm2_pcode_shp, province_shp, adm1_pcode_shp, region_shp,
         admin2_pcode_res, admin2_pcode_data, province_data, admin1_pcode_data, region_data,
         type_lien, valeur_heritee, methode, a_verifier, note,
         n_communes_shp, n_com_apparies, part_com, n_origines, part_origine,
         admin2_pcode_nomen, admin2_pcode_com, admin2_pcode_pc, admin2_pcode_nom, admin2_pcode_man,
         distance_pcode = d_pc) |>
  arrange(desc(a_verifier), region_shp, province_shp)

orphelines <- ref_prov |> filter(!admin2_pcode %in% provinces$admin2_pcode_data)
print(table(provinces$type_lien))

# ---------------------------------------------------------------------
# G6. Régions : ancien découpage (13) <-> nouveau (17), par les communes
# ---------------------------------------------------------------------
regions <- lien_com |>
  filter(!is.na(adm1_pcode_shp), !is.na(admin1_pcode)) |>
  count(adm1_pcode_shp, region_shp, admin1_pcode, region, name = "n_communes") |>
  group_by(adm1_pcode_shp) |> mutate(part_dans_nouvelle = round(n_communes / sum(n_communes), 3)) |>
  group_by(admin1_pcode)   |> mutate(part_de_ancienne  = round(n_communes / sum(n_communes), 3)) |>
  ungroup() |>
  arrange(adm1_pcode_shp, desc(n_communes))

# ---------------------------------------------------------------------
# G7. CONASUR : ajout des Pcodes du fond de carte
# ---------------------------------------------------------------------
conasur_geo <- conasur |>
  left_join(communes |>
              select(admin3_pcode, commune_cle, adm3_pcode_shp, commune_shp, adm2_pcode_shp, province_shp,
                     adm1_pcode_shp, region_shp, methode_geo = methode, geo_a_verifier = a_verifier),
            by = c("admin3_pcode", "commune_cle")) |>
  # Pcode proposé pour les communes au Pcode faux (ex. Koundougou), retrouvées par le nom
  mutate(admin3_pcode_propose = if_else(coalesce(pcode_a_verifier, FALSE) & !is.na(adm3_pcode_shp),
                                        adm3_pcode_shp, NA_character_))
stopifnot(nrow(conasur_geo) == nrow(conasur))

# ---------------------------------------------------------------------
# G8. OpenData : taux par province (2005-2025)
# ---------------------------------------------------------------------
i_od <- grep("2005_2025", names(od))
i_od <- i_od[map_lgl(i_od, \(k) "region_name" %in% names(od[[k]]))]
od_dossier <- attr(od, "dossier")
if (!is.null(od_dossier)) i_od <- i_od[order(od_dossier[names(od)[i_od]] != "TAUX_SCO_REGION")]

opendata_noms <- tibble(); od_non_app <- tibble()
od_prov <- tibble(); od_carte <- tibble()
if (length(i_od) == 0) {
  warning("Table OpenData provinciale (« 2005_2025 ») introuvable : sortie OpenData vide.", call. = FALSE)
} else {
  message("Table OpenData provinciale : ", names(od)[i_od[1]])
  od_raw <- as_tibble(od[[i_od[1]]])
  cles_reg <- unique(cle_region(ref_reg$region))
  
  opendata_noms <- od_raw |>
    distinct(nom_src = as.character(region_name)) |>
    filter(!is.na(nom_src)) |>
    mutate(cle_r = cle_region(nom_src),
           niveau_geo = case_when(cle_r == "national" ~ "national",
                                  cle_r %in% cles_reg ~ "region",
                                  TRUE ~ "province"),
           province_cle = cle_province(nom_src))
  pr <- opendata_noms$niveau_geo == "province"
  nm <- rapprocher(opendata_noms$province_cle[pr], ref_prov$province_cle, seuil = 0.2)
  opendata_noms$admin2_pcode <- NA_character_; opendata_noms$methode <- NA_character_
  opendata_noms$distance <- NA_real_
  opendata_noms$admin2_pcode[pr] <- ref_prov$admin2_pcode[nm$j]
  opendata_noms$methode[pr]      <- coalesce(nm$methode, "aucune correspondance")
  opendata_noms$distance[pr]     <- nm$distance
  if (nrow(corrections_opendata) > 0) {
    k <- match(opendata_noms$nom_src, corrections_opendata$nom_src)
    opendata_noms <- opendata_noms |>
      mutate(admin2_pcode = coalesce(corrections_opendata$admin2_pcode[k], admin2_pcode),
             methode = if_else(!is.na(k), "correction manuelle", methode))
  }
  opendata_noms <- opendata_noms |>
    mutate(province_std = ref_prov$province[match(admin2_pcode, ref_prov$admin2_pcode)],
           admin1_pcode = ref_prov$admin1_pcode[match(admin2_pcode, ref_prov$admin2_pcode)],
           a_verifier   = niveau_geo == "province" & !methode %in% c("nom identique", "correction manuelle")) |>
    select(-cle_r)
  od_non_app <- opendata_noms |> filter(niveau_geo == "province", is.na(admin2_pcode))
  dbl_od <- opendata_noms |> filter(!is.na(admin2_pcode)) |> count(admin2_pcode) |> filter(n > 1)
  if (nrow(dbl_od) > 0) warning("Plusieurs noms OpenData pour une même province : ",
                                paste(dbl_od$admin2_pcode, collapse = ", "), call. = FALSE)
  
  # Ancien découpage : une ligne par valeur, colonnes d'origine conservées (années NON décalées :
  # le calage sur la DGESS reste fait dans 04_analyse)
  od_prov <- od_raw |>
    mutate(nom_src = as.character(region_name)) |>
    inner_join(opendata_noms |> filter(niveau_geo == "province", !is.na(admin2_pcode)) |>
                 select(nom_src, admin2_pcode, province_std, admin1_pcode, methode_nom = methode),
               by = "nom_src")
  # Fond de carte : chaque valeur portée sur son (ses) polygone(s)
  od_carte <- od_prov |>
    inner_join(provinces |> filter(!is.na(admin2_pcode_res)) |>
                 select(admin2_pcode_res, adm2_pcode_shp, province_shp, adm1_pcode_shp, region_shp,
                        type_lien, valeur_heritee),
               by = c("admin2_pcode" = "admin2_pcode_res"), relationship = "many-to-many")
  message("OpenData : ", n_distinct(od_prov$admin2_pcode), " provinces -> ",
          n_distinct(od_carte$adm2_pcode_shp), " polygones sur ", nrow(provinces))
}

# ---------------------------------------------------------------------
# G9. Polygones prêts à cartographier (sf)
# ---------------------------------------------------------------------
prov_sf <- sh_prov |>
  select(adm2_pcode_shp = adm2_pcode) |>
  left_join(select(provinces, adm2_pcode_shp, province_shp, region_shp, admin2_pcode_res,
                   province_data, admin1_pcode_data, region_data, type_lien, valeur_heritee),
            by = "adm2_pcode_shp")
# Anciennes régions (13) reconstituées par fusion des polygones provinces
reg_anc_sf <- prov_sf |>
  filter(!is.na(admin1_pcode_data)) |>
  group_by(admin1_pcode_data) |>
  summarise(.groups = "drop") |>
  mutate(region_data = ref_reg$region[match(admin1_pcode_data, ref_reg$admin1_pcode)])

# ---------------------------------------------------------------------
# G10. Contrôles
# ---------------------------------------------------------------------
resume <- tibble(
  controle = c("Polygones provinces (fond de carte)",
               "Provinces des données (ancien découpage)",
               "Polygones rattachés à une province des données",
               "  dont nouvelles provinces 2025",
               "  dont valeur héritée de la province d'origine",
               "Polygones sans valeur",
               "Provinces des données sans polygone",
               "Polygones à vérifier",
               "Communes CONASUR rattachées au fond de carte",
               "Communes CONASUR non rattachées",
               "Communes CONASUR à vérifier",
               "Communes CONASUR dont le Pcode change",
               "Noms OpenData provinciaux non appariés"),
  valeur = c(nrow(provinces), nrow(ref_prov),
             sum(!is.na(provinces$admin2_pcode_data)),
             sum(provinces$type_lien == "nouvelle province (découpage 2025)"),
             sum(provinces$valeur_heritee),
             sum(is.na(provinces$admin2_pcode_res)),
             nrow(orphelines), sum(provinces$a_verifier),
             sum(!is.na(communes$adm3_pcode_shp)), sum(is.na(communes$adm3_pcode_shp)),
             sum(communes$a_verifier), sum(communes$pcode_change),
             nrow(od_non_app))
)

controles <- list(
  resume               = resume,
  provinces_a_verifier = provinces |> filter(a_verifier),
  provinces_orphelines = orphelines,
  communes_a_verifier  = communes |> filter(a_verifier),
  pcodes_communes_modifies = communes |> filter(pcode_change) |>
    select(commune, admin3_pcode, adm3_pcode_shp, commune_shp, province, province_shp, methode),
  pcodes_proposes      = conasur_geo |> filter(!is.na(admin3_pcode_propose)) |>
    count(commune, admin3_pcode, admin3_pcode_propose, commune_shp),
  opendata_non_apparies = od_non_app
)

cat("\n================ CORRESPONDANCE GÉOGRAPHIQUE ================\n")
print(as.data.frame(resume), row.names = FALSE)
cat("\nPolygones à vérifier :\n")
print(as.data.frame(controles$provinces_a_verifier |>
                      select(adm2_pcode_shp, province_shp, admin2_pcode_data, province_data, methode, note)),
      row.names = FALSE)
if (nrow(orphelines) > 0)
  warning("Provinces des données sans polygone : ", paste(orphelines$province, collapse = ", "), call. = FALSE)
if (nrow(controles$pcodes_proposes) > 0) {
  cat("\nPcodes proposés pour les communes au Pcode faux (à reporter dans corrections_pcodes du 02) :\n")
  print(as.data.frame(controles$pcodes_proposes), row.names = FALSE)
}
if (nrow(od_non_app) > 0)
  message("Noms OpenData non appariés (à ajouter dans corrections_opendata) : ",
          paste(od_non_app$nom_src, collapse = ", "))

# ---------------------------------------------------------------------
# G11. Export
# ---------------------------------------------------------------------
geo <- list(
  provinces     = provinces,
  communes      = communes,
  regions       = regions,
  opendata_noms = opendata_noms,
  polygones     = list(provinces = prov_sf, regions_anciennes = reg_anc_sf),
  controles     = controles,
  parametres    = list(heriter_nouvelles = HERITER_NOUVELLES, seuil_nom = SEUIL_NOM,
                       part_min = PART_MIN, date = Sys.time())
)
saveRDS(geo, FIC$geo_corresp_rds)
saveRDS(conasur_geo, FIC$conasur_geo)
saveRDS(list(donnees = od_prov, carte = od_carte), FIC$opendata_prov)

# Format de la table du diagnostic, à l'endroit lu par 04_analyse et 05_interpretation
readr::write_excel_csv2(
  provinces |> transmute(adm2_pcode_shp, nom_shp = province_shp, region_shp, admin2_pcode_res,
                         province_res = province_data, methode, type_lien, valeur_heritee, a_verifier, note),
  FIC$corresp_prov_csv, na = "")

onglets <- c(list(resume = resume, provinces = provinces, communes = communes, regions = regions,
                  opendata_noms = opendata_noms),
             controles[setdiff(names(controles), "resume")])
onglets <- Filter(\(x) is.data.frame(x) && ncol(x) > 0, onglets)
names(onglets) <- substr(names(onglets), 1, 31)
writexl::write_xlsx(onglets, FIC$geo_corresp_xlsx)

message("Correspondance géographique -> ", FIC$geo_corresp_rds,
        "\nTable lue par l'analyse et l'application -> ", FIC$corresp_prov_csv,
        "\nRelancer ensuite 04_analyse et supprimer le cache : file.remove(FIC$cache_shiny)")
