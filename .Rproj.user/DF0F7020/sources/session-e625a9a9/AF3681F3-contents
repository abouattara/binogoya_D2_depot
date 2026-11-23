# =============================================================================
# DATA CHALLENGE BURKINA - Défi 2 : disparités d'accès à l'éducation
# Où sont les écarts (filles-garçons, ville-campagne) et comment évoluent-ils ?
#
# Chaîne d'analyse alignée sur la note d'intention :
#   1. Indice multidimensionnel de disparité éducative (IMDE) : genre, accès, moyens
#      pondérations égales + contrôle par ACP + simulations de sensibilité
#   2. Désagrégation au niveau le plus fin disponible :
#        région (DGESS, OPENDATA), province (OPENDATA 2005-2025), commune (CONASUR)
#   3. Évolution des écarts dans le temps (parité, convergence, urbain/rural)
#   4. Cartes des zones les plus inégales + tableau de bord HTML
#
# Limite assumée : les données éducatives ne sont pas communales. Au niveau
# communal, seul le déplacement forcé (CONASUR) est observé. L'indice communal
# produit ici est un indice de PRIORITÉ provisoire (IMDE de la province x pression
# de déplacement), en attendant l'estimation sur petits domaines (Fay-Herriot)
# prévue dans la note, qui exige les annuaires par commune et l'EHCVM.
#
# Entrées (produites par les étapes 01-02, chemins dans le fichier de configuration) :
#   Data/cleandata/binogoya_02-nettoyage_D2_engineer_dgess-tables_v1.rds
#   Data/cleandata/binogoya_02-nettoyage_D2_engineer_opendata-tables_v1.rds
#   Data/cleandata/binogoya_02-nettoyage_D2_engineer_conasur-pdi_v1.rds
#   Data/shapefile/admin_boundaries/*.shp (extraits par 01_collecte)
# Sorties : 04_analyse/sorties/ (résultats .rds/.xlsx, figures, tableau de bord HTML)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Paramètres et packages
# -----------------------------------------------------------------------------
source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))

params <- list(
  dir_shp   = CH$shp_admin,        # .shp OCHA extraits de bfa_admin_boundaries.shp.zip
  dir_out   = CH$sorties,
  annee_ref = NULL,                # NULL = dernière année suffisamment renseignée
  decalage_opendata = NULL,        # NULL = calage automatique sur la DGESS (-1, 0 ou +1 an)
  n_sim     = 2000,                # tirages pour la sensibilité des pondérations
  seed      = 2026
)

pkgs <- c("tidyverse", "sf", "stringi", "leaflet", "plotly", "DT",
          "htmltools", "htmlwidgets", "writexl", "scales")
installer(pkgs)

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(stringi)
  library(htmltools)
})
sf::sf_use_s2(FALSE)
set.seed(params$seed)

ou <- function(a, b) if (is.null(a)) b else a

# Palette (rôles fixes dans tous les graphiques)
col <- list(
  filles = "#eb6834", garcons = "#2a78d6",
  urbain = "#2a78d6", rural = "#eb6834",
  serie  = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100"),
  accent = "#1c5cab", gris = "#c3c2b7", critique = "#d03b3b",
  surface = "#fcfcfb", texte = "#0b0b0b", texte2 = "#52514e", muet = "#898781",
  grille = "#e1e0d9", neutre = "#f0efec", bleu_div = "#2a78d6", rouge_div = "#e34948"
)
rampe_bleu <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

theme_ecarts <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = col$grille, linewidth = 0.3),
      axis.text  = element_text(colour = col$texte2),
      axis.title = element_text(colour = col$texte2),
      plot.title = element_text(face = "bold", size = 13, colour = col$texte),
      plot.subtitle = element_text(colour = col$texte2),
      plot.caption  = element_text(colour = col$muet, size = 8),
      legend.position = "top",
      plot.background = element_rect(fill = col$surface, colour = NA)
    )
}
theme_carte <- function() {
  theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(colour = col$texte2),
          plot.caption = element_text(colour = col$muet, size = 8),
          legend.position = "right",
          plot.background = element_rect(fill = col$surface, colour = NA))
}
sauver <- function(p, nom, w = 9, h = 6) {
  ggsave(chemin_fig(nom), p, width = w, height = h, dpi = 200)
}
fmt <- function(x, d = 2) ifelse(is.na(x), "n.d.",
                                 formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ","))


# -----------------------------------------------------------------------------
# 1. Chargement des données
# -----------------------------------------------------------------------------
g <- function(nom) if (exists(nom, envir = globalenv(), inherits = FALSE)) get(nom, envir = globalenv()) else NULL

# Fichiers des étapes précédentes ; à défaut, objets déjà présents dans la session
lire <- function(f, objet) if (file.exists(f)) readRDS(f) else g(objet)
res_obj <- list(tables = { x <- lire(FIC$dgess, "res"); if (!is.null(x$tables)) x$tables else x })
od      <- lire(FIC$opendata, "tables")
conasur <- lire(FIC$conasur, "conasur")

if (!is.list(res_obj$tables) || length(res_obj$tables) == 0)
  stop("Tables DGESS introuvables : ", FIC$dgess, "\nLancez d'abord le script d'extraction DGESS.")
if (!is.list(od) || is.data.frame(od))
  stop("Tables OPENDATA introuvables : ", FIC$opendata, "\nLancez d'abord le script d'extraction OPENDATA.")
if (!is.data.frame(conasur))
  stop("Table CONASUR introuvable : ", FIC$conasur, "\nLancez d'abord le script d'extraction CONASUR.")

# Sélection d'une table OPENDATA : nom correspondant au motif ET colonnes attendues présentes.
# Les tables du dossier TAUX_SCO_REGION sont prioritaires (les autres dossiers contiennent des
# fichiers aux noms proches : effectifs, nombre d'écoles...).
od_dossier <- attr(od, "dossier")
od_get <- function(motif, cols) {
  i <- grep(motif, names(od))
  i <- i[map_lgl(i, \(k) all(cols %in% names(od[[k]])))]
  if (!is.null(od_dossier)) i <- i[order(od_dossier[names(od)[i]] != "TAUX_SCO_REGION")]
  if (length(i) == 0) {
    warning("Table OPENDATA introuvable : ", motif, " (colonnes : ", paste(cols, collapse = ", "), ")", call. = FALSE)
    return(NULL)
  }
  if (length(i) > 1) message("Plusieurs tables pour « ", motif, " » : ", names(od)[i[1]], " retenue.")
  as_tibble(od[[i[1]]])
}
od_prov_raw  <- od_get("2005_2025",                      # région ET province, par ordre, sexe, âge
                       c("region_name", "ordre_d_enseignement", "tranche_d_ages_name", "sexe", "valeur"))
od_pp_raw    <- od_get("^TAUX_SCOLARISATION_POSTPRIMAIRE",  # TBS post-primaire région x sexe
                       c("region_name", "sexe", "valeur"))
od_sec_raw   <- od_get("^TAUX_SCOLARISATION_SECONDAIRE", c("region_name", "sexe", "valeur"))
od_ppsec_raw <- od_get("POSTP_&_SECONDAIRE",             # national 1996-2024, par sexe (rubrique)
                       c("rubrique_name", "valeur"))
od_tbs_raw   <- od_get("TBS_PPRIMAIRE",                  # contient le TNS secondaire
                       c("region_name", "indicateur_name", "units", "sexe", "valeur"))


# -----------------------------------------------------------------------------
# 2. Harmonisation : libellés géographiques, sexe, années
# -----------------------------------------------------------------------------
norm_lib <- function(x) {
  x |> as.character() |> stri_trans_general("Latin-ASCII") |> str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "")
}
# Clé région sur l'ancien découpage (13 régions, Pcodes OCHA BF46... BF13)
cle_region <- function(x) {
  k <- norm_lib(x)
  case_when(
    k %in% c("national", "burkinafaso", "totalgeneral", "bfa", "bf", "ensemble") ~ "national",
    k == "hautbassins" ~ "hautsbassins",
    k == "centresansouagadougou" ~ "centre",   # approximation signalée : Centre hors Ouagadougou
    TRUE ~ k
  )
}
sexe_std <- function(x) {
  k <- norm_lib(x)
  case_when(
    k %in% c("f", "fem", "feminin", "filles", "fille") ~ "F",
    k %in% c("g", "m", "mas", "masculin", "garcons", "garcon") ~ "G",
    k %in% c("t", "ens", "ensemble", "total") ~ "T",
    TRUE ~ NA_character_
  )
}
# Année = année de FIN de l'année scolaire (2015-2016 -> 2016 ; session CEP 2016 -> 2016)
annee_fin <- function(annee_scolaire, annee) {
  a <- str_squish(as.character(annee_scolaire))
  deb <- suppressWarnings(as.integer(str_extract(a, "^\\d{4}")))
  plage <- coalesce(str_detect(a, "^\\d{4}\\s*[-/]\\s*\\d{2,4}$"), FALSE)
  if_else(plage, deb + 1L, coalesce(deb, as.integer(annee)))
}
agreger <- function(d, ...) {
  d |> filter(!is.na(valeur)) |> group_by(...) |> summarise(valeur = mean(valeur), .groups = "drop")
}
mm <- function(x) {   # min-max, 0 = situation la plus favorable
  x[!is.finite(x)] <- NA
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) == 0) return(rep(NA_real_, length(x)))
  (x - r[1]) / diff(r)
}

# Clé province : libellé normalisé sans article initial (« Les Balé » = « Balé »)
cle_province <- function(x) sub("^les(?=[a-z]{3,})", "", norm_lib(x), perl = TRUE)

# Appariement fond de carte -> résultats, polygone par polygone :
#   1. Pcode identique ; 2. table de correspondance validée (prioritaire si elle existe,
#   produite par le script de diagnostic) ; 3. nom normalisé, exact puis approché.
F_CORRESP_PROV <- file.path(CH$shp, nf("04_analyse", "analyst", "correspondance-provinces", "csv"))
# Lecture tolérante de la table de correspondance corrigée dans Excel :
# séparateur « ; » ou « , », encodage UTF-8 (avec ou sans BOM) ou Windows (latin1), espaces retirés.
lire_correspondance <- function(f) {
  if (!file.exists(f)) { message("Table de correspondance absente : ", f); return(NULL) }
  lignes <- readLines(f, warn = FALSE)
  n_pv <- lengths(regmatches(lignes[1], gregexpr(";", lignes[1])))
  n_vg <- lengths(regmatches(lignes[1], gregexpr(",", lignes[1])))
  enc <- if (all(validUTF8(lignes))) "UTF-8-BOM" else "latin1"
  man <- utils::read.csv(f, sep = if (n_pv >= n_vg) ";" else ",", colClasses = "character",
                         fileEncoding = enc, check.names = FALSE, na.strings = c("", "NA"))
  names(man) <- trimws(sub("^\ufeff", "", names(man)))
  for (v in names(man)) man[[v]] <- trimws(man[[v]])
  if (!all(c("adm2_pcode_shp", "admin2_pcode_res") %in% names(man))) {
    warning("Table de correspondance illisible : colonnes trouvées = ", paste(names(man), collapse = " | "), call. = FALSE)
    return(NULL)
  }
  man <- man[!is.na(man$admin2_pcode_res), ]
  message("Table de correspondance lue : ", nrow(man), " lignes renseignées (", basename(f), ")")
  man
}
apparier_provinces <- function(code_shp, nom_shp, ref_code, ref_nom, f_manuel = F_CORRESP_PROV) {
  out <- if_else(code_shp %in% ref_code, code_shp, NA_character_)
  n_pc <- sum(!is.na(out))
  man <- lire_correspondance(f_manuel)
  if (!is.null(man) && nrow(man) > 0)
    out <- coalesce(man$admin2_pcode_res[match(code_shp, man$adm2_pcode_shp)], out)
  i <- which(is.na(out))
  if (length(i)) out[i] <- ref_code[apparier(cle_province(nom_shp[i]), cle_province(ref_nom))]
  message("Fond de carte : ", n_pc, " provinces appariées par Pcode, ",
          sum(!is.na(out)) - n_pc, " par la table ou le nom, ", sum(is.na(out)), " non appariées",
          if (any(is.na(out))) paste0(" (", paste(nom_shp[is.na(out)], collapse = ", "), ")") else "")
  out
}

# Référentiels géographiques à partir des Pcodes CONASUR (seule source codée)
cn0 <- as_tibble(conasur)
ref_reg <- cn0 |>
  filter(!is.na(region_std), !is.na(admin1_pcode)) |>
  distinct(admin1_pcode, region_std) |>
  distinct(admin1_pcode, .keep_all = TRUE) |>
  mutate(cle = cle_region(region_std))
ref_prov <- cn0 |>
  filter(!is.na(admin2_pcode)) |>
  distinct(admin2_pcode, province_std, admin1_pcode) |>
  distinct(admin2_pcode, .keep_all = TRUE) |>
  mutate(cle_p = cle_province(province_std))
geos_reg <- c(ref_reg$cle, "national")
nom_region <- function(cle) {
  out <- ref_reg$region_std[match(cle, ref_reg$cle)]
  if_else(cle == "national", "Burkina Faso", out)
}

apparier <- function(cles, ref_cles) {   # exact, puis approché si une seule candidate
  m <- match(cles, ref_cles)
  for (i in which(is.na(m))) {
    j <- agrep(cles[i], ref_cles, max.distance = 0.2)
    if (length(j) == 1) m[i] <- j
  }
  m
}


# -----------------------------------------------------------------------------
# 3. Indicateurs DGESS (région x année x sexe)
# -----------------------------------------------------------------------------
dg_all <- imap_dfr(res_obj$tables, \(d, n) {
  d <- as_tibble(d)
  for (v in c("titre", "region", "milieu", "sexe", "statut", "niveau", "annee_scolaire"))
    if (!v %in% names(d)) d[[v]] <- NA_character_
  if (!"annee" %in% names(d)) d$annee <- NA_integer_
  d |> transmute(
    table_dg = n,
    titre = as.character(titre),
    geo = coalesce(as.character(milieu), as.character(region)),
    sexe = as.character(sexe),
    statut = as.character(statut),
    annee_scolaire = as.character(annee_scolaire),
    annee0 = suppressWarnings(as.integer(annee)),
    valeur = suppressWarnings(as.numeric(valeur))
  )
}) |>
  mutate(annee = annee_fin(annee_scolaire, annee0),
         titre_n = norm_lib(titre),
         cle = cle_region(geo))

# Extraction d'un indicateur par motif sur le titre du tableau
ind_dg <- function(nom, motif, exclure = "ips", borne = c(0, 300), par_sexe = TRUE,
                   total_statut = FALSE, geos = geos_reg) {
  d <- dg_all |> filter(str_detect(titre_n, motif))
  if (!is.na(exclure)) d <- d |> filter(!str_detect(titre_n, exclure))
  if (total_statut) d <- d |> filter(norm_lib(statut) %in% c("total", "totalgeneral", "national"))
  d |>
    filter(cle %in% geos, !is.na(annee), between(valeur, borne[1], borne[2])) |>
    mutate(sexe = if (par_sexe) sexe_std(sexe) else "T") |>
    filter(!is.na(sexe)) |>
    agreger(cle, annee, sexe) |>
    mutate(indic = nom, source = "DGESS")
}

ind_dgess <- bind_rows(
  ind_dg("tbs_prim", "tbsauprimaire"),
  ind_dg("tba_prim", "tbaauprimaire"),
  ind_dg("tbs_pre",  "tbsauprescolaire"),
  ind_dg("cep",      "cep", borne = c(0, 100)),
  ind_dg("eff_prim", "effectifsduprimaire.*region", borne = c(1, 1e8)),
  ind_dg("ips_tbs_prim", "ipsdutbsauprimaire", exclure = NA, borne = c(0.2, 3), par_sexe = FALSE),
  ind_dg("ecoles_prim", "ecolesprimaires", borne = c(1, 1e6), par_sexe = FALSE, total_statut = TRUE)
)
cat("Indicateurs DGESS extraits :\n"); print(count(ind_dgess, indic, sexe))


# -----------------------------------------------------------------------------
# 4. Indicateurs OPENDATA (avec calage des années sur la DGESS)
# -----------------------------------------------------------------------------
od_regional <- function(d, nom, borne = c(0, 300), filtre = identity) {
  if (is.null(d)) return(tibble())
  d <- filtre(d)
  sx <- if ("sexe_name" %in% names(d)) coalesce(as.character(d$sexe_name), as.character(d$sexe)) else as.character(d$sexe)
  d |>
    mutate(annee = as.integer(annee), cle = cle_region(region_name),
           sexe = sexe_std(sx), valeur = suppressWarnings(as.numeric(valeur))) |>
    filter(cle %in% geos_reg, !is.na(sexe), between(valeur, borne[1], borne[2])) |>
    agreger(cle, annee, sexe) |>
    mutate(indic = nom, source = "OPENDATA")
}

ind_od <- bind_rows(
  od_regional(od_pp_raw,  "tbs_pp"),
  od_regional(od_sec_raw, "tbs_sec"),
  od_regional(od_tbs_raw, "tns_sec", borne = c(0, 100),
              filtre = \(d) filter(d, str_detect(norm_lib(indicateur_name), "net"),
                                   as.character(units) == "%"))
)

# Calage : décalage d'années qui aligne au mieux le taux primaire OPENDATA sur le TBS DGESS
od_pri_nat <- od_regional(od_prov_raw, "tx_pri",
                          filtre = \(d) filter(d, as.character(ordre_d_enseignement) == "PRI")) |>
  filter(cle == "national", sexe == "T") |> select(annee, valeur)
dg_pri_nat <- ind_dgess |> filter(indic == "tbs_prim", cle == "national", sexe == "T") |> select(annee, valeur)
caler <- function(a, b) {
  sc <- map_dbl(-1:1, \(k) {
    x <- inner_join(a, mutate(b, annee = annee + k), by = "annee")
    if (nrow(x) < 3) return(NA_real_)
    mean(abs((x$valeur.x - mean(x$valeur.x)) - (x$valeur.y - mean(x$valeur.y))))
  })
  if (all(is.na(sc))) 0L else (-1:1)[which.min(sc)]
}
decal <- ou(params$decalage_opendata, caler(dg_pri_nat, od_pri_nat))
message("Décalage appliqué aux années OPENDATA : ", decal,
        " an(s). Vérifier : TBS primaire national 2023/2024 = 78,2 % (INSD).")
ind_od <- ind_od |> mutate(annee = annee + decal)

ind_long <- bind_rows(ind_dgess, ind_od)


# -----------------------------------------------------------------------------
# 5. Déplacement forcé (CONASUR) : seule donnée communale
# -----------------------------------------------------------------------------
cn <- cn0 |>
  filter(!coalesce(doublon_exact, FALSE), !is.na(admin3_pcode)) |>
  mutate(date_donnee = as.Date(date_donnee),
         an = as.integer(format(date_donnee, "%Y")))
ok_enf <- cn |> filter(!is.na(total_enfants), total_individus > 0)
part_enf <- if (nrow(ok_enf) > 0) sum(ok_enf$total_enfants) / sum(ok_enf$total_individus) else NA_real_
cn <- cn |>
  mutate(enfants = coalesce(total_enfants, enfants_moins_5 + enfants_plus_5, total_individus * part_enf),
         enfants_impute = is.na(total_enfants))

# Photographie de fin d'année (dernière collecte de chaque année)
dates_an <- cn |> group_by(an) |> summarise(date = max(date_donnee, na.rm = TRUE), .groups = "drop")
com_an <- cn |>
  inner_join(dates_an, by = c("an", "date_donnee" = "date")) |>
  group_by(an, admin3_pcode) |>
  summarise(commune = first(commune_std), admin2_pcode = first(admin2_pcode),
            province = first(province_std), admin1_pcode = first(admin1_pcode),
            pdi = sum(total_individus, na.rm = TRUE), pdi_enfants = sum(enfants, na.rm = TRUE),
            .groups = "drop")
date_der <- max(cn$date_donnee, na.rm = TRUE)
an_der_pdi <- max(com_an$an)
com_der <- com_an |> filter(an == an_der_pdi)
pdi_serie <- cn |> group_by(date_donnee) |>
  summarise(pdi = sum(total_individus, na.rm = TRUE), pdi_enfants = sum(enfants, na.rm = TRUE), .groups = "drop")

pdi_reg <- com_an |>
  group_by(admin1_pcode, annee = an) |>
  summarise(pdi_enfants = sum(pdi_enfants), pdi = sum(pdi), .groups = "drop") |>
  inner_join(select(ref_reg, admin1_pcode, cle), by = "admin1_pcode") |>
  select(-admin1_pcode)
pdi_reg <- bind_rows(pdi_reg,
                     pdi_reg |> group_by(annee) |> summarise(across(c(pdi_enfants, pdi), sum), .groups = "drop") |>
                       mutate(cle = "national"))


# -----------------------------------------------------------------------------
# 6. Panel région x année et indicateurs d'écart
# -----------------------------------------------------------------------------
pan <- ind_long |>
  select(cle, annee, indic, sexe, valeur) |>
  pivot_wider(names_from = c(indic, sexe), values_from = valeur, names_sep = "_", values_fn = mean)

attendus <- c("tbs_prim_T", "tbs_prim_F", "tbs_prim_G",
              "cep_T", "cep_F", "cep_G", "eff_prim_T", "eff_prim_F", "ecoles_prim_T",
              "ips_tbs_prim_T", "tbs_pp_T", "tbs_pp_F", "tbs_pp_G",
              "tbs_sec_T", "tbs_sec_F", "tbs_sec_G", "tns_sec_T", "tbs_pre_T")
for (v in setdiff(attendus, names(pan))) pan[[v]] <- NA_real_

pan <- pan |>
  left_join(pdi_reg, by = c("cle", "annee")) |>
  group_by(cle) |> arrange(annee, .by_group = TRUE) |>
  tidyr::fill(pdi_enfants, pdi, .direction = "down") |>   # après 2023 : dernier stock connu
  ungroup() |>
  mutate(
    pdi_reporte  = annee > an_der_pdi,
    ips_prim     = coalesce(ips_tbs_prim_T, tbs_prim_F / tbs_prim_G),
    ips_cep      = cep_F / cep_G,
    ips_pp       = tbs_pp_F / tbs_pp_G,
    ips_sec      = tbs_sec_F / tbs_sec_G,
    transition   = tbs_pp_T / tbs_prim_T,
    eleves_ecole = eff_prim_T / ecoles_prim_T,
    enf_dep_100  = 100 * pdi_enfants / eff_prim_T,
    # Écarts de genre : |log IPS| (symétrique ; 0 = parité)
    g_prim = abs(log(ips_prim)), g_cep = abs(log(ips_cep)),
    g_pp   = abs(log(ips_pp)),   g_sec = abs(log(ips_sec)),
    # Déficits d'accès (TBS plafonné à 100 % : au-delà, il reflète des élèves hors âge)
    a_prim  = 1 - pmin(tbs_prim_T, 100) / 100,
    a_pp    = 1 - pmin(tbs_pp_T, 100) / 100,
    a_sec   = 1 - pmin(tbs_sec_T, 100) / 100,
    a_trans = 1 - pmin(transition, 1),
    # Moyens et contexte
    m_eleves = eleves_ecole,
    m_pdi    = enf_dep_100
  )
pan$ips_moy <- exp(rowMeans(log(as.matrix(pan[c("ips_prim", "ips_pp", "ips_sec")])), na.rm = TRUE))
pan$nom <- nom_region(pan$cle)

spec_reg <- tribble(
  ~var,      ~dim,     ~libelle,
  "g_prim",  "genre",  "Parité TBS primaire",
  "g_cep",   "genre",  "Parité réussite CEP",
  "g_pp",    "genre",  "Parité TBS post-primaire",
  "g_sec",   "genre",  "Parité TBS secondaire",
  "a_prim",  "acces",  "Déficit TBS primaire",
  "a_pp",    "acces",  "Déficit TBS post-primaire",
  "a_sec",   "acces",  "Déficit TBS secondaire",
  "a_trans", "acces",  "Perte primaire -> post-primaire",
  "m_eleves","moyens", "Élèves par école primaire",
  "m_pdi",   "moyens", "Enfants déplacés pour 100 élèves"
)
lib_dim <- c(genre = "Équité de genre", acces = "Équité d'accès (spatiale)",
             moyens = "Moyens et contexte", contexte = "Contexte (déplacement)")


# -----------------------------------------------------------------------------
# 7. IMDE : score, ACP, sensibilité
# -----------------------------------------------------------------------------
rdir <- function(k) { g <- rgamma(k, 1); g / sum(g) }

scorer <- function(df, spec, n_sim = 0) {
  vars <- spec$var[spec$var %in% names(df)]
  for (v in vars) df[[v]][!is.finite(df[[v]])] <- NA
  vars <- vars[map_lgl(vars, \(v) sum(!is.na(df[[v]])) >= 3 && isTRUE(sd(df[[v]], na.rm = TRUE) > 0))]
  spec <- spec |> filter(var %in% vars)
  dims <- unique(spec$dim)
  
  S <- map_dfc(set_names(vars), \(v) mm(df[[v]]))
  D <- map_dfc(set_names(dims), \(d) {
    vv <- spec$var[spec$dim == d]
    m <- as.matrix(S[vv])
    out <- rowMeans(m, na.rm = TRUE)
    out[rowSums(!is.na(m)) < ceiling(length(vv) / 2)] <- NA
    out
  })
  Dm <- as.matrix(D)
  imde <- rowMeans(Dm, na.rm = TRUE)
  imde[rowSums(!is.na(Dm)) < min(2, length(dims))] <- NA
  
  out <- bind_cols(df, rename_with(S, \(x) paste0("s_", x)), D, tibble(imde = imde)) |>
    mutate(rang = rank(-imde, na.last = "keep", ties.method = "min"))
  poids_acp <- NULL
  
  if (n_sim > 0) {
    X <- as.matrix(S)
    for (j in seq_len(ncol(X))) X[is.na(X[, j]), j] <- mean(X[, j], na.rm = TRUE)
    ok <- !is.na(imde)
    grp <- match(spec$dim, dims)
    R <- replicate(n_sim, {
      wd <- rdir(length(dims)); w <- numeric(ncol(X))
      for (k in seq_along(dims)) { j <- which(grp == k); w[j] <- wd[k] * rdir(length(j)) }
      rank(-(X[ok, , drop = FALSE] %*% w), ties.method = "average")
    })
    R <- matrix(R, nrow = sum(ok))
    top_k <- max(3, round(sum(ok) / 5))
    out$rang_med <- NA_real_; out$rang_p05 <- NA_real_; out$rang_p95 <- NA_real_; out$p_top <- NA_real_
    out$rang_med[ok] <- apply(R, 1, median)
    out$rang_p05[ok] <- apply(R, 1, quantile, 0.05)
    out$rang_p95[ok] <- apply(R, 1, quantile, 0.95)
    out$p_top[ok]    <- rowMeans(R <= top_k)
    
    # Contrôle par ACP : poids = |charges| sur la 1re composante
    keep <- apply(X[ok, , drop = FALSE], 2, sd) > 0
    pc <- prcomp(X[ok, keep, drop = FALSE], scale. = TRUE)
    w_acp <- abs(pc$rotation[, 1]); w_acp <- w_acp / sum(w_acp)
    out$imde_acp <- NA_real_
    out$imde_acp[ok] <- mm(as.vector(scale(X[ok, keep, drop = FALSE]) %*% w_acp))
    poids_acp <- tibble(var = names(w_acp), poids_acp = w_acp,
                        var_expliquee_cp1 = summary(pc)$importance[2, 1]) |>
      left_join(spec, by = "var")
  }
  list(scores = out, spec = spec, poids_acp = poids_acp)
}

# Typologie (note, valeur ajoutée 2) : filles désavantagées vs exclusion générale
typologie <- function(d_genre, d_acces, ips) {
  hg <- d_genre >= quantile(d_genre, 2 / 3, na.rm = TRUE)
  ha <- d_acces >= quantile(d_acces, 2 / 3, na.rm = TRUE)
  case_when(
    is.na(d_genre) | is.na(d_acces) ~ "Données insuffisantes",
    hg & ha & ips < 1 ~ "Cumul : exclusion générale + filles désavantagées",
    ha                ~ "Exclusion générale (tous les enfants)",
    hg & ips < 1      ~ "Filles désavantagées",
    hg & ips >= 1     ~ "Garçons désavantagés",
    TRUE              ~ "Écarts modérés"
  )
}

reg_all <- pan |> filter(cle != "national", !is.na(annee))
cov_an <- sapply(split(reg_all, reg_all$annee),
                 \(d) mean(!is.na(as.matrix(d[intersect(spec_reg$var, names(d))]))))
annee_ref <- ou(params$annee_ref,
                if (any(cov_an >= 0.6)) max(as.integer(names(cov_an))[cov_an >= 0.6])
                else as.integer(names(cov_an))[which.max(cov_an)])
message("Année de référence de l'IMDE régional : ", annee_ref,
        " (taux de renseignement ", round(100 * cov_an[as.character(annee_ref)]), " %)")

res_reg <- scorer(filter(reg_all, annee == annee_ref), spec_reg, n_sim = params$n_sim)
imde_reg <- res_reg$scores |>
  left_join(select(ref_reg, cle, admin1_pcode), by = "cle") |>
  mutate(typologie = typologie(genre, acces, ips_moy)) |>
  arrange(rang)
cor_acp <- cor(imde_reg$imde, imde_reg$imde_acp, method = "spearman", use = "complete.obs")
message("Corrélation de rang IMDE égal / IMDE pondéré ACP : ", round(cor_acp, 2))


# -----------------------------------------------------------------------------
# 8. Évolution des écarts
# -----------------------------------------------------------------------------
# 8a. IMDE « structurel » (genre + accès, normalisation commune à toutes les années)
res_evo <- scorer(reg_all, filter(spec_reg, dim != "moyens"))
evo <- res_evo$scores |> select(cle, nom, annee, genre, acces, imde) |> filter(!is.na(imde))
tendance <- evo |>
  group_by(cle, nom) |>
  filter(n() >= 4) |>
  summarise(pente_imde = coef(lm(imde ~ annee))[2],
            imde_debut = first(imde), imde_fin = last(imde),
            periode = paste(min(annee), max(annee), sep = "-"), .groups = "drop") |>
  mutate(sens = if_else(pente_imde > 0, "Écarts qui se creusent", "Écarts qui se réduisent")) |>
  arrange(desc(pente_imde))

# 8b. Convergence entre régions (sigma-convergence du TBS primaire)
sigma <- reg_all |>
  filter(!is.na(tbs_prim_T)) |>
  group_by(annee) |>
  summarise(cv_tbs_prim = sd(tbs_prim_T, na.rm = TRUE) / mean(tbs_prim_T, na.rm = TRUE),
            ecart_max_min = max(tbs_prim_T, na.rm = TRUE) - min(tbs_prim_T, na.rm = TRUE),
            n = sum(!is.na(tbs_prim_T)), .groups = "drop") |>
  filter(n >= 10)

# 8c. Parité nationale par niveau
nat_genre <- pan |>
  filter(cle == "national") |>
  select(annee, Primaire = ips_prim,
         `Post-primaire` = ips_pp, Secondaire = ips_sec) |>
  pivot_longer(-annee, names_to = "niveau", values_to = "ips") |>
  filter(!is.na(ips))
if (!is.null(od_ppsec_raw)) {
  long_ppsec <- od_ppsec_raw |>
    mutate(annee = as.integer(annee) + decal, sexe = sexe_std(rubrique_name),
           valeur = suppressWarnings(as.numeric(valeur))) |>
    filter(sexe %in% c("F", "G")) |>
    agreger(annee, sexe) |>
    pivot_wider(names_from = sexe, values_from = valeur) |>
    transmute(annee, niveau = "Post-primaire + secondaire (série longue)", ips = F / G)
  nat_genre <- bind_rows(nat_genre, long_ppsec)
}

# 8d. Ville-campagne (national uniquement : effectifs par milieu)
milieu <- bind_rows(
  ind_dg("eff_prim", "effectifsduprimaire.*milieu", borne = c(1, 1e8), geos = c("urbain", "rural")),
  ind_dg("eff_pre",  "effectifsduprescolaire.*milieu", borne = c(1, 1e8), geos = c("urbain", "rural"))
) |>
  pivot_wider(names_from = sexe, values_from = valeur)
for (v in c("F", "G", "T")) if (!v %in% names(milieu)) milieu[[v]] <- NA_real_
milieu <- milieu |>
  mutate(milieu = str_to_title(cle),
         part_filles = 100 * F / coalesce(T, F + G),
         ips_effectifs = F / G) |>
  group_by(indic, milieu) |> arrange(annee, .by_group = TRUE) |>
  mutate(indice_100 = 100 * coalesce(T, F + G) / first(coalesce(T, F + G))) |>
  ungroup()
if (nrow(milieu) == 0) warning("Aucun effectif par milieu trouvé : graphiques ville-campagne vides.")


# -----------------------------------------------------------------------------
# 9. Niveau provincial (OPENDATA 2005-2025) : désagrégation infra-régionale
# -----------------------------------------------------------------------------
imde_prov <- tibble()
if (!is.null(od_prov_raw)) {
  paire_age <- c(PRI = "6a11ans", PPR = "12a15ans", SEC = "16a18ans")
  prov_long <- od_prov_raw |>
    mutate(annee = as.integer(annee) + decal,
           ordre = as.character(ordre_d_enseignement),
           tr = norm_lib(tranche_d_ages_name),
           cle_p = cle_province(region_name), nom_src = as.character(region_name),
           sexe = sexe_std(coalesce(as.character(sexe_name), as.character(sexe))),
           valeur = suppressWarnings(as.numeric(valeur))) |>
    filter(ordre %in% names(paire_age), between(valeur, 0, 300), !is.na(sexe))
  if (any(prov_long$tr == paire_age[prov_long$ordre], na.rm = TRUE))
    prov_long <- prov_long |> filter(tr == paire_age[ordre])
  
  cands <- prov_long |>
    distinct(cle_p, nom_src) |>
    filter(!cle_region(nom_src) %in% geos_reg) |>
    distinct(cle_p, .keep_all = TRUE)
  cands$admin2_pcode <- ref_prov$admin2_pcode[apparier(cands$cle_p, ref_prov$cle_p)]
  non_app <- cands |> filter(is.na(admin2_pcode))
  if (nrow(non_app) > 0) message("Provinces OPENDATA non appariées : ", paste(non_app$nom_src, collapse = ", "))
  
  prov_long <- prov_long |> inner_join(select(cands, cle_p, admin2_pcode) |> filter(!is.na(admin2_pcode)), by = "cle_p")
  n_prov_an <- prov_long |> group_by(annee) |> summarise(n = n_distinct(admin2_pcode))
  annee_prov <- max(n_prov_an$annee[n_prov_an$n >= 30], na.rm = TRUE)
  message("Année de référence provinciale : ", annee_prov)
  
  prov_w <- prov_long |>
    filter(annee == annee_prov) |>
    group_by(admin2_pcode, ordre, sexe) |>
    summarise(v = mean(valeur), .groups = "drop") |>
    pivot_wider(names_from = c(ordre, sexe), values_from = v)
  for (v in c("PRI_F", "PRI_G", "PRI_T", "PPR_F", "PPR_G", "PPR_T", "SEC_F", "SEC_G", "SEC_T"))
    if (!v %in% names(prov_w)) prov_w[[v]] <- NA_real_
  
  pdi_prov <- com_der |> group_by(admin2_pcode) |>
    summarise(pdi = sum(pdi), pdi_enfants = sum(pdi_enfants), .groups = "drop")
  
  prov_df <- prov_w |>
    left_join(select(ref_prov, admin2_pcode, province = province_std), by = "admin2_pcode") |>
    left_join(pdi_prov, by = "admin2_pcode") |>
    mutate(admin1_pcode = substr(admin2_pcode, 1, 4),
           region = ref_reg$region_std[match(admin1_pcode, ref_reg$admin1_pcode)],
           pdi = coalesce(pdi, 0), pdi_enfants = coalesce(pdi_enfants, 0),
           ips_pri = PRI_F / PRI_G, ips_ppr = PPR_F / PPR_G, ips_sec = SEC_F / SEC_G,
           g_pri = abs(log(ips_pri)), g_ppr = abs(log(ips_ppr)), g_sec = abs(log(ips_sec)),
           a_pri = 1 - pmin(PRI_T, 100) / 100, a_ppr = 1 - pmin(PPR_T, 100) / 100,
           a_sec = 1 - pmin(SEC_T, 100) / 100, a_trans = 1 - pmin(PPR_T / PRI_T, 1),
           c_pdi = log1p(pdi_enfants))
  prov_df$ips_moy <- exp(rowMeans(log(as.matrix(prov_df[c("ips_pri", "ips_ppr", "ips_sec")])), na.rm = TRUE))
  
  spec_prov <- tribble(
    ~var,      ~dim,       ~libelle,
    "g_pri",   "genre",    "Parité primaire",
    "g_ppr",   "genre",    "Parité post-primaire",
    "g_sec",   "genre",    "Parité secondaire",
    "a_pri",   "acces",    "Déficit primaire",
    "a_ppr",   "acces",    "Déficit post-primaire",
    "a_sec",   "acces",    "Déficit secondaire",
    "a_trans", "acces",    "Perte primaire -> post-primaire",
    "c_pdi",   "contexte", "Enfants déplacés (log)"
  )
  res_prov <- scorer(prov_df, spec_prov, n_sim = params$n_sim)
  imde_prov <- res_prov$scores |>
    left_join(select(imde_reg, admin1_pcode, rang_region = rang), by = "admin1_pcode") |>
    mutate(
      typologie = typologie(genre, acces, ips_moy),
      quintile = ntile(imde, 5),
      prioritaire = quintile == 5,
      # Poche cachée : province très inégale dans une région qui ne l'est pas
      poche_cachee = prioritaire & rang_region > ceiling(nrow(imde_reg) / 3)
    ) |>
    arrange(rang)
}


# -----------------------------------------------------------------------------
# 10. Communes : indice de priorité provisoire
# -----------------------------------------------------------------------------
# Lecture des seules couches de POLYGONES (l'archive OCHA contient aussi des lignes
# « admbndl » et des points « admbndp » qui portent les mêmes Pcodes) ; noms de
# colonnes en minuscules et rendus uniques.
lire_shp <- function(dir) {
  f <- list.files(dir, "\\.shp$", recursive = TRUE, full.names = TRUE)
  if (length(f) == 0) return(list())
  map(f, \(p) tryCatch({
    x <- st_read(p, quiet = TRUE)
    if (!all(as.character(st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) return(NULL)
    d <- st_drop_geometry(x); names(d) <- make.unique(tolower(names(d)), sep = "_")
    st_make_valid(st_sf(d, geometry = st_geometry(x)))
  }, error = \(e) NULL)) |>
    set_names(basename(f)) |> compact()
}
couche <- function(lst, avec, sans = character()) {
  for (x in lst) if (all(avec %in% names(x)) && !any(sans %in% names(x))) return(x)
  NULL
}
nom_col <- function(x, cands) intersect(cands, names(x))[1]

shp <- lire_shp(params$dir_shp)
shp_com  <- couche(shp, "adm3_pcode")
shp_prov <- couche(shp, "adm2_pcode", sans = "adm3_pcode")
# On ne garde que les colonnes utiles (évite doublons et conflits lors des jointures)
cols_nom2 <- c("adm2_fr", "adm2_name1", "adm2_name")
cols_nom3 <- c("adm3_fr", "adm3_name1", "adm3_name")
if (!is.null(shp_com))
  shp_com <- shp_com |> select(adm3_pcode, any_of(c("adm2_pcode", cols_nom3)))
if (!is.null(shp_prov))
  shp_prov <- shp_prov |> select(adm2_pcode, any_of(c(cols_nom2, "adm1_pco_1", "adm1_pcode")))
if (is.null(shp_prov) && !is.null(shp_com) && "adm2_pcode" %in% names(shp_com))
  shp_prov <- shp_com |> group_by(adm2_pcode) |> summarise(.groups = "drop")
if (!is.null(shp_prov)) message("Couche provinces : ", nrow(shp_prov), " polygones ; colonnes : ",
                                paste(setdiff(names(shp_prov), "geometry"), collapse = ", "))
if (is.null(shp_prov)) message("Aucun shapefile trouvé dans ", params$dir_shp, " : cartes non produites.")

shp_reg <- NULL
if (!is.null(shp_prov)) {
  # Appariement des provinces, polygone par polygone (avant : seulement si moins de 50 %
  # des Pcodes concordaient, ce qui laissait en gris les provinces recodées)
  nc <- nom_col(shp_prov, cols_nom2)
  shp_prov$adm2_pcode <- apparier_provinces(shp_prov$adm2_pcode,
                                            if (!is.na(nc)) shp_prov[[nc]] else rep(NA_character_, nrow(shp_prov)),
                                            ref_prov$admin2_pcode, ref_prov$province_std)
  # Ancienne région (13) : préfixe du Pcode province apparié, sinon colonne adm1_pco_1
  c1 <- substr(shp_prov$adm2_pcode, 1, 4)
  shp_prov$admin1_pcode <- if (mean(c1 %in% ref_reg$admin1_pcode, na.rm = TRUE) > 0.5) c1 else
    if ("adm1_pco_1" %in% names(shp_prov)) shp_prov$adm1_pco_1 else shp_prov$adm1_pcode
  # Nouvelles provinces (découpage 2025) sans données : rattachées à l'ancienne région d'une
  # province de la même nouvelle région (colonne adm1_pcode du fond de carte)
  if ("adm1_pcode" %in% names(shp_prov))
    for (k in which(is.na(shp_prov$admin1_pcode))) {
      j <- which(shp_prov$adm1_pcode == shp_prov$adm1_pcode[k] & !is.na(shp_prov$admin1_pcode))
      if (length(j)) shp_prov$admin1_pcode[k] <- shp_prov$admin1_pcode[j[1]]
    }
  shp_reg <- shp_prov |> group_by(admin1_pcode) |> summarise(.groups = "drop")
}

# Table communale : toutes les communes du fond de carte (0 PDI si absente de CONASUR)
base_com <- if (!is.null(shp_com)) {
  nc <- nom_col(shp_com, cols_nom3)
  b <- st_drop_geometry(shp_com)
  tibble(admin3_pcode = b$adm3_pcode,
         commune_shp = if (!is.na(nc)) as.character(b[[nc]]) else NA_character_,
         admin2_shp = if ("adm2_pcode" %in% names(b)) b$adm2_pcode else NA_character_)
} else select(com_der, admin3_pcode)

com_tab <- base_com |>
  left_join(select(com_der, admin3_pcode, commune, admin2_pcode, province, pdi, pdi_enfants), by = "admin3_pcode") |>
  mutate(admin2_pcode = coalesce(admin2_pcode, if ("admin2_shp" %in% names(base_com)) admin2_shp else NA_character_),
         commune = coalesce(commune, if ("commune_shp" %in% names(base_com)) commune_shp else NA_character_),
         pdi = coalesce(pdi, 0), pdi_enfants = coalesce(pdi_enfants, 0))
if (nrow(imde_prov) > 0) {
  com_tab <- com_tab |>
    left_join(select(imde_prov, admin2_pcode, province_imde = province, imde_prov = imde,
                     prov_prioritaire = prioritaire), by = "admin2_pcode") |>
    mutate(province = coalesce(province, province_imde))
} else {
  com_tab$imde_prov <- NA_real_; com_tab$prov_prioritaire <- NA
}
com_tab <- com_tab |>
  mutate(s_pdi = mm(log1p(pdi_enfants)),
         priorite = rowMeans(cbind(s_pdi, imde_prov), na.rm = TRUE),
         poche = coalesce(prov_prioritaire, FALSE) &
           pdi_enfants >= quantile(pdi_enfants[pdi_enfants > 0], 0.75, na.rm = TRUE)) |>
  arrange(desc(priorite))


# -----------------------------------------------------------------------------
# 11. Graphiques
# -----------------------------------------------------------------------------
cap <- "Sources : DGESS/MENAPLN, INSD OpenData, CONASUR. Calculs : équipe Data Challenge."

# 11a. Tableau des écarts de genre (IPS) par région, année de référence
ecarts_long <- imde_reg |>
  select(nom, Primaire = ips_prim, CEP = ips_cep,
         `Post-primaire` = ips_pp, Secondaire = ips_sec) |>
  pivot_longer(-nom, names_to = "indicateur", values_to = "ips") |>
  mutate(indicateur = factor(indicateur, c("Primaire", "CEP", "Post-primaire", "Secondaire")),
         nom = factor(nom, rev(imde_reg$nom)))
p_ecarts <- ggplot(ecarts_long, aes(indicateur, nom, fill = ips,
                                    text = paste0(nom, "<br>", indicateur, " : IPS ", fmt(ips)))) +
  geom_tile(colour = col$surface, linewidth = 1) +
  geom_text(aes(label = fmt(ips)), size = 3, colour = col$texte) +
  scale_fill_gradient2(low = col$rouge_div, mid = col$neutre, high = col$bleu_div, midpoint = 1,
                       limits = c(0.6, 1.4), oob = scales::squish, na.value = col$grille,
                       name = "IPS filles/garçons") +
  labs(title = paste0("Écarts filles-garçons par région (", annee_ref, ")"),
       subtitle = "Indice de parité : < 1 filles désavantagées, > 1 garçons ; parité entre 0,97 et 1,03",
       x = NULL, y = NULL, caption = cap) +
  theme_ecarts() + theme(panel.grid = element_blank())
sauver(p_ecarts, "01_tableau_ecarts_genre", 9, 7)

# 11b. Classement IMDE avec intervalle de sensibilité
p_rang <- imde_reg |>
  filter(!is.na(imde)) |>
  mutate(nom = fct_reorder(nom, -rang)) |>
  ggplot(aes(y = nom, text = paste0(nom, "<br>IMDE ", fmt(imde), "<br>Rang ", rang,
                                    " (", fmt(rang_p05, 0), "-", fmt(rang_p95, 0), ")<br>",
                                    "Prob. top : ", scales::percent(p_top, 1)))) +
  geom_linerange(aes(xmin = rang_p05, xmax = rang_p95), colour = col$gris, linewidth = 2) +
  geom_point(aes(x = rang), colour = col$accent, size = 3) +
  scale_x_reverse(breaks = 1:nrow(imde_reg)) +
  labs(title = "Classement des régions selon l'IMDE",
       subtitle = paste0("Point : rang à pondérations égales ; barre : 90 % des ", params$n_sim,
                         " pondérations simulées (1 = plus inégal)"),
       x = "Rang", y = NULL, caption = cap) +
  theme_ecarts()
sauver(p_rang, "02_classement_imde_sensibilite")

# 11c. Décomposition par dimension
p_dims <- imde_reg |>
  select(nom, any_of(c("genre", "acces", "moyens"))) |>
  pivot_longer(-nom, names_to = "dim", values_to = "score") |>
  mutate(dim = lib_dim[dim], nom = factor(nom, rev(imde_reg$nom))) |>
  ggplot(aes(score, nom, text = paste0(nom, "<br>", dim, " : ", fmt(score)))) +
  geom_col(fill = col$accent, width = 0.7) +
  facet_wrap(~dim) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  labs(title = "Ce qui fait l'inégalité : les trois dimensions de l'IMDE",
       subtitle = "0 = région la plus favorable, 1 = la plus défavorable", x = NULL, y = NULL, caption = cap) +
  theme_ecarts()
sauver(p_dims, "03_dimensions_imde", 10, 6)

# 11d. Parité nationale dans le temps
lbl_nat <- nat_genre |> group_by(niveau) |> slice_max(annee, n = 1)
p_nat <- ggplot(nat_genre, aes(annee, ips, colour = niveau, group = niveau,
                               text = paste0(niveau, " ", annee, " : IPS ", fmt(ips)))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.97, ymax = 1.03, fill = col$neutre) +
  geom_hline(yintercept = 1, colour = col$gris) +
  geom_line(linewidth = 0.9) +
  geom_point(data = lbl_nat, size = 2.5) +
  scale_colour_manual(values = setNames(c(col$serie, col$muet)[seq_along(unique(nat_genre$niveau))],
                                        unique(nat_genre$niveau)), name = NULL) +
  labs(title = "Parité filles/garçons au niveau national",
       subtitle = "Bande grise : zone de parité (0,97-1,03)", x = NULL, y = "IPS", caption = cap) +
  theme_ecarts()
sauver(p_nat, "04_parite_nationale")

# 11e. Heatmap région x année de l'IPS primaire
p_heat <- pan |>
  filter(cle != "national", !is.na(ips_prim)) |>
  mutate(nom = factor(nom, rev(imde_reg$nom))) |>
  ggplot(aes(factor(annee), nom, fill = ips_prim,
             text = paste0(nom, " ", annee, " : IPS ", fmt(ips_prim)))) +
  geom_tile(colour = col$surface, linewidth = 0.8) +
  scale_fill_gradient2(low = col$rouge_div, mid = col$neutre, high = col$bleu_div, midpoint = 1,
                       limits = c(0.8, 1.2), oob = scales::squish, name = "IPS") +
  labs(title = "Parité au primaire : évolution par région",
       subtitle = "Rouge : filles désavantagées ; bleu : garçons désavantagés",
       x = NULL, y = NULL, caption = cap) +
  theme_ecarts() + theme(panel.grid = element_blank())
sauver(p_heat, "05_evolution_parite_primaire", 10, 6)

# 11f. IMDE structurel dans le temps : 3 régions où les écarts se creusent le plus
top_evo <- head(tendance$cle, 3)
evo_p <- evo |> mutate(mis_en_avant = cle %in% top_evo)
p_evo <- ggplot(evo_p, aes(annee, imde, group = cle,
                           text = paste0(nom, " ", annee, " : ", fmt(imde)))) +
  geom_line(data = filter(evo_p, !mis_en_avant), colour = col$gris, linewidth = 0.6) +
  geom_line(data = filter(evo_p, mis_en_avant), colour = col$accent, linewidth = 1) +
  geom_text(data = evo_p |> filter(mis_en_avant) |> group_by(cle) |> slice_max(annee, n = 1),
            aes(label = nom), hjust = -0.1, size = 3.2, colour = col$texte) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(title = "IMDE structurel (genre + accès) : trajectoires régionales",
       subtitle = "En couleur : les 3 régions où les écarts se creusent le plus (pente)",
       x = NULL, y = "IMDE structurel", caption = cap) +
  theme_ecarts()
sauver(p_evo, "06_trajectoires_imde")

# 11g. Convergence
p_sigma <- ggplot(sigma, aes(annee, cv_tbs_prim, text = paste0(annee, " : CV ", fmt(cv_tbs_prim)))) +
  geom_line(colour = col$accent, linewidth = 0.9, group = 1) +
  geom_point(colour = col$accent, size = 2.5) +
  labs(title = "Dispersion du TBS primaire entre régions",
       subtitle = "Coefficient de variation : une hausse signifie que les régions divergent",
       x = NULL, y = "Coefficient de variation", caption = cap) +
  theme_ecarts()
sauver(p_sigma, "07_convergence_regions")

# 11h. Ville-campagne
mil_p <- milieu |> filter(indic == "eff_prim")
p_mil <- ggplot(mil_p, aes(annee, indice_100, colour = milieu, group = milieu,
                           text = paste0(milieu, " ", annee, " : ", fmt(indice_100, 0)))) +
  geom_hline(yintercept = 100, colour = col$gris) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_colour_manual(values = c(Urbain = col$urbain, Rural = col$rural), name = NULL) +
  labs(title = "Effectifs du primaire : urbain vs rural",
       subtitle = "Indice base 100 en première année", x = NULL, y = "Indice", caption = cap) +
  theme_ecarts()
sauver(p_mil, "08_effectifs_urbain_rural")

p_mil_f <- ggplot(mil_p, aes(annee, part_filles, colour = milieu, group = milieu,
                             text = paste0(milieu, " ", annee, " : ", fmt(part_filles, 1), " % de filles"))) +
  geom_hline(yintercept = 50, colour = col$gris) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_colour_manual(values = c(Urbain = col$urbain, Rural = col$rural), name = NULL) +
  labs(title = "Part des filles parmi les élèves du primaire",
       x = NULL, y = "% de filles", caption = cap) +
  theme_ecarts()
sauver(p_mil_f, "09_part_filles_milieu")

# 11i. Cartes statiques
if (!is.null(shp_reg)) {
  carte_reg <- shp_reg |> left_join(select(imde_reg, admin1_pcode, nom, imde, rang, typologie), by = "admin1_pcode")
  p_carte_reg <- ggplot(carte_reg) +
    geom_sf(aes(fill = imde), colour = col$surface, linewidth = 0.4) +
    geom_sf_text(data = filter(carte_reg, rang <= 3), aes(label = nom), size = 3, colour = col$texte) +
    scale_fill_gradientn(colours = rampe_bleu, limits = c(0, 1), na.value = col$grille, name = "IMDE") +
    labs(title = paste0("IMDE par région (", annee_ref, ")"),
         subtitle = "Plus foncé = plus inégal ; étiquettes : 3 régions les plus inégales", caption = cap) +
    theme_carte()
  sauver(p_carte_reg, "10_carte_imde_regions", 8, 7)
}
if (!is.null(shp_prov) && nrow(imde_prov) > 0) {
  carte_prov <- shp_prov |>
    left_join(select(imde_prov, admin2_pcode, province, region, imde, rang, typologie, prioritaire, poche_cachee),
              by = c("adm2_pcode" = "admin2_pcode"))     # shapefile : adm2_pcode ; résultats : admin2_pcode
  nc_carte <- nom_col(carte_prov, cols_nom2)
  gris_prov <- if (!is.na(nc_carte)) sort(as.character(carte_prov[[nc_carte]][is.na(carte_prov$imde)])) else character()
  p_carte_prov <- ggplot(carte_prov) +
    geom_sf(aes(fill = imde), colour = col$surface, linewidth = 0.3) +
    geom_sf(data = filter(carte_prov, coalesce(prioritaire, FALSE)), fill = NA,
            colour = col$critique, linewidth = 0.8) +
    geom_sf_text(data = filter(carte_prov, rang <= 10), aes(label = province), size = 2.6, colour = col$texte) +
    geom_sf(data = shp_reg, fill = NA, colour = col$texte2, linewidth = 0.5) +
    scale_fill_gradientn(colours = rampe_bleu, limits = c(0, 1), na.value = col$grille, name = "IMDE") +
    labs(title = paste0("Zones les plus inégales : IMDE par province (", annee_prov, ")"),
         subtitle = "Contour rouge : 20 % de provinces prioritaires ; étiquettes : 10 premières",
         caption = paste0(cap, if (length(gris_prov)) paste0("\nEn gris : nouvelles provinces (découpage 2025) sans données propres : ",
                                                             paste(gris_prov, collapse = ", "), ".") else "")) +
    theme_carte()
  sauver(p_carte_prov, "11_carte_imde_provinces", 8, 7)
}
if (!is.null(shp_com)) {
  carte_com <- shp_com |> left_join(select(com_tab, admin3_pcode, commune, province, pdi_enfants, priorite, poche),
                                    by = c("adm3_pcode" = "admin3_pcode"))
  p_carte_com <- ggplot(carte_com) +
    geom_sf(aes(fill = priorite), colour = col$surface, linewidth = 0.1) +
    geom_sf(data = filter(carte_com, coalesce(poche, FALSE)), fill = NA, colour = col$critique, linewidth = 0.5) +
    geom_sf(data = shp_reg, fill = NA, colour = col$texte2, linewidth = 0.4) +
    scale_fill_gradientn(colours = rampe_bleu, limits = c(0, 1), na.value = col$grille, name = "Priorité") +
    labs(title = "Communes : indice de priorité provisoire",
         subtitle = "IMDE de la province x enfants déplacés (CONASUR) ; contour rouge : poches",
         caption = paste(cap, "Indice provisoire en attendant l'estimation sur petits domaines.")) +
    theme_carte()
  sauver(p_carte_com, "12_carte_priorite_communes", 8, 7)
}


# -----------------------------------------------------------------------------
# 12. Exports tabulaires
# -----------------------------------------------------------------------------
exports <- list(
  imde_regions = imde_reg |> select(nom, admin1_pcode, annee, imde, rang, rang_med, rang_p05, rang_p95,
                                    p_top, imde_acp, any_of(c("genre", "acces", "moyens")), typologie,
                                    ips_prim, ips_cep, ips_pp, ips_sec,
                                    tbs_prim_T, tbs_pp_T, tbs_sec_T, transition,
                                    eleves_ecole, enf_dep_100),
  poids_acp = ou(res_reg$poids_acp, tibble()),
  tendances = tendance,
  convergence = sigma,
  parite_nationale = nat_genre,
  urbain_rural = milieu,
  provinces = if (nrow(imde_prov) > 0) imde_prov |>
    select(province, admin2_pcode, region, imde, rang, rang_med, rang_p05, rang_p95, p_top,
           any_of(c("genre", "acces", "contexte")), typologie, prioritaire, poche_cachee,
           ips_pri, ips_ppr, ips_sec, PRI_T, PPR_T, SEC_T, pdi_enfants) else tibble(),
  communes = com_tab |> select(admin3_pcode, commune, province, pdi, pdi_enfants, imde_prov, priorite, poche),
  panel_regions = pan
)
writexl::write_xlsx(exports, FIC$resultats_xlsx)
saveRDS(exports, FIC$resultats_rds)


# -----------------------------------------------------------------------------
# 13. Tableau de bord HTML
# -----------------------------------------------------------------------------
# ggplotly() échoue avec certaines combinaisons plotly / ggplot2 (« pm[[2]] : indice hors limites »).
# En cas d'échec, on affiche le PNG déjà enregistré dans sorties/figures : le tableau de bord est
# toujours produit. Pour retrouver l'interactivité : install.packages("plotly") (version >= 4.10.4).
vers_plotly <- function(p, h = 380, nom) {
  w <- tryCatch(suppressWarnings(plotly::ggplotly(p, tooltip = "text", height = h)),
                error = function(e) e)
  if (!inherits(w, "error")) return(plotly::config(w, displaylogo = FALSE))
  message("ggplotly a échoué pour ", nom, " (", conditionMessage(w), ") : image statique utilisée.")
  tags$img(src = file.path("figures", basename(chemin_fig(nom))), alt = nom,
           style = "width:100%;height:auto;display:block")
}

# Carte interactive (3 niveaux, un seul affiché à la fois)
carte_web <- NULL
if (!is.null(shp_reg)) {
  pal <- leaflet::colorNumeric(rampe_bleu, domain = c(0, 1), na.color = col$grille)
  cr <- carte_reg |> st_transform(4326) |>
    mutate(popup = sprintf("<b>%s</b><br>IMDE : %s (rang %s)<br>%s", nom, fmt(imde), rang, typologie))
  carte_web <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 5)) |>
    # Fond gris Esri : sans clé d'accès (les fonds CARTO exigent désormais une clé API)
    leaflet::addProviderTiles("Esri.WorldGrayCanvas") |>
    leaflet::addPolygons(data = cr, group = "Régions (IMDE)", fillColor = ~pal(imde), fillOpacity = 0.85,
                         color = col$surface, weight = 1.5, label = ~nom, popup = ~popup,
                         highlightOptions = leaflet::highlightOptions(weight = 3, color = col$texte, bringToFront = TRUE))
  groupes <- "Régions (IMDE)"
  if (exists("carte_prov")) {
    cp <- carte_prov |> st_transform(4326) |>
      mutate(popup = sprintf("<b>%s</b> (%s)<br>IMDE : %s, rang %s<br>%s%s", province, region, fmt(imde), rang,
                             typologie, if_else(coalesce(poche_cachee, FALSE), "<br><b>Poche cachée</b>", "")))
    carte_web <- carte_web |>
      leaflet::addPolygons(data = cp, group = "Provinces (IMDE)", fillColor = ~pal(imde), fillOpacity = 0.85,
                           color = ~if_else(coalesce(prioritaire, FALSE), col$critique, col$surface),
                           weight = ~if_else(coalesce(prioritaire, FALSE), 2.5, 1),
                           label = ~province, popup = ~popup,
                           highlightOptions = leaflet::highlightOptions(weight = 3, color = col$texte, bringToFront = TRUE))
    groupes <- c(groupes, "Provinces (IMDE)")
  }
  if (exists("carte_com")) {
    cc <- carte_com |> st_transform(4326) |>
      mutate(popup = sprintf("<b>%s</b> (%s)<br>Enfants déplacés : %s<br>Priorité : %s%s", commune, province,
                             fmt(pdi_enfants, 0), fmt(priorite), if_else(coalesce(poche, FALSE), "<br><b>Poche</b>", "")))
    carte_web <- carte_web |>
      leaflet::addPolygons(data = cc, group = "Communes (priorité)", fillColor = ~pal(priorite), fillOpacity = 0.85,
                           color = ~if_else(coalesce(poche, FALSE), col$critique, col$surface),
                           weight = ~if_else(coalesce(poche, FALSE), 2, 0.4),
                           label = ~commune, popup = ~popup)
    groupes <- c(groupes, "Communes (priorité)")
  }
  carte_web <- carte_web |>
    leaflet::addLegend("bottomright", pal = pal, values = c(0, 1), title = "0 = moins inégal<br>1 = plus inégal") |>
    leaflet::addLayersControl(baseGroups = groupes, options = leaflet::layersControlOptions(collapsed = FALSE))
}

# Indicateurs clés
nat_ref <- pan |> filter(cle == "national", annee == annee_ref)
tbs_reg <- imde_reg$tbs_prim_T
kpi <- function(valeur, libelle, note) {
  div(class = "kpi", div(class = "v", valeur), div(class = "l", libelle), div(class = "n", note))
}
kpis <- div(class = "kpis",
            kpi(paste0(fmt(nat_ref$tbs_prim_T, 1), " %"), paste("TBS primaire national", annee_ref), "DGESS"),
            kpi(fmt(nat_ref$ips_prim), "Parité filles/garçons au primaire", "1 = parité"),
            kpi(paste0(fmt(min(tbs_reg, na.rm = TRUE), 1), " - ", fmt(max(tbs_reg, na.rm = TRUE), 1), " %"),
                "TBS primaire : région min - max", "écart spatial"),
            kpi(fmt(sum(com_der$pdi_enfants), 0), "Enfants déplacés internes",
                paste("CONASUR,", format(date_der, "%d/%m/%Y"))),
            kpi(if (nrow(imde_prov) > 0) as.character(sum(imde_prov$prioritaire, na.rm = TRUE)) else "n.d.",
                "Provinces prioritaires", "20 % les plus inégales")
)

carte_ <- function(titre, sous_titre, widget, pleine = FALSE) {
  div(class = paste("card", if (pleine) "full" else ""), h2(titre), p(sous_titre), widget)
}
tab_reg <- DT::datatable(
  exports$imde_regions |>
    transmute(Région = nom, Rang = rang, IMDE = round(imde, 2),
              `Rang (sensibilité)` = paste0(round(rang_p05), "-", round(rang_p95)),
              Genre = round(genre, 2), Accès = round(acces, 2),
              `Moyens` = if ("moyens" %in% names(exports$imde_regions)) round(moyens, 2) else NA,
              Typologie = typologie, `IPS primaire` = round(ips_prim, 2),
              `TBS primaire` = round(tbs_prim_T, 1), `TBS post-primaire` = round(tbs_pp_T, 1)),
  rownames = FALSE, options = list(pageLength = 13, dom = "tp"))
tab_com <- DT::datatable(
  com_tab |> filter(pdi_enfants > 0) |> head(30) |>
    transmute(Commune = commune, Province = province, `Enfants déplacés` = round(pdi_enfants),
              `IMDE province` = round(imde_prov, 2), Priorité = round(priorite, 2), Poche = if_else(poche, "oui", "")),
  rownames = FALSE, options = list(pageLength = 10, dom = "tp"))

css <- "
body{margin:0;background:#f9f9f7;font-family:system-ui,-apple-system,'Segoe UI',sans-serif;color:#0b0b0b}
.wrap{max-width:1320px;margin:0 auto;padding:24px}
h1{font-size:22px;margin:0 0 4px}.sub{color:#52514e;margin:0 0 20px;font-size:14px}
.kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:12px;margin-bottom:16px}
.kpi,.card{background:#fcfcfb;border:1px solid rgba(11,11,11,.1);border-radius:10px;padding:14px 16px}
.kpi .v{font-size:26px;font-weight:600}.kpi .l{color:#52514e;font-size:13px}.kpi .n{color:#898781;font-size:12px;margin-top:4px}
.grid{display:grid;grid-template-columns:1fr 1fr;gap:16px}.full{grid-column:1/-1}
.card h2{font-size:15px;margin:0 0 2px}.card p{color:#52514e;font-size:13px;margin:0 0 10px}
.note{color:#898781;font-size:12px;margin-top:20px;line-height:1.5}
@media (max-width:900px){.grid{grid-template-columns:1fr}}
"

page <- tagList(
  tags$head(tags$meta(charset = "utf-8"), tags$title("Écarts d'accès à l'éducation - Burkina Faso"),
            tags$style(HTML(css))),
  div(class = "wrap",
      h1("Où sont les écarts d'accès à l'éducation, et comment évoluent-ils ?"),
      p(class = "sub", paste0("Indice multidimensionnel de disparité éducative (IMDE), régions ", annee_ref,
                              if (nrow(imde_prov) > 0) paste0(", provinces ", annee_prov) else "",
                              ", communes : déplacement forcé au ", format(date_der, "%d/%m/%Y"), ".")),
      kpis,
      div(class = "grid",
          if (!is.null(carte_web)) carte_("Carte des zones les plus inégales",
                                          "Choisir le niveau en haut à droite. Contour rouge : provinces prioritaires ou poches communales.",
                                          carte_web, pleine = TRUE),
          carte_("Tableau des écarts filles-garçons", "IPS par niveau et par région", vers_plotly(p_ecarts, 460, "01_tableau_ecarts_genre")),
          carte_("Classement IMDE et sensibilité", "Rang stable si la barre est courte", vers_plotly(p_rang, 460, "02_classement_imde_sensibilite")),
          carte_("Les trois dimensions de l'IMDE", "Distingue l'exclusion des filles de l'exclusion générale",
                 vers_plotly(p_dims, 420, "03_dimensions_imde"), pleine = TRUE),
          carte_("Parité nationale dans le temps", "Par niveau d'enseignement", vers_plotly(p_nat, 380, "04_parite_nationale")),
          carte_("Parité au primaire par région", "Évolution annuelle", vers_plotly(p_heat, 380, "05_evolution_parite_primaire")),
          carte_("Trajectoires de l'IMDE structurel", "Genre + accès, échelle commune à toutes les années", vers_plotly(p_evo, 380, "06_trajectoires_imde")),
          carte_("Convergence entre régions", "Dispersion du TBS primaire", vers_plotly(p_sigma, 380, "07_convergence_regions")),
          carte_("Ville-campagne : effectifs", "Primaire, base 100", vers_plotly(p_mil, 380, "08_effectifs_urbain_rural")),
          carte_("Ville-campagne : part des filles", "Primaire", vers_plotly(p_mil_f, 380, "09_part_filles_milieu")),
          carte_("Classement des régions", "Exportable dans resultats_ecarts_education.xlsx", tab_reg, pleine = TRUE),
          carte_("Communes à cibler en priorité", "Indice provisoire : IMDE de la province x enfants déplacés", tab_com, pleine = TRUE)
      ),
      p(class = "note", HTML(paste0(
        "Méthode : chaque indicateur est ramené entre 0 (situation la plus favorable) et 1 ; ",
        "les dimensions sont des moyennes, l'IMDE la moyenne des dimensions. Écart de genre = |log IPS|. ",
        "Le TBS est plafonné à 100 % dans les déficits d'accès. Sensibilité : ", params$n_sim,
        " tirages de Dirichlet sur les poids ; corrélation de rang avec la pondération ACP : ", fmt(cor_acp), ". ",
        "Les années OPENDATA sont décalées de ", decal, " an(s) pour s'aligner sur la DGESS. ",
        "L'écart urbain-rural n'est disponible qu'au niveau national. L'indice communal est provisoire : ",
        "il sera remplacé par l'estimation sur petits domaines dès que les annuaires communaux et l'EHCVM seront intégrés.<br>",
        cap)))
  )
)
f_html <- FIC$tableau_bord
htmltools::save_html(page, file = f_html, libdir = "lib")


# -----------------------------------------------------------------------------
# 14. Résumé console
# -----------------------------------------------------------------------------
cat("\n================ RÉSULTATS ================\n")
cat("Régions les plus inégales (IMDE", annee_ref, ") :\n")
print(imde_reg |> select(nom, imde, rang, rang_p05, rang_p95, typologie) |> head(5))
cat("\nRégions où les écarts se creusent le plus :\n")
print(head(select(tendance, nom, periode, pente_imde, sens), 3))
if (nrow(imde_prov) > 0) {
  cat("\nProvinces prioritaires :\n")
  print(imde_prov |> filter(prioritaire) |> select(province, region, imde, typologie, poche_cachee))
}
cat("\nFichiers produits dans", params$dir_out, ":\n",
    "-", basename(FIC$tableau_bord), "(+ dossier lib/)\n",
    "-", basename(FIC$resultats_rds), "/ .xlsx\n",
    "- figures/*.png\n")