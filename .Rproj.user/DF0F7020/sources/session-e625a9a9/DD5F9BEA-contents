# =====================================================================
# DATA CHALLENGE BURKINA, DÉFI 2 : disparités d'accès à l'éducation
# Question : où les écarts d'accès (filles-garçons, ville-campagne) sont-ils
# les plus grands, et évoluent-ils ?
# Périmètre : préscolaire et primaire, années scolaires 2015/2016 à 2024/2025.
# Échelle : région (DGESS) et province (OpenData), pas de commune.
#
# Entrées (étape 02, chemins dans le fichier de configuration) :
#   Data/cleandata/binogoya_02-nettoyage_D2_engineer_dgess-tables_v1.rds
#   Data/cleandata/binogoya_02-nettoyage_D2_engineer_opendata-tables_v1.rds
#   Data/cleandata/binogoya_02-nettoyage_D2_engineer_conasur-pdi_v1.rds   (facultatif)
#   Data/shapefile/admin_boundaries/*.shp                                   (facultatif)
# Sorties : 04_analyse/sorties/binogoya_04-analyse_D2_analyst_parite-*_v1.csv
#           04_analyse/sorties/figures/binogoya_04-analyse_D2_analyst_fig-parite-*_v1.png
# =====================================================================
source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("tidyverse", "stringi", "sf"))
suppressPackageStartupMessages({ library(tidyverse); library(stringi) })

## 0. PARAMÈTRES -------------------------------------------------------
P <- list(
  ans            = 2016:2025,       # an_fin : 2016 = année scolaire 2015/2016
  seuil_parite   = c(0.97, 1.03),   # convention de parité, à justifier dans la note
  an_rupture     = 2020,            # 1re année de la période « après » (2019/2020)
  seuil_tendance = 0.10,            # seuil du test de Mann-Kendall (séries courtes, n = 10)
  plage_ips      = c(0.3, 3),       # IPS jugés plausibles (écarte les valeurs mal lues)
  plage_taux     = c(0, 200),       # taux (%) jugés plausibles
  niveaux_od     = c(PRE = "3 5 ANS", PRI = "6 A 11 ANS"),  # tranche d'âge retenue par niveau
  dir_shp        = CH$shp_admin,
  dir_sortie     = CH$sorties
)

## 1. OUTILS -----------------------------------------------------------
norm <- function(x) x |> as.character() |> stri_trans_general("Latin-ASCII") |>
  str_to_upper() |> str_replace_all("[^A-Z0-9]+", " ") |> str_squish()

REGIONS <- c("BOUCLE DU MOUHOUN", "CASCADES", "CENTRE", "CENTRE EST", "CENTRE NORD",
             "CENTRE OUEST", "CENTRE SUD", "EST", "HAUTS BASSINS", "NORD",
             "PLATEAU CENTRAL", "SAHEL", "SUD OUEST")

std_region <- function(x) {
  z <- norm(x)
  if_else(z %in% c("AUTS BASSINS", "HAUT BASSINS", "LES HAUTS BASSINS"), "HAUTS BASSINS", z)
}

std_sexe <- function(x) {
  z <- norm(x)
  case_when(z %in% c("G", "MAS", "GARCONS", "MASCULIN", "HOMMES") ~ "G",
            z %in% c("F", "FEM", "FILLES", "FEMININ")             ~ "F",
            z %in% c("T", "ENS", "TOTAL", "ENSEMBLE")             ~ "T",
            TRUE ~ NA_character_)
}

# « 2015-2016 » -> 2016 ; « 2015/16 » ou « 2015-16 » -> 2016 ; « 2016 » -> 2016
# (hypothèse : année seule = année de fin). Correction : l'ancienne version ne lisait que les
# groupes de 4 chiffres et renvoyait 2015 pour « 2015/16 ».
an_fin <- function(x) {
  x <- str_squish(as.character(x))
  deb <- suppressWarnings(as.numeric(str_extract(x, "(19|20)\\d{2}")))
  fin4 <- suppressWarnings(as.numeric(str_match(x, "(19|20)\\d{2}\\s*[-/]\\s*((19|20)\\d{2})")[, 3]))   # 3e colonne = année de fin sur 4 chiffres
  fin2 <- str_detect(coalesce(x, ""), "(19|20)\\d{2}\\s*[-/]\\s*\\d{2}$")
  case_when(!is.na(fin4) ~ fin4,
            fin2 ~ deb + 1,
            TRUE ~ deb)
}

# Reconnaît l'indicateur d'un tableau DGESS à partir de son titre
typer <- function(titre) {
  t <- norm(titre)
  case_when(
    str_detect(t, "IPS DU TBA")                        ~ "ips_tba_pri",
    str_detect(t, "IPS DU TBS AU PRESCOLAIRE")         ~ "ips_tbs_pre",
    str_detect(t, "IPS DU TBS")                        ~ "ips_tbs_pri",
    str_detect(t, "IPS DU TAP|ACHEVEMENT")             ~ "hors_perimetre",  # rétention : autre défi
    str_detect(t, "REUSSITE AU CEP")                   ~ "hors_perimetre",
    str_detect(t, "DU TBA AU PRIMAIRE")                ~ "tba_pri",
    str_detect(t, "DU TBS AU PRESCOLAIRE")             ~ "tbs_pre",
    str_detect(t, "DU TBS AU PRIMAIRE")                ~ "tbs_pri",
    str_detect(t, "NOUVEAUX INSCRITS")                 ~ "cp1",
    str_detect(t, "EFFECTIFS DU PRESCOLAIRE.*MILIEU")  ~ "eff_pre_milieu",
    str_detect(t, "EFFECTIFS DU PRIMAIRE.*MILIEU")     ~ "eff_pri_milieu",
    str_detect(t, "EFFECTIFS DU PRESCOLAIRE.*REGION")  ~ "eff_pre_region",
    str_detect(t, "EFFECTIFS DU PRIMAIRE.*REGION")     ~ "eff_pri_region",
    str_detect(t, "NOMBRE D ECOLES PRIMAIRES")         ~ "ecoles_pri",
    TRUE ~ "autre")
}

classer <- function(x) case_when(
  is.na(x) ~ NA_character_,
  x < P$seuil_parite[1] ~ "Défavorable aux filles",
  x > P$seuil_parite[2] ~ "Défavorable aux garçons",
  TRUE ~ "Parité")

# Passe les sexes en colonnes Fil / Gar / Tot
wide_fg <- function(d, val = "valeur") {
  w <- d |> pivot_wider(names_from = sexe, values_from = all_of(val))
  for (cc in c("F", "G", "T")) if (!cc %in% names(w)) w[[cc]] <- NA_real_
  w |> rename(Fil = all_of("F"), Gar = all_of("G"), Tot = all_of("T"))
}

# Tendance d'une série courte : pente de Sen + test de Mann-Kendall
tendance <- function(x, y) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 6) return(tibble(n = length(x), pente_sen = NA_real_, tau = NA_real_, p_mk = NA_real_))
  pr <- combn(length(x), 2)
  sl <- (y[pr[2, ]] - y[pr[1, ]]) / (x[pr[2, ]] - x[pr[1, ]])
  ct <- suppressWarnings(cor.test(x, y, method = "kendall", exact = FALSE))
  tibble(n = length(x), pente_sen = median(sl), tau = unname(ct$estimate), p_mk = ct$p.value)
}
sens_tendance <- function(p, pente) case_when(
  is.na(p) ~ NA_character_,
  p < P$seuil_tendance & pente < 0 ~ "Convergence",
  p < P$seuil_tendance & pente > 0 ~ "Divergence",
  TRUE ~ "Pas de tendance nette")

# Noms conformes : binogoya_04-analyse_D2_analyst_parite-<nom>_v1.csv / fig-parite-<nom>
slug <- function(x) gsub("_", "-", x)
sortie <- function(df, nom) readr::write_excel_csv(
  df, file.path(P$dir_sortie, nf("04_analyse", "analyst", paste0("parite-", slug(nom)), "csv")))
fig <- function(p, nom, w = 9, h = 6) ggsave(
  file.path(CH$figures, nf("04_analyse", "analyst", paste0("fig-parite-", slug(nom)), "png")),
  p, width = w, height = h, dpi = 150)

## 2. DONNÉES DGESS RÉGIONALES ------------------------------------------
exiger(FIC$dgess, "02_nettoyage/binogoya_02-nettoyage_D2_engineer_dgess-nettoyage_v1.R")
exiger(FIC$opendata, "02_nettoyage/binogoya_02-nettoyage_D2_engineer_opendata-nettoyage_v1.R")
dg <- readRDS(FIC$dgess)$tables
op <- readRDS(FIC$opendata)

reg_brut <- bind_rows(dg$A_region, dg$B_region, dg$C_region) |>
  mutate(indic = typer(titre), an_fin = an_fin(annee_scolaire),
         sexe = std_sexe(sexe), reg_std = std_region(region))

cat("\n== Indicateurs reconnus (nombre de lignes) ==\n"); print(count(reg_brut, indic))
cat("\n== Libellés de région écartés (totaux, milieux, etc.) ==\n")
print(reg_brut |> filter(!reg_std %in% REGIONS) |> count(region, sort = TRUE))

reg <- reg_brut |>
  filter(reg_std %in% REGIONS, !indic %in% c("autre", "hors_perimetre"), an_fin %in% P$ans) |>
  arrange(is.na(valeur))

dup <- reg |> count(indic, reg_std, an_fin, sexe) |> filter(n > 1)
if (nrow(dup)) message("Doublons dans les tableaux DGESS (premier relevé conservé) : ", nrow(dup))
reg <- distinct(reg, indic, reg_std, an_fin, sexe, .keep_all = TRUE)

# Valeurs aberrantes (ex. une année lue comme valeur d'IPS) : mises à NA et comptées
est_ips  <- str_starts(reg$indic, "ips_")
est_taux <- reg$indic %in% c("tba_pri", "tbs_pri", "tbs_pre")
aberr <- (est_ips  & (reg$valeur < P$plage_ips[1]  | reg$valeur > P$plage_ips[2])) |
  (est_taux & (reg$valeur < P$plage_taux[1] | reg$valeur > P$plage_taux[2]))
aberr[is.na(aberr)] <- FALSE
cat("\nValeurs aberrantes mises à NA :", sum(aberr), "\n")
if (sum(aberr)) print(reg[aberr, c("indic", "reg_std", "an_fin", "sexe", "valeur")])
reg$valeur[aberr] <- NA_real_

## 3. ÉCARTS FILLES-GARÇONS PAR RÉGION -----------------------------------
# IPS recalculé (filles / garçons) à partir des taux, puis comparé à l'IPS publié
ips_calc <- reg |> filter(indic %in% c("tba_pri", "tbs_pri", "tbs_pre"), sexe %in% c("F", "G")) |>
  select(indic, reg_std, an_fin, sexe, valeur) |> wide_fg() |>
  transmute(indic = paste0("ips_", indic), reg_std, an_fin, ips_calc = Fil / Gar, ecart_pts = Fil - Gar)

ips_pub <- reg |> filter(str_starts(indic, "ips_")) |> select(indic, reg_std, an_fin, ips_pub = valeur)

controle <- inner_join(ips_calc, ips_pub, by = c("indic", "reg_std", "an_fin")) |>
  mutate(diff = abs(ips_calc - ips_pub))
cat("\n== Contrôle : IPS recalculé vs IPS publié ==\n")
print(controle |> group_by(indic) |>
        summarise(n = n(), diff_mediane = median(diff, na.rm = TRUE), diff_max = max(diff, na.rm = TRUE)))

ips <- full_join(ips_calc, ips_pub, by = c("indic", "reg_std", "an_fin")) |>
  mutate(ips = coalesce(ips_pub, ips_calc), ecart_parite = abs(ips - 1), classe = classer(ips)) |>
  filter(!is.na(ips), indic %in% c("ips_tba_pri", "ips_tbs_pri", "ips_tbs_pre"))

# Où les écarts sont-ils les plus grands ? Moyenne des 3 dernières années
recent <- ips |> group_by(indic) |> filter(an_fin > max(an_fin) - 3) |>
  group_by(indic, reg_std) |>
  summarise(ips_moy = mean(ips), ecart_moy = mean(ecart_parite), n_ans = n(), .groups = "drop") |>
  group_by(indic) |> mutate(rang = min_rank(desc(ecart_moy)), classe = classer(ips_moy)) |>
  ungroup() |> arrange(indic, rang)
cat("\n== Régions les plus éloignées de la parité (3 dernières années) ==\n")
print(recent |> group_by(indic) |> slice_head(n = 3))

## 4. ÉVOLUTION DES ÉCARTS ---------------------------------------------
tend_ecart <- ips |> group_by(indic, reg_std) |>
  group_modify(~ tendance(.x$an_fin, .x$ecart_parite)) |> ungroup() |>
  mutate(sens = sens_tendance(p_mk, pente_sen))      # pente < 0 : l'écart à la parité se réduit

avant_apres <- ips |> mutate(periode = if_else(an_fin < P$an_rupture, "avant", "apres")) |>
  group_by(indic, reg_std, periode) |> summarise(ecart = mean(ecart_parite), .groups = "drop") |>
  pivot_wider(names_from = periode, values_from = ecart, names_prefix = "ecart_") |>
  mutate(delta_ecart = ecart_apres - ecart_avant)

# Les régions se rapprochent-elles entre elles ? (dispersion régionale par année)
sigma_ips <- ips |> group_by(indic, an_fin) |>
  summarise(sd_ips = sd(ips), etendue = max(ips) - min(ips), n_reg = n(), .groups = "drop")

sigma_taux <- reg |> filter(indic %in% c("tba_pri", "tbs_pri", "tbs_pre"), sexe %in% c("F", "G", "T"), !is.na(valeur)) |>
  group_by(indic, sexe, an_fin) |>
  summarise(moy = mean(valeur), sd = sd(valeur), cv = sd / moy, etendue = max(valeur) - min(valeur),
            n_reg = n(), .groups = "drop")
tend_sigma <- sigma_taux |> filter(n_reg >= 10) |> group_by(indic, sexe) |>
  group_modify(~ tendance(.x$an_fin, .x$cv)) |> ungroup() |>
  mutate(sens = sens_tendance(p_mk, pente_sen))
cat("\n== Dispersion régionale des taux (CV) : tendance ==\n"); print(tend_sigma)

## 5. ÉCART VILLE-CAMPAGNE (effectifs : pas de taux par milieu dans la DGESS) ----
mil <- bind_rows(
  dg$B_milieu |> transmute(niveau = "primaire", milieu = norm(milieu), an_fin = an_fin(annee_scolaire),
                           sexe = std_sexe(sexe), eff = valeur),
  reg_brut |> filter(indic == "eff_pre_milieu") |>   # préscolaire : milieu stocké dans la colonne région
    transmute(niveau = "prescolaire", milieu = norm(region), an_fin, sexe, eff = valeur)
) |> filter(milieu %in% c("URBAIN", "RURAL", "NATIONAL"), an_fin %in% P$ans)

cat("\nLignes milieu sans sexe (ignorées) :", sum(is.na(mil$sexe)), "\n")
mil_w <- mil |> filter(!is.na(sexe)) |> arrange(is.na(eff)) |>
  distinct(niveau, milieu, an_fin, sexe, .keep_all = TRUE) |> wide_fg(val = "eff") |>
  mutate(Tot = coalesce(Tot, Fil + Gar))

milieu_long <- mil_w |> filter(milieu %in% c("URBAIN", "RURAL")) |>
  mutate(part_filles = Fil / (Fil + Gar), rapport_FG = Fil / Gar) |>
  group_by(niveau, milieu) |> arrange(an_fin, .by_group = TRUE) |>
  mutate(indice_100 = 100 * Tot / first(na.omit(Tot))) |> ungroup()

ecart_milieu <- milieu_long |>
  select(niveau, milieu, an_fin, Tot, part_filles, rapport_FG, indice_100) |>
  pivot_wider(names_from = milieu, values_from = c(Tot, part_filles, rapport_FG, indice_100)) |>
  mutate(part_rurale = Tot_RURAL / (Tot_RURAL + Tot_URBAIN),
         diff_part_filles = part_filles_RURAL - part_filles_URBAIN,
         ratio_indices = indice_100_RURAL / indice_100_URBAIN)

tend_milieu <- ecart_milieu |> group_by(niveau) |>
  group_modify(~ bind_rows(
    tendance(.x$an_fin, .x$part_rurale)      |> mutate(mesure = "part_rurale"),
    tendance(.x$an_fin, .x$diff_part_filles) |> mutate(mesure = "diff_part_filles"))) |> ungroup()
cat("\n== Ville-campagne : tendances ==\n"); print(tend_milieu)

# Option : si vous fournissez la population par milieu (RGPH 2019, projections), on passe aux taux
#   pop_milieu : colonnes niveau, milieu (URBAIN/RURAL), an_fin, pop   (population en âge scolaire)
taux_milieu <- NULL
if (exists("pop_milieu", envir = globalenv())) {
  taux_milieu <- mil_w |> filter(milieu %in% c("URBAIN", "RURAL")) |>
    inner_join(get("pop_milieu", envir = globalenv()), by = c("niveau", "milieu", "an_fin")) |>
    mutate(taux_brut = 100 * Tot / pop)
  cat("\nTaux par milieu calculés sur", nrow(taux_milieu), "lignes (dénominateurs externes).\n")
}

## 6. PROVINCES (OpenData : taux de scolarisation par sexe, 2005-2025) -----
od_liste <- op |> keep(\(d) all(c("ordre_d_enseignement", "tranche_d_ages_name", "region_name", "sexe", "valeur") %in% names(d)))
# priorité à la table région + province 2005-2025 du dossier TAUX_SCO_REGION
dos <- attr(op, "dossier")
hors_dossier <- if (is.null(dos)) rep(FALSE, length(od_liste)) else coalesce(dos[names(od_liste)] != "TAUX_SCO_REGION", TRUE)
od_liste <- od_liste[order(!grepl("2005_2025", names(od_liste)), hors_dossier)]
if (length(od_liste)) message("Table OpenData provinces retenue : ", names(od_liste)[1])
ips_geo <- rang_prov <- tend_prov <- NULL
if (length(od_liste)) {
  od <- od_liste[[1]] |>
    transmute(niveau = ordre_d_enseignement, tranche = norm(tranche_d_ages_name), nom = region_name,
              reg_std = std_region(region_name), an = annee, sexe = std_sexe(sexe), valeur) |>
    filter(niveau %in% names(P$niveaux_od), tranche == unname(P$niveaux_od[niveau])) |>
    mutate(geo = case_when(norm(nom) == "BURKINA FASO" ~ "national",
                           reg_std %in% REGIONS ~ "region", TRUE ~ "province"),
           valeur = if_else(valeur < P$plage_taux[1] | valeur > P$plage_taux[2], NA_real_, valeur))
  
  # Alignement des années : on teste un décalage de -1, 0, +1 an contre le TBS primaire DGESS
  dg_tbs <- reg |> filter(indic == "tbs_pri", !is.na(sexe)) |> select(reg_std, an_fin, sexe, v_dg = valeur)
  lags <- map_dfr(-1:1, \(l) od |> filter(geo == "region", niveau == "PRI") |> mutate(an_fin = an + l) |>
                    inner_join(dg_tbs, by = c("reg_std", "an_fin", "sexe")) |>
                    summarise(decalage = l, n = n(), cor = suppressWarnings(cor(valeur, v_dg, use = "complete.obs")),
                              ecart_abs_moyen = mean(abs(valeur - v_dg), na.rm = TRUE)))
  cat("\n== OpenData vs DGESS (TBS primaire) : test de décalage d'année ==\n"); print(lags)
  decal <- if (nrow(lags) && any(is.finite(lags$ecart_abs_moyen))) lags$decalage[which.min(lags$ecart_abs_moyen)] else 0
  cat("Décalage retenu :", decal, "(à vérifier : un écart moyen élevé signifie que les deux indicateurs diffèrent)\n")
  
  ips_geo <- od |> mutate(an_fin = an + decal) |> filter(sexe %in% c("F", "G"), an_fin %in% P$ans) |>
    select(geo, nom, reg_std, niveau, an_fin, sexe, valeur) |> wide_fg() |>
    mutate(ips = Fil / Gar, ecart_pts = Fil - Gar, ecart_parite = abs(ips - 1))
  
  rang_prov <- ips_geo |> filter(geo == "province", !is.na(ips)) |> group_by(niveau) |>
    filter(an_fin > max(an_fin) - 3) |> group_by(niveau, nom) |>
    summarise(ips_moy = mean(ips), ecart_moy = mean(ecart_parite), n_ans = n(), .groups = "drop") |>
    group_by(niveau) |> mutate(rang = min_rank(desc(ecart_moy)), classe = classer(ips_moy)) |>
    ungroup() |> arrange(niveau, rang)
  tend_prov <- ips_geo |> filter(geo == "province", !is.na(ips)) |> group_by(niveau, nom) |>
    group_modify(~ tendance(.x$an_fin, .x$ecart_parite)) |> ungroup() |>
    mutate(sens = sens_tendance(p_mk, pente_sen))
  cat("\n== Provinces les plus éloignées de la parité (primaire) ==\n")
  print(rang_prov |> filter(niveau == "PRI") |> slice_head(n = 10))
} else message("Table OpenData région/province introuvable : section provinces ignorée.")

## 7. CONTEXTE (explicatif seulement, n = 13 régions : corrélations indicatives) ----
contexte <- NULL
cn <- if (file.exists(FIC$conasur)) readRDS(FIC$conasur) else NULL
if (!is.null(cn)) {
  stock <- cn |> mutate(an = as.integer(format(date_donnee, "%Y")),
                        reg_std = std_region(coalesce(region_std, region)),
                        lieu = coalesce(commune_std, commune)) |>
    group_by(reg_std, lieu, an) |> slice_max(date_donnee, n = 1, with_ties = FALSE) |>
    group_by(reg_std, an) |> summarise(enfants_deplaces = sum(coalesce(total_enfants, enfants_moins_5 + enfants_plus_5), na.rm = TRUE),
                                       .groups = "drop") |>
    filter(reg_std %in% REGIONS)
  an_cn <- stock |> count(an) |> filter(n >= 10) |> slice_max(an, n = 1) |> pull(an)
  if (length(an_cn)) {
    contexte <- avant_apres |> filter(indic == "ips_tbs_pri") |> select(reg_std, delta_ecart) |>
      left_join(stock |> filter(an == an_cn) |> select(reg_std, enfants_deplaces), by = "reg_std")
    r <- suppressWarnings(cor.test(contexte$delta_ecart, contexte$enfants_deplaces, method = "spearman", exact = FALSE))
    cat("\nEnfants déplacés (", an_cn, ") vs évolution de l'écart de parité : rho =", round(r$estimate, 2),
        ", p =", round(r$p.value, 3), "(indicatif)\n")
  }
}

ecoles <- dg$D_region_statut |> mutate(indic = typer(titre), reg_std = std_region(region),
                                       an_fin = an_fin(annee_scolaire), statut = norm(statut)) |>
  filter(indic == "ecoles_pri", statut %in% c("TOTAL", "TOTAL GENERAL"), reg_std %in% REGIONS, an_fin %in% P$ans) |>
  distinct(reg_std, an_fin, .keep_all = TRUE) |> select(reg_std, an_fin, n_ecoles = valeur)
offre <- reg |> filter(indic == "eff_pri_region", !is.na(sexe)) |>
  select(reg_std, an_fin, sexe, valeur) |> wide_fg() |> mutate(Tot = coalesce(Tot, Fil + Gar)) |>
  select(reg_std, an_fin, eff = Tot) |> inner_join(ecoles, by = c("reg_std", "an_fin")) |>
  mutate(eleves_par_ecole = eff / n_ecoles)

## 8. TABLEAU DE BORD : une ligne par région ------------------------------
tbs_dernier <- reg |> filter(indic == "tbs_pri", sexe == "T", !is.na(valeur)) |> group_by(reg_std) |>
  slice_max(an_fin, n = 1, with_ties = FALSE) |> ungroup() |> select(reg_std, an_tbs = an_fin, tbs_pri = valeur)

tableau_bord <- recent |> filter(indic %in% c("ips_tba_pri", "ips_tbs_pri")) |> group_by(reg_std) |>
  summarise(score_inegalite = mean(ecart_moy),
            ips_tba_pri = mean(ips_moy[indic == "ips_tba_pri"]),
            ips_tbs_pri = mean(ips_moy[indic == "ips_tbs_pri"]), .groups = "drop") |>
  mutate(classe_parite = classer(rowMeans(cbind(ips_tba_pri, ips_tbs_pri), na.rm = TRUE)),
         rang = min_rank(desc(score_inegalite))) |>
  left_join(tbs_dernier, by = "reg_std") |>
  left_join(tend_ecart |> filter(indic == "ips_tbs_pri") |> select(reg_std, tendance_ecart = sens, pente_sen), by = "reg_std") |>
  left_join(avant_apres |> filter(indic == "ips_tbs_pri") |> select(reg_std, delta_ecart_apres_avant = delta_ecart), by = "reg_std") |>
  arrange(rang)
cat("\n== TABLEAU DE BORD ==\n"); print(tableau_bord, width = Inf)

## 9. FIGURES -----------------------------------------------------------
ordre <- rev(tableau_bord$reg_std)
etiq <- c(ips_tba_pri = "IPS du TBA (primaire)", ips_tbs_pri = "IPS du TBS (primaire)", ips_tbs_pre = "IPS du TBS (préscolaire)")
lim <- max(abs(ips$ips - 1), na.rm = TRUE)

f1 <- ips |> filter(indic == "ips_tbs_pri") |> mutate(reg_std = factor(reg_std, levels = ordre)) |>
  ggplot(aes(factor(an_fin), reg_std, fill = ips)) + geom_tile(colour = "white") +
  scale_fill_gradient2(midpoint = 1, low = "#b2182b", mid = "white", high = "#2166ac", limits = 1 + c(-lim, lim)) +
  labs(title = "Parité filles-garçons du TBS primaire, par région",
       subtitle = "Rouge : filles défavorisées ; bleu : garçons défavorisés ; blanc : parité",
       x = "Année scolaire (fin)", y = NULL, fill = "IPS") + theme_minimal()
fig(f1, "01_ips_tbs_primaire_regions")

f2 <- ips |> mutate(reg_std = factor(reg_std, levels = rev(ordre)), indic = etiq[indic]) |>
  ggplot(aes(an_fin, ips, colour = indic)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = P$seuil_parite[1], ymax = P$seuil_parite[2], alpha = .15) +
  geom_hline(yintercept = 1, linetype = 2) + geom_line() + facet_wrap(~ reg_std) +
  labs(title = "Évolution de l'IPS par région (bande grise : parité)", x = NULL, y = "IPS", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
fig(f2, "02_evolution_ips_regions", 11, 8)

f3 <- sigma_taux |> filter(sexe == "T", n_reg >= 10) |>
  ggplot(aes(an_fin, cv, colour = indic)) + geom_line() + geom_point() +
  labs(title = "Les régions se rapprochent-elles ? (coefficient de variation des taux)",
       x = NULL, y = "Dispersion relative entre régions", colour = NULL) + theme_minimal()
fig(f3, "03_convergence_regionale")

f4 <- ecart_milieu |> ggplot(aes(an_fin, indice_100_RURAL / indice_100_URBAIN * 100, colour = niveau)) +
  geom_hline(yintercept = 100, linetype = 2) + geom_line() + geom_point() +
  labs(title = "Effectifs ruraux relativement aux effectifs urbains (base 100 = 1re année)",
       x = NULL, y = "Indice rural / indice urbain", colour = NULL) + theme_minimal()
fig(f4, "04_rural_vs_urbain")

## 10. CARTES (si les shapefiles sont disponibles) ------------------------
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
# Couches de polygones seulement (l'archive OCHA contient aussi lignes et points),
# noms de colonnes en minuscules (OCHA : ADM2_FR, ADM2_PCODE...)
lire_polygones <- function(dir) {
  shp <- list.files(dir, "\\.shp$", recursive = TRUE, full.names = TRUE)
  map(shp, \(p) tryCatch({
    x <- sf::st_read(p, quiet = TRUE)
    if (!all(as.character(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) return(NULL)
    d <- sf::st_drop_geometry(x); names(d) <- make.unique(tolower(names(d)), sep = "_")
    sf::st_sf(d, geometry = sf::st_geometry(x))
  }, error = \(e) NULL)) |> set_names(tools::file_path_sans_ext(basename(shp))) |> compact()
}
couches <- if (requireNamespace("sf", quietly = TRUE) && dir.exists(P$dir_shp)) lire_polygones(P$dir_shp) else NULL

if (length(couches) && requireNamespace("sf", quietly = TRUE)) {
  prov_sf <- couches |> keep(\(x) "adm2_pcode" %in% names(x) && !"adm3_pcode" %in% names(x))
  if (length(prov_sf) && !is.null(rang_prov)) tryCatch({
    prov_sf <- prov_sf[[1]]
    col_nom <- intersect(c("adm2_fr", "adm2_name1", "adm2_name"), names(prov_sf))[1]
    if (is.na(col_nom)) stop("colonne de nom de province introuvable")
    prov_sf <- prov_sf |> mutate(cle = norm(.data[[col_nom]]))
    # Découpage 2025 : provinces renommées (Kossin = Kossi...) ou scindées (Djelgodji et
    # Karo-Peli = ancien Soum...). La table validée par le script de diagnostic redonne
    # à chaque polygone le nom de la province des statistiques.
    man <- lire_correspondance(file.path(CH$shp, nf("04_analyse", "analyst", "correspondance-provinces", "csv")))
    if (!is.null(man) && "province_res" %in% names(man)) {
      man <- man[!is.na(man$province_res), ]
      i <- match(prov_sf$adm2_pcode, man$adm2_pcode_shp)
      prov_sf$cle <- dplyr::coalesce(norm(man$province_res[i]), prov_sf$cle)
    }
    d <- rang_prov |> filter(niveau == "PRI") |> mutate(cle = norm(nom))
    cat("\nProvinces sans correspondance cartographique :", paste(setdiff(d$cle, prov_sf$cle), collapse = ", "), "\n")
    fig(ggplot(left_join(prov_sf, d, by = "cle")) + geom_sf(aes(fill = ips_moy), colour = "white", linewidth = .1) +
          scale_fill_gradient2(midpoint = 1, low = "#b2182b", mid = "white", high = "#2166ac") +
          labs(title = "Parité du taux de scolarisation primaire, par province", fill = "IPS") + theme_void(),
        "05_carte_provinces")
  }, error = \(e) message("Carte provinces non produite : ", conditionMessage(e)))
}

## 11. EXPORTS ------------------------------------------------------------
sortie(tableau_bord, "tableau_bord_regions")
sortie(ips, "ips_regions_annees")
sortie(recent, "classement_regions_recent")
sortie(tend_ecart, "tendances_ecart_parite")
sortie(avant_apres, "avant_apres_rupture")
sortie(sigma_taux, "dispersion_regionale_taux")
sortie(ecart_milieu, "ecart_ville_campagne_effectifs")
sortie(controle, "controle_ips_recalcule_vs_publie")
if (!is.null(rang_prov)) { sortie(rang_prov, "classement_provinces"); sortie(tend_prov, "tendances_provinces") }
if (!is.null(contexte)) sortie(contexte, "contexte_deplaces")
if (nrow(offre)) sortie(offre, "offre_eleves_par_ecole")
cat("\nTerminé. Sorties dans :", normalizePath(P$dir_sortie), "\n")