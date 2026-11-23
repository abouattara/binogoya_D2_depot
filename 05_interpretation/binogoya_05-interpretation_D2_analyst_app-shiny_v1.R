# =============================================================================
# DATA CHALLENGE BURKINA - Défi 2 : application Shiny
# Cartes des zones les plus inégales (régions, provinces, communes)
#
# Prérequis :
#   1. 04_analyse/binogoya_04-analyse_D2_analyst_ecarts-education_v1.R exécuté
#      -> 04_analyse/sorties/binogoya_04-analyse_D2_analyst_resultats-ecarts_v1.rds
#   2. Fonds de carte dans Data/shapefile (archives .zip, extraites par 01_collecte)
# Lancement (depuis le projet binogoya_D2_depot.Rproj) :
#   shiny::runApp("05_interpretation/binogoya_05-interpretation_D2_analyst_app-shiny_v1.R")
#
# La première exécution prépare les géométries et compte les écoles par commune,
# puis met le résultat en cache (04_analyse/sorties/...cache-shiny_v1.rds). Le cache est
# refait automatiquement si les résultats d'analyse sont plus récents.
# =============================================================================

source(here::here("binogoya_00-config_D2_engineer_chemins_v1.R"))
installer(c("shiny", "bslib", "leaflet", "sf", "dplyr", "tidyr", "purrr",
            "stringr", "stringi", "ggplot2", "DT", "scales"))

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(leaflet); library(sf)
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(stringi); library(ggplot2); library(DT)
})
sf_use_s2(FALSE)

# Chemins de l'application (définis dans le fichier de configuration)
CHA <- list(
  resultats  = FIC$resultats_rds,
  cache      = FIC$cache_shiny,
  admin      = CH$shp_admin,
  ecoles     = CH$shp_ecoles,
  zip_admin  = FIC$zip_admin,
  zip_ecoles = FIC$zip_ecoles
)

# -----------------------------------------------------------------------------
# Palette et petits utilitaires
# -----------------------------------------------------------------------------
COL <- list(accent = "#1c5cab", gris = "#c3c2b7", grille = "#e1e0d9", neutre = "#f0efec",
            critique = "#d03b3b", rouge = "#e34948", bleu = "#2a78d6", surface = "#fcfcfb",
            texte = "#0b0b0b", texte2 = "#52514e", muet = "#898781",
            serie = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100"))
RAMPE <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
PAL_TYPO <- c(
  "Cumul : exclusion générale + filles désavantagées" = "#eb6834",
  "Exclusion générale (tous les enfants)"             = "#2a78d6",
  "Filles désavantagées"                              = "#1baf7a",
  "Garçons désavantagés"                              = "#6250d6",
  "Écarts modérés"                                    = "#e1e0d9",
  "Données insuffisantes"                             = "#ffffff"
)

norm_lib <- function(x) {
  x |> as.character() |> stri_trans_general("Latin-ASCII") |> str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "")
}
mm <- function(x) {
  x[!is.finite(x)] <- NA
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) == 0) return(rep(NA_real_, length(x)))
  (x - r[1]) / diff(r)
}
fmt <- function(x, d = 2) ifelse(is.na(x), "n.d.",
                                 formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ","))
apparier <- function(cles, ref) {
  m <- match(cles, ref)
  for (i in which(is.na(m) & !is.na(cles))) {
    j <- agrep(cles[i], ref, max.distance = 0.2)
    if (length(j) == 1) m[i] <- j
  }
  m
}
premier_nom <- function(x, cands) intersect(cands, names(x))[1]
assurer <- function(d, cols) { for (v in cols) if (!v %in% names(d)) d[[v]] <- NA_real_; d }
# Moyenne pondérée ligne à ligne, en ignorant les valeurs manquantes
wmean <- function(M, w) {
  M <- as.matrix(M)
  W <- matrix(w, nrow(M), ncol(M), byrow = TRUE) * !is.na(M)
  M[is.na(M)] <- 0
  out <- rowSums(M * W) / rowSums(W)
  out[rowSums(W) == 0] <- NA
  out
}


# -----------------------------------------------------------------------------
# Préparation des données (exécutée une fois, puis mise en cache)
# -----------------------------------------------------------------------------
extraire_si_besoin <- function() {
  if (length(list.files(CHA$admin, "\\.shp$", recursive = TRUE)) == 0 && file.exists(CHA$zip_admin))
    unzip(CHA$zip_admin, exdir = CHA$admin)
  if (length(list.files(CHA$ecoles, "\\.shp$", recursive = TRUE)) == 0 && file.exists(CHA$zip_ecoles))
    unzip(CHA$zip_ecoles, exdir = CHA$ecoles)
}

# polygones = TRUE : ne garde que les couches de polygones (l'archive OCHA contient aussi
# des lignes « admbndl » et des points « admbndp » portant les mêmes Pcodes)
lire_couches <- function(dir, polygones = FALSE) {
  f <- list.files(dir, "\\.shp$", recursive = TRUE, full.names = TRUE)
  map(f, \(p) tryCatch({
    x <- st_read(p, quiet = TRUE)
    if (polygones && !all(as.character(st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) return(NULL)
    d <- st_drop_geometry(x); names(d) <- make.unique(tolower(names(d)), sep = "_")
    x <- st_sf(d, geometry = st_geometry(x))
    if (is.na(st_crs(x))) st_crs(x) <- 4326
    st_transform(st_make_valid(x), 4326)
  }, error = \(e) { message("Lecture impossible : ", p); NULL })) |> compact()
}
couche <- function(lst, avec, sans = character()) {
  for (x in lst) if (all(avec %in% names(x)) && !any(sans %in% names(x))) return(x)
  NULL
}

preparer <- function() {
  if (!file.exists(CHA$resultats))
    stop("Résultats introuvables (", CHA$resultats, ") : lancez d'abord ",
         "04_analyse/binogoya_04-analyse_D2_analyst_ecarts-education_v1.R")
  if (file.exists(CHA$cache) && file.mtime(CHA$cache) > file.mtime(CHA$resultats))
    return(readRDS(CHA$cache))
  
  message("Préparation des cartes (première exécution, quelques minutes)...")
  res <- readRDS(CHA$resultats)
  reg_tab  <- assurer(res$imde_regions, c("genre", "acces", "moyens"))
  prov_tab <- res$provinces
  if (is.null(prov_tab) || nrow(prov_tab) == 0) stop("Pas de résultats provinciaux dans ", CHA$resultats)
  prov_tab <- assurer(prov_tab, c("genre", "acces", "contexte"))
  com_tab  <- res$communes
  
  extraire_si_besoin()
  adm <- lire_couches(CHA$admin, polygones = TRUE)
  com_sf  <- couche(adm, "adm3_pcode")
  prov_sf <- couche(adm, "adm2_pcode", sans = "adm3_pcode")
  if (is.null(com_sf)) stop("Couche communale (adm3) introuvable dans ", CHA$admin)
  # colonnes utiles seulement (évite doublons et conflits lors des jointures)
  com_sf <- com_sf |> select(adm3_pcode, any_of(c("adm2_pcode", "adm3_fr", "adm3_name1", "adm3_name")))
  if (!is.null(prov_sf))
    prov_sf <- prov_sf |> select(adm2_pcode, any_of(c("adm2_fr", "adm2_name1", "adm2_name", "adm1_pco_1", "adm1_pcode")))
  if (is.null(prov_sf)) prov_sf <- com_sf |> group_by(adm2_pcode) |> summarise(.groups = "drop")
  
  # --- Provinces : code = Pcode CONASUR (celui des résultats)
  nm <- premier_nom(prov_sf, c("adm2_fr", "adm2_name1", "adm2_name"))
  prov_sf$nom_shp <- if (!is.na(nm)) prov_sf[[nm]] else prov_sf$adm2_pcode
  prov_sf$code <- prov_sf$adm2_pcode
  if (mean(prov_sf$code %in% prov_tab$admin2_pcode) < 0.5)
    prov_sf$code <- prov_tab$admin2_pcode[apparier(norm_lib(prov_sf$nom_shp), norm_lib(prov_tab$province))]
  c1 <- substr(prov_sf$code, 1, 4)
  prov_sf$reg_code <- if (mean(c1 %in% reg_tab$admin1_pcode, na.rm = TRUE) > 0.5) c1 else
    if ("adm1_pco_1" %in% names(prov_sf)) prov_sf$adm1_pco_1 else prov_sf$adm1_pcode
  
  # --- Régions (13 anciennes régions) par fusion des provinces
  reg_sf <- prov_sf |>
    filter(!is.na(reg_code)) |>
    group_by(code = reg_code) |>
    summarise(.groups = "drop") |>
    left_join(reg_tab |> rename(code = admin1_pcode), by = "code") |>
    mutate(reg_code = code, nom_aff = nom, region = nom, ctx = moyens)
  
  prov_sf <- prov_sf |>
    select(code, reg_code, nom_shp) |>
    left_join(prov_tab |> rename(code = admin2_pcode) |> select(-any_of(c("region", "admin1_pcode"))),
              by = "code") |>
    mutate(nom_aff = coalesce(province, nom_shp),
           region = reg_tab$nom[match(reg_code, reg_tab$admin1_pcode)],
           ctx = contexte)
  
  # --- Communes
  nm <- premier_nom(com_sf, c("adm3_fr", "adm3_name1", "adm3_name"))
  com_sf$nom_shp <- if (!is.na(nm)) com_sf[[nm]] else com_sf$adm3_pcode
  com_sf <- com_sf |> select(code = adm3_pcode, nom_shp)
  com_sf$code_cn <- com_sf$code
  if (mean(com_sf$code %in% com_tab$admin3_pcode) < 0.5)
    com_sf$code_cn <- com_tab$admin3_pcode[apparier(norm_lib(com_sf$nom_shp), norm_lib(com_tab$commune))]
  # Province de chaque commune : par localisation (robuste aux écarts de codification)
  rattach <- suppressWarnings(st_point_on_surface(com_sf)) |>
    st_join(select(prov_sf, prov_code = code, reg_code), join = st_intersects) |>
    st_drop_geometry() |>
    distinct(code, .keep_all = TRUE) |>
    select(code, prov_code, reg_code)
  com_sf <- com_sf |>
    left_join(rattach, by = "code") |>
    left_join(com_tab |> select(code_cn = admin3_pcode, commune, pdi, pdi_enfants), by = "code_cn") |>
    mutate(nom_aff = coalesce(commune, nom_shp),
           pdi = coalesce(pdi, 0), pdi_enfants = coalesce(pdi_enfants, 0))
  com_sf$province <- prov_sf$nom_aff[match(com_sf$prov_code, prov_sf$code)]
  com_sf$region   <- reg_tab$nom[match(com_sf$reg_code, reg_tab$admin1_pcode)]
  com_sf$area_km2 <- as.numeric(st_area(st_transform(com_sf, 32630))) / 1e6
  
  # --- Écoles OpenStreetMap : nombre et densité par commune
  ecoles <- tibble(lng = numeric(), lat = numeric(), nom = character(), type = character())
  ec <- lire_couches(CHA$ecoles)
  if (length(ec) > 0) {
    ec <- map(ec, \(x) {
      if (!all(st_geometry_type(x) == "POINT")) x <- suppressWarnings(st_point_on_surface(x))
      nn <- premier_nom(x, c("name", "name_fr", "nom"))
      tt <- premier_nom(x, c("amenity", "building", "type"))
      st_sf(nom = if (!is.na(nn)) as.character(x[[nn]]) else NA_character_,
            type = if (!is.na(tt)) as.character(x[[tt]]) else NA_character_,
            geometry = st_geometry(x))
    })
    ec <- do.call(rbind, ec)
    ec <- ec |> filter(is.na(type) | type %in% c("school", "kindergarten", "college"))
    n_par_com <- st_join(ec, select(com_sf, code), join = st_intersects) |>
      st_drop_geometry() |> count(code, name = "n_ecoles")
    com_sf <- com_sf |> left_join(n_par_com, by = "code")
    xy <- st_coordinates(ec)
    ecoles <- tibble(lng = xy[, 1], lat = xy[, 2], nom = ec$nom, type = ec$type)
  } else {
    message("Aucune couche d'écoles trouvée dans ", CHA$ecoles)
    com_sf$n_ecoles <- NA_integer_
  }
  com_sf <- com_sf |>
    mutate(n_ecoles = coalesce(n_ecoles, if (nrow(ecoles) > 0) 0L else NA_integer_),
           ecoles_100km2 = 100 * n_ecoles / area_km2,
           enf_dep_ecole = pdi_enfants / pmax(n_ecoles, 1),
           s_pdi = mm(log1p(pdi_enfants)),                  # 1 = forte pression de déplacement
           s_ecole = 1 - mm(log1p(ecoles_100km2)))          # 1 = faible densité d'écoles
  
  # --- Allègement des géométries pour l'affichage
  alleger <- function(x, tol) suppressWarnings(st_simplify(x, preserveTopology = TRUE, dTolerance = tol))
  out <- list(reg = alleger(reg_sf, 0.004), prov = alleger(prov_sf, 0.003), com = alleger(com_sf, 0.002),
              ecoles = ecoles, res = res)
  saveRDS(out, CHA$cache)
  out
}

D <- preparer()
REGIONS <- D$reg |> st_drop_geometry() |> filter(!is.na(nom)) |> arrange(nom) |> select(nom, reg_code)


# -----------------------------------------------------------------------------
# Indicateurs proposés par niveau (sens : +1 = plus élevé = plus inégal)
# -----------------------------------------------------------------------------
INDIC <- list(
  reg = c("IMDE (pondéré)" = "imde_w", "Dimension genre" = "genre", "Dimension accès" = "acces",
          "Moyens et contexte" = "ctx", "Typologie" = "typologie", "IPS primaire" = "ips_prim",
          "IPS post-primaire" = "ips_pp", "TBS primaire (%)" = "tbs_prim_T",
          "TBS post-primaire (%)" = "tbs_pp_T", "Enfants déplacés pour 100 élèves" = "enf_dep_100"),
  prov = c("IMDE (pondéré)" = "imde_w", "Dimension genre" = "genre", "Dimension accès" = "acces",
           "Contexte (déplacement)" = "ctx", "Typologie" = "typologie", "IPS primaire" = "ips_pri",
           "IPS post-primaire" = "ips_ppr", "Taux primaire (%)" = "PRI_T",
           "Taux post-primaire (%)" = "PPR_T", "Enfants déplacés" = "pdi_enfants"),
  com = c("Indice de priorité communal" = "score_com", "IMDE de la province" = "imde_prov_w",
          "Enfants déplacés" = "pdi_enfants", "Écoles OSM" = "n_ecoles",
          "Écoles OSM pour 100 km²" = "ecoles_100km2", "Enfants déplacés par école" = "enf_dep_ecole")
)
INVERSE <- c("tbs_prim_T", "tbs_pp_T", "PRI_T", "PPR_T", "n_ecoles", "ecoles_100km2")
lib_indic <- function(v) { l <- unlist(unname(INDIC)); names(l)[match(v, l)][1] }

gravite <- function(v, x) {
  if (startsWith(v, "ips")) abs(log(x)) else if (v %in% INVERSE) -x else x
}
valeur_txt <- function(v, x) {
  if (v == "typologie") return(as.character(x))
  d <- if (v %in% c("pdi_enfants", "n_ecoles")) 0 else if (v %in% c("tbs_prim_T", "tbs_pp_T", "PRI_T", "PPR_T", "ecoles_100km2", "enf_dep_100", "enf_dep_ecole")) 1 else 2
  fmt(as.numeric(x), d)
}


# -----------------------------------------------------------------------------
# Interface
# -----------------------------------------------------------------------------
theme_app <- bs_theme(version = 5, primary = COL$accent, bg = "#f9f9f7", fg = COL$texte,
                      base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"))

ui <- page_navbar(
  title = "Écarts d'accès à l'éducation - Burkina Faso",
  theme = theme_app,
  fillable = FALSE,
  
  nav_panel(
    "Carte des inégalités",
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        radioButtons("niveau", "Niveau géographique",
                     c("Régions" = "reg", "Provinces" = "prov", "Communes" = "com"), selected = "prov"),
        selectInput("indic", "Indicateur cartographié", INDIC$prov),
        selectInput("filtre_reg", "Région", c("Toutes" = "", setNames(REGIONS$reg_code, REGIONS$nom))),
        sliderInput("top_pct", "Zones les plus inégales à signaler (%)", 5, 50, 20, step = 5, post = " %"),
        accordion(
          open = FALSE,
          accordion_panel(
            "Pondérations de l'IMDE",
            sliderInput("w_genre", "Équité de genre", 0, 3, 1, step = 0.5),
            sliderInput("w_acces", "Équité d'accès", 0, 3, 1, step = 0.5),
            sliderInput("w_ctx", "Moyens / déplacement", 0, 3, 1, step = 0.5)
          ),
          accordion_panel(
            "Indice communal",
            sliderInput("w_prov", "IMDE de la province", 0, 3, 1, step = 0.5),
            sliderInput("w_pdi", "Enfants déplacés", 0, 3, 1, step = 0.5),
            sliderInput("w_ecole", "Faible densité d'écoles", 0, 3, 0.5, step = 0.5)
          )
        ),
        checkboxInput("voir_ecoles", "Afficher les écoles (OpenStreetMap)", FALSE),
        helpText("Contour rouge : zones les plus inégales selon l'indicateur choisi. ",
                 "Cliquez sur une zone pour afficher sa fiche.")
      ),
      layout_columns(
        fill = FALSE,
        value_box("Zones signalées", textOutput("vb_top")),
        value_box("Enfants déplacés (zone affichée)", textOutput("vb_pdi")),
        value_box("Écoles OSM (zone affichée)", textOutput("vb_ecoles"))
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(full_screen = TRUE, card_header(textOutput("titre_carte", inline = TRUE)),
             leafletOutput("carte", height = 620)),
        card(card_header("Fiche de la zone"), uiOutput("fiche"))
      ),
      card(card_header("Zones les plus inégales"), DTOutput("tab_top"))
    )
  ),
  
  nav_panel(
    "Classement",
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Classement selon l'indicateur choisi"), plotOutput("p_classement", height = 640)),
      card(card_header("Sensibilité du classement (pondérations égales)"),
           p(class = "text-muted small", "Rang médian et intervalle à 90 % sur 2 000 pondérations simulées."),
           DTOutput("tab_sensib"))
    )
  ),
  
  nav_panel(
    "Écarts filles-garçons",
    card(card_header("Tableau des écarts : indice de parité par région"), plotOutput("p_ecarts", height = 520)),
    card(card_header("Provinces : parité et accès"), DTOutput("tab_ips_prov"))
  ),
  
  nav_panel(
    "Évolution",
    layout_columns(
      col_widths = c(6, 6, 6, 6),
      card(card_header("Parité nationale par niveau"), plotOutput("p_nat", height = 340)),
      card(card_header("Tendance de l'IMDE structurel par région"), plotOutput("p_tend", height = 340)),
      card(card_header("Dispersion du TBS primaire entre régions"), plotOutput("p_conv", height = 340)),
      card(card_header("Effectifs du primaire : urbain et rural"), plotOutput("p_mil", height = 340))
    )
  ),
  
  nav_panel(
    "Méthode",
    card(markdown("
**IMDE** : chaque indicateur est ramené entre 0 (situation la plus favorable) et 1 (la plus défavorable).
Trois dimensions : équité de genre (|log IPS| aux différents niveaux), équité d'accès (déficits de
scolarisation, perte au passage primaire - post-primaire), moyens et contexte (élèves par école,
enfants déplacés). L'IMDE est la moyenne pondérée des dimensions : les curseurs permettent de tester
d'autres pondérations ; le classement à pondérations égales et sa sensibilité figurent dans l'onglet Classement.

**Niveaux** : régions (DGESS, INSD OpenData), provinces (OpenData 2005-2025), communes.
Aucune donnée éducative n'est publiée par commune : l'**indice de priorité communal** combine l'IMDE
de la province, le nombre d'enfants déplacés (CONASUR) et la densité d'écoles recensées dans
OpenStreetMap. C'est un outil de ciblage provisoire, qui sera remplacé par l'estimation sur petits
domaines (Fay-Herriot) dès l'intégration des annuaires communaux et de l'EHCVM.

**Précautions** : la couverture d'OpenStreetMap est inégale (mieux renseignée en ville) ; les effectifs
de déplacés sont ceux de la dernière collecte CONASUR ; l'écart urbain-rural n'est disponible qu'au
niveau national (onglet Évolution).

Sources : DGESS/MENAPLN, INSD OpenData, CONASUR, OCHA (limites administratives), HOT OSM (écoles).
"))
  )
)


# -----------------------------------------------------------------------------
# Serveur
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$niveau, {
    updateSelectInput(session, "indic", choices = INDIC[[input$niveau]])
    selection(NULL)
  })
  
  selection <- reactiveVal(NULL)
  
  # Données du niveau choisi, avec IMDE recalculé selon les pondérations
  donnees <- reactive({
    w <- c(input$w_genre, input$w_acces, input$w_ctx)
    prov <- D$prov |> mutate(imde_w = wmean(cbind(genre, acces, ctx), w))
    d <- switch(input$niveau,
                reg  = D$reg |> mutate(imde_w = wmean(cbind(genre, acces, ctx), w)),
                prov = prov,
                com  = D$com |>
                  left_join(st_drop_geometry(prov) |> select(prov_code = code, imde_prov_w = imde_w), by = "prov_code") |>
                  mutate(score_com = wmean(cbind(imde_prov_w, s_pdi, s_ecole),
                                           c(input$w_prov, input$w_pdi, input$w_ecole)))
    )
    if (nzchar(input$filtre_reg)) d <- d |> filter(reg_code == input$filtre_reg)
    d
  })
  
  # Indicateur affiché + repérage des zones les plus inégales
  carto <- reactive({
    d <- donnees(); v <- input$indic
    req(v %in% names(d))
    vg <- if (v == "typologie") "imde_w" else v
    g <- gravite(vg, as.numeric(d[[vg]]))
    seuil <- quantile(g, 1 - input$top_pct / 100, na.rm = TRUE)
    d$top <- !is.na(g) & g >= seuil
    d$gravite <- g
    d$valeur <- d[[v]]
    d
  })
  
  output$titre_carte <- renderText({
    paste0(lib_indic(input$indic), " - ",
           c(reg = "régions", prov = "provinces", com = "communes")[input$niveau],
           if (nzchar(input$filtre_reg)) paste0(" (", REGIONS$nom[REGIONS$reg_code == input$filtre_reg], ")") else "")
  })
  
  output$carte <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5)) |>
      # Fonds sans clé d'accès (les fonds CARTO exigent désormais une clé API)
      addProviderTiles("Esri.WorldGrayCanvas", group = "Fond gris") |>
      addTiles(group = "OpenStreetMap") |>
      addLayersControl(baseGroups = c("Fond gris", "OpenStreetMap"),
                       options = layersControlOptions(collapsed = TRUE), position = "topleft") |>
      setView(lng = -1.6, lat = 12.3, zoom = 6)
  })
  
  observe({
    d <- carto(); v <- input$indic
    x <- d$valeur
    if (v == "typologie") {
      x <- factor(x, levels = names(PAL_TYPO))
      fill <- unname(PAL_TYPO[as.character(x)])
      fill[is.na(fill)] <- PAL_TYPO["Données insuffisantes"]
    } else if (startsWith(v, "ips")) {
      pal <- colorNumeric(c(COL$rouge, COL$neutre, COL$bleu), domain = c(0.6, 1.4), na.color = COL$grille)
      fill <- pal(pmin(pmax(as.numeric(x), 0.6), 1.4))
    } else {
      dom <- range(as.numeric(x), na.rm = TRUE); if (!all(is.finite(dom))) dom <- c(0, 1)
      pal <- colorNumeric(RAMPE, domain = dom, reverse = v %in% INVERSE, na.color = COL$grille)
      fill <- pal(as.numeric(x))
    }
    lbl <- paste0(d$nom_aff, " : ", valeur_txt(v, x))
    top <- d |> filter(top)
    
    m <- leafletProxy("carte") |> clearShapes() |> removeControl("legende") |>
      addPolygons(data = d, layerId = ~code, fillColor = fill, fillOpacity = 0.85,
                  color = COL$surface, weight = if (input$niveau == "com") 0.4 else 1.2,
                  label = lbl,
                  highlightOptions = highlightOptions(weight = 3, color = COL$texte, bringToFront = TRUE)) 
    if (nrow(top) > 0)
      m <- m |> addPolygons(data = top, layerId = ~paste0("top_", code), fill = FALSE,
                            color = COL$critique, weight = 2.5, opacity = 1, label = ~nom_aff)
    if (v == "typologie") {
      m |> addLegend("bottomright", layerId = "legende", colors = unname(PAL_TYPO), labels = names(PAL_TYPO), title = "Typologie")
    } else if (startsWith(v, "ips")) {
      m |> addLegend("bottomright", layerId = "legende", pal = pal, values = c(0.6, 1.4),
                     title = "IPS<br><small>&lt; 1 filles désavantagées</small>")
    } else {
      m |> addLegend("bottomright", layerId = "legende", pal = pal, values = as.numeric(x), title = lib_indic(v))
    }
  })
  
  # Recadrage quand le niveau ou la région change
  observeEvent(list(input$niveau, input$filtre_reg), {
    b <- st_bbox(donnees())
    if (all(is.finite(b))) leafletProxy("carte") |> fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]])
  })
  
  observe({
    p <- leafletProxy("carte") |> clearGroup("ecoles")
    if (isTRUE(input$voir_ecoles) && nrow(D$ecoles) > 0) {
      p |> addCircleMarkers(data = D$ecoles, lng = ~lng, lat = ~lat, group = "ecoles",
                            radius = 4, stroke = FALSE, fillColor = COL$texte2, fillOpacity = 0.8,
                            label = ~coalesce(nom, "École sans nom"),
                            clusterOptions = markerClusterOptions())
    }
  })
  
  observeEvent(input$carte_shape_click, {
    selection(sub("^top_", "", input$carte_shape_click$id))
  })
  
  output$fiche <- renderUI({
    d <- carto(); id <- selection()
    if (is.null(id) || !id %in% d$code) return(p(class = "text-muted", "Cliquez sur une zone de la carte."))
    z <- st_drop_geometry(d)[d$code == id, , drop = FALSE]
    vars <- INDIC[[input$niveau]]
    lignes <- map2(names(vars), vars, \(l, v)
                   if (v %in% names(z)) tags$tr(tags$td(l), tags$td(class = "text-end fw-semibold", valeur_txt(v, z[[v]]))))
    rang <- sum(d$gravite > z$gravite, na.rm = TRUE) + 1
    tagList(
      h4(z$nom_aff),
      p(class = "text-muted mb-1",
        paste(na.omit(c(if (input$niveau == "com") z$province, if (input$niveau != "reg") z$region)), collapse = " - ")),
      p(paste0("Rang ", rang, " sur ", sum(!is.na(d$gravite)), " selon « ", lib_indic(input$indic), " »"),
        if (isTRUE(z$top)) tags$span(class = "badge bg-danger ms-1", "zone prioritaire")),
      tags$table(class = "table table-sm", tags$tbody(lignes))
    )
  })
  
  output$vb_top <- renderText({ d <- carto(); paste0(sum(d$top), " sur ", sum(!is.na(d$gravite))) })
  output$vb_pdi <- renderText({
    d <- D$com; if (nzchar(input$filtre_reg)) d <- filter(d, reg_code == input$filtre_reg)
    fmt(sum(d$pdi_enfants, na.rm = TRUE), 0)
  })
  output$vb_ecoles <- renderText({
    d <- D$com; if (nzchar(input$filtre_reg)) d <- filter(d, reg_code == input$filtre_reg)
    if (all(is.na(d$n_ecoles))) "n.d." else fmt(sum(d$n_ecoles, na.rm = TRUE), 0)
  })
  
  tableau_top <- reactive({
    d <- carto() |> st_drop_geometry() |> arrange(desc(gravite))
    v <- input$indic
    cols <- intersect(unique(c("nom_aff", "province", "region", v, "imde_w", "typologie",
                               if (input$niveau == "com") c("score_com", "pdi_enfants", "n_ecoles"))), names(d))
    d |> filter(top) |> select(all_of(cols)) |>
      mutate(across(where(is.numeric), \(x) round(x, 2))) |>
      rename(Zone = nom_aff)
  })
  output$tab_top <- renderDT(datatable(tableau_top(), rownames = FALSE,
                                       options = list(pageLength = 10, dom = "tip", scrollX = TRUE)))
  
  # --- Onglet Classement
  output$p_classement <- renderPlot({
    d <- carto() |> st_drop_geometry() |> filter(!is.na(gravite)) |> arrange(desc(gravite)) |> head(30)
    v <- if (input$indic == "typologie") "imde_w" else input$indic
    d$val <- as.numeric(d[[v]])
    ggplot(d, aes(val, reorder(nom_aff, gravite), fill = top)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = c(`TRUE` = COL$accent, `FALSE` = COL$gris), guide = "none") +
      labs(x = lib_indic(v), y = NULL,
           subtitle = "En bleu foncé : zones signalées (30 premières au plus)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
            plot.subtitle = element_text(colour = COL$texte2))
  })
  output$tab_sensib <- renderDT({
    d <- if (input$niveau == "reg") D$reg else D$prov
    d |> st_drop_geometry() |> filter(!is.na(imde)) |> arrange(rang) |>
      transmute(Zone = nom_aff, Rang = rang, `Rang médian` = round(rang_med, 1),
                Intervalle = paste0(round(rang_p05), "-", round(rang_p95)),
                `Prob. tête` = scales::percent(p_top, 1)) |>
      datatable(rownames = FALSE, options = list(pageLength = 15, dom = "tip"))
  })
  
  # --- Onglet Écarts filles-garçons
  output$p_ecarts <- renderPlot({
    r <- D$res$imde_regions
    lg <- r |>
      select(nom, Primaire = ips_prim, CEP = ips_cep,
             `Post-primaire` = ips_pp, Secondaire = ips_sec) |>
      pivot_longer(-nom, names_to = "indicateur", values_to = "ips") |>
      mutate(indicateur = factor(indicateur, c("Primaire", "CEP", "Post-primaire", "Secondaire")),
             nom = factor(nom, rev(r$nom[order(r$rang)])))
    ggplot(lg, aes(indicateur, nom, fill = ips)) +
      geom_tile(colour = COL$surface, linewidth = 1) +
      geom_text(aes(label = fmt(ips)), size = 3.6) +
      scale_fill_gradient2(low = COL$rouge, mid = COL$neutre, high = COL$bleu, midpoint = 1,
                           limits = c(0.6, 1.4), oob = scales::squish, na.value = COL$grille, name = "IPS") +
      labs(x = NULL, y = NULL,
           subtitle = "IPS filles/garçons : < 1 filles désavantagées, > 1 garçons ; parité entre 0,97 et 1,03") +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(), legend.position = "top",
            plot.subtitle = element_text(colour = COL$texte2))
  })
  output$tab_ips_prov <- renderDT({
    D$prov |> st_drop_geometry() |> filter(!is.na(imde)) |> arrange(rang) |>
      transmute(Province = nom_aff, Région = region, `IPS primaire` = round(ips_pri, 2),
                `IPS post-primaire` = round(ips_ppr, 2), `IPS secondaire` = round(ips_sec, 2),
                `Taux primaire` = round(PRI_T, 1), `Taux post-primaire` = round(PPR_T, 1),
                Typologie = typologie) |>
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # --- Onglet Évolution
  theme_evo <- theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "top")
  output$p_nat <- renderPlot({
    d <- D$res$parite_nationale; req(nrow(d) > 0)
    niv <- unique(d$niveau)
    ggplot(d, aes(annee, ips, colour = niveau)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.97, ymax = 1.03, fill = COL$neutre) +
      geom_hline(yintercept = 1, colour = COL$gris) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = setNames(c(COL$serie, COL$muet)[seq_along(niv)], niv), name = NULL) +
      labs(x = NULL, y = "IPS") + theme_evo
  })
  output$p_tend <- renderPlot({
    d <- D$res$tendances; req(nrow(d) > 0)
    ggplot(d, aes(pente_imde, reorder(nom, pente_imde), fill = pente_imde > 0)) +
      geom_col(width = 0.7) +
      geom_vline(xintercept = 0, colour = COL$gris) +
      scale_fill_manual(values = c(`TRUE` = COL$accent, `FALSE` = COL$gris),
                        labels = c(`TRUE` = "Écarts qui se creusent", `FALSE` = "Écarts qui se réduisent"), name = NULL) +
      labs(x = "Pente annuelle de l'IMDE structurel", y = NULL) + theme_evo
  })
  output$p_conv <- renderPlot({
    d <- D$res$convergence; req(nrow(d) > 0)
    ggplot(d, aes(annee, cv_tbs_prim)) +
      geom_line(colour = COL$accent, linewidth = 0.9) + geom_point(colour = COL$accent, size = 2.5) +
      labs(x = NULL, y = "Coefficient de variation") + theme_evo
  })
  output$p_mil <- renderPlot({
    d <- D$res$urbain_rural; req(nrow(d) > 0)
    d <- d |> filter(indic == "eff_prim")
    ggplot(d, aes(annee, indice_100, colour = milieu)) +
      geom_hline(yintercept = 100, colour = COL$gris) +
      geom_line(linewidth = 0.9) + geom_point(size = 2) +
      scale_colour_manual(values = c(Urbain = "#2a78d6", Rural = "#eb6834"), name = NULL) +
      labs(x = NULL, y = "Indice base 100") + theme_evo
  })
}

shinyApp(ui, server)