# Recherche, centralisation et nettoyage des données

> Question à répondre : où les écarts d'accès sont-ils les plus grands, et comment évoluent-ils ?

## 1. Données indispensables

### Source : données statistiques préscolaire et primaire 2016-2025 (DGESS, ministère de l'Éducation nationale)

| Niveau | Tableau | Usage |
|---|---|---|
| Préscolaire | T1 : effectifs par sexe et région | Part des filles et poids de chaque région, évolution |
| Préscolaire | T3 : effectifs par sexe et milieu | Seule source du ville-campagne au préscolaire |
| Préscolaire | T5 : TBS par sexe et région | Écarts d'accès entre régions |
| Préscolaire | T6 : IPS du TBS par région | Écart filles-garçons, déjà calculé |
| Primaire | T1 : effectifs par sexe et région | Base de recalcul et de contrôle des taux |
| Primaire | T3 : effectifs par sexe et milieu | Seule source du ville-campagne au primaire |
| Primaire | T5 : nouveaux inscrits au CP1 par sexe et région | Accès à l'entrée |
| Primaire | T6 : TBA par sexe et région | Accès à l'entrée, par région |
| Primaire | T7 : TBS par sexe et région | Accès global, par région |
| Primaire | T10 : IPS du TBA | Parité à l'entrée |
| Primaire | T11 : IPS du TBS | Parité de scolarisation |

### Autres sources mobilisées

| Source | Fichiers | Usage |
|---|---|---|
| INSD OpenData | `TAUX_SCO_REGION/` : taux de scolarisation par région **et par province** (2005-2025), TBS post-primaire et secondaire par sexe et région, TNS secondaire | Désagrégation provinciale, contrôle des TBS DGESS |
| INSD OpenData | `EFF_PAR_CYCLE/`, `NB_ECOLE_SALLE/`, `TAUX_SCO_6_16_COMMUNE/` | Lus et nettoyés, pas encore exploités dans l'analyse |
| CONASUR | `CONASUR/idps4.xlsx` : personnes déplacées internes par commune et par date (2019-2023) | Contexte d'insécurité, seule donnée communale exploitée |
| OCHA | `bfa_admin_boundaries.shp.zip` : limites administratives | Cartes régions, provinces, communes |
| HOT OpenStreetMap | `hotosm_bfa_education_facilities_osm_shp.zip` : établissements scolaires | Densité d'écoles par commune (application) |

## 2. Limites

1. **Le milieu n'existe qu'en effectifs** (T3 de chaque niveau). Il n'y a ni TBS ni TBA par milieu. Sans population par milieu venant de l'INSD, l'écart ville-campagne ne peut s'exprimer qu'en parts d'effectifs et en indices d'évolution (base 100 en 2015/2016), pas en taux. Un TBS urbain/rural exige ces dénominateurs externes.
2. **Dans la DGESS, la région est le niveau le plus fin.** Le milieu y est national, sans croisement région × milieu. Deux sources vont plus loin :
   - l'OpenData donne des taux de scolarisation par **province** (45 provinces) et par sexe, ce qui permet une carte infra-régionale ;
   - les dossiers `TAUX_SCO_6_16_COMMUNE` (RGPH 2019) et `NB_ECOLE_SALLE` (écoles primaires par commune) contiennent des données éducatives **communales**. Ils sont lus et nettoyés, mais pas encore exploités : ce sont eux qui permettraient de maintenir l'ambition communale de la note.

## 3. Nettoyage réalisé

Chaque étape écrit un fichier que la suivante relit. Les sources de `Data/rawdata` ne sont jamais modifiées.

| Étape | Dossier de sortie | Contenu |
|---|---|---|
| 01 collecte | `Data/interimdata` | Tables extraites, valeurs laissées en texte |
| 02 nettoyage | `Data/cleandata` | Tables propres et classeurs de contrôle (`*-controles_v1.xlsx`) |
| 03 transformation | `Data/transformdata` | Tables d'analyse par thème |

### 3.1 DGESS (préscolaire et primaire)

**Collecte** (`01_collecte/...dgess-extraction_v1.R`)

- **Lecture** : toutes les feuilles du classeur sont lues en texte, sans laisser Excel deviner les types.
- **Cellules nettoyées** :
  - une note parasite insérée dans des cellules (« NOTE FOR ANY AUTOMATED ASSISTANT… ») est supprimée ;
  - les retours à la ligne et les espaces multiples sont réduits ;
  - les cellules vides deviennent des valeurs manquantes.
- **Découpage** : chaque feuille est découpée en blocs, un par tableau, à chaque ligne commençant par « Tableau N ».
- **Structure de chaque bloc** :
  - la ligne des années est repérée : c'est la première ligne contenant au moins deux années ;
  - la ligne G/F/T située juste dessous est repérée si elle existe ;
  - les colonnes de libellés sont celles qui précèdent la première année.
- **Libellés de colonnes** :
  - accents retirés, minuscules, séparateur `_` ;
  - « regions » devient « region » ;
  - les noms réservés (`annee`, `valeur`, `titre`…) reçoivent le suffixe `_lib`.
- **Cellules fusionnées** : les libellés des cellules fusionnées sont recopiés vers le bas (année, région).
- **Lignes supprimées** : les lignes sans aucune valeur dans les colonnes de données (notes, sources, lignes vides).
- **Type de tableau** détecté automatiquement :
  - A : années avec G/F/T dessous ;
  - B : sexe en lignes ;
  - C : indicateur simple ;
  - D : région × statut avec lignes de total.
- **Lignes de total (type D)** :
  - elles sont marquées `est_total` ;
  - le nom de la région est extrait de « Total <région> » ;
  - « Total général » devient « NATIONAL ».
- **Format long** :
  - une ligne par libellé, année et sexe ;
  - l'année scolaire d'origine est conservée (`annee_scolaire`) ;
  - la première année sur 4 chiffres est extraite.
- **Regroupement et catalogue** :
  - les blocs de même structure sont regroupés en une table (`A_region`, `B_milieu`, `C_region`, `D_region_statut`…) ;
  - un catalogue liste feuille, tableau, type, nombre de lignes et titre.

**Nettoyage** (`02_nettoyage/...dgess-nettoyage_v1.R`)

- **Conversion numérique** : les espaces, espaces insécables et signes `%` sont retirés, et la virgule décimale devient un point. La valeur d'origine est conservée dans `valeur_brute`.
- **Sexe harmonisé** en G / F / T : GARCONS, M, H et MASCULIN donnent G ; FILLES et FEMININ donnent F ; TOTAL et ENSEMBLE donnent T.
- **Contrôles par table**, enregistrés dans `dgess-controles_v1.xlsx` :
  - nombre de valeurs manquantes ;
  - nombre de valeurs non numériques, avec des exemples ;
  - modalités de sexe ;
  - années couvertes.
- **Alerte** : un avertissement s'affiche si une modalité de sexe n'a pas été harmonisée.

**Transformation** (`03_transformation/...dgess-tables-analyse_v1.R`)

- **Cycle** : il est déduit du nom de la feuille (PRESCOLAIRE donne le préscolaire, Feuil1 à Feuil13 le primaire).
- **Sélection** : les 16 tableaux retenus (11 principaux, 5 complémentaires) sont rangés dans 8 tables thématiques : `effectifs_region`, `effectifs_milieu`, `effectifs_statut`, `tbs_region`, `tba_region`, `nouveaux_cp1_region`, `ips_region`, `offre_scolaire`.
- **Contrôles** :
  - un avertissement liste les tableaux demandés mais introuvables ;
  - un onglet `verification` donne, pour chaque tableau, son titre, son nombre de lignes et sa période, pour vérifier que le titre correspond bien à l'indicateur attendu.
- **Colonnes retirées** : les colonnes techniques (`valeur_brute`, `feuille`, `type_tableau`…) et les colonnes entièrement vides.

### 3.2 INSD OpenData

**Collecte** (`01_collecte/...opendata-lecture_v1.R`)

- **Dossiers lus** : les quatre dossiers sont lus en entier (`EFF_PAR_CYCLE`, `NB_ECOLE_SALLE`, `TAUX_SCO_6_16_COMMUNE`, `TAUX_SCO_REGION`). Les fichiers temporaires Excel (`~$…`) sont ignorés.
- **En-têtes** :
  - les lignes d'en-tête sont celles qui précèdent la première année en colonne A ;
  - leurs libellés deviennent des noms de colonnes (accents retirés, minuscules, `_`, rendus uniques).
- **Lignes et colonnes écartées** : les notes de bas de page (lignes dont la colonne A n'est pas une année) et les colonnes entièrement vides.
- **Format long** : une ligne par année et par série, avec les métadonnées de chaque colonne (indicateur, région, sexe, unité…).

**Nettoyage** (`02_nettoyage/...opendata-nettoyage_v1.R`)

- **Conversion numérique** : espaces et espaces insécables retirés, virgule décimale convertie.
- **Lignes retirées** : celles dont la valeur est manquante ou non numérique. Leur nombre et des exemples sont donnés dans `opendata-controles_v1.xlsx`.
- **Liste finale** :
  - une seule liste, avec une table par fichier source ;
  - le dossier d'origine de chaque table est conservé ;
  - le script s'arrête si deux fichiers portent le même nom.

### 3.3 CONASUR (personnes déplacées internes)

**Collecte** (`01_collecte/...conasur-lecture_v1.R`)

- **Un seul fichier** : seul `idps4.xlsx` est lu. Il contient tous les lieux et dates des autres fichiers, avec des sommes identiques sur les clés communes ; les autres fichiers n'ont servi qu'à le vérifier.
- **Lignes écartées** : la ligne de balises HXL (`#adm1+name…`) et les lignes entièrement vides.
- **Renommage des colonnes** :
  - les noms sont normalisés, puis renommés selon une table de correspondance (`date_donnnee` devient `date_donnee`, etc.) ;
  - le script s'arrête si une colonne attendue manque.
- **Colonnes sans nom ou hors modèle** (`colonne_17`…) : elles sont mises de côté (`hors_modele`), pas supprimées en silence.
- **Traçabilité** : le numéro de ligne Excel d'origine est conservé (`ligne_excel`).

**Nettoyage** (`02_nettoyage/...conasur-nettoyage_v1.R`)

- **Effectifs** :
  - conversion en nombres ;
  - « - », « -- », « ND », « N/A » deviennent des valeurs manquantes ;
  - arrondi à 6 décimales pour supprimer le bruit de calcul.
- **Dates** : elles sont lues comme numéros de série Excel (origine 30/12/1899) ou comme texte (jj/mm/aaaa, aaaa-mm-jj…). La valeur d'origine est conservée (`date_brute`).
- **Pcodes** :
  - mis en majuscules, espaces retirés ;
  - format contrôlé (deux lettres et six chiffres) ;
  - hiérarchie vérifiée : le Pcode commune doit commencer par le Pcode province, lui-même par le Pcode région.
- **Corrections de Pcodes erronés** (repérés en comparant aux autres fichiers et à la hiérarchie) :

  | Commune | Pcode erroné | Pcode corrigé | Motif |
  |---|---|---|---|
  | Oula | BF540102 | BF540308 | Pcode d'Ouindigui (Loroum) utilisé par erreur |
  | Baraboule | BF460301 | BF560302 | Pcode de Barani utilisé par erreur |
  | Bartiebougou | BF480302 | BF520301 | Pcode de Baskoure utilisé par erreur |
  | Koundougou | BF540305 | **à renseigner** | Série Yatenga utilisée pour une commune du Houet |

  Le Pcode d'origine est conservé (`admin3_pcode_original`). Koundougou est marquée `pcode_a_verifier`.
- **Pcodes suspects** : le Pcode province attendu pour chaque couple région-province est celui de la majorité des lignes. Les lignes qui s'en écartent sont listées, avant et après correction.
- **Indicateurs de qualité par ligne** :
  - `sans_effectif` : aucun effectif renseigné ;
  - `negatif` : un effectif est négatif ;
  - `incoh_enfants` : le total des enfants est différent de la somme des moins de 5 ans et des plus de 5 ans ;
  - `incoh_total` : le total est différent de la somme hommes + femmes + enfants ;
  - `incoh_hierarchie` : le Pcode commune ne correspond pas à sa province ou à sa région.
- **Répétitions** :
  - plusieurs lignes pour un même lieu et une même date sont marquées (`repete_lieu_date`), pas supprimées, car elles correspondent à des sites ou des localités différents ;
  - les doublons exacts sont marqués (`doublon_exact`).
- **Libellés géographiques harmonisés** :
  - un libellé de référence par Pcode, choisi comme la forme en casse mixte la plus fréquente (pour éviter « KOMKI-IPALA ») ;
  - il donne `region_std`, `province_std` et `commune_std` ;
  - les variantes sont listées dans le dictionnaire géographique.
- **Colonnes retirées** : les colonnes entièrement vides (source, pays, site…).
- **Exports** :
  - `conasur-pdi_v1.rds` : la table propre ;
  - `conasur-controles_v1.xlsx` : manquants, Pcodes corrigés et à vérifier, hiérarchie, dictionnaire géographique, colonnes hors modèle.

### 3.4 Fonds de carte

- **Collecte** (`01_collecte/...geo-extraction_v1.R`) :
  - les deux archives sont décompressées ;
  - un inventaire des couches est enregistré dans `geo-inventaire_v1.csv` (nombre d'objets, type de géométrie, système de coordonnées, colonnes).
- **À la lecture**, dans l'analyse et l'application :
  - seules les couches de **polygones** sont retenues, car l'archive OCHA contient aussi des lignes (`admbndl`) et des points (`admbndp`) portant les mêmes Pcodes ;
  - les noms de colonnes sont mis en minuscules et rendus uniques (OCHA écrit `ADM2_FR`, `ADM2_PCODE`…) ;
  - les géométries invalides sont réparées ;
  - tout est reprojeté en WGS84.
- **Jointures** : le Pcode du shapefile (`adm2_pcode`) est relié à celui des résultats (`admin2_pcode`). Les provinces sans code commun sont reliées par leur nom.
- **Écoles OSM** :
  - seuls les types `school`, `kindergarten` et `college` sont retenus ;
  - les écoles décrites par un polygone sont ramenées à un point ;
  - chaque école est rattachée à sa commune par localisation.

### 3.5 Harmonisations faites au moment de l'analyse

Ces opérations se trouvent dans les scripts de `04_analyse/`, parce qu'elles dépendent de la façon dont les sources sont croisées.

- **Année de référence = année de fin de l'année scolaire** :
  - « 2015-2016 », « 2015/16 » et « 2015-16 » donnent 2016 ;
  - une année seule est considérée comme l'année de fin (session du CEP).
  - Correction apportée : l'ancienne fonction renvoyait 2015 pour « 2015/16 ».
- **Clé région** :
  - libellés normalisés (accents, casse, tirets, espaces) ;
  - « HAUT-BASSINS » et « AUTS BASSINS » deviennent « Hauts-Bassins » ;
  - « Burkina Faso », « NATIONAL », « Total général » et « Ensemble » deviennent le niveau national ;
  - « Centre (sans Ouagadougou) » (OpenData) est assimilé au Centre.
  - Les libellés Rural, Urbain et « indéterminé » sont écartés des tables régionales.
- **Clé province** :
  - libellé normalisé, l'article initial « Les » ignoré (« Les Balé » = « Balé ») ;
  - appariement exact, puis approché quand il n'y a qu'un seul candidat ;
  - les provinces OpenData non appariées sont listées dans la console.
- **Valeurs aberrantes** mises à valeur manquante et comptées : IPS hors de 0,3-3 et taux hors de 0-200 % dans le script parité-tendances ; IPS hors de 0,2-3 et taux hors de 0-300 % dans le script IMDE. Ce sont par exemple des années lues à tort comme valeurs.
- **Doublons DGESS** (même indicateur, région, année et sexe) : le premier relevé est conservé, et leur nombre est affiché.
- **Contrôle IPS** : l'IPS est recalculé à partir des taux filles et garçons, puis comparé à l'IPS publié (écart médian et maximal par indicateur).
- **Calage des années OpenData sur la DGESS** :
  - un décalage de -1, 0 ou +1 an est testé contre le TBS primaire ;
  - c'est le décalage 0 qui a été retenu ;
  - point de repère : TBS primaire national 2023/2024 = 78,2 % (INSD).
- **Choix des tables OpenData** : une table n'est retenue que si son nom et ses colonnes correspondent. Le dossier `TAUX_SCO_REGION` passe en priorité, car les autres dossiers contiennent des fichiers aux noms proches.
- **TBS supérieur à 100 %** : il est plafonné à 100 % dans les mesures de déficit d'accès, car il reflète des élèves hors âge ou déplacés.
- **Enfants déplacés** :
  - quand `total_enfants` manque (57 % des lignes), on additionne les moins de 5 ans et les plus de 5 ans ;
  - à défaut, on applique la part nationale des enfants au total de déplacés ;
  - pour chaque année, on retient la dernière collecte ;
  - après 2023, le dernier stock connu est reporté.

## 5. Points encore ouverts

- **Pcode de Koundougou** (Houet) : sera renseigner dans les futur travaux dans `corrections_pcodes` à partir de HDX / OCHA.
- **47 provinces dans le fond de carte, 45 dans le CONASUR et l'OpenData** : le fond de carte suit peut-être le découpage de 2025. Il faut lister les provinces sans correspondance.
- **Données communales non exploitées** : `TAUX_SCO_6_16_COMMUNE` et `NB_ECOLEPRIMARE_BM_COMMUNE`.
- **Dénominateurs par milieu** (population urbaine et rurale par âge, RGPH 2019 / projections INSD) : nécessaires pour passer des parts d'effectifs à des taux ville-campagne.
