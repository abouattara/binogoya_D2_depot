oex export
==========

Generated:        2026-11-02 07:37:24 UTC
oex version:      0.4.19
Project:          https://github.com/osgeonepal/oex

Country (ISO3):   BFA
Boundary:         geoBoundaries gbOpen ADM0 (buffered +10000m, simplified 1000m)
Bounding box:     (-5.6084, 9.3118, 2.4964, 15.1726)

Dataset:          education_facilities
Format:           ESRI Shapefile (shp)
Features:         11,974

Source:           OpenStreetMap contributors
Source URL:       https://www.openstreetmap.org/
Snapshot:         2026-11-02
License:          Open Database License (ODC-ODbL)
License URL:      https://opendatacommons.org/licenses/odbl/1-0/

About the source
  OpenStreetMap is a community-edited geographic dataset of the world. Country
  features are extracted from the source PBF via quackosm with the union of
  all category tag filters; per-category exports apply tag predicates at query
  time.

Notes
  - Shapefile output is split by geometry type:
    <category>_polygons.shp, <category>_lines.shp, <category>_points.shp.
    This is a shapefile-format limitation, not a data limitation.
  - Field names are truncated to 10 characters in shp; gpkg keeps them full.

Feedback:         https://github.com/osgeonepal/oex/issues
Engine: geofabrik