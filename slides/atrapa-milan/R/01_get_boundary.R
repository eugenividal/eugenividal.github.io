## R/01_get_boundary.R
## Create + save the Milan (Comune di Milano) perimeter from OSM.
## Behaviour:
## - If proc_dir/<city_tag>_perimeter.gpkg exists: do nothing.
## - Otherwise: download admin boundary (level 8), keep Milano only, write layer "perimeter".
##
## Requires (from 00_setup.R):
## proc_dir, city_tag

stopifnot(exists("proc_dir"), exists("city_tag"))

out_gpkg <- file.path(proc_dir, paste0(city_tag, "_perimeter.gpkg"))

get_boundary_wgs <- function() {
  sf::st_read(out_gpkg, layer = "perimeter", quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

if (file.exists(out_gpkg)) {
  message("Skip: perimeter already exists -> ", out_gpkg)
  invisible(get_boundary_wgs())
} else {
  
  # Milan query anchor (tight and unambiguous)
  place <- "Milano, Italia"
  
  # getbb: use "matrix" for opq(bbox = ...)
  bb <- osmdata::getbb(city_boundary_place, format_out = "matrix")
    if (is.null(bb)) stop("Could not get bounding box for: ", place)
  
  od <- osmdata::opq(bbox = bb) |>
    osmdata::add_osm_feature(key = "boundary", value = "administrative") |>
    osmdata::add_osm_feature(key = "admin_level", value = "8") |>
    osmdata::osmdata_sf(quiet = TRUE)
  
  mp <- od$osm_multipolygons
  if (is.null(mp) || !nrow(mp)) stop("No admin multipolygons returned for Milan.")
  
  mp <- sf::st_make_valid(mp)
  
  # Prefer exact match on Italian name if available, otherwise on name
  name_it <- tolower(trimws(as.character(mp[["name:it"]])))
  name_en <- tolower(trimws(as.character(mp[["name"]])))
  
  cand <- mp[!is.na(name_it) & name_it == "milano", , drop = FALSE]
  if (!nrow(cand)) cand <- mp[!is.na(name_en) & name_en %in% c("milano", "milan"), , drop = FALSE]
  if (!nrow(cand)) stop("Could not isolate the Comune di Milano polygon (name match failed).")
  
  # If multiple remain, keep the largest polygon
  areas <- as.numeric(sf::st_area(sf::st_transform(cand, 3857)))
  cand <- cand[which.max(areas), , drop = FALSE]
  
  perim <- cand |>
    sf::st_union() |>
    sf::st_make_valid() |>
    sf::st_cast("MULTIPOLYGON")
  
  perim <- sf::st_as_sf(perim)
  sf::st_crs(perim) <- 4326
  perim <- sf::st_transform(perim, 4326)
  
  sf::st_write(perim, out_gpkg, layer = "perimeter", driver = "GPKG",
               append = FALSE, quiet = TRUE)
  
  message("Saved Milan perimeter -> ", out_gpkg)
  invisible(perim)
}
