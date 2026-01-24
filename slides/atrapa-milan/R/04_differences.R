# ================================================================
# R/04_differences.R
# Compute differences between baseline and follow-up.
#
# Outputs (in proc_dir):
#   OSM Cycling:      osm_ci_added_<bl>_<fu>.gpkg (+ removed)
#   OSM Ped-priority: osm_pedpr_added_<bl>_<fu>.gpkg (+ removed)
#   Official Ped/ZTL: official_ped_area_added_<bl>_<fu>.gpkg (+ removed)
#                     official_ztl_added_<bl>_<fu>.gpkg (+ removed)
#
# Requires (from 00_setup.R):
#   proc_dir, crs_work, ver_bl, ver_fu, ver_code_bl, ver_code_fu
# ================================================================

stopifnot(exists("proc_dir"), exists("crs_work"))
stopifnot(exists("ver_bl"), exists("ver_fu"), exists("ver_code_bl"), exists("ver_code_fu"))
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# helper: find exactly one file in proc_dir
# -----------------------------
find_one_file <- function(pattern, what = "file") {
  hits <- list.files(proc_dir, pattern = pattern, full.names = TRUE)
  if (length(hits) == 0) stop("Missing ", what, " in proc_dir. Pattern: ", pattern, "\nproc_dir: ", proc_dir)
  if (length(hits) > 1) stop("Ambiguous ", what, " (more than one match). Pattern: ", pattern, "\nMatches:\n", paste(hits, collapse = "\n"))
  hits[[1]]
}

# -----------------------------
# readers (NO prefix, NO city_tag)
# -----------------------------
read_osm_ci <- function(ver_code) {
  # matches e.g. milan_ci_osmextract_custom_total_160101.gpkg OR ci_osmextract_custom_total_160101.gpkg
  f <- find_one_file(paste0("ci_osmextract_custom_total_", ver_code, "\\.gpkg$"), what = "OSM cycling gpkg")
  x <- sf::st_read(f, quiet = TRUE)
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  x
}

read_osm_pedpr <- function(ver_code) {
  # matches e.g. milan_ped_priority_osm_total_160101.gpkg OR ped_priority_osm_total_160101.gpkg
  f <- find_one_file(paste0("ped_priority_osm_total_", ver_code, "\\.gpkg$"), what = "OSM ped-priority gpkg")
  x <- sf::st_read(f, quiet = TRUE)
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  x
}

read_official_layer <- function(layer) {
  # matches e.g. milan_official_clean.gpkg OR official_clean.gpkg
  f <- find_one_file("official_clean\\.gpkg$", what = "official_clean gpkg")
  x <- sf::st_read(f, layer = layer, quiet = TRUE)
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  x
}

# -----------------------------
# differencing: OSM LINES with tolerance
# -----------------------------
osm_added_removed <- function(bl, fu, tol_m = 10) {
  bl_m <- sf::st_transform(bl, crs_work)
  fu_m <- sf::st_transform(fu, crs_work)
  
  if (!nrow(bl_m) && !nrow(fu_m)) {
    return(list(
      added = sf::st_transform(fu_m, 4326),
      removed = sf::st_transform(bl_m, 4326)
    ))
  }
  if (!nrow(bl_m)) {
    return(list(
      added = sf::st_transform(fu_m, 4326),
      removed = sf::st_transform(bl_m, 4326)
    ))
  }
  if (!nrow(fu_m)) {
    return(list(
      added = sf::st_transform(fu_m, 4326),
      removed = sf::st_transform(bl_m, 4326)
    ))
  }
  
  bl_buf <- sf::st_buffer(sf::st_union(sf::st_geometry(bl_m)), tol_m / 2)
  fu_buf <- sf::st_buffer(sf::st_union(sf::st_geometry(fu_m)), tol_m / 2)
  
  add_poly <- suppressWarnings(sf::st_difference(fu_buf, bl_buf))
  rem_poly <- suppressWarnings(sf::st_difference(bl_buf, fu_buf))
  
  added   <- suppressWarnings(sf::st_intersection(fu_m, add_poly))
  removed <- suppressWarnings(sf::st_intersection(bl_m, rem_poly))
  
  added   <- added[!sf::st_is_empty(added), , drop = FALSE]
  removed <- removed[!sf::st_is_empty(removed), , drop = FALSE]
  
  list(
    added   = sf::st_transform(added, 4326),
    removed = sf::st_transform(removed, 4326)
  )
}

# -----------------------------
# differencing: OFFICIAL POLYGONS (ped areas + ZTL)
# -----------------------------
official_poly_bl_fu <- function(off_poly, year_bl, year_fu, year_field = "year_exec") {
  stopifnot(inherits(off_poly, "sf"))
  stopifnot(year_field %in% names(off_poly))
  
  off_poly <- off_poly |> dplyr::filter(!is.na(.data[[year_field]]))
  bl <- off_poly |> dplyr::filter(.data[[year_field]] <= year_bl)
  fu <- off_poly |> dplyr::filter(.data[[year_field]] <= year_fu)
  
  list(bl = bl, fu = fu)
}

poly_added_removed <- function(bl, fu) {
  bl_m <- sf::st_transform(bl, crs_work)
  fu_m <- sf::st_transform(fu, crs_work)
  
  if (!nrow(bl_m) && !nrow(fu_m)) {
    add_sf <- sf::st_as_sf(sf::st_sfc(sf::st_geometrycollection(), crs = crs_work))
    rem_sf <- sf::st_as_sf(sf::st_sfc(sf::st_geometrycollection(), crs = crs_work))
    return(list(
      added = sf::st_transform(add_sf, 4326),
      removed = sf::st_transform(rem_sf, 4326)
    ))
  }
  
  bl_u <- suppressWarnings(sf::st_union(sf::st_make_valid(bl_m)))
  fu_u <- suppressWarnings(sf::st_union(sf::st_make_valid(fu_m)))
  
  add_poly <- suppressWarnings(sf::st_difference(fu_u, bl_u))
  rem_poly <- suppressWarnings(sf::st_difference(bl_u, fu_u))
  
  add_sf <- sf::st_as_sf(sf::st_sfc(add_poly, crs = crs_work))
  rem_sf <- sf::st_as_sf(sf::st_sfc(rem_poly, crs = crs_work))
  
  add_sf <- add_sf[!sf::st_is_empty(add_sf), , drop = FALSE]
  rem_sf <- rem_sf[!sf::st_is_empty(rem_sf), , drop = FALSE]
  
  list(
    added   = sf::st_transform(add_sf, 4326),
    removed = sf::st_transform(rem_sf, 4326)
  )
}

# -----------------------------
# main
# -----------------------------
tol_m_use <- if (exists("tol_m")) tol_m else 10
year_bl_i <- as.integer(ver_bl)
year_fu_i <- as.integer(ver_fu)

# 1) OSM CYCLING
osm_ci_bl <- read_osm_ci(ver_code_bl)
osm_ci_fu <- read_osm_ci(ver_code_fu)
osm_ci_diff <- osm_added_removed(osm_ci_bl, osm_ci_fu, tol_m = tol_m_use)

out_osm_ci_added   <- file.path(proc_dir, paste0("osm_ci_added_",   ver_bl, "_", ver_fu, ".gpkg"))
out_osm_ci_removed <- file.path(proc_dir, paste0("osm_ci_removed_", ver_bl, "_", ver_fu, ".gpkg"))

if (file.exists(out_osm_ci_added))   file.remove(out_osm_ci_added)
if (file.exists(out_osm_ci_removed)) file.remove(out_osm_ci_removed)

sf::st_write(osm_ci_diff$added,   out_osm_ci_added,   layer = "added",   quiet = TRUE)
sf::st_write(osm_ci_diff$removed, out_osm_ci_removed, layer = "removed", quiet = TRUE)

message("Wrote OSM CI added:   ", out_osm_ci_added)
message("Wrote OSM CI removed: ", out_osm_ci_removed)

# 2) OSM PEDESTRIAN-PRIORITY
osm_ped_bl <- read_osm_pedpr(ver_code_bl)
osm_ped_fu <- read_osm_pedpr(ver_code_fu)
osm_ped_diff <- osm_added_removed(osm_ped_bl, osm_ped_fu, tol_m = tol_m_use)

out_osm_ped_added   <- file.path(proc_dir, paste0("osm_pedpr_added_",   ver_bl, "_", ver_fu, ".gpkg"))
out_osm_ped_removed <- file.path(proc_dir, paste0("osm_pedpr_removed_", ver_bl, "_", ver_fu, ".gpkg"))

if (file.exists(out_osm_ped_added))   file.remove(out_osm_ped_added)
if (file.exists(out_osm_ped_removed)) file.remove(out_osm_ped_removed)

sf::st_write(osm_ped_diff$added,   out_osm_ped_added,   layer = "added",   quiet = TRUE)
sf::st_write(osm_ped_diff$removed, out_osm_ped_removed, layer = "removed", quiet = TRUE)

message("Wrote OSM ped-priority added:   ", out_osm_ped_added)
message("Wrote OSM ped-priority removed: ", out_osm_ped_removed)

# 3) OFFICIAL PEDESTRIAN AREAS + ZTL (POLYGONS)
off_pedztl <- read_official_layer("official_ped_ztl_clean")

if (!"area_cat" %in% names(off_pedztl)) stop("official_ped_ztl_clean is missing area_cat")

if (!"year_exec" %in% names(off_pedztl)) {
  message("Note: official_ped_ztl_clean has no year_exec; skipping polygon diffs.")
} else {
  # Ped areas
  off_ped_area <- off_pedztl |> dplyr::filter(area_cat == "ped_area")
  if (nrow(off_ped_area) > 0) {
    blfu <- official_poly_bl_fu(off_ped_area, year_bl_i, year_fu_i, year_field = "year_exec")
    ped_area_diff <- poly_added_removed(blfu$bl, blfu$fu)
    
    out_off_ped_added   <- file.path(proc_dir, paste0("official_ped_area_added_",   ver_bl, "_", ver_fu, ".gpkg"))
    out_off_ped_removed <- file.path(proc_dir, paste0("official_ped_area_removed_", ver_bl, "_", ver_fu, ".gpkg"))
    
    if (file.exists(out_off_ped_added))   file.remove(out_off_ped_added)
    if (file.exists(out_off_ped_removed)) file.remove(out_off_ped_removed)
    
    sf::st_write(ped_area_diff$added,   out_off_ped_added,   layer = "added",   quiet = TRUE)
    sf::st_write(ped_area_diff$removed, out_off_ped_removed, layer = "removed", quiet = TRUE)
    
    message("Wrote OFFICIAL ped areas added:   ", out_off_ped_added)
    message("Wrote OFFICIAL ped areas removed: ", out_off_ped_removed)
  } else {
    message("Note: No official ped_area features; skipping ped_area diffs.")
  }
  
  # ZTL
  off_ztl <- off_pedztl |> dplyr::filter(area_cat == "ztl")
  if (nrow(off_ztl) > 0) {
    blfu <- official_poly_bl_fu(off_ztl, year_bl_i, year_fu_i, year_field = "year_exec")
    ztl_diff <- poly_added_removed(blfu$bl, blfu$fu)
    
    out_off_ztl_added   <- file.path(proc_dir, paste0("official_ztl_added_",   ver_bl, "_", ver_fu, ".gpkg"))
    out_off_ztl_removed <- file.path(proc_dir, paste0("official_ztl_removed_", ver_bl, "_", ver_fu, ".gpkg"))
    
    if (file.exists(out_off_ztl_added))   file.remove(out_off_ztl_added)
    if (file.exists(out_off_ztl_removed)) file.remove(out_off_ztl_removed)
    
    sf::st_write(ztl_diff$added,   out_off_ztl_added,   layer = "added",   quiet = TRUE)
    sf::st_write(ztl_diff$removed, out_off_ztl_removed, layer = "removed", quiet = TRUE)
    
    message("Wrote OFFICIAL ZTL added:   ", out_off_ztl_added)
    message("Wrote OFFICIAL ZTL removed: ", out_off_ztl_removed)
  } else {
    message("Note: No official ztl features; skipping ztl diffs.")
  }
}

message("Done.")

