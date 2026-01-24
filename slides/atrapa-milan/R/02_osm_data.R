# ================================================================
# R/02_osm_networks_custom.R
# Build processed OSM networks per snapshot from clipped OSM lines:
#   1) Cycling infrastructure (4-category scheme)
#   2) Pedestrian-priority streets (highway=pedestrian, living_street)
#
# Outputs (in proc_dir):
#   ci_osmextract_custom_total_<version>.gpkg
#   ped_priority_osm_total_<version>.gpkg
#
# Requires (from R/00_setup.R):
#   infra_region, crs_work, VERSIONS, FORCE_BUILD, proc_dir
# ================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(osmextract)
})

stopifnot(exists("proc_dir"), exists("infra_region"), exists("crs_work"),
          exists("VERSIONS"), exists("FORCE_BUILD"))
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Cycling tag value lists
# -----------------------------
STRONG_ONROAD_VALS   <- c("track", "opposite_track")
MODERATE_ONROAD_VALS <- c("lane", "opposite_lane")
WEAK_ONROAD_VALS     <- c("share_busway", "shared_lane")

FOOT_SHARED_HWY  <- c("path", "footway", "pedestrian")
FOOT_SHARED_BIC  <- c("yes", "designated")
FOOT_SHARED_FOOT <- c("yes", "designated")

PAVEMENT_LANE_HWY <- c("footway", "path", "pedestrian")
PAVEMENT_LANE_BIC <- c("designated")
PAVEMENT_LANE_ALLOW_SEGREGATED_YES <- TRUE

# -----------------------------
# Perimeter (expects proc_dir/perimeter.gpkg)
# -----------------------------
# -----------------------------
# Perimeter (proc_dir)
# - accepts either:
#   proc_dir/perimeter.gpkg
#   proc_dir/*_perimeter.gpkg   (e.g., milan_perimeter.gpkg)
# -----------------------------
read_perim_ll <- function() {
  p1 <- file.path(proc_dir, "perimeter.gpkg")
  if (file.exists(p1)) {
    p <- p1
  } else {
    hits <- list.files(proc_dir, pattern = "_perimeter\\.gpkg$", full.names = TRUE)
    if (length(hits) == 1) {
      p <- hits[1]
    } else if (length(hits) > 1) {
      stop(
        "More than one *_perimeter.gpkg found in proc_dir. Keep only one.\n",
        paste(hits, collapse = "\n")
      )
    } else {
      stop(
        "Perimeter file not found in proc_dir.\n",
        "Expected either:\n",
        "  ", p1, "\n",
        "  or one file matching *_perimeter.gpkg\n\n",
        "Files currently in proc_dir:\n",
        paste(list.files(proc_dir, full.names = FALSE), collapse = "\n")
      )
    }
  }
  
  sf::st_read(p, layer = "perimeter", quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}


# -----------------------------
# small utilities (kept minimal)
# -----------------------------
cycleway_cols <- function(x) names(x)[grepl("^cycleway($|[:_])", names(x), ignore.case = TRUE)]

has_cycleway_vals <- function(x, vals) {
  cols <- cycleway_cols(x)
  if (!length(cols)) return(rep(FALSE, nrow(x)))
  vals <- tolower(vals)
  out  <- rep(FALSE, nrow(x))
  for (cc in cols) {
    v <- tolower(trimws(as.character(x[[cc]])))
    v[is.na(v)] <- ""
    hit <- vapply(strsplit(v, ";", fixed = TRUE),
                  function(parts) any(trimws(parts) %in% vals),
                  logical(1))
    out <- out | hit
  }
  out
}

normalize_lines <- function(x) {
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x)) return(x)
  gt <- sf::st_geometry_type(x)
  x <- x[gt %in% c("LINESTRING", "MULTILINESTRING"), , drop = FALSE]
  if (!nrow(x)) return(x)
  x <- suppressWarnings(sf::st_cast(x, "LINESTRING"))
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  sf::st_make_valid(x)
}

# -----------------------------
# Builder (per version): write ONLY missing outputs
# -----------------------------
build_networks <- function(version) {
  
  out_ci  <- file.path(proc_dir, paste0("ci_osmextract_custom_total_", version, ".gpkg"))
  out_ped <- file.path(proc_dir, paste0("ped_priority_osm_total_", version, ".gpkg"))
  
  ci_exists  <- file.exists(out_ci)
  ped_exists <- file.exists(out_ped)
  
  # if not forcing, and both exist -> skip
  if (!isTRUE(FORCE_BUILD) && ci_exists && ped_exists) {
    message("[", version, "] skip (both exist)")
    return(invisible())
  }
  
  # decide what to build
  need_ci  <- isTRUE(FORCE_BUILD) || !ci_exists
  need_ped <- isTRUE(FORCE_BUILD) || !ped_exists
  
  # if forcing, delete only what we will rebuild
  if (isTRUE(FORCE_BUILD)) {
    if (need_ci && file.exists(out_ci)) file.remove(out_ci)
    if (need_ped && file.exists(out_ped)) file.remove(out_ped)
  }
  
  message("[", version, "] need_ci=", need_ci, " need_ped=", need_ped)
  
  # Download + clip once (only if we need at least one)
  perim_ll <- read_perim_ll()
  
  message("  Download OSM lines: ", infra_region, " @ ", version)
  lines <- osmextract::oe_get(
    place                 = infra_region,
    boundary              = sf::st_bbox(perim_ll),
    boundary_type         = "clipsrc",
    layer                 = "lines",
    version               = version,
    extra_tags = c(extra_tags = c(
      "highway",
      "cycleway", "cycleway:left", "cycleway:right", "cycleway:both",
      "bicycle", "foot", "segregated"
    )),
    force_vectortranslate = TRUE,
    quiet                 = FALSE
  )
  
  perim_m <- sf::st_transform(perim_ll, crs_work)
  lines_m <- sf::st_transform(lines, crs_work)
  lines_m <- sf::st_intersection(sf::st_make_valid(lines_m), sf::st_make_valid(perim_m))
  if (!nrow(lines_m)) stop("0 features after clipping for version ", version)
  
  lines_m <- normalize_lines(lines_m)
  if (!nrow(lines_m)) stop("0 line features after normalisation for version ", version)
  
  # =====================================================
  # 1) CYCLING (only if needed)
  # =====================================================
  if (need_ci) {
    highway    <- tolower(trimws(as.character(lines_m$highway)))
    bicycle    <- tolower(trimws(as.character(lines_m$bicycle)))
    foot       <- tolower(trimws(as.character(lines_m$foot)))
    segregated <- tolower(trimws(as.character(lines_m$segregated)))
    
    is_cyclewy <- !is.na(highway) & highway == "cycleway"
    has_strong_onroad   <- has_cycleway_vals(lines_m, STRONG_ONROAD_VALS)
    has_moderate_onroad <- has_cycleway_vals(lines_m, MODERATE_ONROAD_VALS)
    has_weak_onroad     <- has_cycleway_vals(lines_m, WEAK_ONROAD_VALS)
    
    is_foot_shared <- (!is.na(highway) & highway %in% FOOT_SHARED_HWY) &
      (!is.na(bicycle) & bicycle %in% FOOT_SHARED_BIC) &
      (!is.na(foot) & foot %in% FOOT_SHARED_FOOT) &
      !(segregated %in% "yes")
    
    is_pavement_lane <- (!is.na(highway) & highway %in% PAVEMENT_LANE_HWY) &
      (!is.na(bicycle) & bicycle %in% PAVEMENT_LANE_BIC) &
      (isTRUE(PAVEMENT_LANE_ALLOW_SEGREGATED_YES) | !(segregated %in% "yes"))
    
    ci <- lines_m
    ci$cycle_cat <- NA_character_
    
    ci$cycle_cat[is_cyclewy] <- "strong_ci"
    
    sel <- (is_foot_shared | is_pavement_lane) & is.na(ci$cycle_cat)
    ci$cycle_cat[sel] <- "shared_foot"
    
    sel <- is.na(ci$cycle_cat)
    ci$cycle_cat[sel & has_strong_onroad] <- "strong_ci"
    
    sel <- is.na(ci$cycle_cat)
    ci$cycle_cat[sel & has_moderate_onroad] <- "moderate_ci"
    
    sel <- is.na(ci$cycle_cat)
    ci$cycle_cat[sel & has_weak_onroad] <- "weak_ci"
    
    ci_core <- ci[!is.na(ci$cycle_cat), , drop = FALSE]
    if (!nrow(ci_core)) stop("0 cycling features for version ", version)
    
    ci_ll <- sf::st_transform(ci_core, 4326)
    sf::st_write(ci_ll, out_ci, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    message("  Wrote: ", basename(out_ci), " (n=", nrow(ci_ll), ")")
  } else {
    message("  Skip CI (exists): ", basename(out_ci))
  }
  
  # =====================================================
  # 2) PEDESTRIAN PRIORITY (only if needed)
  # =====================================================
  if (need_ped) {
    ped <- lines_m
    hwy <- tolower(trimws(as.character(ped$highway)))
    
    ped$ped_cat <- NA_character_
    ped$ped_cat[!is.na(hwy) & hwy == "pedestrian"]    <- "pedestrian_street"
    ped$ped_cat[!is.na(hwy) & hwy == "living_street"] <- "living_street"
    
    ped_core <- ped[!is.na(ped$ped_cat), , drop = FALSE]
    ped_ll <- sf::st_transform(ped_core, 4326)
    
    sf::st_write(ped_ll, out_ped, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    message("  Wrote: ", basename(out_ped), " (n=", nrow(ped_ll), ")")
  } else {
    message("  Skip PED (exists): ", basename(out_ped))
  }
  
  invisible()
}

for (v in VERSIONS) build_networks(v)
