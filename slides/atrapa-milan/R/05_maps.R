# ================================================================
# R/05_maps.R
# Interactive leaflet maps (robust for Quarto HTML)
#
# Focus:
#   - Works reliably for official pedestrian data as (MULTI)POLYGON
#   - Always ensures geometry + CRS are correct before leaflet
#   - Avoids st_is_longlat() NA traps
#
# Requires (from 00_setup.R):
#   proc_dir, crs_work, ver_bl, ver_fu, ver_code_bl, ver_code_fu
# Optional:
#   city_tag (prefer <city_tag>_perimeter.gpkg if present)
# ================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(leaflet)
  library(htmltools)
  library(htmlwidgets)
})

stopifnot(exists("proc_dir"), exists("crs_work"))
stopifnot(exists("ver_bl"), exists("ver_fu"), exists("ver_code_bl"), exists("ver_code_fu"))
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

MAP_HEIGHT_PX <- 420L

# ------------------------------------------------
# File helper
# ------------------------------------------------
pick_file <- function(patterns, what = "file") {
  patterns <- as.character(patterns)
  hits <- character()
  
  for (pat in patterns) {
    hits <- c(hits, list.files(proc_dir, pattern = pat, full.names = TRUE))
  }
  hits <- unique(hits)
  
  if (length(hits) == 0) {
    stop(
      "Missing ", what, " in proc_dir.\n",
      "Tried patterns:\n- ", paste(patterns, collapse = "\n- "), "\n",
      "proc_dir: ", proc_dir
    )
  }
  if (length(hits) > 1) {
    stop(
      "Ambiguous ", what, " (more than one match).\n",
      "Tried patterns:\n- ", paste(patterns, collapse = "\n- "), "\n\n",
      "Matches:\n- ", paste(basename(hits), collapse = "\n- ")
    )
  }
  hits[[1]]
}

as_int_safe <- function(x) suppressWarnings(as.integer(as.character(x)))

# ------------------------------------------------
# Geometry + CRS safety (critical for MULTIPOLYGON)
# ------------------------------------------------
set_active_geom <- function(x, preferred = c("geom", "geometry")) {
  if (is.null(x) || !inherits(x, "sf")) return(x)
  
  nm <- names(x)
  
  # If there is already an active geometry AND it points to a real sfc column, keep it
  g <- tryCatch(sf::st_geometry(x), error = function(e) NULL)
  if (!is.null(g) && inherits(g, "sfc")) {
    sfcol <- attr(g, "sf_column")
    if (!is.null(sfcol) && sfcol %in% nm && inherits(x[[sfcol]], "sfc")) {
      return(x)
    }
  }
  
  # Try preferred names
  cand <- preferred[preferred %in% nm]
  if (length(cand) > 0 && inherits(x[[cand[[1]]]], "sfc")) {
    sf::st_geometry(x) <- cand[[1]]
    return(x)
  }
  
  # Fallback: first sfc column
  is_sfc <- vapply(x, inherits, logical(1), what = "sfc")
  if (any(is_sfc)) {
    sf::st_geometry(x) <- names(x)[which(is_sfc)[1]]
    return(x)
  }
  
  stop("No geometry column found in sf object.")
}

drop_empty <- function(x) {
  if (is.null(x) || !inherits(x, "sf") || !nrow(x)) return(x)
  x[!sf::st_is_empty(sf::st_geometry(x)), , drop = FALSE]
}

fix_crs_if_missing <- function(x, default_crs = crs_work) {
  if (is.null(x) || !inherits(x, "sf") || !nrow(x)) return(x)
  if (is.na(sf::st_crs(x))) sf::st_crs(x) <- default_crs
  x
}

# Make leaflet-safe for *any* geometry; extra care for polygons
clean_sf <- function(x, polygon_mode = FALSE) {
  if (is.null(x) || !inherits(x, "sf") || !nrow(x)) return(x)
  
  x <- set_active_geom(x)
  
  # Drop Z/M early
  x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  
  # Validity repair (polygons often need it)
  x <- sf::st_make_valid(x)
  
  # After make_valid, polygons may become collections. Extract polygon parts.
  if (isTRUE(polygon_mode)) {
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    # cast to MULTIPOLYGON so leaflet handling is consistent
    x <- suppressWarnings(sf::st_cast(x, "MULTIPOLYGON"))
  }
  
  x <- drop_empty(x)
  x
}

to_wgs84 <- function(x, default_crs = crs_work) {
  if (is.null(x) || !inherits(x, "sf") || !nrow(x)) return(x)
  
  x <- set_active_geom(x)
  x <- fix_crs_if_missing(x, default_crs = default_crs)
  
  epsg <- sf::st_crs(x)$epsg
  # Always transform unless it is already EPSG:4326
  if (is.na(epsg) || epsg != 4326) {
    x <- sf::st_transform(x, 4326)
  }
  x
}

# Strict check: leaflet data must be lon/lat-ish
stop_if_not_lonlat <- function(x, where = "object") {
  if (is.null(x) || !inherits(x, "sf") || !nrow(x)) return(invisible(TRUE))
  bb <- sf::st_bbox(x)
  # lon must be within [-180, 180] and lat within [-90, 90] (allow small tolerance)
  if (isTRUE(bb[["xmin"]] < -200) || isTRUE(bb[["xmax"]] > 200) ||
      isTRUE(bb[["ymin"]] < -100) || isTRUE(bb[["ymax"]] > 100)) {
    stop(where, " is not in lon/lat (EPSG:4326). bbox=", paste(unname(bb), collapse = ", "))
  }
  invisible(TRUE)
}

# ------------------------------------------------
# Leaflet widget fix for Quarto
# ------------------------------------------------
fix_leaflet_widget <- function(m, height_px = MAP_HEIGHT_PX) {
  stopifnot(inherits(m, "leaflet"))
  m$height <- as.integer(height_px)
  
  htmlwidgets::onRender(
    m,
    sprintf(
      "
      function(el, x) {
        el.style.height = '%spx';
        var map = HTMLWidgets.find('#' + el.id);
        function inv() {
          try {
            if (map && map.getMap) map.getMap().invalidateSize();
          } catch(e) {}
        }
        setTimeout(inv, 50);
        setTimeout(inv, 250);
        setTimeout(inv, 800);

        if (window.ResizeObserver) {
          const ro = new ResizeObserver(() => inv());
          ro.observe(el);
          setTimeout(() => { try { ro.disconnect(); } catch(e) {} }, 5000);
        }
      }
      ",
      as.integer(height_px)
    )
  )
}

overlay_groups <- function(...) {
  x <- unlist(list(...), use.names = FALSE)
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

# ------------------------------------------------
# Boundary
# ------------------------------------------------
read_boundary <- function() {
  if (exists("city_tag")) {
    f_try <- file.path(proc_dir, paste0(city_tag, "_perimeter.gpkg"))
    if (file.exists(f_try)) {
      return(sf::st_read(f_try, layer = "perimeter", quiet = TRUE))
    }
  }
  f <- pick_file(patterns = c("(^|.*_)perimeter\\.gpkg$"), what = "perimeter gpkg")
  sf::st_read(f, layer = "perimeter", quiet = TRUE)
}

get_boundary_wgs <- function() {
  b <- read_boundary()
  b <- clean_sf(b, polygon_mode = TRUE)
  b <- to_wgs84(b)
  stop_if_not_lonlat(b, "Boundary")
  b
}

# ------------------------------------------------
# GSV popup helper
# ------------------------------------------------
add_gsv_popup <- function(x_wgs, label = "Feature", crs_metric = NULL) {
  if (is.null(x_wgs) || !inherits(x_wgs, "sf") || !nrow(x_wgs)) return(x_wgs)
  if (is.null(crs_metric)) crs_metric <- crs_work
  
  x_wgs <- set_active_geom(x_wgs)
  x_wgs <- fix_crs_if_missing(x_wgs, default_crs = 4326) # should already be 4326
  x_wgs <- drop_empty(x_wgs)
  if (!nrow(x_wgs)) return(x_wgs)
  
  x_m <- sf::st_transform(x_wgs, crs_metric)
  g <- sf::st_geometry(x_m)
  gt <- as.character(sf::st_geometry_type(g, by_geometry = TRUE))
  
  pts_m <- tryCatch({
    if (any(grepl("POLYGON", gt))) {
      sf::st_point_on_surface(g)
    } else if (any(grepl("LINESTRING", gt))) {
      s <- suppressWarnings(sf::st_line_sample(g, sample = 0.5))
      if (length(s) && !all(sf::st_is_empty(s))) s else sf::st_centroid(g)
    } else {
      sf::st_centroid(g)
    }
  }, error = function(e) sf::st_centroid(g))
  
  pts_sf <- sf::st_as_sf(pts_m)
  pts_wgs <- sf::st_transform(pts_sf, 4326)
  coords <- sf::st_coordinates(pts_wgs)
  
  lon <- coords[, "X"]
  lat <- coords[, "Y"]
  
  gsv_url <- paste0(
    "https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=",
    lat, ",", lon
  )
  
  x_wgs$gsv_url <- gsv_url
  x_wgs$popup_html <- paste0(
    "<b>", label, "</b>",
    "<br><a href='", gsv_url, "' target='_blank' rel='noopener'>Open in GSV</a>"
  )
  x_wgs
}

# ------------------------------------------------
# Labels
# ------------------------------------------------
label_osm_cycle_cat <- function(x) {
  dplyr::case_when(
    x == "strong_ci"   ~ "Separated cycling infrastructure",
    x == "moderate_ci" ~ "Painted on-road cycle lane",
    x == "weak_ci"     ~ "Mixed traffic (cars or buses)",
    x == "shared_foot" ~ "Cycling on pedestrian infrastructure",
    TRUE               ~ as.character(x)
  )
}

label_osm_ped_cat <- function(x) {
  dplyr::case_when(
    x == "pedestrian_street" ~ "Pedestrian street",
    x == "living_street"     ~ "Living street",
    TRUE                     ~ as.character(x)
  )
}

# ------------------------------------------------
# Base leaflet + fit
# ------------------------------------------------
leaf_base <- function(bnd_wgs = NULL) {
  m <- leaflet(options = leafletOptions(preferCanvas = TRUE), height = MAP_HEIGHT_PX) |>
    addProviderTiles(providers$CartoDB.Positron, group = "Positron")
  
  if (!is.null(bnd_wgs) && inherits(bnd_wgs, "sf") && nrow(bnd_wgs)) {
    m <- m |>
      addPolylines(
        data = bnd_wgs,
        group = "Boundary",
        color = "#111827",
        weight = 1,
        opacity = 0.8
      )
  }
  m
}

fit_to <- function(m, bnd_wgs = NULL, x_wgs = NULL, zoom_fallback = 13) {
  bb_to_num <- function(bb) unname(as.numeric(bb))
  
  if (!is.null(bnd_wgs) && inherits(bnd_wgs, "sf") && nrow(bnd_wgs)) {
    bb <- bb_to_num(sf::st_bbox(bnd_wgs))
    if (length(bb) == 4 && all(is.finite(bb))) {
      return(m |> fitBounds(bb[1], bb[2], bb[3], bb[4]))
    }
  }
  
  if (!is.null(x_wgs) && inherits(x_wgs, "sf") && nrow(x_wgs)) {
    bb <- bb_to_num(sf::st_bbox(x_wgs))
    if (length(bb) == 4 && all(is.finite(bb))) {
      return(m |> fitBounds(bb[1], bb[2], bb[3], bb[4]))
    }
    
    ctr <- tryCatch(
      sf::st_coordinates(sf::st_centroid(sf::st_union(sf::st_geometry(x_wgs))))[1, ],
      error = function(e) NULL
    )
    if (!is.null(ctr) && all(is.finite(ctr))) {
      return(m |> setView(lng = unname(ctr[1]), lat = unname(ctr[2]), zoom = zoom_fallback))
    }
  }
  
  m
}

# ------------------------------------------------
# Readers
# ------------------------------------------------
read_osm_cycling_snapshot <- function(ver_code) {
  f <- pick_file(
    patterns = c(paste0("(^|.*_)ci_osmextract_custom_total_", ver_code, "\\.gpkg$")),
    what = "OSM cycling gpkg"
  )
  x <- sf::st_read(f, quiet = TRUE)
  x <- clean_sf(x, polygon_mode = FALSE)
  if (!("cycle_cat" %in% names(x))) stop("cycle_cat missing in: ", basename(f))
  x
}

read_osm_ped_snapshot <- function(ver_code) {
  f <- pick_file(
    patterns = c(paste0("(^|.*_)ped_priority_osm_total_", ver_code, "\\.gpkg$")),
    what = "OSM ped-priority gpkg"
  )
  x <- sf::st_read(f, quiet = TRUE)
  x <- clean_sf(x, polygon_mode = FALSE)
  if (!("ped_cat" %in% names(x))) stop("ped_cat missing in: ", basename(f))
  x
}

read_official_gpkg <- function() {
  pick_file(patterns = c("(^|.*_)official_clean\\.gpkg$"), what = "official_clean gpkg")
}

read_official_layer <- function(layer, polygon_mode = FALSE) {
  f <- read_official_gpkg()
  layers <- tryCatch(sf::st_layers(f)$name, error = function(e) character())
  if (!length(layers)) stop("Could not read layers from: ", f)
  
  if (!(layer %in% layers)) {
    stop(
      "Layer not found in official_clean.gpkg: ", layer, "\n",
      "Available layers:\n- ", paste(layers, collapse = "\n- ")
    )
  }
  
  x <- sf::st_read(f, layer = layer, quiet = TRUE)
  x <- set_active_geom(x, preferred = c("geom", "geometry"))
  x <- clean_sf(x, polygon_mode = polygon_mode)
  x
}

# ================================================================
# CYCLING MAPS
# ================================================================
make_osm_cycling_snapshot_map <- function(year_label, version_code,
                                          line_col = "#111827", line_opacity = 0.75) {
  bnd_wgs <- tryCatch(get_boundary_wgs(), error = function(e) NULL)
  
  x <- read_osm_cycling_snapshot(version_code)
  x <- to_wgs84(x)
  stop_if_not_lonlat(x, "OSM cycling snapshot")
  
  x$cat_key <- as.character(x$cycle_cat)
  x <- add_gsv_popup(x, label = paste0("OSM cycling ", year_label))
  
  cats <- unique(x$cat_key)
  cats <- cats[order(match(cats, c("strong_ci", "moderate_ci", "weak_ci", "shared_foot")), na.last = TRUE)]
  
  m <- leaf_base(bnd_wgs)
  
  for (ck in cats) {
    dfc <- x[x$cat_key == ck, , drop = FALSE]
    if (!nrow(dfc)) next
    
    grp <- paste0("OSM: ", label_osm_cycle_cat(ck))
    dfc$popup_html <- paste0(
      "<b>OSM ", year_label, " - ", label_osm_cycle_cat(ck), "</b>",
      "<br><a href='", dfc$gsv_url, "' target='_blank' rel='noopener'>Open in GSV</a>"
    )
    
    m <- m |>
      addPolylines(
        data = dfc,
        group = grp,
        color = line_col,
        weight = 2,
        opacity = line_opacity,
        popup = ~popup_html
      )
  }
  
  og <- overlay_groups(
    if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "",
    paste0("OSM: ", label_osm_cycle_cat(cats))
  )
  
  m <- m |>
    addLayersControl(baseGroups = "Positron", overlayGroups = og,
                     options = layersControlOptions(collapsed = TRUE))
  
  fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs, x_wgs = x))
}

make_osm_cycling_diff_map <- function(year_bl = ver_bl, year_fu = ver_fu,
                                      added_col = "#0072B2", removed_col = "#D95F02") {
  bnd_wgs <- tryCatch(get_boundary_wgs(), error = function(e) NULL)
  
  f_add <- pick_file(paste0("(^|.*_)osm_ci_added_", year_bl, "_", year_fu, "\\.gpkg$"),
                     what = "OSM cycling added gpkg")
  f_rem <- pick_file(paste0("(^|.*_)osm_ci_removed_", year_bl, "_", year_fu, "\\.gpkg$"),
                     what = "OSM cycling removed gpkg")
  
  add <- sf::st_read(f_add, layer = "added", quiet = TRUE) |> clean_sf(polygon_mode = FALSE) |> to_wgs84()
  rem <- sf::st_read(f_rem, layer = "removed", quiet = TRUE) |> clean_sf(polygon_mode = FALSE) |> to_wgs84()
  
  if (!is.null(add) && nrow(add)) stop_if_not_lonlat(add, "OSM cycling added")
  if (!is.null(rem) && nrow(rem)) stop_if_not_lonlat(rem, "OSM cycling removed")
  
  add <- add_gsv_popup(add, label = paste0("OSM cycling added ", year_bl, " to ", year_fu))
  rem <- add_gsv_popup(rem, label = paste0("OSM cycling removed ", year_bl, " to ", year_fu))
  
  m <- leaf_base(bnd_wgs)
  
  if (!is.null(add) && nrow(add)) {
    m <- m |>
      addPolylines(data = add, group = "OSM: Added",
                   color = added_col, weight = 2, opacity = 0.9,
                   popup = ~popup_html)
  }
  if (!is.null(rem) && nrow(rem)) {
    m <- m |>
      addPolylines(data = rem, group = "OSM: Removed",
                   color = removed_col, weight = 2, opacity = 0.9,
                   popup = ~popup_html)
  }
  
  og <- overlay_groups(
    if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "",
    if (!is.null(add) && nrow(add)) "OSM: Added" else "",
    if (!is.null(rem) && nrow(rem)) "OSM: Removed" else ""
  )
  
  m <- m |>
    addLayersControl(baseGroups = "Positron", overlayGroups = og,
                     options = layersControlOptions(collapsed = TRUE))
  
  x_all <- dplyr::bind_rows(add, rem)
  fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs, x_wgs = x_all))
}

make_official_cycling_year_map <- function(year_label, year_exec,
                                           line_col = "#111827", line_opacity = 0.75,
                                           layer_cycling = "official_cycling_clean",
                                           cat_field = NULL) {
  bnd_wgs <- tryCatch(get_boundary_wgs(), error = function(e) NULL)
  
  off <- read_official_layer(layer_cycling, polygon_mode = FALSE)
  if (!("year_exec" %in% names(off))) stop("year_exec missing in layer: ", layer_cycling)
  
  off$year_exec_num <- as_int_safe(off$year_exec)
  off <- off |>
    dplyr::filter(!is.na(year_exec_num), year_exec_num <= as.integer(year_exec)) |>
    dplyr::select(-year_exec_num)
  
  off <- clean_sf(off, polygon_mode = FALSE)
  off <- to_wgs84(off)
  stop_if_not_lonlat(off, "Official cycling year")
  
  m <- leaf_base(bnd_wgs)
  
  if (is.null(off) || !nrow(off)) {
    og <- overlay_groups(if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "")
    m <- m |>
      addLayersControl(baseGroups = "Positron", overlayGroups = og,
                       options = layersControlOptions(collapsed = TRUE))
    return(fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs)))
  }
  
  off <- add_gsv_popup(off, label = paste0("Official cycling ", year_label))
  
  # pick category field
  if (is.null(cat_field)) {
    candidates <- c("tipologia_clean", "typology_clean", "tipologia", "rete", "gerarchia", "type", "category")
    cat_field <- candidates[candidates %in% names(off)][1]
    if (is.na(cat_field)) {
      stop(
        "No category field found. Provide `cat_field = ...`.\n",
        "Available fields:\n- ", paste(names(off), collapse = "\n- ")
      )
    }
  } else {
    if (!(cat_field %in% names(off))) stop("cat_field not found in data: ", cat_field)
  }
  
  off$cat_key <- as.character(off[[cat_field]])
  off$cat_key[is.na(off$cat_key) | !nzchar(off$cat_key)] <- "Unknown"
  
  cats <- sort(unique(off$cat_key))
  
  # add one polyline layer per category (one toggle each)
  for (ck in cats) {
    dfc <- off[off$cat_key == ck, , drop = FALSE]
    if (!nrow(dfc)) next
    
    grp <- paste0("Official: ", ck)
    
    # keep popup informative per category
    dfc$popup_html <- paste0(
      "<b>Official cycling ", year_label, "</b>",
      "<br><b>", cat_field, ":</b> ", ck,
      "<br><a href='", dfc$gsv_url, "' target='_blank' rel='noopener'>Open in GSV</a>"
    )
    
    m <- m |>
      addPolylines(
        data = dfc,
        group = grp,
        color = line_col, weight = 2, opacity = line_opacity,
        popup = ~popup_html
      )
  }
  
  og <- overlay_groups(
    if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "",
    paste0("Official: ", cats)
  )
  
  m <- m |>
    addLayersControl(baseGroups = "Positron", overlayGroups = og,
                     options = layersControlOptions(collapsed = TRUE))
  
  fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs, x_wgs = off))
}


make_official_cycling_diff_map <- function(year_bl = ver_bl, year_fu = ver_fu,
                                           line_col = "#0072B2", line_opacity = 0.9,
                                           layer_cycling = "official_cycling_clean") {
  bnd_wgs <- tryCatch(get_boundary_wgs(), error = function(e) NULL)
  
  off <- read_official_layer(layer_cycling, polygon_mode = FALSE)
  if (!("year_exec" %in% names(off))) stop("year_exec missing in layer: ", layer_cycling)
  
  yb <- as.integer(year_bl)
  yf <- as.integer(year_fu)
  
  off$year_exec_num <- as_int_safe(off$year_exec)
  
  add <- off |>
    dplyr::filter(!is.na(year_exec_num), year_exec_num > yb, year_exec_num <= yf) |>
    dplyr::select(-year_exec_num)
  
  add <- clean_sf(add, polygon_mode = FALSE)
  add <- to_wgs84(add)
  stop_if_not_lonlat(add, "Official cycling diff (added)")
  
  m <- leaf_base(bnd_wgs)
  
  if (is.null(add) || !nrow(add)) {
    og <- overlay_groups(if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "")
    m <- m |>
      addLayersControl(baseGroups = "Positron", overlayGroups = og,
                       options = layersControlOptions(collapsed = TRUE))
    return(fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs)))
  }
  
  add <- add_gsv_popup(add, label = paste0("Official cycling added ", year_bl, " to ", year_fu))
  
  m <- m |>
    addPolylines(
      data = add,
      group = "Official: Added",
      color = line_col, weight = 2, opacity = line_opacity,
      popup = ~popup_html
    )
  
  og <- overlay_groups(
    if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "",
    "Official: Added"
  )
  
  m <- m |>
    addLayersControl(baseGroups = "Positron", overlayGroups = og,
                     options = layersControlOptions(collapsed = TRUE))
  
  fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs, x_wgs = add))
}

# ================================================================
# PEDESTRIAN PRIORITY MAPS
# ================================================================
make_osm_ped_snapshot_map <- function(year_label, version_code,
                                      line_col = "#111827", line_opacity = 0.75) {
  bnd_wgs <- tryCatch(get_boundary_wgs(), error = function(e) NULL)
  
  x <- read_osm_ped_snapshot(version_code)
  x <- to_wgs84(x)
  stop_if_not_lonlat(x, "OSM ped snapshot")
  
  x$cat_key <- as.character(x$ped_cat)
  x <- add_gsv_popup(x, label = paste0("OSM ped-priority ", year_label))
  
  cats <- unique(x$cat_key)
  cats <- cats[order(match(cats, c("pedestrian_street", "living_street")), na.last = TRUE)]
  
  m <- leaf_base(bnd_wgs)
  
  for (ck in cats) {
    dfc <- x[x$cat_key == ck, , drop = FALSE]
    if (!nrow(dfc)) next
    
    grp <- paste0("OSM: ", label_osm_ped_cat(ck))
    dfc$popup_html <- paste0(
      "<b>OSM ", year_label, " - ", label_osm_ped_cat(ck), "</b>",
      "<br><a href='", dfc$gsv_url, "' target='_blank' rel='noopener'>Open in GSV</a>"
    )
    
    m <- m |>
      addPolylines(
        data = dfc,
        group = grp,
        weight = 2,
        color = line_col,
        opacity = line_opacity,
        popup = ~popup_html
      )
  }
  
  og <- overlay_groups(
    if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "",
    paste0("OSM: ", label_osm_ped_cat(cats))
  )
  
  m <- m |>
    addLayersControl(baseGroups = "Positron", overlayGroups = og,
                     options = layersControlOptions(collapsed = TRUE))
  
  fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs, x_wgs = x))
}

make_official_pedztl_year_map <- function(year_label, year_exec,
                                          fill_opacity = 0.35, border_opacity = 0.65,
                                          layer_pedztl = "official_ped_ztl_clean",
                                          ped_fill = "#1B9E77",
                                          ztl_fill = "#7570B3",
                                          other_fill = "#9CA3AF",
                                          show_no_year_default = FALSE) {
  
  bnd_wgs <- tryCatch(get_boundary_wgs(), error = function(e) NULL)
  
  # polygon_mode = TRUE is critical for MULTI(POLYGON)
  off <- read_official_layer(layer_pedztl, polygon_mode = TRUE)
  
  if (!("area_class" %in% names(off))) stop("area_class missing in layer: ", layer_pedztl)
  if (!("year_ped_ready" %in% names(off))) stop("year_ped_ready missing in layer: ", layer_pedztl)
  
  # ensure area_cat exists (so the code never crashes)
  if (!("area_cat" %in% names(off))) off$area_cat <- NA_character_
  
  off$year_ped_ready <- suppressWarnings(as.integer(off$year_ped_ready))
  
  off_no_year <- off |> dplyr::filter(is.na(year_ped_ready))
  off_year    <- off |> dplyr::filter(!is.na(year_ped_ready), year_ped_ready <= as.integer(year_exec))
  
  if (nrow(off_no_year)) {
    off_no_year <- off_no_year |> clean_sf(polygon_mode = TRUE) |> to_wgs84()
    stop_if_not_lonlat(off_no_year, "Official ped+ZTL (no year)")
  }
  if (nrow(off_year)) {
    off_year <- off_year |> clean_sf(polygon_mode = TRUE) |> to_wgs84()
    stop_if_not_lonlat(off_year, "Official ped+ZTL (with year)")
  }
  
  m <- leaf_base(bnd_wgs)
  
  if ((nrow(off_year) == 0) && (nrow(off_no_year) == 0)) {
    og <- overlay_groups(if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "")
    m <- m |>
      addLayersControl(baseGroups = "Positron", overlayGroups = og,
                       options = layersControlOptions(collapsed = TRUE))
    return(fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs)))
  }
  
  norm_class <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("[[:space:]]+", "_", x)
    x <- gsub("-+", "_", x)
    x
  }
  
  fill_for <- function(cls) {
    if (cls == "ped_area") return(ped_fill)
    if (cls == "ztl") return(ztl_fill)
    other_fill
  }
  
  nice <- function(cls) {
    if (cls == "ped_area") return("Pedestrian areas")
    if (cls == "ztl") return("ZTL")
    paste0("Other (", cls, ")")
  }
  
  groups_added <- character()
  
  add_block <- function(map, df, group_prefix, label_prefix, include_year = TRUE) {
    if (is.null(df) || !nrow(df)) return(map)
    
    df$area_class <- norm_class(df$area_class)
    
    classes <- sort(unique(df$area_class))
    classes <- classes[!is.na(classes) & nzchar(classes)]
    
    for (cls in classes) {
      
      cats <- sort(unique(df$area_cat[df$area_class == cls]))
      cats <- cats[!is.na(cats) & nzchar(cats)]
      if (!length(cats)) cats <- NA_character_
      
      for (acat in cats) {
        
        if (is.na(acat)) {
          dfc <- df[df$area_class == cls & (is.na(df$area_cat) | !nzchar(df$area_cat)), , drop = FALSE]
          cat_label <- "(missing)"
        } else {
          dfc <- df[df$area_class == cls & df$area_cat == acat, , drop = FALSE]
          cat_label <- acat
        }
        
        if (!nrow(dfc)) next
        
        grp <- paste0(group_prefix, nice(cls), " | area_cat: ", cat_label)
        groups_added <<- c(groups_added, grp)
        
        dfc <- add_gsv_popup(dfc, label = paste0(label_prefix, nice(cls)))
        
        # popup includes BOTH fields + year when relevant
        dfc$popup_html <- paste0(
          "<b>", label_prefix, nice(cls), "</b>",
          "<br><b>area_class:</b> ", dfc$area_class,
          "<br><b>area_cat:</b> ", ifelse(is.na(dfc$area_cat) | !nzchar(dfc$area_cat), "(missing)", dfc$area_cat),
          if (isTRUE(include_year)) paste0("<br><b>Ready year:</b> ", dfc$year_ped_ready) else "",
          "<br><a href='", dfc$gsv_url, "' target='_blank' rel='noopener'>Open in GSV</a>"
        )
        
        map <- map |>
          addPolygons(
            data = dfc,
            group = grp,
            weight = 1,
            color = "#111827",
            fillColor = fill_for(cls),
            fillOpacity = fill_opacity,
            opacity = border_opacity,
            popup = ~popup_html
          )
      }
    }
    
    map
  }
  
  if (nrow(off_year)) {
    m <- add_block(
      m,
      off_year,
      group_prefix = paste0("Official ", year_label, " (with year): "),
      label_prefix = paste0("Official ", year_label, " - "),
      include_year = TRUE
    )
  }
  
  if (nrow(off_no_year)) {
    m <- add_block(
      m,
      off_no_year,
      group_prefix = "Official (no year): ",
      label_prefix = "Official (no year) - ",
      include_year = FALSE
    )
  }
  
  og <- overlay_groups(
    if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "",
    groups_added
  )
  
  m <- m |>
    addLayersControl(baseGroups = "Positron", overlayGroups = og,
                     options = layersControlOptions(collapsed = TRUE))
  
  if (!show_no_year_default) {
    nog <- grep("^Official \\(no year\\):", og, value = TRUE)
    if (length(nog)) for (g in nog) m <- m |> hideGroup(g)
  }
  
  x_fit <- dplyr::bind_rows(
    if (nrow(off_year)) off_year else NULL,
    if (nrow(off_no_year)) off_no_year else NULL
  )
  
  fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs, x_wgs = x_fit))
}


make_osm_ped_diff_map <- function(year_bl = ver_bl, year_fu = ver_fu,
                                  added_col = "#0072B2", removed_col = "#D95F02") {
  bnd_wgs <- tryCatch(get_boundary_wgs(), error = function(e) NULL)
  
  f_add <- pick_file(paste0("(^|.*_)osm_pedpr_added_", year_bl, "_", year_fu, "\\.gpkg$"),
                     what = "OSM ped-priority added gpkg")
  f_rem <- pick_file(paste0("(^|.*_)osm_pedpr_removed_", year_bl, "_", year_fu, "\\.gpkg$"),
                     what = "OSM ped-priority removed gpkg")
  
  add <- sf::st_read(f_add, layer = "added", quiet = TRUE) |> clean_sf(polygon_mode = FALSE) |> to_wgs84()
  rem <- sf::st_read(f_rem, layer = "removed", quiet = TRUE) |> clean_sf(polygon_mode = FALSE) |> to_wgs84()
  
  if (!is.null(add) && nrow(add)) stop_if_not_lonlat(add, "OSM ped added")
  if (!is.null(rem) && nrow(rem)) stop_if_not_lonlat(rem, "OSM ped removed")
  
  add <- add_gsv_popup(add, label = paste0("OSM ped-priority added ", year_bl, " to ", year_fu))
  rem <- add_gsv_popup(rem, label = paste0("OSM ped-priority removed ", year_bl, " to ", year_fu))
  
  m <- leaf_base(bnd_wgs)
  
  if (!is.null(add) && nrow(add)) {
    m <- m |>
      addPolylines(data = add, group = "OSM: Added",
                   color = added_col, weight = 2, opacity = 0.9,
                   popup = ~popup_html)
  }
  if (!is.null(rem) && nrow(rem)) {
    m <- m |>
      addPolylines(data = rem, group = "OSM: Removed",
                   color = removed_col, weight = 2, opacity = 0.9,
                   popup = ~popup_html)
  }
  
  og <- overlay_groups(
    if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "",
    if (!is.null(add) && nrow(add)) "OSM: Added" else "",
    if (!is.null(rem) && nrow(rem)) "OSM: Removed" else ""
  )
  
  m <- m |>
    addLayersControl(baseGroups = "Positron", overlayGroups = og,
                     options = layersControlOptions(collapsed = TRUE))
  
  x_all <- dplyr::bind_rows(add, rem)
  fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs, x_wgs = x_all))
}

make_official_pedztl_diff_map <- function(year_bl = 2016, year_fu = 2022,
                                            fill_opacity = 0.35, border_opacity = 0.65,
                                            layer_pedztl = "official_ped_ztl_clean",
                                            ped_fill = "#1B9E77",
                                            ztl_fill = "#7570B3",
                                            other_fill = "#9CA3AF",
                                            added_fill = "#0072B2",
                                            show_no_year_default = FALSE) {
  
  bnd_wgs <- tryCatch(get_boundary_wgs(), error = function(e) NULL)
  
  off <- read_official_layer(layer_pedztl, polygon_mode = TRUE)
  
  if (!("area_class" %in% names(off))) stop("area_class missing in layer: ", layer_pedztl)
  if (!("year_ped_ready" %in% names(off))) stop("year_ped_ready missing in layer: ", layer_pedztl)
  
  # ensure area_cat exists
  if (!("area_cat" %in% names(off))) off$area_cat <- NA_character_
  
  off$year_ped_ready <- suppressWarnings(as.integer(off$year_ped_ready))
  
  off_no_year <- off |> dplyr::filter(is.na(year_ped_ready))
  off_added   <- off |> dplyr::filter(!is.na(year_ped_ready),
                                      year_ped_ready > as.integer(year_bl),
                                      year_ped_ready <= as.integer(year_fu))
  
  if (nrow(off_no_year)) {
    off_no_year <- off_no_year |> clean_sf(polygon_mode = TRUE) |> to_wgs84()
    stop_if_not_lonlat(off_no_year, "Official ped+ZTL change (no year)")
  }
  if (nrow(off_added)) {
    off_added <- off_added |> clean_sf(polygon_mode = TRUE) |> to_wgs84()
    stop_if_not_lonlat(off_added, "Official ped+ZTL change (added)")
  }
  
  m <- leaf_base(bnd_wgs)
  
  if ((nrow(off_added) == 0) && (nrow(off_no_year) == 0)) {
    og <- overlay_groups(if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "")
    m <- m |>
      addLayersControl(baseGroups = "Positron", overlayGroups = og,
                       options = layersControlOptions(collapsed = TRUE))
    return(fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs)))
  }
  
  norm_class <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("[[:space:]]+", "_", x)
    x <- gsub("-+", "_", x)
    x
  }
  
  fill_for_base <- function(cls) {
    if (cls == "ped_area") return(ped_fill)
    if (cls == "ztl") return(ztl_fill)
    other_fill
  }
  
  nice <- function(cls) {
    if (cls == "ped_area") return("Pedestrian areas")
    if (cls == "ztl") return("ZTL")
    paste0("Other (", cls, ")")
  }
  
  groups_added <- character()
  
  # ADDED block (blue)
  if (nrow(off_added)) {
    off_added$area_class <- norm_class(off_added$area_class)
    
    classes <- sort(unique(off_added$area_class))
    classes <- classes[!is.na(classes) & nzchar(classes)]
    
    for (cls in classes) {
      
      cats <- sort(unique(off_added$area_cat[off_added$area_class == cls]))
      cats <- cats[!is.na(cats) & nzchar(cats)]
      if (!length(cats)) cats <- NA_character_
      
      for (acat in cats) {
        
        if (is.na(acat)) {
          dfc <- off_added[off_added$area_class == cls & (is.na(off_added$area_cat) | !nzchar(off_added$area_cat)), , drop = FALSE]
          cat_label <- "(missing)"
        } else {
          dfc <- off_added[off_added$area_class == cls & off_added$area_cat == acat, , drop = FALSE]
          cat_label <- acat
        }
        
        if (!nrow(dfc)) next
        
        grp <- paste0(
          "Official: Added ", as.integer(year_bl) + 1, " to ", year_fu,
          " - ", nice(cls), " | area_cat: ", cat_label
        )
        groups_added <- c(groups_added, grp)
        
        dfc <- add_gsv_popup(dfc, label = paste0("Official added - ", nice(cls)))
        
        dfc$popup_html <- paste0(
          "<b>Official added - ", nice(cls), "</b>",
          "<br><b>area_class:</b> ", dfc$area_class,
          "<br><b>area_cat:</b> ", ifelse(is.na(dfc$area_cat) | !nzchar(dfc$area_cat), "(missing)", dfc$area_cat),
          "<br><b>Ready year:</b> ", dfc$year_ped_ready,
          "<br><a href='", dfc$gsv_url, "' target='_blank' rel='noopener'>Open in GSV</a>"
        )
        
        m <- m |>
          addPolygons(
            data = dfc,
            group = grp,
            weight = 1,
            color = "#111827",
            fillColor = added_fill,
            fillOpacity = fill_opacity,
            opacity = border_opacity,
            popup = ~popup_html
          )
      }
    }
  }
  
  # NO YEAR block (base colours)
  if (nrow(off_no_year)) {
    off_no_year$area_class <- norm_class(off_no_year$area_class)
    
    classes <- sort(unique(off_no_year$area_class))
    classes <- classes[!is.na(classes) & nzchar(classes)]
    
    for (cls in classes) {
      
      cats <- sort(unique(off_no_year$area_cat[off_no_year$area_class == cls]))
      cats <- cats[!is.na(cats) & nzchar(cats)]
      if (!length(cats)) cats <- NA_character_
      
      for (acat in cats) {
        
        if (is.na(acat)) {
          dfc <- off_no_year[off_no_year$area_class == cls & (is.na(off_no_year$area_cat) | !nzchar(off_no_year$area_cat)), , drop = FALSE]
          cat_label <- "(missing)"
        } else {
          dfc <- off_no_year[off_no_year$area_class == cls & off_no_year$area_cat == acat, , drop = FALSE]
          cat_label <- acat
        }
        
        if (!nrow(dfc)) next
        
        grp <- paste0("Official (no year): ", nice(cls), " | area_cat: ", cat_label)
        groups_added <- c(groups_added, grp)
        
        dfc <- add_gsv_popup(dfc, label = paste0("Official (no year) - ", nice(cls)))
        
        dfc$popup_html <- paste0(
          "<b>Official (no year) - ", nice(cls), "</b>",
          "<br><b>area_class:</b> ", dfc$area_class,
          "<br><b>area_cat:</b> ", ifelse(is.na(dfc$area_cat) | !nzchar(dfc$area_cat), "(missing)", dfc$area_cat),
          "<br><a href='", dfc$gsv_url, "' target='_blank' rel='noopener'>Open in GSV</a>"
        )
        
        m <- m |>
          addPolygons(
            data = dfc,
            group = grp,
            weight = 1,
            color = "#111827",
            fillColor = fill_for_base(cls),
            fillOpacity = fill_opacity,
            opacity = border_opacity,
            popup = ~popup_html
          )
      }
    }
  }
  
  og <- overlay_groups(
    if (!is.null(bnd_wgs) && nrow(bnd_wgs)) "Boundary" else "",
    groups_added
  )
  
  m <- m |>
    addLayersControl(baseGroups = "Positron", overlayGroups = og,
                     options = layersControlOptions(collapsed = TRUE))
  
  if (!show_no_year_default) {
    nog <- grep("^Official \\(no year\\):", og, value = TRUE)
    if (length(nog)) for (g in nog) m <- m |> hideGroup(g)
  }
  
  x_fit <- dplyr::bind_rows(
    if (nrow(off_added)) off_added else NULL,
    if (nrow(off_no_year)) off_no_year else NULL
  )
  
  fix_leaflet_widget(fit_to(m, bnd_wgs = bnd_wgs, x_wgs = x_fit))
}





# ================================================================
# 6-map layouts
# ================================================================
wrap_grid_2col <- function(titles, widgets) {
  stopifnot(length(titles) == length(widgets))
  leaflet_dep <- htmlwidgets::getDependency("leaflet")
  
  cells <- Map(
    f = function(ttl, w) tags$div(
      style = "display:grid; grid-template-rows:auto 1fr; min-height: 0;",
      tags$h4(ttl, style = "margin:0 0 6px 0;"),
      htmltools::tagList(w)
    ),
    ttl = titles,
    w = widgets
  )
  
  container <- tags$div(
    style = "display:grid; grid-template-columns: 1fr 1fr; gap: 12px; align-items: stretch;",
    cells
  )
  
  htmltools::browsable(htmltools::attachDependencies(container, leaflet_dep, append = TRUE))
}

make_six_maps_cycling <- function() {
  m_osm_bl <- make_osm_cycling_snapshot_map(year_label = ver_bl, version_code = ver_code_bl)
  m_off_bl <- make_official_cycling_year_map(year_label = ver_bl, year_exec = as.integer(ver_bl))
  
  m_osm_fu <- make_osm_cycling_snapshot_map(year_label = ver_fu, version_code = ver_code_fu)
  m_off_fu <- make_official_cycling_year_map(year_label = ver_fu, year_exec = as.integer(ver_fu))
  
  m_osm_df <- make_osm_cycling_diff_map(year_bl = ver_bl, year_fu = ver_fu)
  m_off_df <- make_official_cycling_diff_map(year_bl = ver_bl, year_fu = ver_fu)
  
  wrap_grid_2col(
    titles = c(
      paste0(ver_bl, " - OSM (cycling)"),
      paste0(ver_bl, " - Official (cycling)"),
      paste0(ver_fu, " - OSM (cycling)"),
      paste0(ver_fu, " - Official (cycling)"),
      paste0("Change ", ver_bl, " to ", ver_fu, " - OSM (cycling)"),
      paste0("Change ", ver_bl, " to ", ver_fu, " - Official (cycling)")
    ),
    widgets = list(m_osm_bl, m_off_bl, m_osm_fu, m_off_fu, m_osm_df, m_off_df)
  )
}

make_six_maps_ped_priority <- function() {
  m_osm_bl <- make_osm_ped_snapshot_map(year_label = ver_bl, version_code = ver_code_bl)
  m_off_bl <- make_official_pedztl_year_map(year_label = ver_bl, year_exec = as.integer(ver_bl))
  
  m_osm_fu <- make_osm_ped_snapshot_map(year_label = ver_fu, version_code = ver_code_fu)
  m_off_fu <- make_official_pedztl_year_map(year_label = ver_fu, year_exec = as.integer(ver_fu))
  
  m_osm_df <- make_osm_ped_diff_map(year_bl = ver_bl, year_fu = ver_fu)
  m_off_df <- make_official_pedztl_diff_map(year_bl = ver_bl, year_fu = ver_fu)
  
  wrap_grid_2col(
    titles = c(
      paste0(ver_bl, " - OSM (ped-priority)"),
      paste0(ver_bl, " - Official (ped areas + ZTL)"),
      paste0(ver_fu, " - OSM (ped-priority)"),
      paste0(ver_fu, " - Official (ped areas + ZTL)"),
      paste0("Change ", ver_bl, " to ", ver_fu, " - OSM (ped-priority)"),
      paste0("Change ", ver_bl, " to ", ver_fu, " - Official (ped areas + ZTL)")
    ),
    widgets = list(m_osm_bl, m_off_bl, m_osm_fu, m_off_fu, m_osm_df, m_off_df)
  )
}



