# ================================================================
# 02_official_process_simple.R
# Clean and standardise official Milan datasets:
#   A) Cycling segments (lines)
#   B) Pedestrian areas + ZTL (polygons)
#
# Output:
#   proc_dir/<city_tag>_official_clean.gpkg
#     - official_cycling_clean
#     - official_ped_ztl_clean
#
# Cycling derived fields:
#   - year_exec, len_m
#
# Ped/ZTL derived fields:
#   - type_raw       (from 'tipo')
#   - area_cat       (REAL category: cleaned 'tipo')
#   - area_class     (ped_area | ztl | other, derived from tipo codes)
#   - year_ped_ready (STRICT: from val_inizio, then delibera; NA stays NA)
#   - area_m2
# ================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
})

# ------------------------------------------------
# Paths
# ------------------------------------------------
proj_dir <- getwd()
raw_dir  <- file.path(proj_dir, "data", "official")
proc_dir <- file.path(proj_dir, "data", "processed")
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

city_tag <- if (exists("city_tag")) city_tag else "milan"
crs_work <- if (exists("crs_work")) crs_work else 32632

out_gpkg <- file.path(proc_dir, paste0(city_tag, "_official_clean.gpkg"))

stop_if_missing <- function(path, label = "file") {
  if (!file.exists(path)) stop("Cannot find ", label, ": ", path)
}

# ------------------------------------------------
# Geometry cleaning helpers
# ------------------------------------------------
clean_lines <- function(x) {
  x <- st_make_valid(x)
  x <- x[!st_is_empty(st_geometry(x)), , drop = FALSE]
  x <- st_zm(x, drop = TRUE, what = "ZM")
  x <- st_cast(x, "MULTILINESTRING", warn = FALSE)
  x
}

clean_polygons <- function(x) {
  x <- st_make_valid(x)
  x <- x[!st_is_empty(st_geometry(x)), , drop = FALSE]
  x <- st_zm(x, drop = TRUE, what = "ZM")
  x <- suppressWarnings(st_collection_extract(x, "POLYGON"))
  x <- x[!st_is_empty(st_geometry(x)), , drop = FALSE]
  x <- st_cast(x, "MULTIPOLYGON", warn = FALSE)
  x
}

# ------------------------------------------------
# Year extraction (robust to timestamps and common date formats)
# ------------------------------------------------
extract_year_smart <- function(x) {
  s <- trimws(as.character(x))
  s[is.na(s)] <- ""
  
  # Prefer explicit 4-digit year anywhere (works for "1998-03-09 00:00:00+01")
  y4 <- str_extract(s, "(19|20)\\d{2}")
  out <- suppressWarnings(as.integer(y4))
  
  # Parse dd/mm/yyyy etc if still NA
  need <- is.na(out) & nzchar(s)
  if (any(need)) {
    ss <- s[need]
    fmts <- c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d")
    parsed <- rep(as.Date(NA), length(ss))
    for (fmt in fmts) {
      idx <- is.na(parsed)
      if (!any(idx)) break
      parsed[idx] <- suppressWarnings(as.Date(ss[idx], format = fmt))
    }
    out[need] <- suppressWarnings(as.integer(format(parsed, "%Y")))
  }
  
  # Parse dd/mm/yy if still NA
  need2 <- is.na(out) & nzchar(s)
  if (any(need2)) {
    ss <- s[need2]
    parsed <- rep(as.Date(NA), length(ss))
    fmts2 <- c("%d/%m/%y", "%d-%m-%y")
    for (fmt in fmts2) {
      idx <- is.na(parsed)
      if (!any(idx)) break
      parsed[idx] <- suppressWarnings(as.Date(ss[idx], format = fmt))
    }
    out[need2] <- suppressWarnings(as.integer(format(parsed, "%Y")))
  }
  
  out
}

# ================================================================
# A) OFFICIAL CYCLING (LINES)
# ================================================================
cycling_file <- file.path(raw_dir, "bike_ciclabili_shp", "bike_ciclabili.shp")
stop_if_missing(cycling_file, "cycling file")

year_col_cyc <- "fine_lavor"
typ_col_cyc  <- "tipologia"

off_cyc <- st_read(cycling_file, quiet = TRUE)
if (is.na(st_crs(off_cyc))) stop("Cycling data has no CRS (missing .prj).")

if (!(year_col_cyc %in% names(off_cyc))) stop("Missing cycling year field: ", year_col_cyc)
if (!(typ_col_cyc  %in% names(off_cyc))) stop("Missing cycling type field: ", typ_col_cyc)

off_cyc <- off_cyc |>
  st_transform(crs_work) |>
  clean_lines() |>
  mutate(
    year_exec = extract_year_smart(.data[[year_col_cyc]]),
    year_exec = ifelse(.data[[year_col_cyc]] %in% c("0", 0, NA, "NA", ""), NA_integer_, year_exec),
    len_m     = as.numeric(st_length(geometry))
  )

# ================================================================
# B) OFFICIAL PEDESTRIAN AREAS + ZTL (POLYGONS)
# ================================================================
pedztl_file <- file.path(raw_dir, "disciplina_aree_shp", "disciplina_aree.shp")
stop_if_missing(pedztl_file, "ped/ZTL file")

off_ped <- st_read(pedztl_file, quiet = TRUE)
if (is.na(st_crs(off_ped))) stop("Ped/ZTL data has no CRS (missing .prj).")

off_ped <- off_ped |>
  st_transform(crs_work) |>
  clean_polygons()

# Required fields
if (!("tipo" %in% names(off_ped))) {
  stop("Missing required field in ped/ZTL data: tipo\nAvailable fields:\n- ",
       paste(names(off_ped), collapse = ", "))
}
if (!("val_inizio" %in% names(off_ped))) {
  stop("Missing required field in ped/ZTL data: val_inizio\nAvailable fields:\n- ",
       paste(names(off_ped), collapse = ", "))
}

# Clean tipo and build categories
off_ped <- off_ped |>
  mutate(
    type_raw = toupper(trimws(as.character(.data[["tipo"]]))),
    
    # REAL category from the dataset
    area_cat = type_raw,
    
    # Simplified class for mapping
    area_class = case_when(
      type_raw == "AP" ~ "ped_area",
      type_raw %in% c("ZTL", "AREA_B", "AREA_C") ~ "ztl",
      type_raw == "CR" ~ "other",
      TRUE ~ "other"
    )
  )

# Strict year_ped_ready: val_inizio then delibera, else NA (for ALL types)
y_from_val_inizio <- extract_year_smart(off_ped[["val_inizio"]])

y_from_delibera <- if ("delibera" %in% names(off_ped)) {
  extract_year_smart(off_ped[["delibera"]])
} else {
  rep(NA_integer_, nrow(off_ped))
}

off_ped <- off_ped |>
  mutate(
    year_ped_ready = dplyr::coalesce(y_from_val_inizio, y_from_delibera),
    area_m2        = as.numeric(st_area(geometry))
  )

# ------------------------------------------------
# Sanity prints
# ------------------------------------------------
message("\nPed/ZTL: top values of area_cat (tipo codes):")
print(utils::head(sort(table(off_ped$area_cat), decreasing = TRUE), 25))

message("\nPed/ZTL: area_class counts (derived from tipo codes):")
print(table(off_ped$area_class, useNA = "ifany"))

message("\nPed/ZTL: year_ped_ready summary (all types):")
print(summary(off_ped$year_ped_ready))

message("\nPed/ZTL: year_ped_ready by area_class:")
print(tapply(off_ped$year_ped_ready, off_ped$area_class, summary))

message("\nExamples val_inizio (first 20):")
print(utils::head(off_ped$val_inizio, 20))

if ("delibera" %in% names(off_ped)) {
  message("\nExamples delibera (first 20):")
  print(utils::head(off_ped$delibera, 20))
}

# ================================================================
# WRITE (ONE GPKG, TWO LAYERS)
# ================================================================
if (file.exists(out_gpkg)) file.remove(out_gpkg)

st_write(off_cyc, out_gpkg, layer = "official_cycling_clean", quiet = TRUE)
st_write(off_ped, out_gpkg, layer = "official_ped_ztl_clean", quiet = TRUE)

message("\nWrote: ", out_gpkg)
message(" - layer: official_cycling_clean (n = ", nrow(off_cyc), ")")
message(" - layer: official_ped_ztl_clean (n = ", nrow(off_ped), ")")

# ------------------------------------------------------------------
# Helper for cumulative baseline / follow-up / added (by year field)
# ------------------------------------------------------------------
build_official_bl_fu_changes <- function(off_sf,
                                         year_bl = 2016,
                                         year_fu = 2022,
                                         year_field = "year_exec") {
  
  stopifnot(inherits(off_sf, "sf"))
  stopifnot(year_field %in% names(off_sf))
  
  off_sf <- off_sf |>
    filter(!is.na(.data[[year_field]]))
  
  off_bl    <- off_sf |> filter(.data[[year_field]] <= year_bl)
  off_fu    <- off_sf |> filter(.data[[year_field]] <= year_fu)
  off_added <- off_sf |> filter(.data[[year_field]] > year_bl, .data[[year_field]] <= year_fu)
  
  list(bl = off_bl, fu = off_fu, added = off_added)
}
