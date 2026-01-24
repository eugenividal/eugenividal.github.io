# ================================================================
# 00_setup.R (minimal)
# Core settings and paths for Milan pipeline
# ================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(osmdata)
  library(osmextract)
  library(leaflet)
  library(stringr)
  library(htmltools)# used by 03_official_data.R
})

sf::sf_use_s2(FALSE)
options(sf_max_proj_search = 100)
Sys.setenv(OGR_ENABLE_PARTITION = "TRUE")

# ----------------------------
# City + sources
# ----------------------------
city_tag            <- "milan"
city_name           <- "Milan"
city_boundary_place <- "Milan, Italy"  # used to fetch admin boundary polygon
infra_region        <- "Italy"         # used by osmextract::oe_get
city_wikidata_id <- "Q490"

# ----------------------------
# Time points
# ----------------------------
ver_bl      <- "2016"
ver_fu      <- "2022"
ver_code_bl <- "160101"
ver_code_fu <- "220101"
VERSIONS    <- c(ver_code_bl, ver_code_fu)

# ----------------------------
# CRS
# ----------------------------
crs_work <- 32632
crs_wgs  <- 4326

# ----------------------------
# Flags
# ----------------------------
FORCE_PERIM <- FALSE
FORCE_BUILD <- FALSE

# ----------------------------
# Paths
# ----------------------------
proc_dir <- file.path("data", "processed")
dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
