library(terra)
library(lubridate)

# ----------------------------
# Directory
# ----------------------------
input_dir  <- "./DPC"
output_dir <- file.path(input_dir, "aggregati")
dir.create(output_dir, showWarnings = FALSE)

# ----------------------------
# Utility
# ----------------------------
parse_datetime <- function(fname) {
  x <- regmatches(fname, regexpr("\\d{8}_\\d{4}", fname))
  as.POSIXct(x, format = "%Y%m%d_%H%M", tz = "UTC")
}

group_by_day <- function(files) {
  dates <- as.Date(sapply(basename(files), parse_datetime))
  split(files, dates)
}

# ----------------------------
# File list
# ----------------------------
all_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

temp_files <- grep("^TEMP_", basename(all_files), value = TRUE)
srt_files  <- grep("^SRT1_", basename(all_files), value = TRUE)

temp_files <- file.path(input_dir, temp_files)
srt_files  <- file.path(input_dir, srt_files)

# ----------------------------
# TEMPERATURA — MIN / MAX
# ----------------------------
temp_by_day <- group_by_day(temp_files)

for (day in names(temp_by_day)) {

  message("🌡️ TEMP ", day)

  r <- rast(temp_by_day[[day]])
  NAflag(r) <- -99999

  rmin <- app(r, min, na.rm = TRUE)
  rmax <- app(r, max, na.rm = TRUE)

  writeRaster(
    rmin,
    file.path(output_dir, paste0("TEMP_MIN_", day, ".tif")),
    overwrite = TRUE,
    NAflag = -99999
  )

  writeRaster(
    rmax,
    file.path(output_dir, paste0("TEMP_MAX_", day, ".tif")),
    overwrite = TRUE,
    NAflag = -99999
  )
}

# ----------------------------
# PIOGGIA — CUMULATA GIORNALIERA
# ----------------------------
srt_by_day <- group_by_day(srt_files)

for (day in names(srt_by_day)) {

  message("🌧️ SRT1 ", day)

  r <- rast(srt_by_day[[day]])
  NAflag(r) <- -9999

  rs <- app(r, sum, na.rm = TRUE)

  writeRaster(
    rs,
    file.path(output_dir, paste0("SRT1_SUM_", day, ".tif")),
    overwrite = TRUE,
    NAflag = -9999
  )
}
