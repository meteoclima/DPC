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
# Raggruppa i file per giorno estraendo YYYYMMDD
group_by_day <- function(files) {
  day_str <- sub(".*_(\\d{8})_\\d{4}\\.tif$", "\\1", basename(files))
  if(any(nchar(day_str) != 8)) stop("Errore: alcuni nomi di file non seguono il pattern YYYYMMDD_HHMM.tif")
  split(files, day_str)
}

# Estrae la data e l'ora dal nome del file
extract_datetime <- function(fname) {
  dt <- regmatches(fname, regexpr("\\d{8}_\\d{4}", fname))
  as.POSIXct(dt, format = "%Y%m%d_%H%M", tz = "UTC")
}

# ----------------------------
# Lista file
# ----------------------------
all_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

temp_files <- grep("^TEMP_", basename(all_files), value = TRUE)
srt_files  <- grep("^SRT1_", basename(all_files), value = TRUE)

temp_files <- file.path(input_dir, temp_files)
srt_files  <- file.path(input_dir, srt_files)

# ----------------------------
# TEMPERATURA — MIN / MAX (1 file ogni ora)
# ----------------------------
temp_by_day <- group_by_day(temp_files)

for (day_str in names(temp_by_day)) {
  message("🌡️ TEMP ", day_str)
  
  r <- rast(temp_by_day[[day_str]])
  NAflag(r) <- -99999
  
  rmin <- app(r, min, na.rm = TRUE)
  rmax <- app(r, max, na.rm = TRUE)
  
  writeRaster(rmin, file.path(output_dir, paste0("TEMP_MIN_", day_str, ".tif")),
              overwrite = TRUE, NAflag = -99999)
  
  writeRaster(rmax, file.path(output_dir, paste0("TEMP_MAX_", day_str, ".tif")),
              overwrite = TRUE, NAflag = -99999)
}

# ----------------------------
# PIOGGIA — TOTALE REALE (1 file per ora)
# ----------------------------

srt_by_day <- group_by_day(srt_files)

for (day_str in names(srt_by_day)) {
  message("🌧️ SRT1 ", day_str)
  
  sfiles <- srt_by_day[[day_str]]
  
  # Estrai datetime dai nomi dei file
  datetimes <- vapply(
    sfiles,
    extract_datetime,
    FUN.VALUE = as.POSIXct(NA),
    USE.NAMES = FALSE
  )
  
  
  # Prendi 1 file per ora
  hours <- lubridate::hour(datetimes)
  
  selected_files <- sfiles[!duplicated(hours)]
  
  if(length(selected_files) == 0) {
    warning("Nessun file valido per il giorno ", day_str)
    next
  }
  
  # Caso singolo file
  if(length(selected_files) == 1) {
    rs <- rast(selected_files[1])
    NAflag(rs) <- -9999
  } else {
    # Caso più file: somma manuale
    rs <- rast(selected_files[1])
    NAflag(rs) <- -9999
    
    for(f in selected_files[-1]) {
      r <- rast(f)
      NAflag(r) <- -9999
      rs <- rs + r
    }
  }
  
  # Scrivi raster aggregato giornaliero
  writeRaster(
    rs,
    file.path(output_dir, paste0("SRT1_SUM_", day_str, ".tif")),
    overwrite = TRUE,
    NAflag = -9999
  )
}
