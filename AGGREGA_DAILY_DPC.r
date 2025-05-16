library(terra)

input_dir <- "./DPC"
output_dir <- file.path(input_dir, "aggregati")
if (!dir.exists(output_dir)) dir.create(output_dir)

extract_date <- function(fname) {
  matches <- regmatches(fname, regexpr("\\d{8}", fname))
  if (length(matches) == 0) return(NA)
  return(matches[1])
}

set_input_nodata <- function(filepath) {
  r <- rast(filepath)
  fname <- basename(filepath)
  
  if (grepl("^TEMP_", fname)) {
    NAflag(r) <- -99999
    tmpfile <- tempfile(fileext = ".tif")
    writeRaster(r, tmpfile, NAflag = -99999, overwrite = TRUE)
  } else if (grepl("^SRT1_", fname)) {
    NAflag(r) <- -9999
    tmpfile <- tempfile(fileext = ".tif")
    writeRaster(r, tmpfile, NAflag = -9999, overwrite = TRUE)
  } else {
    return(NULL)
  }
  
  file.copy(tmpfile, filepath, overwrite = TRUE)
  unlink(tmpfile)
}

all_rasters <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
invisible(lapply(all_rasters, set_input_nodata))

temp_files <- grep("TEMP_\\d{8}_\\d{4}\\.tif$", all_rasters, value = TRUE)
srt_files  <- grep("SRT1_\\d{8}_\\d{4}\\.tif$", all_rasters, value = TRUE)

group_by_date <- function(file_list) {
  split(file_list, sapply(basename(file_list), extract_date))
}

temp_by_day <- group_by_date(temp_files)
srt_by_day  <- group_by_date(srt_files)

process_temp_day <- function(file_list, day) {
  rasters <- rast(file_list)
  min_r <- app(rasters, fun = min, na.rm = TRUE)
  max_r <- app(rasters, fun = max, na.rm = TRUE)
  writeRaster(min_r, file.path(output_dir, paste0("TEMP_MIN_", day, ".tif")),
              overwrite = TRUE, NAflag = -99999)
  writeRaster(max_r, file.path(output_dir, paste0("TEMP_MAX_", day, ".tif")),
              overwrite = TRUE, NAflag = -99999)
}

process_srt_day <- function(file_list, day) {
  rasters <- rast(file_list)
  sum_r <- app(rasters, fun = function(...) {
    vals <- c(...)
    if (all(is.na(vals))) return(NA)
    else return(sum(vals, na.rm = TRUE))
  })
  writeRaster(sum_r, file.path(output_dir, paste0("SRT1_SUM_", day, ".tif")),
              overwrite = TRUE, NAflag = -9999)
}

for (day in names(temp_by_day)) {
  message("Elaboro TEMP per il giorno ", day)
  process_temp_day(temp_by_day[[day]], day)
}

for (day in names(srt_by_day)) {
  message("Elaboro SRT1 per il giorno ", day)
  process_srt_day(srt_by_day[[day]], day)
}
