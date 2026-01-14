library(httr)
library(jsonlite)
library(terra)
library(lubridate)

# ----------------------------
# Directory di output
# ----------------------------
output_dir <- "./DPC"
dir.create(output_dir, showWarnings = FALSE)

# ----------------------------
# PARAMETRI
# ----------------------------
product_types <- c("SRT1", "TEMP")
days_back <- 2   # ultimi 3 giorni, portato a 2

# ----------------------------
# FUNZIONI UTILI
# ----------------------------

# Funzione per scaricare un prodotto dato timestamp
download_product <- function(product_type, timestamp_ms, output_path) {
  body <- list(
    productType = product_type,
    productDate = timestamp_ms
  )
  res <- POST(
    "https://radar-api.protezionecivile.it/downloadProduct",
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json",
    add_headers("Content-Type" = "application/json")
  )
  if (status_code(res) != 200) return(FALSE)
  info <- fromJSON(content(res, "text", encoding = "UTF-8"))
  if (is.null(info$url)) return(FALSE)
  r <- GET(info$url, write_disk(output_path, overwrite = TRUE))
  status_code(r) == 200
}

# Imposta valore nodata nel raster
set_nodata <- function(input_path, output_path, nodata_value) {
  r <- rast(input_path)
  NAflag(r) <- nodata_value
  writeRaster(r, output_path, overwrite = TRUE, NAflag = nodata_value)
}

# Funzione per arrotondare timestamp al passo corretto (period)
# ----------------------------
# CONVERSIONE ISO 8601 PERIOD IN SECONDI
# ----------------------------
iso_period_to_seconds <- function(period_iso) {
  if (is.null(period_iso) || period_iso == "") return(3600)  # default 1h
  x <- gsub("PT", "", period_iso)
  sec <- 0
  if (grepl("H", x)) { sec <- sec + as.numeric(sub("H.*", "", x)) * 3600; x <- sub(".*H", "", x) }
  if (grepl("M", x)) { sec <- sec + as.numeric(sub("M.*", "", x)) * 60; x <- sub(".*M", "", x) }
  if (grepl("S", x)) { sec <- sec + as.numeric(sub("S.*", "", x)) }
  if (is.na(sec) || sec <= 0) sec <- 3600  # fallback 1h
  sec
}

# ----------------------------
# ARROTONDA TIMESTAMP AL PERIODO
# ----------------------------
round_timestamp <- function(ts_ms, period_iso) {
  step_sec <- iso_period_to_seconds(period_iso)
  ts_sec <- floor(ts_ms / 1000 / step_sec) * step_sec
  ts_sec * 1000
}

# ----------------------------
# LOOP SU OGNI PRODOTTO
# ----------------------------
for (product_type in product_types) {
  cat("\n➡️ Prodotto:", product_type, "\n")
  
  # 1️⃣ Recupera ultimo prodotto
  url_info <- paste0("https://radar-api.protezionecivile.it/findLastProductByType?type=", product_type)
  res_info <- GET(url_info)
  if (status_code(res_info)!=200) { warning("⚠️ Errore API per ", product_type); next }
  
  info <- fromJSON(content(res_info, "text", encoding="UTF-8"))
  if (length(info$lastProducts)==0) { warning("⚠️ Nessun prodotto disponibile per ", product_type); next }
  
  last_ts <- info$lastProducts$time[1]
  period <- info$lastProducts$period[1]
  
  # 2️⃣ Calcola intervallo temporale ultimi 3 giorni, arrotondato al periodo
  last_ts_rounded <- round_timestamp(last_ts, period)
  start_ts <- as.numeric(as.POSIXct(Sys.time() - days(days_back), tz="UTC"))*1000
  start_ts_rounded <- round_timestamp(start_ts, period)
  
  # Sequenza di timestamp
  step_sec <- iso_period_to_seconds(period)
  
  times <- seq(from=start_ts_rounded/1000, to=last_ts_rounded/1000, by=step_sec)
  
  # 3️⃣ Scarica ogni timestamp disponibile
  for (t in times) {
    timestamp_ms <- as.numeric(t)*1000
    t_posix <- as.POSIXct(timestamp_ms/1000, origin="1970-01-01", tz="UTC")
    fname <- paste0(product_type, "_", strftime(t_posix, "%Y%m%d_%H%M"), ".tif")
    final_file <- file.path(output_dir, fname)
    if (file.exists(final_file)) next
    
    tmp <- tempfile(fileext = ".tif")
    ok <- tryCatch(download_product(product_type, timestamp_ms, tmp), error=function(e) FALSE)
    
    if (ok) {
      nodata <- ifelse(product_type=="TEMP",-99999,-9999)
      set_nodata(tmp, final_file, nodata)
      unlink(tmp)
      cat("   ✅", fname, "\n")
    } else {
      cat("   ❌", fname, "non disponibile\n")
    }
  }
}

cat("\n DOWNLOAD COMPLETATO!\n")


