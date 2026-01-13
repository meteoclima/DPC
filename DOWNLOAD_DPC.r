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
# Utility
# ----------------------------
get_last_product_info <- function(product_type) {
  url <- paste0(
    "https://radar-api.protezionecivile.it/findLastProductByType?type=",
    product_type
  )
  res <- GET(url)
  stopifnot(status_code(res) == 200)
  fromJSON(content(res, "text", encoding = "UTF-8"))
}


iso_period_to_seconds <- function(iso) {
  # Converti stringa ISO-8601 tipo "PT10M" in Period
  p <- lubridate::as.period(lubridate::iso8601(paste0("P0DT", iso))) 
  # Restituisci secondi
  lubridate::period_to_seconds(p)
}

download_product <- function(product_type, timestamp_ms, output_path) {

  body <- list(
    productType = product_type,
    productDate = timestamp_ms
  )

  res <- POST(
    "https://radar-api.protezionecivile.it/wide/product/downloadProduct",
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

set_nodata <- function(input_path, output_path, nodata_value) {
  r <- rast(input_path)
  NAflag(r) <- nodata_value
  writeRaster(r, output_path, overwrite = TRUE, NAflag = nodata_value)
}

# ----------------------------
# PARAMETRI
# ----------------------------
product_types <- c("SRT1", "TEMP")
days_back <- 3   # quanti giorni indietro scaricare

# ----------------------------
# DOWNLOAD LOOP
# ----------------------------
for (product_type in product_types) {

  message("➡️ Prodotto ", product_type)

  info <- get_last_product_info(product_type)

  last_time <- as.POSIXct(info$timestamp / 1000,
                          origin = "1970-01-01",
                          tz = "UTC")

  step_sec <- iso_period_to_seconds(info$period)

  start_time <- last_time - days(days_back)
  times <- seq(from = start_time, to = last_time, by = step_sec)

  for (t in times) {

    timestamp_ms <- as.numeric(t) * 1000
    fname <- paste0(product_type, "_", format(t, "%Y%m%d_%H%M"), ".tif")
    final_file <- file.path(output_dir, fname)

    if (file.exists(final_file)) next

    tmp <- tempfile(fileext = ".tif")

    ok <- tryCatch(
      download_product(product_type, timestamp_ms, tmp),
      error = function(e) FALSE
    )

    if (ok) {
      nodata <- ifelse(product_type == "TEMP", -99999, -9999)
      set_nodata(tmp, final_file, nodata)
      unlink(tmp)
      message("   ✅ ", fname)
    }
  }
}

