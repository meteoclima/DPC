library(httr)
library(jsonlite)
library(terra)
library(lubridate)

# Directory di output relativa
output_dir <- "./DPC"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Funzione per convertire datetime in timestamp UTC in ms
datetime_to_timestamp_ms <- function(datetime) {
  as.numeric(as.POSIXct(datetime, tz = "UTC")) * 1000
}

# Funzione per verificare esistenza prodotto
exists_product <- function(product_type, timestamp_ms) {
  url <- paste0("https://radar-api.protezionecivile.it/wide/product/existsProduct?type=", product_type, "&time=", timestamp_ms)
  response <- tryCatch({
    GET(url)
  }, error = function(e) {
    message("Errore nella richiesta per verificare il prodotto: ", e$message)
    return(NULL)
  })
  
  if (is.null(response) || status_code(response) != 200) {
    warning("Errore nella richiesta: ", status_code(response))
    return(FALSE)
  }
  content <- content(response, "text", encoding = "UTF-8")
  return(tolower(content) == "true")
}

# Funzione per scaricare prodotto
download_product <- function(product_type, timestamp_ms, output_path) {
  url <- "https://radar-api.protezionecivile.it/wide/product/downloadProduct"
  body <- list(productType = product_type, productDate = timestamp_ms)
  response <- tryCatch({
    POST(
      url,
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json",
      add_headers("Content-Type" = "application/json")
    )
  }, error = function(e) {
    message("Errore nella richiesta per scaricare il prodotto: ", e$message)
    return(NULL)
  })
  
  if (is.null(response) || status_code(response) != 200) {
    warning("❌ Errore nella richiesta: ", status_code(response))
    return(FALSE)
  }
  writeBin(content(response, "raw"), output_path)
  message(paste("✅ Prodotto", product_type, "salvato in:", output_path))
  return(TRUE)
}

# Funzione per impostare nodata
set_nodata_value <- function(input_path, output_path, nodata_value = -9999) {
  r <- tryCatch({
    rast(input_path)
  }, error = function(e) {
    message("Errore nel caricamento del raster: ", e$message)
    return(NULL)
  })
  
  if (is.null(r)) {
    message("❌ Errore nell'elaborazione del raster per", input_path)
    return(FALSE)
  }
  
  NAflag(r) <- nodata_value
  writeRaster(r, output_path, overwrite = TRUE, NAflag = nodata_value)
  return(TRUE)
}

# Tipi di prodotto da scaricare
product_types <- c("SRT1", "TEMP")

# Date di esempio: ultimi 7 giorni
start_date <- Sys.Date() - 2
end_date <- Sys.Date()

interval_hours <- 1

for (product_type in product_types) {
  current_datetime <- as.POSIXct(start_date, tz = "UTC")
  end_datetime <- as.POSIXct(end_date + 1, tz = "UTC")
  while (current_datetime < end_datetime) {
    timestamp_ms <- datetime_to_timestamp_ms(current_datetime)
    datetime_str <- format(current_datetime, "%Y%m%d_%H%M")
    final_file <- file.path(output_dir, paste0(product_type, "_", datetime_str, ".tif"))
    
    if (!file.exists(final_file)) {
      if (exists_product(product_type, timestamp_ms)) {
        temp_file <- tempfile(fileext = ".tif")
        success <- tryCatch({
          download_product(product_type, timestamp_ms, temp_file)
        }, error = function(e) {
          message("Errore durante il download del prodotto: ", e$message)
          return(FALSE)
        })
        
        if (success) {
          nodata_val <- ifelse(product_type == "TEMP", -99999, -9999)
          set_nodata_value(temp_file, final_file, nodata_value = nodata_val)
          unlink(temp_file)
        }
      } else {
        message(paste("❌ Prodotto", product_type, "non disponibile per:", datetime_str))
      }
    } else {
      message(paste("⚠️ File già esistente:", final_file))
    }
    
    current_datetime <- current_datetime + hours(interval_hours)
  }
}
