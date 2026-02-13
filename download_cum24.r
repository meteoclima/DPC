#!/usr/bin/env Rscript

library(terra)
library(lubridate)
library(httr)
library(jsonlite)
library(stringr)

# ----------------------------
# Directory per GitHub Actions
#---------------------------
# Usa direttamente output_dir senza input_dir
output_dir <- Sys.getenv("OUTPUT_DIR", unset = "./DPC/aggregati")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# API Endpoint Protezione Civile
# ----------------------------
BASE_URL <- "https://radar-api.protezionecivile.it"

# ----------------------------
# Funzioni API per CUM24
# ----------------------------

#' Trova TUTTI i timestamp disponibili per CUM24
find_available_cum24 <- function() {
  message("🔍 Cerco tutti i timestamp CUM24 disponibili...")
  
  # Partiamo dall'oggi e andiamo a ritroso giorno per giorno
  end_date <- Sys.Date()
  start_date <- end_date - days(as.numeric(Sys.getenv("MAX_DAYS_BACK", unset = "7")))
  
  dates_to_try <- seq(start_date, end_date, by = "day")
  valid_timestamps <- c()
  
  message("   Verifico disponibilità giorno per giorno...")
  
  for (i in seq_along(dates_to_try)) {
    current_date <- dates_to_try[i]
    
    # Timestamp a mezzogiorno UTC
    ts <- as.numeric(as.POSIXct(paste0(current_date, " 12:00:00"), tz = "UTC")) * 1000
    
    if (i %% 1 == 0) message("   Verificati ", i, "/", length(dates_to_try), " giorni")
    
    body <- list(
      productType = "CUM24",
      productDate = ts
    )
    
    res <- tryCatch({
      POST(
        url = paste0(BASE_URL, "/downloadProduct"),
        body = body,
        encode = "json",
        timeout(10)
      )
    }, error = function(e) NULL)
    
    if (!is.null(res) && status_code(res) == 200) {
      message("   ✅ Trovato CUM24 per: ", current_date)
      valid_timestamps <- c(valid_timestamps, ts)
    }
    
    Sys.sleep(0.2)
  }
  
  message("✅ Totale CUM24 trovati: ", length(valid_timestamps))
  return(valid_timestamps)
}

#' Scarica CUM24 con gestione errori migliorata
download_cum24_safe <- function(timestamp_ms, output_dir) {
  
  file_date <- format(as.POSIXct(timestamp_ms / 1000, origin = "1970-01-01", tz = "UTC"), "%Y%m%d")
  
  body <- list(
    productType = "CUM24",
    productDate = timestamp_ms
  )
  
  message("  ⬇️  Scarico CUM24 per ", file_date, "...")
  
  res <- tryCatch({
    POST(
      url = paste0(BASE_URL, "/downloadProduct"),
      body = body,
      encode = "json",
      timeout(30)
    )
  }, error = function(e) {
    message("    ❌ Errore di connessione: ", e$message)
    return(NULL)
  })
  
  if (is.null(res)) return(NULL)
  
  if (status_code(res) == 404) {
    message("    ⚠️  File non trovato per questa data")
    return(NULL)
  }
  
  if (status_code(res) == 500) {
    message("    ⚠️  Errore server 500 - possibile timestamp non valido")
    return(NULL)
  }
  
  if (status_code(res) != 200) {
    message("    ❌ Errore HTTP ", status_code(res))
    return(NULL)
  }
  
  data <- tryCatch({
    fromJSON(content(res, "text", encoding = "UTF-8"))
  }, error = function(e) {
    message("    ❌ Errore nel parsing JSON")
    return(NULL)
  })
  
  if (is.null(data)) return(NULL)
  
  if (is.null(data$url)) {
    message("    ❌ Nessuna URL nel response")
    return(NULL)
  }
  
  standardized_name <- paste0("CUM24_", file_date, ".tif")
  output_path <- file.path(output_dir, standardized_name)
  
  message("    📥 Download file...")
  file_res <- tryCatch({
    GET(data$url, write_disk(output_path, overwrite = TRUE), timeout(60))
  }, error = function(e) {
    message("    ❌ Errore download file: ", e$message)
    return(NULL)
  })
  
  if (is.null(file_res)) return(NULL)
  
  if (status_code(file_res) == 200) {
    message("    ✅ Salvato: ", standardized_name)
    return(output_path)
  } else {
    message("    ❌ Errore HTTP ", status_code(file_res), " nel download file")
    if (file.exists(output_path)) file.remove(output_path)
    return(NULL)
  }
}

#' Ottiene l'ultimo timestamp CUM24 (metodo alternativo)
get_last_cum24_timestamp_alt <- function() {
  message("🔍 Cerco ultimo CUM24 con metodo alternativo...")
  
  max_days <- as.numeric(Sys.getenv("MAX_DAYS_BACK", unset = "7"))
  
  for (i in 0:max_days) {
    check_date <- Sys.Date() - days(i)
    ts <- as.numeric(as.POSIXct(paste0(check_date, " 12:00:00"), tz = "UTC")) * 1000
    
    body <- list(
      productType = "CUM24",
      productDate = ts
    )
    
    res <- POST(
      url = paste0(BASE_URL, "/downloadProduct"),
      body = body,
      encode = "json"
    )
    
    if (status_code(res) == 200) {
      message("✅ Trovato CUM24 per: ", check_date)
      return(ts)
    }
    
    Sys.sleep(0.2)
  }
  
  stop("Nessun CUM24 trovato negli ultimi ", max_days, " giorni")
}

# ----------------------------
# MAIN SCRIPT - SOLO CUM24
# ----------------------------

message("🚀 DOWNLOAD DIRETTO CUM24 - PROTEZIONE CIVILE")
message("==============================================")
message("📁 Directory output: ", output_dir)

# 1. Trova l'ultimo CUM24 disponibile
last_ts <- tryCatch({
  get_last_cum24_timestamp_alt()
}, error = function(e) {
  message("⚠️ ", e$message)
  message("📌 Provo a cercare tutti i disponibili...")
  return(NULL)
})

# 2. Se abbiamo l'ultimo timestamp, cerchiamo i precedenti
if (!is.null(last_ts)) {
  last_date <- as.Date(as.POSIXct(last_ts / 1000, origin = "1970-01-01", tz = "UTC"))
  max_days_back <- as.numeric(Sys.getenv("MAX_DAYS_BACK", unset = "7"))
  
  start_date <- last_date - days(max_days_back)
  message("\n📅 Periodo basato su ultimo disponibile: ", start_date, " → ", last_date)
  
  dates_to_try <- seq(start_date, last_date, by = "day")
  timestamps_to_try <- as.numeric(as.POSIXct(paste0(dates_to_try, " 12:00:00"), tz = "UTC")) * 1000
  
} else {
  message("\n📅 Uso periodo fisso di default")
  max_days_back <- as.numeric(Sys.getenv("MAX_DAYS_BACK", unset = "7"))
  dates_to_try <- seq(Sys.Date() - days(max_days_back), Sys.Date(), by = "day")
  timestamps_to_try <- as.numeric(as.POSIXct(paste0(dates_to_try, " 12:00:00"), tz = "UTC")) * 1000
}

# 3. Verifica file già scaricati
existing_files <- list.files(output_dir, pattern = "^CUM24_\\d{8}\\.tif$", full.names = TRUE)
existing_dates <- gsub(".*CUM24_(\\d{8})\\.tif$", "\\1", basename(existing_files))

message("\n📁 File già presenti in locale: ", length(existing_dates))

# 4. Download sequenziale con gestione errori
downloaded_files <- c()
failed_dates <- c()

for (i in seq_along(timestamps_to_try)) {
  ts <- timestamps_to_try[i]
  current_date <- format(as.Date(as.POSIXct(ts / 1000, origin = "1970-01-01")), "%Y%m%d")
  
  message(sprintf("\n[%d/%d] ", i, length(timestamps_to_try)), "Processo: ", current_date)
  
  if (current_date %in% existing_dates) {
    message("  ⏭️  Già presente, salto...")
    next
  }
  
  tif_path <- download_cum24_safe(ts, output_dir)
  
  if (!is.null(tif_path)) {
    downloaded_files <- c(downloaded_files, tif_path)
  } else {
    failed_dates <- c(failed_dates, current_date)
  }
}

# 5. Report finale
message("\n", paste(rep("=", 60), collapse = ""))
message("📋 REPORT FINALE")
message(paste(rep("=", 60), collapse = ""))

message("\n📁 Cartella output: ", output_dir)

final_files <- list.files(output_dir, pattern = "^CUM24_\\d{8}\\.tif$", full.names = TRUE)

message("\n📊 RIEPILOGO:")
message("   Totale file CUM24 in locale: ", length(final_files))
message("   Nuovi download: ", length(downloaded_files))
message("   Date non disponibili: ", length(failed_dates))

if (length(final_files) > 0) {
  dates_final <- sort(gsub(".*CUM24_(\\d{8})\\.tif$", "\\1", basename(final_files)))
  message("   Periodo coperto: ", min(dates_final), " → ", max(dates_final))
  
  # Salva un file con l'elenco dei file scaricati
  writeLines(dates_final, file.path(output_dir, "available_dates.txt"))
}

if (length(failed_dates) > 0) {
  message("\n⚠️ Date NON disponibili:")
  print(failed_dates)
  writeLines(failed_dates, file.path(output_dir, "failed_dates.txt"))
}

message("\n✅ PROCEDURA COMPLETATA!")

# Salva informazioni sul workflow
workflow_info <- list(
  timestamp = Sys.time(),
  script_version = "1.0",
  workflow_run = Sys.getenv("GITHUB_RUN_ID", unset = "local"),
  files_downloaded = length(downloaded_files),
  total_files = length(final_files)
)

saveRDS(workflow_info, file.path(output_dir, "workflow_info.rds"))