library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

output_dir <- "./METEO/CSV"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


get_meteo_data <- function(base_url, query_string, var_filter, convert_temp = FALSE) {
  response <- GET(
    url = base_url,
    query = list(
      output_format = "JSON",
      q = query_string
    ),
    accept_json()
  )
  stop_for_status(response)
  
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  observations <- data$data
  
  extract_obs <- function(obs) {
    detail_name <- NULL
    for(d in obs$stat$details){
      if(d$var == "B01019"){
        detail_name <- d$val
        break
      }
    }
    
    val <- NULL
    ref_time <- NULL
    for(p in obs$prod){
      if(p$var == var_filter){
        val <- p$val[[1]]$val
        ref_time <- p$val[[1]]$ref
        break
      }
    }
    
    if(convert_temp && !is.null(val)) {
      val <- val - 273.15
    }
    
    lat <- obs$stat$lat
    lon <- obs$stat$lon
    net <- obs$stat$net
    
    data.frame(
      station_name = detail_name,
      value = val,
      ref_time = ref_time,
      lat = lat,
      lon = lon,
      net = net,
      stringsAsFactors = FALSE
    )
  }
  
  df <- do.call(rbind, lapply(observations, extract_obs))
  return(df)
}

# Giorno precedente
yesterday <- Sys.Date() - 1

# Funzione per generare orari 1 ora a intervallo
generate_hour_ranges <- function(date) {
  hours <- 0:23
  ranges <- lapply(hours, function(h) {
    start <- sprintf("%s %02d:00", date, h)
    end <- sprintf("%s %02d:59", date, h)
    c(start, end)
  })
  return(ranges)
}

hour_ranges <- generate_hour_ranges(yesterday)

# -------------------------
# TEMPERATURA 24 ORE
# -------------------------
temp_list <- list()
for(hr in hour_ranges){
  temp_query <- paste0(
    "reftime:>=", hr[1], ",<=", hr[2], ";",
    "product:B12101;",
    "license:CCBY_COMPLIANT;"
  )
  
  temp_df <- get_meteo_data(
    base_url = "https://meteohub.agenziaitaliameteo.it/api/observations",
    query_string = temp_query,
    var_filter = "B12101",
    convert_temp = TRUE
  )
  
  temp_list[[length(temp_list) + 1]] <- temp_df
  Sys.sleep(0.2)  # Piccola pausa per non sovraccaricare il server
}

temp_full <- do.call(rbind, temp_list)

write.csv(temp_full,
          file.path(output_dir, paste0("temperature_raw_", yesterday, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")


cat("CSV temperatura 24 ore creato.\n")

temp_summary <- temp_full %>%
  group_by(station_name, lat, lon, net) %>%
  summarise(
    temp_min = min(value, na.rm = TRUE),
    temp_max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(temp_summary,
          file.path(output_dir, paste0("temperature_summary_", yesterday, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("CSV temperatura aggregata creato.\n")

# -------------------------
# PRECIPITAZIONI 24 ORE
# -------------------------
prec_list <- list()
for(hr in hour_ranges){
  prec_query <- paste0(
    "reftime:>=", hr[1], ",<=", hr[2], ";",
    "product:B13011;",
    "license:CCBY_COMPLIANT;"
  )
  
  prec_df <- get_meteo_data(
    base_url = "https://meteohub.agenziaitaliameteo.it/api/observations",
    query_string = prec_query,
    var_filter = "B13011",
    convert_temp = FALSE
  )
  
  prec_list[[length(prec_list) + 1]] <- prec_df
  Sys.sleep(0.2)  # Piccola pausa
}

prec_full <- do.call(rbind, prec_list)

write.csv(prec_full,
          file.path(output_dir, paste0("precipitazioni_raw_", yesterday, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")


cat("CSV precipitazioni 24 ore creato.\n")

prec_summary <- prec_full %>%
  group_by(station_name, lat, lon, net) %>%
  summarise(
    total_prec = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(prec_summary,
          file.path(output_dir, paste0("precipitazioni_summary_", yesterday, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")


cat("CSV precipitazioni aggregato creato.\n")

