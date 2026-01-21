library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(zoo)

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
  
  data <- fromJSON(
    content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  
  observations <- data$data
  
  extract_obs <- function(obs) {
    
    # Nome stazione
    station_name <- NULL
    for (d in obs$stat$details) {
      if (d$var == "B01019") {
        station_name <- d$val
        break
      }
    }
    
    lat <- obs$stat$lat
    lon <- obs$stat$lon
    net <- obs$stat$net
    
    rows <- list()
    
    for (p in obs$prod) {
      if (p$var == var_filter) {
        
        for (v in p$val) {
          
          value <- v$val
          ref_time <- v$ref
          
          if (convert_temp && !is.null(value)) {
            value <- value - 273.15
          }
          
          rows[[length(rows) + 1]] <- data.frame(
            station_name = station_name,
            value = value,
            ref_time = ref_time,
            lat = lat,
            lon = lon,
            net = net,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(rows) == 0) return(NULL)
    
    do.call(rbind, rows)
  }
  
  # ➜ COSTRUZIONE DATAFRAME FINALE
  df_list <- lapply(observations, extract_obs)
  df <- do.call(rbind, df_list)
  
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

# Ordina i dati per stazione e tempo
temp_full <- temp_full %>%
  mutate(ref_time = ymd_hms(ref_time)) %>%
  arrange(station_name, ref_time)

# Sostituisci valori fuori range con NA
temp_full <- temp_full %>%
  mutate(value = ifelse(value < -50 | value > 50, NA, value))

# Fai il carry-forward dei valori precedenti per NA
temp_full <- temp_full %>%
  group_by(station_name) %>%
  mutate(value = na.locf(value, na.rm = FALSE)) %>%
  ungroup()

write.csv(temp_full, file.path(output_dir, paste0("temperature_raw_", yesterday, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
cat("CSV temperatura 24 ore creato.\n")

temp_summary <- temp_full %>%
  group_by(station_name, lat, lon, net) %>%
  summarise(
    temp_min = min(value, na.rm = TRUE),
    temp_max = max(value, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(temp_summary, file.path(output_dir, paste0("temperature_summary_", yesterday, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
cat("CSV temperatura aggregata creato.\n")

# -------------------------
# PRECIPITAZIONI RAW
# (cadenza nativa della rete)
# -------------------------

# Intervallo temporale giornaliero
start_day <- paste0(yesterday, " 00:00")
end_day   <- paste0(yesterday, " 23:59")

# Query unica per l'intero giorno
prec_query <- paste0(
  "reftime:>=", start_day, ",<=", end_day, ";",
  "product:B13011;",
  "license:CCBY_COMPLIANT;"
)

# Download dati precipitazione
prec_full <- get_meteo_data(
  base_url = "https://meteohub.agenziaitaliameteo.it/api/observations",
  query_string = prec_query,
  var_filter = "B13011",
  convert_temp = FALSE
)

# Salvataggio CSV RAW (risoluzione originale)
write.csv(
  prec_full,
  file.path(output_dir, paste0("precipitazioni_raw_", yesterday, ".csv")),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("CSV precipitazioni RAW (cadenza nativa rete) creato.\n")

prec_summary <- prec_full %>%
  group_by(station_name, lat, lon, net) %>%
  summarise(
    total_prec = sum(value, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(prec_summary, file.path(output_dir, paste0("precipitazioni_summary_", yesterday, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
cat("CSV precipitazioni aggregato creato.\n")

################## DATI ORARI ####################

# Trasformo il timestamp
temp_full <- temp_full %>%
  mutate(
    ref_time = as.POSIXct(ref_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    hour = floor_date(ref_time, unit = "hour")
  )

prec_full <- prec_full %>%
  mutate(
    ref_time = as.POSIXct(ref_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    hour = floor_date(ref_time, unit = "hour")
  )

######TEMP MEDIA##
temp_hourly <- temp_full %>%
  group_by(station_name, lat, lon, net, hour) %>%
  summarise(
    temp_avg = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

########PREC TOT ORARIA ####

prec_hourly <- prec_full %>%
  group_by(station_name, lat, lon, net, hour) %>%
  summarise(
    prec_sum = sum(value, na.rm = TRUE),
    .groups = "drop"
  )


#####SALVO DATI ORARI############

write.csv(temp_hourly, file.path(output_dir, paste0("temperature_hourly_", yesterday, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(prec_hourly, file.path(output_dir, paste0("precipitation_hourly_", yesterday, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")

