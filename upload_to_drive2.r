library(googledrive)
library(jsonlite)

# 1️⃣ Leggi JSON token dal secret
json_token <- Sys.getenv("GDRIVE_OAUTH_JSON")
if (json_token == "") stop("GDRIVE_OAUTH_JSON vuoto!")

# 2️⃣ Decodifica la stringa JSON e scrivi su file
json_parsed <- jsonlite::fromJSON(json_token, simplifyVector = FALSE)
token_path <- tempfile(fileext = ".json")
jsonlite::write_json(json_parsed, token_path, auto_unbox = TRUE)
on.exit(unlink(token_path))

# 3️⃣ Autenticazione non-interattiva
drive_auth(path = token_path)

# 4️⃣ ID della cartella
drive_folder_id <- Sys.getenv("GDRIVE_FOLDER_ID")
if(drive_folder_id == "") stop("GDRIVE_FOLDER_ID vuoto!")

# 5️⃣ Trova file CSV
local_dir <- "./METEO/CSV"
dir.create(local_dir, showWarnings = FALSE, recursive = TRUE)
files <- list.files(local_dir, pattern = "\\.csv$", full.names = TRUE)

# 6️⃣ Upload dei file
for(file in files){
  message("Uploading ", basename(file))
  drive_upload(
    media = file,
    path = as_id(drive_folder_id),
    overwrite = TRUE
  )
}
