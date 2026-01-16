library(googledrive)

# 1️⃣ Leggi JSON token dal secret
json_token <- Sys.getenv("GDRIVE_OAUTH_JSON")
if (json_token == "") stop("GDRIVE_OAUTH_JSON vuoto!")

# 2️⃣ Scrivi token su file temporaneo
token_path <- tempfile(fileext = ".json")
writeLines(json_token, token_path)
on.exit(unlink(token_path))  # cancella il file alla fine

# 3️⃣ Autenticazione non-interattiva usando il JSON
drive_auth(path = token_path)

# 4️⃣ Prendi ID della cartella
drive_folder_id <- Sys.getenv("GDRIVE_FOLDER_ID")
if(drive_folder_id == "") stop("GDRIVE_FOLDER_ID vuoto!")

# 5️⃣ Trova file CSV da caricare
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
