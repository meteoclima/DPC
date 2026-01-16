library(googledrive)

# Autenticazione tramite OAuth JSON dal secret GitHub
json_token <- Sys.getenv("GDRIVE_OAUTH_JSON")
if(json_token == "") stop("GDRIVE_OAUTH_JSON vuoto!")
token_path <- tempfile(fileext = ".json")
writeLines(json_token, token_path)
on.exit(unlink(token_path))  # cancella file alla fine

drive_auth(path = token_path)

# Cartella Drive
drive_folder_id <- Sys.getenv("GDRIVE_FOLDER_ID")

# Cartella locale
local_dir <- "./METEO/CSV"
dir.create(local_dir, showWarnings = FALSE, recursive = TRUE)

# Upload CSV
files <- list.files(local_dir, pattern = "\\.csv$", full.names = TRUE)
for(file in files){
  message("Uploading ", basename(file))
  drive_upload(
    media = file,
    path = as_id(drive_folder_id),
    overwrite = TRUE
  )
}


