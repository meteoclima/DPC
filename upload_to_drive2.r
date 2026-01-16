library(googledrive)

# Configura OAuth client
json_token <- Sys.getenv("GDRIVE_OAUTH_JSON")
if(json_token == "") stop("GDRIVE_OAUTH_JSON vuoto!")

token_path <- tempfile(fileext = ".json")
writeLines(json_token, token_path)
on.exit(unlink(token_path))

drive_auth_configure(path = token_path)
drive_auth()

drive_folder_id <- Sys.getenv("GDRIVE_FOLDER_ID")

local_dir <- "./METEO/CSV"

dir.create(local_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(local_dir, pattern = "\\.csv$", full.names = TRUE)
for(file in files){
  message("Uploading ", basename(file))
  drive_upload(
    media = file,
    path = as_id(drive_folder_id),
    overwrite = TRUE
  )
}
