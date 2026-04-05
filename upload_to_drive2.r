
library(googledrive)
library(base64enc)

# Leggi il token Base64 dal GitHub Secret
token_b64 <- Sys.getenv("GDRIVE_TOKEN_B64")
token_path <- tempfile(fileext = ".rds")
writeBin(base64decode(token_b64), token_path)

# Autentica con il tuo account personale (usa i tuoi 200GB)
drive_auth(token = readRDS(token_path))

# Google Drive folder ID (remove any URL query parameters)
drive_folder_id <- "1o-1bHJ3nzNEEj1M8eY468BPKZkJXpyYj"

# Local directory with files to upload
local_dir <- "./METEO/CSV"
files <- list.files(local_dir, pattern = "\\.csv$", full.names = TRUE)

# Upload each file to the Drive folder
for (file in files) {
  message("Uploading ", basename(file), " to Google Drive folder with ID: ", drive_folder_id)
  drive_upload(media = file, path = as_id(drive_folder_id), overwrite = TRUE)
}
