library(googledrive)

# Read service account JSON from secret
json_key <- Sys.getenv("GDRIVE_KEY")
if(json_key == "") stop("GDRIVE_KEY is empty!")
json_path <- tempfile(fileext = ".json")
writeLines(json_key, json_path)
on.exit(unlink(json_path))  # remove temp file

# Authenticate using service account
drive_auth(path = json_path)

# ID della cartella Drive
drive_folder_id <- Sys.getenv("GDRIVE_FOLDER_ID")
if(drive_folder_id == "") stop("GDRIVE_FOLDER_ID is empty!")

# Local folder with files
local_dir <- "./DPC/aggregati"
dir.create(local_dir, showWarnings = FALSE, recursive = TRUE)

# Files to upload (CSV in questo caso)
files <- list.files(local_dir, pattern = "\\.csv$", full.names = TRUE)

# Upload each file
for(file in files){
  message("Uploading ", basename(file), " to Drive folder ", drive_folder_id)
  drive_upload(media = file, path = as_id(drive_folder_id), overwrite = TRUE)
}
