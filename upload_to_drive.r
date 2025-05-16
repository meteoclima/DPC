library(googledrive)

# Write the JSON key from environment variable to a temp file
json_key <- Sys.getenv("GDRIVE_SERVICE_ACCOUNT_JSON")
json_path <- tempfile(fileext = ".json")
writeLines(json_key, json_path)

# Authenticate using the service account JSON key
drive_auth(path = json_path)

# Google Drive folder ID (remove any URL query parameters)
drive_folder_id <- "1o-1bHJ3nzNEEj1M8eY468BPKZkJXpyYj"

# Local directory with files to upload
local_dir <- "./DPC/aggregati"
files <- list.files(local_dir, pattern = "\\.tif$", full.names = TRUE)

# Upload each file to the Drive folder
for (file in files) {
  message("Uploading ", basename(file), " to Google Drive folder with ID: ", drive_folder_id)
  drive_upload(media = file, path = as_id(drive_folder_id), overwrite = TRUE)
}
