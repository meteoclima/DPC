library(googledrive)
library(base64enc)

# Read the base64-encoded token from the GitHub secret
token_b64 <- Sys.getenv("GDRIVE_PAT")  # Get token from the secret
token_path <- tempfile(fileext = ".rds")
writeBin(base64decode(token_b64), token_path)  # Decode the base64 string to the .rds token file

# Authenticate using the token (no cache to avoid issues in CI)
drive_auth(token = token_path, cache = FALSE)

# Correct folder ID without query parameters
drive_folder_id <- "1o-1bHJ3nzNEEj1M8eY468BPKZkJXpyYj"

# Directory to upload from
local_dir <- "./DPC/aggregati"
files <- list.files(local_dir, pattern = "\\.tif$", full.names = TRUE)

# Upload each file to the Google Drive folder
for (file in files) {
  message("Uploading ", basename(file), " to Google Drive folder with ID: ", drive_folder_id)
  drive_upload(media = file, path = as_id(drive_folder_id), overwrite = TRUE)
}
