library(googledrive)

# Define the folder ID from your Google Drive
drive_folder_id <- "1o-1bHJ3nzNEEj1M8eY468BPKZkJXpyYj?hl=it"  # Replace this with your folder ID

# Authenticate using the token
drive_auth()

# List the files you want to upload (assuming they are in the 'aggregati' folder)
local_dir <- "./DPC/aggregati"
files <- list.files(local_dir, pattern = "\\.tif$", full.names = TRUE)

# Upload each file to Google Drive
for (file in files) {
  message("Uploading ", basename(file), " to Google Drive folder with ID: ", drive_folder_id)
  drive_upload(media = file, path = as_id(drive_folder_id), overwrite = TRUE)
}
