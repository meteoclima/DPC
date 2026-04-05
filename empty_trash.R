library(googledrive)

json_key <- Sys.getenv("GDRIVE_SERVICE_ACCOUNT_JSON")
json_path <- tempfile(fileext = ".json")
writeLines(json_key, json_path)
drive_auth(path = json_path)

# Delete files older than 30 days
folder_id <- "1o-1bHJ3nzNEEj1M8eY468BPKZkJXpyYj"
files <- drive_ls(as_id(folder_id))

# Calculate date 30 days ago
cutoff_date <- Sys.Date() - 30

old_files <- files[as.Date(files$drive_resource$modifiedTime) < cutoff_date, ]

if (nrow(old_files) > 0) {
  message("Deleting ", nrow(old_files), " old files...")
  for (i in 1:nrow(old_files)) {
    message("Deleting: ", old_files$name[i])
    drive_rm(old_files$id[i])
  }
} else {
  message("No old files to delete")
}
