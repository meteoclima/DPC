library(googledrive)

json_key <- Sys.getenv("GDRIVE_SERVICE_ACCOUNT_JSON")
json_path <- tempfile(fileext = ".json")
writeLines(json_key, json_path)
drive_auth(path = json_path)

# Delete files older than 30 days
folder_id <- "1o-1bHJ3nzNEEj1M8eY468BPKZkJXpyYj"

# List files
files <- drive_ls(as_id(folder_id))

# Check if there are files
if (nrow(files) > 0) {
  # Calculate date 30 days ago
  cutoff_date <- Sys.Date() - 30
  
  # Get modification dates
  mod_dates <- as.Date(sapply(files$drive_resource, function(x) x$modifiedTime))
  
  # Find old files
  old_indices <- which(mod_dates < cutoff_date)
  
  # Delete old files if any
  if (length(old_indices) > 0) {
    old_files <- files[old_indices, ]
    message("Deleting ", nrow(old_files), " old files...")
    
    for (i in 1:nrow(old_files)) {
      message("Deleting: ", old_files$name[i])
      drive_rm(old_files$id[i], forever = TRUE)
    }
    
    message("Done!")
  } else {
    message("No old files to delete")
  }
} else {
  message("No files found in folder")
}
