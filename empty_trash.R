library(googledrive)

# Write the JSON key from environment variable to a temp file
json_key <- Sys.getenv("GDRIVE_SERVICE_ACCOUNT_JSON")
json_path <- tempfile(fileext = ".json")
writeLines(json_key, json_path)

# Authenticate using the service account JSON key
drive_auth(path = json_path)

# Function to permanently delete items from Service Account's Drive trash
permanently_empty_trash <- function(confirm = TRUE) {
  message("Checking for items in trash...")
  
  # List all files in trash
  trash_files <- drive_find(q = "trashed = true", 
                            trashed = TRUE,
                            fields = "files(id, name, mimeType)")
  
  if (nrow(trash_files) == 0) {
    message("No items found in trash.")
    return(invisible(NULL))
  }
  
  message(sprintf("Found %d item(s) in trash:", nrow(trash_files)))
  print(trash_files[, c("name", "id")])
  
  # Confirm deletion if required
  if (confirm) {
    if (interactive()) {
      answer <- readline(prompt = sprintf("Do you want to permanently delete these %d item(s)? (yes/no): ", nrow(trash_files)))
      if (tolower(answer) != "yes") {
        message("Operation cancelled.")
        return(invisible(NULL))
      }
    } else {
      message("Non-interactive session: skipping confirmation.")
      if (!confirm) {
        message("Proceeding with deletion...")
      } else {
        message("Deletion cancelled (confirmation required but not available).")
        return(invisible(NULL))
      }
    }
  }
  
  # Permanently delete each item
  for (i in 1:nrow(trash_files)) {
    file_id <- trash_files$id[i]
    file_name <- trash_files$name[i]
    message(sprintf("Permanently deleting: %s (%s)", file_name, file_id))
    drive_rm(file = as_id(file_id), forever = TRUE)
  }
  
  message("All items have been permanently deleted from trash.")
}

# Execute the trash emptying
# To skip confirmation (for automated scripts), change to FALSE:
permanently_empty_trash(confirm = TRUE)