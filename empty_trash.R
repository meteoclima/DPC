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
  
  # List all files in trash - corrected syntax
  trash_files <- tryCatch({
    drive_find(q = "trashed = true", trashed = TRUE)
  }, error = function(e) {
    message("Error listing trash files: ", e$message)
    # Alternative approach without the trashed parameter
    drive_find(q = "trashed = true")
  })
  
  # Check if we got any results
  if (is.null(trash_files) || nrow(trash_files) == 0) {
    message("No items found in trash.")
    return(invisible(NULL))
  }
  
  message(sprintf("Found %d item(s) in trash:", nrow(trash_files)))
  if (nrow(trash_files) > 0) {
    print(data.frame(name = trash_files$name, id = trash_files$id))
  }
  
  # Confirm deletion if required
  if (confirm) {
    if (interactive()) {
      answer <- readline(prompt = sprintf("Do you want to permanently delete these %d item(s)? (yes/no): ", nrow(trash_files)))
      if (tolower(answer) != "yes") {
        message("Operation cancelled.")
        return(invisible(NULL))
      }
    } else {
      # In non-interactive mode, proceed without confirmation when confirm=TRUE
      message("Non-interactive session: proceeding with deletion...")
    }
  }
  
  # Permanently delete each item
  success_count <- 0
  error_count <- 0
  
  for (i in 1:nrow(trash_files)) {
    file_id <- trash_files$id[i]
    file_name <- trash_files$name[i]
    tryCatch({
      message(sprintf("Permanently deleting: %s (%s)", file_name, file_id))
      drive_rm(file = as_id(file_id), forever = TRUE)
      success_count <- success_count + 1
    }, error = function(e) {
      message(sprintf("Failed to delete %s: %s", file_name, e$message))
      error_count <- error_count + 1
    })
  }
  
  message(sprintf("Deleted %d items, %d errors", success_count, error_count))
}

# Execute the trash emptying
permanently_empty_trash(confirm = FALSE)  # Set to FALSE for automated execution
