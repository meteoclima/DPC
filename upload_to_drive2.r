library(googledrive)

json_key <- Sys.getenv("GDRIVE_SERVICE_ACCOUNT_JSON")
if (json_key == "") stop("GDRIVE_SERVICE_ACCOUNT_JSON vuoto!")

json_path <- tempfile(fileext = ".json")
writeLines(json_key, json_path)
on.exit(unlink(json_path))

drive_auth(path = json_path)

drive_folder_id <- Sys.getenv("GDRIVE_FOLDER_ID")
if (drive_folder_id == "") stop("GDRIVE_FOLDER_ID vuoto!")

local_dir <- "./METEO/CSV"
files <- list.files(local_dir, pattern = "\\.csv$", full.names = TRUE)

for (file in files) {
  message("Uploading ", basename(file))
  drive_upload(
    media = file,
    path = as_id(drive_folder_id),
    overwrite = TRUE
  )
}
