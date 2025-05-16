library(googledrive)
drive_auth()
# Ti si apre una finestra browser per autorizzare l'accesso a Google Drive.
# Dopo aver autorizzato:
token <- drive_token()
saveRDS(token, "C:/test/gdrive_token.rds")
