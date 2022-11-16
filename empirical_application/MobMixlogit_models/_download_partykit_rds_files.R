library(googledrive)
route <- "empirical_application/MobMixlogit_models/"
list_id_files <- list("1m94dp5XkO-RAiUHwJy6ACg0eE19rGCSP", 
                      "1m4UxCZXq1ygJnJWz_LyIHOqW2Us87qF7")

lapply(list_id_files,function(id){
  #recover metadata file
  meta_data_file <- drive_get(id = id)
  # download file from googledrive
  drive_download(
    as_id(id), 
    path = paste0(route,meta_data_file$name), 
    overwrite = TRUE)
})

