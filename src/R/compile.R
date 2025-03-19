#' Get all retrieved sermon metadata 

compile_metadata <- function(catalog_dir = file.path(getwd(), "catalog")){
  
  ## Compile various sermon metadata tables
  csv_files <- list.files(catalog_dir, pattern = "\\.csv", full.names = TRUE)
  metadf <- do.call(dplyr::bind_rows, lapply(csv_files, read.csv)) %>%
    filter(!duplicated(.))  # Retain only unique rows
  
  return(metadf)
}
