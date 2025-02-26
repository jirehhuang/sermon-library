#' Download sermons based on existing metadata
#' 
#' @param catalog_dir Directory from which to read metadata .csv files.
#' @param sermons_dir Target directory to save downloaded and compressed audio files.
#' @param bool_continue Whether or not to attempt to continue interrupted downloads using `curl -C -`.
#' @param bool_compress Whether or not to compress audio using `ffmpeg`.
#' @param n_cores Numeric value specifying number of cores.
#' @return None.
#' @example

download_sermons <- function(catalog_dir = file.path(getwd(), "catalog"),
                             sermons_dir = file.path(getwd(), "sermons"),
                             bool_continue = TRUE,
                             bool_compress = FALSE,
                             n_cores = 1){
  
  metadf <- compile_catalog(catalog_dir = catalog_dir)
  
  ## Download sermons to sermon directory one by one
  cli::cli_alert_info("downloading files for {nrow(metadf)} titles", 
                      .envir = environment())
  
  ## Define function to download sermon
  download_fn <- function(i){
    
    mdi <- metadf[i,]
    
    cli::cli_alert("downloading {i}. {mdi$Title}", .envir = environment())
    
    filename <- file.path(sermons_dir, sprintf("%s.%s", mdi$Name, tools::file_ext(mdi$Audio)))
    
    ## Download if haven't already
    if (!file.exists(filename) &&
        !basename(filename) %in% list.files(sermons_dir)){
      
      # download.file(mdi$audio, filename)  # TODO: doesn't seem to work (corrupted)
      cmd <- sprintf('curl %s --output "%s"', mdi$Audio, filename)
      cli::cli_alert("executing `{cmd}`", .envir = environment())
      system(cmd)
      
    } else if (bool_continue){
      
      ## Attempt to continue interrupted download
      cmd <- sprintf('curl -C - %s --output "%s"', mdi$Audio, filename)
      cli::cli_alert("executing `{cmd}`", .envir = environment())
      system(cmd)
    }
    
    ## Compress audio file
    if (bool_compress){
      
      cli::cli_alert("compressing {basename(filename)}", .envir = environment())
      
      ## Construct and execute the ffmpeg command
      sample_rate <- 16000
      input_file <- filename
      output_file <- file.path(sermons_dir, sprintf("%s - Compressed.%s", 
                                                    mdi$Name, tools::file_ext(mdi$Audio)))
      
      cmd <- sprintf(
        'ffmpeg -nostdin -threads 0 -i "%s" -ac 1 -ar %d "%s"',
        input_file, sample_rate, output_file
      )
      ## Execute the command
      system(cmd, intern = TRUE)
      
      # file.remove(input_file)
    }
  }
  
  ## Execute downloading
  switch_lapply(n_cores = n_cores,
                X = seq_len(nrow(metadf)),
                FUN = download_fn,
                outfolder = file.path(dirname(catalog_dir), "logs"),
                varlist = list(metadf = metadf))
  
  return(invisible(TRUE))
}
