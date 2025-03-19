##======================================================================##
## Utility functions
##======================================================================##



#' Choose directory

switch_choose_dir <- function(dir = "browse", caption = "") {
  
  if (identical(dir, choose.dir) || 
      dir %in% c("browse", "choose", "choose.dir")){
    
    dir <- choose.dir(default = file.path(default = getwd()), caption = caption)
  }
  return(dir)
}



#' Simple function to compile sermon filename

sermon_filename <- function(title,
                            text,
                            teacher,
                            date,
                            bool_lower = FALSE){
  
  sprintf("%s%s%s%s",
          std_text(date, replacement = "_", add_suffix = " - ", bool_lower = bool_lower),
          std_text(text, replacement = "_", add_suffix = " - ", bool_lower = bool_lower),
          std_text(title, replacement = "_", add_suffix = " - ", bool_lower = bool_lower),
          std_text(teacher, replacement = "_", add_suffix = "", bool_lower = bool_lower))
}



#' Convenience function for checking valid value

is_valid <- function(x){
  
  !(missing(x) || is.null(x) || is.na(x) || length(x) == 0 || nchar(x) == 0)
}



#' Standardize text

std_text <- function(x,
                     replacement = "_",
                     add_suffix = "",
                     bool_lower = FALSE){
  
  if (!is_valid(x = x)){
    
    return("")
  }
  if (class(x) == "Date"){
    
    x <- format(x, "%Y-%m-%d")
    
  } else if (bool_lower){
    
    ## TODO: not implemented
    browser()
    
  } else{
    
    x <- iconv(x, to = "ASCII//TRANSLIT")
    x <- gsub(": ", replacement, x)
    x <- gsub(":|\\.", replacement, x)
    x <- gsub("[\\/\\:*?\"<>|]", replacement, x)
    x <- gsub("\\&", replacement, x)
    
    while(grepl("  ", x)){
      
      x <- gsub("  ", " ", x)
    }
  }
  if (nchar(add_suffix)){
    
    x <- sprintf("%s%s", x, add_suffix)
  }
  return(x)
}



#' Extract the common beginning of two strings, if any

get_common_pre <- function(char1, char2){
  
  s1 <- substring(char1, 1, seq_len(nchar(char1)))
  s2 <- substring(char2, 1, seq_len(nchar(char2)))
  
  if (length(w <- which(s1 %in% s2))){
    s2[max(w)]
  } else {
    character(1)
  }
}



#' Simplify text that has a range

simplify_range <- function(text){
  
  if (is.null(text) || is.na(text) || nchar(text) == 0 || !grepl("-", text)){
    
    return(text)
  }
  
  ## Remove spaces adjacent to dash
  while(grepl(" -|- ", text)){
    
    text <- gsub(" -|- ", "-", text)
  }
  split_text <- strsplit(text, "-")[[1]]
  
  ## Get common beginning
  common_pre <- do.call(get_common_pre, as.list(split_text))
  
  ## Stitch back together in the form of {common_pre}{unique_lower}-{uniquer_upper}
  paste0(common_pre, paste(gsub(common_pre, "", split_text), collapse = "-"))
}



#' Validate the number of cores
#'
#' Ensures that the `n_cores` parameter is a positive integer within the range of available cores.
#' If `n_cores` is -1, it sets it to the maximum number of detected cores.
#'
#' @param n_cores An integer specifying the desired number of cores.
#' @return A validated integer number of cores, clamped to the range [1, maximum cores].
#' @examples
#' validated_cores <- validate_n_cores(-1)
#' print(validated_cores)
#' @noRd

validate_n_cores <- function(n_cores){
  
  ## Detect maximum number of cores
  max_cores <- parallel::detectCores() - 1L
  
  ## Coerce to 1 if invalid
  if (!is.numeric(n_cores) || length(n_cores) != 1){
    
    debug_cli(TRUE, cli::cli_alert_warning,
              "n_cores = {n_cores} must be a numeric scalar; coercing to 1",
              .envir = environment())
    
    n_cores <- 1
  }
  
  ## Convert to integer
  n_cores <- as.integer(n_cores)
  
  ## If n_cores == -1, set to maximum number of cores
  if (n_cores == -1){
    
    n_cores <- max_cores
    
  }  # Else if necessary, restrict to [1, max_cores]
  else if (n_cores < 1 || n_cores > max_cores){
    
    debug_cli(TRUE, cli::cli_alert_warning,
              "restricting n_cores = {n_cores} to between 1 and {max_cores}",
              .envir = environment())
    
    n_cores <- min(max(1, n_cores), max_cores)
  }
  
  return(n_cores)
}



#' Switch between parallel and sequential lapply()

switch_lapply <- function(n_cores = 1,
                          X,
                          FUN,
                          varlist = list(),
                          outfolder = getwd(),
                          ...){
  
  ## Validate n_cores; coerce to a valid value if necessary
  n_cores <- validate_n_cores(n_cores = n_cores)
  
  ## If possible, execute in parallel
  if (Sys.info()[["sysname"]] == "Windows" && n_cores > 1){
    
    ## Set up parallel cluster
    cl <- parallel::makeCluster(spec = getOption("cl.cores", n_cores),
                                outfile = file.path(outfolder, sprintf("log_%s.txt", round(as.numeric(Sys.time()) * 1e3))))
    
    ## Export the user-defined function to the cluster
    list2env(varlist, envir = environment())
    parallel::clusterExport(cl, varlist = names(varlist), envir = environment())
    
    ## Execute in parallel
    executed <- parallel::parLapply(cl = cl,
                                    X = X,
                                    fun = FUN,
                                    ...)
    
    parallel::stopCluster(cl)
    
  } else{
    
    ## Execute sequentially
    executed <- lapply(X = X,
                       FUN = FUN,
                       ...)
  }
  return(invisible(executed))
}



#' Generate ID based on time
#'
#' This function generates a unique ID based on the current time in the format 
#' %Y-%m-%d_%H-%M-%OS3, replacing the decimal point in seconds with a dash '-'.
#'
#' @param time A POSIXct object representing the time to use for generating the ID. 
#' Defaults to the current system time (`Sys.time()`).
#' @return A character string representing the unique ID.
#' @examples
#' unique_id <- time2id()
#' print(unique_id)
#' @noRd

time2id <- function(time = Sys.time()){
  
  ## Format the time to include milliseconds (%OS3)
  formatted_time <- format(time, "%Y-%m-%d_%H-%M-%OS3")
  
  ## Replace the decimal point in seconds with a dash '-'
  unique_id <- gsub("\\.", "-", formatted_time)
  
  ## Return the generated unique ID
  return(unique_id)
}



##======================================================================##
## Global constants
##======================================================================##



#' Available audio file extensions
AVAIL_AUDIO_EXT <- c("mp3", "m4a", "wav", "flac")
