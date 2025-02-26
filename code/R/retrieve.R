##======================================================================##
## Retrieving sermon metadata
##======================================================================##



#' Retrieve sermon metadata from gracechurch.org
#' 
#' @param link Character value specifying link to scrape.
#' @param filename Target directory to save metadata data.frame as a .csv file.
#' @param bool_continue Whether or not to attempt to continue interrupted downloads using `curl -C -`.
#' @param n_cores Numeric value specifying number of cores.
#' @return None.

gracechurch_org <- function(link = "https://www.gracechurch.org/sermons/16026",
                            filename = file.path(getwd(), "catalog", sprintf("gracechurch_org.csv")),
                            # bool_continue = TRUE,
                            n_cores = 1){
  
  require(dplyr)
  
  
  ## Create folder if needed
  folder <- dirname(filename)
  
  if (!dir.exists(folder)){
    
    dir.create(folder)
  }
  
  ## Check link
  if (!grepl("gracechurch\\.org/sermons", link)){
    
    cli::cli_abort("link must begin with `gracechurch.org/sermons`")
  }
  
  ## Retrieve URL domain
  link0 <- sprintf("https://%s", urltools::domain(link))
  
  ## Generalize link to allow for multiple pages
  if (!grepl("page=", link)){
    
    link <- sprintf("%s%spage=%s", 
                    link, 
                    ifelse(grepl("\\?", link), "&", "?"),
                    "%g")
  } else if (!grepl("page=%g", link)){
    
    link <- gsub("page=\\d+", "page=%g", link)
  }
  
  
  ## Loop through pages and compile all title links on page(s), starting with page i=1
  cli::cli_alert_info("retrieving links to all titles")
  
  i <- 1
  title_links <- c()
  
  repeat({
    
    cli::cli_alert("reading page {i}", .envir = environment())
    
    ## Retrieve links to each sermon title from HTL elements
    title_linksi <- rvest::read_html(sprintf(link, i)) %>%
      rvest::html_elements(css = ".listing-title") %>%
      rvest::html_children() %>%
      rvest::html_attr("href")
    
    ## Exit if empty page
    if (length(title_linksi) == 0){
      
      cli::cli_alert("page {i} is empty", .envir = environment())
      
      break
    } 
    
    ## Add to title_links
    title_links <- c(title_links, title_linksi)
    
    i <- i + 1
  })
  
  ## If retrieved links to titles, prepend URL domain
  if (length(title_links)){
    
    title_links <- paste0(link0, unique(title_links))
    
    ## Otherwise if empty, assume original link is itself a title link
  } else{
    
    title_links <- link
  }
  
  
  ## TODO: check for and skip duplicate titles
  
  
  ## Retrieve metadata for each title
  cli::cli_alert_info("retrieving metadata for {length(title_links)} titles", 
                      .envir = environment())
  
  ## Define function to retrieve metadata for sermon
  metadata_fn <- function(x){
    
    require(dplyr)
    
    ## Read page from link
    linkx <- title_links[x]
    pagex <- rvest::read_html(linkx)
    
    ## Retrieve title
    title <- pagex %>%
      rvest::html_elements(css = ".title") %>%
      rvest::html_text() %>%
      strsplit(split = "by\\r\\n") %>%
      `[[`(1) %>% `[`(1) %>%
      gsub(pattern = "\\r|\\n", replacement = "") %>%
      trimws()
    
    cli::cli_alert("reading {x}. {title}", .envir = environment())
    
    ## Retrieve existing labels
    label <- pagex %>%
      rvest::html_elements(css = ".meta-label") %>%
      rvest::html_text() %>%
      gsub(pattern = ":", replacement = "") %>%
      trimws()
    
    ## Retrieve content corresponding to labels
    content <- pagex %>%
      rvest::html_elements(css = ".meta-content") %>%
      rvest::html_text() %>%
      gsub(pattern = "\\r|\\n", replacement = "") %>%
      trimws()
    
    ## Generally, detect multiple topics
    if (length(content) > length(label)){
      
      ## Number of topics
      n_topics <- 1L + length(content) - length(label)
      
      ## Identify indices of topics and before topics
      which_topics <- which(label == "Topics") + seq(0, n_topics - 1L)
      pre_topics <- seq_len(min(which_topics) - 1L)
      
      ## Merge topics separated by semicolons
      content <- c(content[pre_topics], 
                   paste0(content[which_topics], collapse = "; "),
                   content[-union(pre_topics, which_topics)])
    }
    
    ## Generally, detect multiple teachers
    if (any(table(label) > 1)){
      
      ## Group content by label and reduce to unique labels
      content <- sapply(unique(label), function(y){
        
        paste(content[label == y], collapse = "; ")
        
      }, USE.NAMES = FALSE)
      
      label <- unique(label)
    }
    
    ## Convert to a data.frame object
    metadatax <- c(title, content) %>%
      matrix(nrow = 1) %>%
      as.data.frame()
    names(metadatax) <- c("Title", label)
    
    ## Add applicable files to metadata row
    files <- pagex %>%
      rvest::html_elements(css = ".btn-outline") %>%
      rvest::html_attr("href")
    
    ## Extract audio and other files
    audio <- files[sapply(files, function(y) 
      any(endsWith(x = y, suffix = AVAIL_AUDIO_EXT)))]
    
    other <- setdiff(files, audio)
    other <- ifelse(length(other), paste(other, collapse = "; "), NA_character_)
    
    ## Add additional metadata
    metadatax <- metadatax %>%
      mutate_all(iconv, to = "ASCII//TRANSLIT") %>%
      mutate(Source = urltools::domain(link),
             Date = as.Date(Date, format = "%m/%d/%y"),
             Page = linkx,
             Audio = audio,
             Files = other) %>%
      mutate(Name = sermon_filename(title = Title,
                                    text = Text,
                                    teacher = Teacher,
                                    date = Date))
    
    return(metadatax)
  }
  
  ## Execute retrieving and compile in table
  metadf <- switch_lapply(n_cores = n_cores,
                          X = seq_len(length(title_links)),
                          FUN = metadata_fn,
                          outfolder = file.path(dirname(dirname(filename)), "logs"),
                          varlist = list(title_links = title_links,
                                         sermon_filename = sermon_filename,
                                         std_text = std_text,
                                         is_valid = is_valid,
                                         AVAIL_AUDIO_EXT = AVAIL_AUDIO_EXT)) %>%
    do.call(bind_rows, .)
  
  
  ## Write metadata data.frame as a .csv file
  write.csv(x = metadf, file = filename, row.names = TRUE)
  
  return(invisible(TRUE))
}
