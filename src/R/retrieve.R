##======================================================================##
## Retrieving sermon metadata
##======================================================================##



#' Get all retrieved sermon metadata 

compile_catalog <- function(catalog_dir = file.path(getwd(), "catalog")){
  
  ## Load necessary package(s)
  require(dplyr)
  
  ## Compile various sermon metadata tables
  csv_files <- list.files(catalog_dir, pattern = "\\.csv", full.names = TRUE)
  metadf <- do.call(dplyr::bind_rows, lapply(csv_files, read.csv)) %>%
    filter(!duplicated(.))  # Retain only unique rows
  
  return(metadf)
}



#' Retrieve sermon metadata from gracechurch.org
#' 
#' @param link Character value specifying link to scrape.
#' @param filename Target directory to save metadata data.frame as a .csv file.
#' @param n_cores Numeric value specifying number of cores.
#' @return None.

gracechurch_org <- function(link = "https://www.gracechurch.org/sermons/16026",
                            filename = file.path(getwd(), "catalog", sprintf("gracechurch_org.csv")),
                            n_cores = 1){
  
  require(dplyr)
  
  
  ## Create folder if needed
  folder <- dirname(filename)
  
  if (!dir.exists(folder)){
    
    dir.create(folder)
  }
  
  ## Check link
  if (!grepl("gracechurch\\.org/sermons|gracechurch\\.org/teaching", link)){
    
    cli::cli_abort("link must begin with `gracechurch.org/sermons` or `gracechurch.org/teaching`")
  }
  
  ## Retrieve URL domain
  link0 <- sprintf("https://%s", urltools::domain(link))
  
  ## Generalize link to allow for multiple pages
  if (!grepl("page=", link)){
    
    link <- sprintf("%s%spage=%s", 
                    link, 
                    ifelse(grepl("\\?", link), "&", "?"),
                    "%g")
    
  } else if (!grepl("page=%g|page=%s", link)){
    
    ## If page= exists but as a number, replace page=1 (for example) with page=%g
    link <- gsub("page=\\d+", "page=%g", link)
  }
  
  
  ## Loop through pages and compile all title links on page(s), starting with page i=1
  cli::cli_alert_info("retrieving links to all titles")
  
  i <- 1
  title_links <- c()
  
  repeat({
    
    cli::cli_alert("reading page {i}", .envir = environment())
    
    ## Retrieve links to each sermon title from HTML elements
    title_linksi <- rvest::read_html(gsub("page=%g|page=%s", sprintf("page=%g", i), link)) %>%
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
             Date = as.Date(Date, format = "%m/%d/%Y"),
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
  
  return(metadf)
}



#' Retrieve sermon metadata from hillside.org
#' 
#' @param subtitle_pattern Character value of pattern to match in subtitle.
#' @param filename Target directory to save metadata data.frame as a .csv file.
#' @param n_cores Numeric value specifying number of cores.
#' @return None.

hillside_org <- function(subtitle_pattern = "Chris Gee",
                         filename = file.path(getwd(), "catalog", sprintf("hillside_org.csv")),
                         n_cores = 1){
  
  require(dplyr)
  
  
  ## Create folder if needed
  folder <- dirname(filename)
  
  if (!dir.exists(folder)){
    
    dir.create(folder)
  }
  
  ## Retrieve URL domain
  link <- "https://subsplash.com/+62a6/media?page=%g"
  
  
  ## Loop through pages and compile all title links on page(s), starting with page i=1
  cli::cli_alert_info("retrieving links to all titles")
  
  i <- 1
  title_links <- c()
  
  repeat({
    
    cli::cli_alert("reading page {i}", .envir = environment())
    
    ## Retrieve links to each sermon title from HTML elements
    title_linksi <- rvest::read_html(gsub("page=%g|page=%s", sprintf("page=%g", i), link)) %>%
      rvest::html_elements(css = ".app-list-content") %>%
      rvest::html_children()
    
    ## Exit if empty page
    if (length(title_linksi) == 0){
      
      cli::cli_alert("page {i} is empty", .envir = environment())
      
      break
    }
    
    ## Subset to links with subtitle text matching pattern and extract URLs
    title_linksi <- title_linksi %>%
      `[`(grepl(subtitle_pattern, sapply(., function(x){
        x %>% rvest::html_elements(".kit-list-item__subtitle") %>% rvest::html_text()
      }))) %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href") %>%
      `[`(grepl("subsplash", .))
    
    ## Add to title_links
    title_links <- c(title_links, title_linksi)
    
    i <- i + 1
  })
  
  
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
    
    ## Extract element that contains "audio"
    parsed_data <- pagex %>% rvest::html_element(xpath = "//*[contains(text(), 'audio')]") %>% rvest::html_text() %>%
      ## Remove escape characters
      gsub("\\\\", "", .) %>%
      ## Extract JSON data and convert
      regmatches(., regexpr("\\{\"data\":\\{.*\\}\\}", .)) %>%
      jsonlite::fromJSON(.) %>%
      `$`(data)
    
    title <- gsub("u0026", "&", parsed_data$title)
    
    cli::cli_alert("reading {x}. {title}", .envir = environment())
    
    ## Assemble metadata
    metadatax <- data.frame(
      Title = title,
      Teacher = parsed_data$speaker, 
      Text = paste(sapply(parsed_data$scriptures, simplify_range), collapse = "; "),
      Ministry = parsed_data$`_embedded`$`media-series`$title,
      ## Extract topics
      Topics = parsed_data$tags %>% 
        `[`(grepl("topic:", .)) %>% 
        gsub("topic:", "", .) %>% 
        paste(collapse = "; "),
      Date = as.Date(parsed_data$date, format = "%Y-%m-%dT%H:%M:%SZ"),
      Source = "www.hillside.org",
      Page = linkx,
      Audio = parsed_data$`_embedded`$audio$`_embedded`$`audio-outputs`$`_links`$related$href,
      Files = parsed_data$`_embedded`$document$`_links`$related %>%
        c(unlist(parsed_data[grepl("o-hySVW-k0", parsed_data)])) %>%
        paste(collapse = "; ")
    ) %>%
      mutate(Name = sermon_filename(
        title = Title,
        text = ifelse(grepl("; ", Text) | length(Text) == 0, "Selected Scriptures",
                      simplify_range(Text)),
        teacher = Teacher,
        date = Date
      ))
    
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
                                         simplify_range = simplify_range,
                                         get_common_pre = get_common_pre,
                                         AVAIL_AUDIO_EXT = AVAIL_AUDIO_EXT)) %>%
    do.call(bind_rows, .)
  
  
  ## Write metadata data.frame as a .csv file
  write.csv(x = metadf, file = filename, row.names = TRUE)
  
  return(metadf)
}



#' Retrieve sermon metadata from citylightbible.org
#' 
#' @param link Character value specifying link with search list of to scrape.
#' @param filename Target directory to save metadata data.frame as a .csv file.
#' @param n_cores Numeric value specifying number of cores.
#' @return None.

citylightbible_org <- function(link = "https://subsplash.com/+fvkd/search?q=chris%20gee",
                               filename = file.path(getwd(), "catalog", sprintf("citylightbible_org.csv")),
                               n_cores = 1){
  
  require(dplyr)
  
  
  ## Create folder if needed
  folder <- dirname(filename)
  
  if (!dir.exists(folder)){
    
    dir.create(folder)
  }
  
  ## Check link
  if (!grepl("subsplash.com/\\+fvkd", link)){
    
    cli::cli_abort("link must begin with `subsplash.com/\\+fvkd`")
  }
  
  ## Retrieve URL domain
  link0 <- sprintf("https://%s", urltools::domain(link))
  
  ## Generalize link to allow for multiple pages
  if (!grepl("page=", link)){
    
    link <- sprintf("%s%spage=%s", 
                    link, 
                    ifelse(grepl("\\?", link), "&", "?"),
                    "%g")
    
  } else if (!grepl("page=%g|page=%s", link)){
    
    ## If page= exists but as a number, replace page=1 (for example) with page=%g
    link <- gsub("page=\\d+", "page=%g", link)
  }
  
  
  ## Loop through pages and compile all title links on page(s), starting with page i=1
  cli::cli_alert_info("retrieving links to all titles")
  
  i <- 1
  title_links <- c()
  
  repeat({
    
    cli::cli_alert("reading page {i}", .envir = environment())
    
    ## Retrieve links to each sermon title from HTML elements
    title_linksi <- rvest::read_html(gsub("page=%g|page=%s", sprintf("page=%g", i), link)) %>%
      rvest::html_elements(css = ".app-list-content") %>%
      rvest::html_children() %>%
      rvest::html_elements("a") %>%
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
      rvest::html_element('meta[property="og:title"]') %>%
      rvest::html_attr("content")
    
    cli::cli_alert("reading {x}. {title}", .envir = environment())
    
    split_subtitle <- pagex %>%
      ## Get and clean subtitle
      rvest::html_element('div[class="u__mt--m"]') %>%
      rvest::html_text() %>%
      gsub("\\n", "", .) %>%
      trimws() %>%
      ## Split subtitle by separator into 
      iconv(to = "ASCII//TRANSLIT") %>%
      gsub(" \\. ", "___", .) %>%
      strsplit(split = "___") %>%
      `[[`(1) %>%
      trimws()
    
    ## Convert to a data.frame object
    metadatax <- c(title, split_subtitle) %>%
      matrix(nrow = 1) %>%
      as.data.frame()
    names(metadatax) <- c("Title", c("Date", "Teacher", "Text")[seq_len(length(split_subtitle))])
    
    ## If Text is missing
    if (is.null(metadatax$Text)){
      
      metadatax$Text <- "Selected Scriptures"
    }
    ## Check for e.g. Genesis 1:1-2, 4
    if (grepl(", \\d", metadatax$Text)){
  
      ## TODO: Pay attention when replacing , with ;    
      browser()
    }
    
    ## Add additional metadata
    metadatax <- metadatax %>%
      mutate_all(iconv, to = "ASCII//TRANSLIT") %>%
      mutate(Text = gsub(", ", "; ", Text),
             Description = pagex %>%
               rvest::html_element('meta[property="og:description"]') %>%
               rvest::html_attr("content"),
             Source = "www.citylightbible.org",
             Date = as.Date(Date, format = "%B %d, %Y"),
             Page = linkx,
             Audio = pagex %>%
               rvest::html_element("source") %>%
               rvest::html_attr("src")) %>%
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
  
  return(metadf)
}
