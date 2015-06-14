#' Obtain URLs from which information will be extracted
#' 
#' Takes search terms and a range of conditions and returns reddit URLs to relevant links
#' 
#' @examples
#' example_urls = ObtainRedditURLs(search_terms="science")
#' 
#' @param search_terms A string of terms to be searched on Reddit
#' @param regex_filter An optional regular expression filter that will remove URLs with titles that do not match the condition.
#' @param cn_threshold Comment number threshold that remove URLs with fewer comments that cn_threshold. 10 by default.
#' @param page_threshold Page threshold that controls the number of pages is going to be searched for a given search word. 100 by default.
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with URLs (links), number of comments (num_comments), title (title) and date (date).
#' @export

ObtainRedditURLs = function(search_terms,
                            regex_filter="",
                            cn_threshold=50,
                            page_threshold=1,
                            wait_time=2){
  
  if(is.null(search_terms) | is.na(search_terms) | length(search_terms)==0 | !is.character(search_terms)){stop("invalid search terms")}
  
  sterms       = gsub("\\s","+",search_terms)
  cached_links = data.frame(date = as.Date(character()), 
                            num_comments = numeric(), 
                            title = character(), 
                            URL = character())
  
    
    search_address = paste0("http://www.reddit.com/search.json?q=",sterms,"&sort=comments")
    search_query   = search_address
    next_page      = index = ""
    page_counter   = 0
    comm_filter    = 10000
    
    while(is.null(next_page)==FALSE & page_counter < page_threshold & comm_filter >= cn_threshold & length(index)>0){
      
      search_JSON  = tryCatch(RJSONIO::fromJSON(readLines(search_query, warn = FALSE)), error = function(e) NULL)
      
      if(is.null(search_JSON)){stop("Cannot connect to the website")}
      
      else{
        
        contents     = search_JSON[[2]]$children
        
        search_links = unlist(lapply(seq(contents),function(x)contents[[x]]$data$permalink))
        search_comms = unlist(lapply(seq(contents),function(x)contents[[x]]$data$num_comments))
        search_title = unlist(lapply(seq(contents),function(x)contents[[x]]$data$title))
        search_date  = format(as.Date(as.POSIXct(unlist(lapply(seq(contents),function(x)contents[[x]]$data$created_utc)),origin="1970-01-01")),"%d-%m-%y")
        
        
        index = which(search_comms > cn_threshold & grepl(regex_filter,search_title,ignore.case=T,perl=T))
        
        if(length(index)>0){
          
          search_links = paste0("http://www.reddit.com",search_links[index])
          search_comms = search_comms[index]
          search_title = search_title[index]
          search_date  = search_date[index]
          
          temp_dat     = data.frame(date         = search_date,
                                    num_comments = search_comms, 
                                    title        = search_title, 
                                    URL          = search_links)
          
          cached_links = as.data.frame(rbind(cached_links,temp_dat))
          
          next_page    = search_JSON$data$after
          comm_filter  = tail(search_comms,1)
          search_query = paste0(search_address,"&after=",next_page)
          page_counter = page_counter + 1
          
        }
        
        Sys.sleep(min(2,wait_time))
        
      }
    }
  
  final_table = cached_links[!duplicated(cached_links),]
  
  if(dim(final_table)[1]==0){
    cat(paste("\nWarning: no results retrieved"))} else{
      
      remove_row  = which(final_table[,1]=="")
      
      if(length(remove_row)>0){final_table = final_table[-remove_row,]}
      
      return(final_table)
      
    }
}



#' Extract data attributes
#' 
#' @examples
#' \dontrun{
#' example_attr = ExtractAttributes(URL="reddit.com/r/gifs/comments/39tzsy/whale_watching")
#' }
#' 
#' @param URL a string or a vector of strings with the URL address of pages of interest
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with post / thread date (post_date), comment date (comm_date), number of comments within a post / thread (num_comments), subreddit (subreddit)
#' upvote proportion (upvote_prop), post /thread score (post_score), author of the post / thread (author), user corresponding to the comment (user),
#' comment score (comment_score), controversiality (controversiality), comment (comment), title (title), post / thread text (post_text), URL referenced (link)
#' domain of the references URL (domain)
#' @export


ExtractAttributes = function(URL,wait_time=2){
  
  if(is.null(URL) | length(URL)==0 | !is.character(URL)){stop("invalid URL parameter")}
  
  # setting up a function for extraction of comment specific information:
  GetAttribute  = function(node,feature){
    
    Attribute   = node$data[[feature]]
    replies     = node$data$replies
    reply.nodes = if (is.list(replies)) replies$data$children else NULL
    
    return(list(Attribute, lapply(reply.nodes,function(x){GetAttribute(x,feature)})))  
    
  }
  
  # setting up the data frame
  data_extract = data.frame(post_date        = as.Date(character()),
                            comm_date        = as.Date(character()),
                            num_comments     = numeric(),
                            subreddit        = character(),
                            upvote_prop      = numeric(),
                            post_score       = numeric(),
                            author           = character(),
                            user             = character(),
                            comment_score    = numeric(),
                            controversiality = numeric(),
                            comment          = character(),
                            title            = character(),
                            post_text        = character(),
                            link             = character(),
                            domain           = character())
  
  if(requireNamespace("tcltk",quietly=TRUE)){
    pb = tcltk::tkProgressBar(title = "Feature Extraction", min = 0, max = length(URL), width = 300)
  }
  for(i in seq(URL)){
    
    X        = paste0(gsub("\\?ref=search_posts$","",URL[i]),".json?limit=500") # 500 is the maximum
    raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)),error = function(e) NULL)
    
    # try again if it fails
    if(is.null(raw_data)){
      Sys.sleep(min(1,wait_time))
      raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)),error = function(e) NULL)
    }
    
    if(is.null(raw_data)==FALSE){
      
      # extracting comment specific information:
      meta.node     = raw_data[[1]]$data$children[[1]]$data
      main.node     = raw_data[[2]]$data$children
      
      TEMP          = as.data.frame(cbind(post_date        = format(as.Date(as.POSIXct(meta.node$created_utc,origin="1970-01-01")),"%d-%m-%y"),
                                          comm_date        = format(as.Date(as.POSIXct(unlist(lapply(main.node, function(x){GetAttribute(x,"created_utc")})),
                                                                                       origin="1970-01-01")),"%d-%m-%y"),
                                          num_comments     = meta.node$num_comments,
                                          subreddit        = meta.node$subreddit,
                                          upvote_prop      = meta.node$upvote_ratio,
                                          post_score       = meta.node$score,
                                          author           = meta.node$author,
                                          user             = unlist(lapply(main.node, function(x){GetAttribute(x,"author")})),
                                          comment_score    = unlist(lapply(main.node, function(x){GetAttribute(x,"score")})),
                                          controversiality = unlist(lapply(main.node, function(x){GetAttribute(x,"controversiality")})),
                                          comment          = unlist(lapply(main.node, function(x){GetAttribute(x,"body")})),
                                          title            = meta.node$title,
                                          post_text        = meta.node$selftext,
                                          link             = meta.node$url,
                                          domain           = meta.node$domain))
      
      if(dim(TEMP)[1]>0 & dim(TEMP)[2]>0) data_extract <- rbind(TEMP,data_extract)
      else print(paste("missed",i,":",URL[i]))
                 
    }
    
    if(requireNamespace("tcltk",quietly=TRUE)){
      tcltk::setTkProgressBar(pb, i, label=paste0(i," of ",length(URL)," (",round(i/length(URL)*100, 0),"%) done"))
    }
    
    Sys.sleep(min(2,wait_time))
  }
  
  if(requireNamespace("tcltk",quietly=TRUE)){
    close(pb)
  }
  
  return(data_extract)
  
}

#' Get all data attributes from search query (wrapper for a combination of ObtainRedditURLs and ExtractAttributes functions)
#' 
#' @examples
#' \dontrun{
#' example_data = GetReddit(search_terms="economy")
#' }
#' 
#' @param search_terms A string of terms to be searched on Reddit
#' @param regex_filter An optional regular expression filter that will remove URLs with titles that do not match the condition.
#' @param cn_threshold Comment number threshold that remove URLs with fewer comments that cn_threshold. 10 by default.
#' @param page_threshold Page threshold that controls the number of pages is going to be searched for a given search word. 100 by default.
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with post / thread date (post_date), comment date (comm_date), number of comments within a post / thread (num_comments), subreddit (subreddit)
#' upvote proportion (upvote_prop), post /thread score (post_score), author of the post / thread (author), user corresponding to the comment (user),
#' comment score (comment_score), controversiality (controversiality), comment (comment), title (title), post / thread text (post_text), URL referenced (link)
#' domain of the references URL (domain)
#' @export

GetReddit = function(search_terms,
                     regex_filter="",
                     cn_threshold=50,
                     page_threshold=1,
                     wait_time=2){
  
  URL = unique(as.character(ObtainRedditURLs(search_terms,
                                             regex_filter="",
                                             cn_threshold=50,
                                             page_threshold=1,
                                             wait_time=2)$URL))
  
  retrieved_data = ExtractAttributes(URL)
  
  return(retrieved_data)
  
}
