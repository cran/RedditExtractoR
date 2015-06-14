#' Returns relevant reddit URLs
#' 
#' @examples
#' example_urls = reddit_urls(search_terms="science")
#' 
#' @param search_terms A string of terms to be searched on Reddit.
#' @param regex_filter An optional regular expression filter that will remove URLs with titles that do not match the condition.
#' @param subreddit An optional character string that will restrict the search to the specified subreddit.
#' @param cn_threshold Comment number threshold that remove URLs with fewer comments that cn_threshold. 0 by default.
#' @param page_threshold Page threshold that controls the number of pages is going to be searched for a given search word. 1 by default.
#' @param sort_by Sorting parameter, either "comments" (default) or "new".
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with URLs (links), number of comments (num_comments), title (title),date (date) and subreddit (subreddit).
#' @export

reddit_urls = function(search_terms=NA,
                       regex_filter="",
                       subreddit=NA,
                       cn_threshold=0,
                       page_threshold=1,
                       sort_by="comments",
                       wait_time=2){
  
  if(!grepl("^comments$|^new$",sort_by)){stop("sort_by must be either 'new' or 'comments'")}
  
  sterms       = ifelse(is.na(search_terms),NA,gsub("\\s","+",search_terms))
                        
  cached_links = data.frame(date = as.Date(character()), 
                            num_comments = numeric(), 
                            title = character(), 
                            subreddit=character(),
                            URL = character())
  
  subreddit      = ifelse(is.na(subreddit),"",paste0("r/",subreddit,"/"))
  sterms         = ifelse(is.na(sterms),"",paste0("q=",sterms,"&"))
  sterms_prefix  = ifelse(sterms=="","new","search")
  
  search_address = search_query = paste0("http://www.reddit.com/",subreddit,sterms_prefix,".json?",sterms,"sort=",sort_by)

  next_page      = index = ""
  page_counter   = 0
  comm_filter    = 10000
  
  while(is.null(next_page)==FALSE & page_counter < page_threshold & comm_filter >= cn_threshold & length(index)>0){
    
    search_JSON  = tryCatch(RJSONIO::fromJSON(readLines(search_query, warn = FALSE)), error = function(e) NULL)
    
    if(is.null(search_JSON)){stop("Cannot connect to the website")}
    
    else{
      
      contents      = search_JSON[[2]]$children

      search_permalink    = paste0("http://www.reddit.com",sapply(seq(contents),function(x)contents[[x]]$data$permalink))
      search_num_comments = sapply(seq(contents),function(x)contents[[x]]$data$num_comments)
      search_title        = sapply(seq(contents),function(x)contents[[x]]$data$title)
      search_score        = sapply(seq(contents),function(x)contents[[x]]$data$score)
      search_subreddit    = sapply(seq(contents),function(x)contents[[x]]$data$subreddit)
      
      index = which(search_num_comments >= cn_threshold & grepl(regex_filter,search_title,ignore.case=T,perl=T))
      
      if(length(index)>0){
        
        search_date  = format(as.Date(as.POSIXct(unlist(lapply(seq(contents),function(x)contents[[x]]$data$created_utc)),origin="1970-01-01")),"%d-%m-%y")
        
        
        temp_dat     = data.frame(date         = search_date,
                                  num_comments = search_num_comments, 
                                  title        = search_title, 
                                  subreddit    = search_subreddit,
                                  URL          = search_permalink
        )[index,]
        
        cached_links = as.data.frame(rbind(cached_links,temp_dat))
        
        next_page    = search_JSON$data$after
        comm_filter  = utils::tail(search_num_comments,1)
        search_query = paste0(search_address,"&after=",next_page)
        page_counter = page_counter + 1
        
      }
      Sys.sleep(min(2,wait_time))
    }
  }
  
  final_table = cached_links[!duplicated(cached_links),]
  
  if(dim(final_table)[1]==0){
    cat(paste("\nNo results retrieved, check your query"))} else{
      
      remove_row  = which(final_table[,1]=="")
      
      if(length(remove_row)>0){final_table = final_table[-remove_row,]}
      
      return(final_table)
      
    }
}



#' Extract data attributes
#' 
#' @examples
#' \dontrun{
#' example_attr = reddit_content(URL="reddit.com/r/gifs/comments/39tzsy/whale_watching")
#' }
#' 
#' @param URL a string or a vector of strings with the URL address of pages of interest
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with post / thread date (post_date), comment date (comm_date), number of comments within a post / thread (num_comments), subreddit (subreddit)
#' upvote proportion (upvote_prop), post /thread score (post_score), author of the post / thread (author), user corresponding to the comment (user),
#' comment score (comment_score), controversiality (controversiality), comment (comment), title (title), post / thread text (post_text), URL referenced (link)
#' domain of the references URL (domain)
#' @export


reddit_content = function(URL,wait_time=2){
  
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
  
  
  pb = utils::txtProgressBar(min = 0, max = length(URL), style = 3)
  
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
      
      if(min(length(meta.node),length(main.node))>0){
        
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
      
    }
    
    utils::setTxtProgressBar(pb, i)
    
    Sys.sleep(min(2,wait_time))
  }
  
  close(pb)
  
  return(data_extract)
  
}

#' Get all data attributes from search query
#' 
#' @examples
#' \dontrun{
#' example_data = get_reddit(search_terms="economy")
#' }
#' 
#' @param search_terms A string of terms to be searched on Reddit.
#' @param regex_filter An optional regular expression filter that will remove URLs with titles that do not match the condition.
#' @param subreddit An optional character string that will restrict the search to the specified subreddit.
#' @param cn_threshold Comment number threshold that remove URLs with fewer comments that cn_threshold. 0 by default.
#' @param page_threshold Page threshold that controls the number of pages is going to be searched for a given search word. 1 by default.
#' @param sort_by Sorting parameter, either "comments" (default) or "new".
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with post / thread date (post_date), comment date (comm_date), number of comments within a post / thread (num_comments), subreddit (subreddit)
#' upvote proportion (upvote_prop), post /thread score (post_score), author of the post / thread (author), user corresponding to the comment (user),
#' comment score (comment_score), controversiality (controversiality), comment (comment), title (title), post / thread text (post_text), URL referenced (link)
#' domain of the references URL (domain)
#' @export

get_reddit = function(search_terms=NA,
                      regex_filter="",
                      subreddit=NA,
                      cn_threshold=1,
                      page_threshold=1,
                      sort_by="comments",
                      wait_time=2){
  
  URL = unique(as.character(reddit_urls(search_terms,
                                        regex_filter,
                                        subreddit,
                                        cn_threshold,
                                        page_threshold,
                                        sort_by,
                                        wait_time)$URL))
  
  retrieved_data = reddit_content(URL,wait_time)
  
  return(retrieved_data)
  
}
