#' Get Shot Locations
#'
#' Extracts Shot location data for a specific game
#'
#' @param game_ids Vector of ESPN game_ids
#' @return A data frame with shot details (including location) for given game_ids
#' @export
get_shot_locs <- function(game_ids) {
  if(any(is.na(game_ids))) {
    error("game_ids missing with no default")
  }
  n <- length(game_ids)
  for(i in 1:n) {
    message(paste("Getting Shots for Game", i, "of", n))
    url <- paste0('https://www.espn.com/mens-college-basketball/playbyplay?gameId=', game_ids[i])
    
    ### Try and get PBP data
    txt <- try(RCurl::getURL(url), silent = T)
    
    ### Check if PBP Data is Available
    if(class(txt) == "try-error") {
      message("No shot location data available for this game.")
      next
    } else {
      x <- strsplit(txt, '"shtChrt":\\{\"plays\":')[[1]]
      if(length(x) > 1) {
        x <- x[2]
        tmp <- jsonlite::fromJSON(gsub(',\"tms\":\\{.*', '', x))
      } else {
        message("No shot location data available for this game.")
        next 
      }
    }
    
    date <- gsub('^.*-\\s+', '', gsub('\\|.*$', '', strsplit(txt, 'Men&#x27;s College Basketball Play-By-Play')[[1]][2]))
    date <- as.Date(stripwhite(date), '%b %d, %Y')
    
    info <- jsonlite::fromJSON(gsub(',"ntrlSte.*$', '', gsub('^.*,\"tms\":', '', txt)))
    
    total_df <- 
      data.frame('game_id' = game_ids[i],
                 'date' = date,
                 'shot_text' = tmp$text,
                 'x' = tmp$coordinate$x,
                 'y' = tmp$coordinate$y,
                 stringsAsFactors = F) %>% 
      dplyr::mutate("outcome" = ifelse(grepl("made", shot_text), "made", "missed"),
                    "shooter" = stripwhite(gsub("made.*", "", shot_text)),
                    "shooter" = stripwhite(gsub("missed.*", "", shooter)),
                    "assisted" = stripwhite(gsub(".{1}$", "", gsub(".*Assisted by", "", shot_text))),
                    "assisted" = stripwhite(ifelse(grepl("made", assisted) |
                                                     grepl("missed", assisted), NA, assisted)),
                    "three_pt" = grepl("Three Point", shot_text)) %>% 
      dplyr::mutate('x' = ifelse(tmp$homeAway == 'away', 50 - x, x),
                    'y' = ifelse(tmp$homeAway == 'away', 94 - y, y)) %>% 
      dplyr::mutate('color' = paste0('#', ifelse(tmp$homeAway == 'away', info$away$teamColor, info$home$teamColor)),
                    'team_name' = ifelse(tmp$homeAway == 'away', info$away$shortDisplayName, info$home$shortDisplayName))
    
    if(!exists("total_df_all")) {
      total_df_all <- total_df
    }else{
      total_df_all <- rbind(total_df_all, total_df)
    }
  }
  
  if(!exists("total_df_all")) {
    return(NULL)
  }
  return(total_df_all)
}
