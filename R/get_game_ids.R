######################### Get Game IDs ########################################
#' Get Team game_ids
#'
#' Gets team game_ids for current season.
#'
#' @param team Team for which to get game_ids
#' @param season Season for which to get schedule. In form "2019-20". Default equals current season.
#' @return A vector of the team's ESPN game_ids for current season
#' @export
get_game_ids <- function(team, season = current_season) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  
  ### Current Season (get game_ids from team clubhouse)
  if(season == current_season) {
    base_url <- "https://www.espn.com/mens-college-basketball/team/_/id/"
    url <- paste0(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team])
    
    x <- scan(url, what = "", sep = "\n", quiet = T)
    x <- x[grep("club-schedule", x)]
    x <- gsub('\\?gameId=', '/gameId/', x)
    x <- unlist(strsplit(x, "/gameId/"))
    x <- x[-1]
    x <- x[1:(floor(length(x)/2))]
    reg_flag <- grep("<h2>Regular Season</h2>", x)
    game_ids <- substring(x, 1, 9)
    
    ### Move Postseason Games to End of Schedule
    if(length(reg_flag) > 0) {
      game_ids <- c(game_ids[-c(1:reg_flag)], game_ids[1:reg_flag])
    }
    game_ids <- unique(game_ids)
  } else { ### Old Season (Get Them from Schedule)
    base_url <- "https://www.espn.com/mens-college-basketball/team/schedule/_/id/"
    url <- paste0(base_url, ids$id[ids$team == team], "/season/", as.numeric(substring(season, 1, 4)) + 1)
    
    x <- RCurl::getURL(url)
    x <- strsplit(x, 'gameId')[[1]]
    
    game_ids <- gsub('[^0-9]*', '', gsub('\".*', '', x))[-1]
    dates <- stringr::str_extract_all(x, '<span>(Mon|Tue|Wed|Thu|Fri|Sat|Sun),\\s+[A-z]+\\s+\\d+')
    dates <- purrr::map(dates, ~gsub('[<>]', '', gsub('\\<span\\>', '', .x)))
    game_ids <- game_ids[!duplicated(game_ids)]
    
    played_dates <- purrr::map_chr(dates, dplyr::last)
    played_dates <- played_dates[!is.na(played_dates)]
    
    unplayed_dates <- unlist(purrr::map(dates, ~setdiff(.x, dplyr::last(.x))))
    if(length(unplayed_dates) == 0 & length(game_ids) > length(played_dates)) {
      delta <- length(game_ids) - length(played_dates)
      game_ids <- game_ids[c(1:(length(played_dates) - delta + 1), length(game_ids))]
    } else if(length(unplayed_dates) > 0) {
      game_ids <- 
        unique(c(game_ids[1:length(played_dates)],
                 game_ids[(length(game_ids)-length(unplayed_dates) + 1):length(game_ids)]))
    }
    
    df_id <- dplyr::tibble('game_id' = game_ids,
                           'date' = c(played_dates, unplayed_dates)) 
    df_id <- dplyr::mutate(df_id, 'year' = ifelse(grepl('Nov|Dec', date), 
                                                  substring(season, 1, 4),
                                                  paste0('20', substring(season, 6, 7))))
    df_id <- dplyr::mutate(df_id, 'date' = as.Date(paste(year, date), '%Y %a, %b %d'))
    df_id <- dplyr::arrange(df_id, date)
    
    game_ids <- df_id$game_id
  }
  
  return(game_ids)
}




