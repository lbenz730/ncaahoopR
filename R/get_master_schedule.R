####################### Function To Get a Schedule by Date #####################
#' Get Master Schedule
#'
#' Gets schedule for all games on a given date
#'
#' @param date Date for which to get schedule (YYYY-MM-DD)
#' @return A data frame of the day's schedule of games
#' @export
get_master_schedule <- function(date) {
  ### Error Testing
  if(is.na(date)) {
    stop("date is missing with no default")
  }
  
  tmp <- try(as.Date(date))
  if(class(tmp) == "try-error") {
    stop("Please enter valid date in the form YYYY-MM-DD")
  }
  
  
  date_ <- gsub("-", "", as.character(date))
  url <- paste0("https://www.espn.com/mens-college-basketball/schedule/_/date/", date_)
  
  z <- XML::readHTMLTable(RCurl::getURL(url))
  if(length(z) > 1) {
    schedule <- as.data.frame(z[[1]])[,c(1,2)]
    completed <- as.data.frame(z[[2]][-1,1:3])
    names(completed) <- c("away", "home", "result")
    names(schedule) <- c("away", "home")
  } else {
    ### No Games Scheduled
    if(z[[1]][1,1] == "No games scheduled") {
      cat("No games on", as.character(date), "\n")
      return(NULL)
    }
    completed <- as.data.frame(z[[1]][,1:3])
    names(completed) <- c("away", "home", "result")
    schedule <- NA
  }
  
  n_canceled <- sum(grepl("Canceled", completed$result))
  n_postponed <- sum(grepl("Postponed", completed$result))
  completed <- dplyr::filter(completed, result != "Canceled", result != "Postponed")
  
  ### Extract Ranking
  ranking <- function(team) {
    rank <- gsub("[^0-9]", "", team)
    return(ifelse(rank == "", NA, rank))
  }
  
  ### Clean Team Name
  clean <- function(team) {
    team <- gsub('Â', '', team)
    team <- gsub('Ã©','é', team) 
    team <- gsub('@','', team) 
    team <- gsub("[#0-9]", "", team)
    # team <- gsub("\\s[A-Z]*-*[A-Z]*$", "", team)
    team <- gsub("TA&M", "", team)
    team <- gsub("W&M", "", team)
    team <- gsub("\\s*$", "", gsub("^\\s*", "", team))
    return(team)
  }
  
  if(any(!is.na(schedule[1]))) {
    schedule <- dplyr::mutate(schedule,
                              "away" = as.character(sapply(schedule$away, clean)),
                              "home" = as.character(sapply(schedule$home, clean)),
                              "away_rank" = as.numeric(sapply(schedule$away, ranking)),
                              "home_rank" = as.numeric(sapply(schedule$home, ranking)),
                              "away_score" = NA,
                              "home_score" = NA)
  }
  
  x <- RCurl::getURL(url)
  in_progress <- strsplit(x, "/mens-college-basketball/game\\?gameId=")[[1]]
  in_progress <- suppressWarnings(as.numeric(unname(sapply(in_progress, function(y){ substring(y, 1, 9) }))))
  in_progress <- in_progress[-1]
  in_progress <- in_progress[!is.na(in_progress) & !duplicated(in_progress)]
  
  x <- strsplit(x, "/mens-college-basketball/game/_/gameId/")
  x <- suppressWarnings(as.numeric(unname(sapply(x, function(y){ substring(y, 1, 9) }))))
  x <- x[-1]
  # if(date == Sys.Date()) {
  #   x <- c(in_progress, x)
  # }
  x <- c(in_progress, x)
  x <- x[!is.na(x) & !duplicated(x)]
  x <- x[1:(length(x) - n_canceled - n_postponed)]
  
  ### Add in Completed Games
  find_anchor <- function(team) {
    cleaned <- clean(team)
    team <- gsub("[#0-9]", "", team)
    team <- gsub("\\s*$", "", gsub("^\\s*", "", team))
    anchor <- unlist(strsplit(team, ""))[-c(1:(nchar(cleaned) + 1))]
    return(paste0(anchor, collapse = ""))
  }
  
  completed <- dplyr::mutate(completed,
                             "away" = as.character(sapply(away, clean)),
                             "home" = as.character(sapply(home, clean)),
                             "result" = as.character(result),
                             "away_rank" = as.numeric(sapply(completed$away, ranking)),
                             "home_rank" = as.numeric(sapply(completed$home, ranking)),
                             "away_anchor" = sapply(completed$away, find_anchor),
                             "away_score" = NA,
                             "home_score" = NA)
  
  winners <- unname(sapply(completed$result, function(y) { gsub("\\s[0-9]*.*", "", y) }))
  scores <- as.numeric(gsub("[^0-9]", "", gsub("\\(.*\\)", "", unlist(strsplit(completed$result, ",")))))
  
  if(length(scores) > 0) {
    winning_scores <- scores[seq(1, length(scores) - 1, 2)]
    losing_scores <- scores[seq(2, length(scores), 2)]
    
    index <- sapply(completed$away_anchor, function(y) { y %in% winners })
    completed$home_score[index] <- losing_scores[index]
    completed$home_score[!index] <- winning_scores[!index]
    completed$away_score[!index] <- losing_scores[!index]
    completed$away_score[index] <- winning_scores[index]
  }
  
  if(any(!is.na(schedule[1]))) {
    schedule <- rbind(schedule, dplyr::select(completed, -away_anchor, -result))
  }else{
    schedule <- completed
  }
  
  schedule <- dplyr::filter(schedule, !is.na(home), !is.na(away))
  schedule <- dplyr::mutate(schedule, "game_id" = x[1:nrow(schedule)])
  schedule <- dplyr::select(schedule, game_id, away, home, away_rank, home_rank, away_score, home_score)
  schedule$date <- as.Date(date)
  
  return(schedule)
}
