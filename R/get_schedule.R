################################  Get Schedule #################################
#' Get Team Schedule
#'
#' Gets team schedule for requested season.
#'
#' @param team Team to get schedule for
#' @param season Season to get schedule for. In form "2019-20". Default equals current season.
#' @return A data frame of the team's schedule for requested season
#' @export
get_schedule <- function(team, season = current_season) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }

  ### Scrape Team Schedule
  base_url <- "https://www.espn.com/mens-college-basketball/team/schedule/_/id/"
  if(season == current_season) {
    url <- paste0(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team])
  } else {
    url <- paste0(base_url, ids$id[ids$team == team], "/season/", as.numeric(substring(season, 1, 4)) + 1)
  }
  schedule <-  XML::readHTMLTable(RCurl::getURL(url))
  if(length(schedule) == 0) {
    stop(paste0("No team schedule available for ", team, " / ", season,
                ". Current ESPN season = \"2020-21\". If you are trying to find the most recent season (2019-20),",
                " please  supply season = \"2019-20\" argument."))
  }
  schedule <- schedule[[1]][-1,]
  schedule <- schedule[,1:4]
  names(schedule) <- c("date", "opponent", "result", "record")
  schedule <- schedule[!is.na(schedule$opponent) & schedule$opponent != "Opponent" & schedule$opponent != "OPPONENT",]
  rm_ids <- which(schedule$result %in% c("Postponed", "Cancelled", "Canceled"))
  schedule <- schedule[schedule$result != "Postponed",]
  schedule <- schedule[schedule$result != "Cancelled",]
  schedule <- schedule[schedule$result != "Canceled",]

  ### Locations
  schedule$location <- ifelse(grepl("[*]", schedule$opponent), "N",
                              ifelse(grepl("^vs", schedule$opponent), "H", "A"))

  ### Clean Opponent Names
  schedule$opponent <- gsub("^vs", "", schedule$opponent)
  schedule$opponent <- gsub("[@#*()]", "", schedule$opponent)
  schedule$opponent <- gsub("[0-9]*", "", schedule$opponent)
  schedule$opponent <- stripwhite(gsub("^ ", "", schedule$opponent))

  ### Scores/Results
  schedule$result[grep(":", schedule$result)] <- NA
  schedule$result[grep("TBD", schedule$result)] <- NA
  scores <- unlist(sapply(gsub("[A-z]*", "", schedule$result), strsplit, "-"))
  scores <- gsub("\\s.*", "", scores)
  team_scores <- suppressWarnings(as.numeric(scores[seq(1, length(scores), 2)]))
  opp_scores <- suppressWarnings(as.numeric(scores[seq(2, length(scores), 2)]))
  schedule <- dplyr::mutate(schedule, team_score = NA, opp_score = NA)
  schedule$team_score[1:length(team_scores)] <- team_scores
  schedule$opp_score[1:length(opp_scores)] <- opp_scores
  index <- grepl("L", schedule$result)
  tmp <- schedule$team_score[index]
  schedule$team_score[index] <- schedule$opp_score[index]
  schedule$opp_score[index] <- tmp

  ### Dates
  schedule$day <- as.numeric(gsub("[^0-9]*", "", schedule$date))
  schedule$month <- substring(schedule$date, 6, 8)
  schedule$month[schedule$month == "Nov"] <- 11
  schedule$month[schedule$month == "Dec"] <- 12
  schedule$month[schedule$month == "Jan"] <- 1
  schedule$month[schedule$month == "Feb"] <- 2
  schedule$month[schedule$month == "Mar"] <- 3
  schedule$month[schedule$month == "Apr"] <- 4
  schedule$month <- as.numeric(schedule$month)
  yy <- as.numeric(substring(season, 3, 4))
  schedule$year <- ifelse(schedule$month <= 4, yy + 1, yy)
  schedule$date <- paste(schedule$month, schedule$day, schedule$year, sep = "/")

  ### Game IDs
  schedule$date <- as.Date(schedule$date, "%m/%d/%y")
  schedule <- dplyr::arrange(schedule, date)
  if(length(rm_ids) > 0) {
    schedule$game_id <- get_game_ids(team, season)[-rm_ids]
  } else {
    schedule$game_id <- get_game_ids(team, season)
  }

  ### Return Schedule
  return(schedule[,c("game_id", "date", "opponent", "location",
                     "team_score", "opp_score", "record" )])
}
