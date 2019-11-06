source("R/helpers.R")

############ Function to get PBP Data for a set of ESPN Game IDs ###############
#' Get Game Play-by-Play Data
#'
#' Scrapes ESPN Play-by-Play data for the desired games.
#'
#' @param game_ids Vector of ESPN game-IDs
#' @param win_prob Logical whether to return win probability from home team perspective
#' on each play of the game. Default = F.
#' @return A data-frame of the Play-by-Play data fror desired games.
#' @export
get_pbp_game <- function(game_ids) {
  ### Error Testing
  if(all(is.na(game_ids))) {
    stop("game_ids is missing with no default")
  }

  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  ### Get Play by Play Data
  base_url <- "https://www.espn.com/mens-college-basketball/playbyplay?gameId="
  summary_url <- "https://www.espn.com/mens-college-basketball/game?gameId="
  j <- 0

  for(i in 1:length(game_ids)) {
    message(paste0("Scraping Data for Game: ", i, " of ", length(game_ids)))
    if(is.nit(game_ids[i])) {
      message("NIT Game--Play by Play Data Not Available at this time")
      next
    }
    url <- paste(base_url, game_ids[i], sep = "")
    tmp <- try(XML::readHTMLTable(RCurl::getURL(url)), silent = T)

    ### Check if PBP Data is Available
    if(length(tmp) == 0) {
      message("Play by Play Data Not Available")
      next
    }else if(length(tmp) < ncol(tmp[[1]]) | length(tmp) == 0) {
      message("Play by Play Data Not Available")
      next
    }else{
      t1 <- as.numeric(unlist(strsplit(as.character(tmp[[2]][2,1]), ":")))
      t2 <- as.numeric(unlist(strsplit(as.character(tmp[[2]][5,1]), ":")))
      if(60 * t1[1] + t1[2] < 60 * t2[1] + t2[2]) {
        message("Game In Progress--Play by Play Data Not Available. Please Check Back After the Game")
        next
      }
      j <- j + 1
    }

    n <- length(tmp)

    if(ncol(tmp[[1]]) == 4) {
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 0)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 0)
      pbp <- rbind(half_1, half_2)
    }

    ### 1 OT
    else if(ncol(tmp[[1]]) == 5 & ((n == 6 & ncol(tmp[[5]]) == 4) | (n == 5 & ncol(tmp[[4]]) == 5))) {
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 1)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 1)
      half_3 <- clean(as.data.frame(tmp[[4]]), 3, 1)
      pbp <- rbind(half_1, half_2, half_3)
    }

    ### 2 OT
    else if(ncol(tmp[[1]]) == 5 & ((n == 7 & ncol(tmp[[6]]) == 4) | (n == 6 & ncol(tmp[[5]]) == 5))) {
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 2)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 2)
      half_3 <- clean(as.data.frame(tmp[[4]]), 3, 2)
      half_4 <- clean(as.data.frame(tmp[[5]]), 4, 2)
      pbp <- rbind(half_1, half_2, half_3, half_4)
    }

    ### 3 OT
    else if(ncol(tmp[[1]]) == 5 & ((n == 8 & ncol(tmp[[7]]) == 4) | (n == 7 & ncol(tmp[[6]]) == 5))){
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 3)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 3)
      half_3 <- clean(as.data.frame(tmp[[4]]), 3, 3)
      half_4 <- clean(as.data.frame(tmp[[5]]), 4, 3)
      half_5 <- clean(as.data.frame(tmp[[6]]), 5, 3)
      pbp <- rbind(half_1, half_2, half_3, half_4, half_5)
    }

    ### 4 OT
    else if(ncol(tmp[[1]]) == 5 & ((n == 9 & ncol(tmp[[8]]) == 4) | (n == 8 & ncol(tmp[[7]]) == 5))) {
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 4)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 4)
      half_3 <- clean(as.data.frame(tmp[[4]]), 3, 4)
      half_4 <- clean(as.data.frame(tmp[[5]]), 4, 4)
      half_5 <- clean(as.data.frame(tmp[[6]]), 5, 4)
      half_6 <- clean(as.data.frame(tmp[[7]]), 6, 4)
      pbp <- rbind(half_1, half_2, half_3, half_4, half_5, half_6)
    }

    these <- grep(T, is.na(pbp$home_score))
    pbp[these, c("home_score", "away_score")] <- pbp[these - 1 , c("home_score", "away_score")]

    ### Get full team names
    url2 <- paste(summary_url, game_ids[i], sep = "")
    tmp <- XML::readHTMLTable(RCurl::getURL(url2))
    pbp$away <- as.character(as.data.frame(tmp[[2]])[1,1])
    pbp$home <- as.character(as.data.frame(tmp[[2]])[2,1])
    away_abv <- as.character(as.data.frame(tmp[[1]])[1,1])
    home_abv <- as.character(as.data.frame(tmp[[1]])[2,1])

    ### Get Game Line
    y <- scan(url2, what = "", sep = "\n")
    y <- y[grep("Line:", y)]
    if(length(y) > 0) {
      y <- gsub("<[^<>]*>", "", y)
      y <- gsub("\t", "", y)
      y <- strsplit(y, ": ")[[1]][2]
      line <- as.numeric(strsplit(y, " ")[[1]][2])
      abv <- strsplit(y, " ")[[1]][1]
      if(abv == home_abv) {
        line <- line * -1
      }
    }else {
      line <- NA
    }

    pbp$home_favored_by <- line
    pbp$play_id <- 1:nrow(pbp)
    pbp$game_id <- game_ids[i]
    pbp$date <- get_date(game_ids[i])
    pbp$score_diff <- pbp$home_score - pbp$away_score

    ### Win Probability by Play
    if(is.na(pbp$home_favored_by[1])) {
      pbp$home_favored_by <- get_line(pbp)
    }
    if(!is.na(pbp$home_favored_by[1])){
      pbp$pre_game_prob <- predict(prior, newdata = data.frame(pred_score_diff = pbp$home_favored_by),
                                   type = "response")
    }else{
      pbp$pre_game_prob <- 0.5
    }

    ### Relative Time
    pbp$secs_remaining_relative <- NA
    msec <- max(pbp$secs_remaining)
    for(k in 1:nrow(pbp)) {
      pbp$secs_remaining_relative[k] <-
        secs_to_model(pbp$secs_remaining[k], msec)[2]
    }

    ### Compute Win Prob
    pbp$win_prob <- wp_compute(pbp)

    ### Hardcode to 50-50 if Line = 0 or NA
    if(is.na(pbp$home_favored_by[1]) | pbp$home_favored_by[1] == 0) {
      pbp$win_prob[1] <- 0.5
    }

    ### Time Outs
    timeout <- dplyr::filter(pbp, sapply(pbp$description, grepl, pattern = "Timeout")) %>%
      dplyr::filter(description != "Official TV Timeout")

    timeout$team <- sapply(timeout$description, function(z) gsub("\\s* Timeout", "", z))
    timeout$tmp <- paste(timeout$team, timeout$secs_remaining)
    timeout <- dplyr::filter(timeout, !duplicated(tmp))
    teams <- unique(timeout$team)
    pos_teams <- c(pbp$home[1], pbp$away[1])
    if(nrow(timeout) > 0) {
      home <- pos_teams[which.min(stringdist::stringdist(teams, pbp$home[1]))]
      away <- setdiff(pos_teams, home)
    }else{
      home <- pos_teams[1]
      away <- pos_teams[2]
    }
    pbp$home_time_out_remaining <- 4
    pbp$away_time_out_remaining <- 4
    pbp$home_timeout_ind <- 0
    pbp$away_timeout_ind <- 0
    nplay <- nrow(pbp)
    if(nrow(timeout) > 0) {
      for(j in 1:nrow(timeout)) {
        play_id <- timeout$play_id[j]
        secs_remaining <- timeout$secs_remaining_relative[j]
        half <- timeout$half[j]

        if(timeout$team[j] == home) {
          pbp$home_time_out_remaining[play_id:nplay] <- pbp$home_time_out_remaining[play_id:nplay] - 1
          pbp$home_timeout_ind[pbp$secs_remaining_relative <= secs_remaining & pbp$secs_remaining_relative
                               >= secs_remaining - 60 & pbp$half == half] <- 1
        }else {
          pbp$away_time_out_remaining[play_id:nplay] <- pbp$away_time_out_remaining[play_id:nplay] - 1
          pbp$away_timeout_ind[pbp$secs_remaining_relative <= secs_remaining & pbp$secs_remaining_relative
                               >= secs_remaining - 60 & pbp$half == half] <- 1
        }
      }
    }
    pbp$home_time_out_remaining[pbp$half > 2] <-
      pbp$home_time_out_remaining[pbp$half > 2] + (pbp$half[pbp$half > 2] - 2)
    pbp$away_time_out_remaining[pbp$half > 2] <-
      pbp$away_time_out_remaining[pbp$half > 2] + (pbp$half[pbp$half > 2] - 2)

    if(any(pbp$home_time_out_remaining < 0) | any(pbp$away_time_out_remaining < 0)) {
      pbp$home_time_out_remaining <- pbp$home_time_out_remaining + 2
      pbp$away_time_out_remaining <- pbp$away_time_out_remaining + 2
    }else{
      if(max(pbp$home_time_out_remaining[pbp$half == 2]) < 4) {
        pbp$home_time_out_remaining[pbp$half >= 2] <-
          pbp$home_time_out_remaining[pbp$half >= 2] + 1
      }
      if(max(pbp$away_time_out_remaining[pbp$half == 2]) < 4) {
        pbp$away_time_out_remaining[pbp$half >= 2] <-
          pbp$away_time_out_remaining[pbp$half >= 2] + 1
      }
    }

    ### Play Length
    pbp$play_length <- 0
    pbp$play_length[1:(nrow(pbp)-1)] <-
      pbp$secs_remaining[1:(nrow(pbp)-1)] -
      pbp$secs_remaining[2:nrow(pbp)]

    pbp <- dplyr::select(pbp, -pre_game_prob)
    pbp <- dplyr::select(pbp, play_id, half, time_remaining_half,
                         secs_remaining_relative, secs_remaining, description,
                         home_score, away_score, score_diff, play_length,
                         win_prob, home, away, home_time_out_remaining,
                         away_time_out_remaining, home_timeout_ind,
                         away_timeout_ind, home_favored_by, game_id, date) %>%
      dplyr::rename("secs_remaining_absolute" = secs_remaining,
                    "secs_remaining" = secs_remaining_relative)

    if(!exists("pbp_all")) {
      pbp_all <- pbp
    }
    else{
      pbp_all <- rbind(pbp_all, pbp)
    }
  }

  if(!exists("pbp_all")) {
    pbp_all <- NULL
  }

  return(pbp_all)
}

###################################Get Season Long PBP Data ####################
#' Get Team Play-by-Play Data
#'
#' Scrapes the current season's Play-by-Play data for desired team. Team
#' is assumed to be the ESPN team name, which can be looked up in the ids
#' dataframe.
#'
#' @param team Team to get Play-by-Play data for
#' @return A data-frame of the team's Play-by-Play data for the current season
#' @export
get_pbp <- function(team) {
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

  message(paste("Getting Game IDs: ", team, sep = ""))

  ### Get Game IDs
  game_ids <- get_game_ids(team)

  ### Get PBP Data
  pbp_season <- get_pbp_game(game_ids)

  return(pbp_season)
}

################################  Get Schedule #################################
#' Get Team Schedule
#'
#' Gets team schedule for current season.
#'
#' @param team Team to get schedule for
#' @return A data-frame of the team's schedule for current season
#' @export
get_schedule <- function(team) {
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
  url <- paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  schedule <- XML::readHTMLTable(RCurl::getURL(url))[[1]][-1,]
  schedule <- schedule[,1:4]
  names(schedule) <- c("date", "opponent", "result", "record")
  schedule <- schedule[!is.na(schedule$opponent) & schedule$opponent != "Opponent",]

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
  schedule$year <- ifelse(schedule$month <= 4, 20, 19)
  schedule$date <- paste(schedule$month, schedule$day, schedule$year, sep = "/")

  ### Game IDs
  schedule$date <- as.Date(schedule$date, "%m/%d/%y")
  schedule <- dplyr::arrange(schedule, date)
  schedule$game_id <- get_game_ids(team)

  ### Return Schedule
  return(schedule[,c("game_id", "date", "opponent", "location",
                     "team_score", "opp_score", "record" )])
}

######################### Get Game IDs ########################################
#' Get Team game_ids
#'
#' Gets team game_ids for current season.
#'
#' @param team Team to get game_ids
#' @return A vector of the team's ESPN game_ids for current season
#' @export
get_game_ids <- function(team) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  base_url <- "http://www.espn.com/mens-college-basketball/team/_/id/"
  url <- paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")

  x <- scan(url, what = "", sep = "\n")
  x <- x[grep("club-schedule", x)]
  x <- unlist(strsplit(x, "gameId="))
  x <- x[-1]
  x <- x[1:(floor(length(x)/2))]
  reg_flag <- grep("<h2>Regular Season</h2>", x)

  game_ids <- substring(x, 1, 9)
  if(length(reg_flag) > 0) {
    game_ids <- c(game_ids[-c(1:reg_flag)], game_ids[1:reg_flag])
  }
  game_ids <- unique(game_ids)

  return(game_ids)
}

####################### Function To Get a Team's Roster ########################
#' Get Team Roster
#'
#' Gets team roster for current season.
#'
#' @param team Team to get roster for
#' @return A data-frame of the team's roster for current season
#' @export
get_roster <- function(team) {
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
  base_url <- "https://www.espn.com/mens-college-basketball/team/roster/_/id/"
  url <-  paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  tmp <- try(XML::readHTMLTable(RCurl::getURL(url)))
  if(class(tmp) == "try-error") {
    warning("Unable to get roster. ESPN is updating CBB files. Check back again soon")
    return(NULL)
  }
  tmp <- as.data.frame(tmp[[1]])
  names(tmp) <- c("number", "name", "position", "height", "weight", "class", "hometown")
  for(i in 1:ncol(tmp)) {
    tmp[,i] <- as.character(tmp[,i])
  }
  tmp$number <- as.numeric(gsub("[^0-9]", "", tmp$name))
  tmp$name <- gsub("[0-9]*", "", tmp$name)
  tmp <- dplyr::arrange(tmp, number)
  return(tmp)
}

####################### Function To Get a Schedule by Date #####################
#' Get Master Schedule
#'
#' Gets schedule for all games on a given date
#'
#' @param year year for which to get master schedule
#' @param month month for which to get master schedule
#' @param day day for which to get master schedule
#' @return A data-frame of the day's schedule of games
#' @export
get_master_schedule <- function(year, month, day) {
  ### Error Testing
  if(is.na(year)) {
    stop("year is missing with no default")
  }
  if(is.na(month)) {
    stop("month is missing with no default")
  }
  if(is.na(day)) {
    stop("day is missing with no default")
  }

  tmp <- try(as.Date(paste(year, month, day, sep = "-")))
  if(class(tmp) == "try-error") {
    stop("Please enter valid date")
  }

  date <- paste0(year, ifelse(nchar(month) == 1, paste0("0", month), month),
                 ifelse(nchar(day) == 1, paste0("0", day), day))
  url <- paste0("https://www.espn.com/mens-college-basketball/schedule/_/date/", date)
  z <- XML::readHTMLTable(RCurl::getURL(url))
  if(length(z) > 1) {
    schedule <- as.data.frame(z[[1]])[,c(1,2)]
    completed <- as.data.frame(z[[2]][-1,1:3])
    names(completed) <- c("away", "home", "result")
    names(schedule) <- c("away", "home")
  }else{
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
    team <- gsub("[#0-9]", "", team)
    team <- gsub("\\s[A-Z]*-*[A-Z]*$", "", team)
    team <- gsub("TA&M", "", team)
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

  x <- scan(url, sep = "\n", what = "")
  x <- x[grep("gameId", x)[1]]
  x <- gsub("[A-z]", "", x)
  x <- strsplit(x, "\\?=")[[1]]
  x <- suppressWarnings(as.numeric(unname(sapply(x, function(y){ substring(y, 1, 9) }))))
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

  schedule <- dplyr::mutate(schedule, "game_id" = x)
  schedule <- dplyr::select(schedule, game_id, away, home, away_rank, home_rank, away_score, home_score)

  return(schedule)
}
