### Helper Function
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

####################### Function to clean PBP data #############################
clean <- function(data, half, OTs) {
  cleaned <- data %>% mutate(play_id = 1:nrow(data),
                             half = half,
                             time_remaining_half = as.character(V1),
                             description = V3,
                             away_score = suppressWarnings(as.numeric(gsub("-.*", "", V4))),
                             home_score = suppressWarnings(as.numeric(gsub(".*-", "", V4))))
  cleaned$time_remaining_half[1] <- ifelse(half <= 2, "20:00", "5:00")
  mins <- suppressWarnings(as.numeric(gsub(":.*","", cleaned$time_remaining_half)))
  secs <- suppressWarnings(as.numeric(gsub(".*:","", cleaned$time_remaining_half)))
  cleaned$secs_remaining <- max(20 * (2 - half), 0) * 60 +
    5 * 60 * max((OTs * as.numeric(half <=2)), ((OTs + 2 - half) * as.numeric(half > 2))) + 60 * mins + secs
  if(half == 1) {
    cleaned[1, c("home_score", "away_score")] <- c(0,0)
  }
  cleaned <- select(cleaned, play_id, half, time_remaining_half, secs_remaining, description,
                    home_score, away_score)
  return(cleaned)
}

### Make ids df (only if package not loaded in memory)
create_ids_df <- function() {
  test <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops_Play_By_Play/master/ids.csv",
                   as.is = T)
  teams_url <- "http://www.espn.com/mens-college-basketball/teams"
  x <- scan(teams_url, what = "", sep = "\n")
  x <- x[grep("mens-college-basketball/team/schedule/_/id/", x)][2]
  x <- strsplit(x, "Clubhouse")[[1]]

  ids <- data.frame("team" = rep(NA, 353),
                    "id" = rep(NA, 353),
                    "link" = rep(NA, 353))

  for(i in 2:length(x)) {
    y <- strsplit(x[i], "mens-college-basketball/team/_/id/")[[1]][2]
    y <- unlist(strsplit(y, "/"))
    ids$id[i-1] <- y[1]
    ids$link[i-1] <- gsub("\".*", "", y[2])
    name <- test$team[ids$link[i-1] == test$link]
    ids$team[i-1] <- ifelse(length(name) > 0, name, NA)
  }

  tofill <- which(is.na(ids$team))
  for(i in 1:length(tofill)) {
    k <- which.min(stringdist::stringdist(ids$link[tofill[i]], test$link[tofill]))
    ids$team[tofill[i]] <- test$team[tofill[k]]
  }

  return(ids)
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
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }

  print(paste("Getting Game IDs: ", team, sep = ""))

  ### Get Game IDs
  gameIDs <- get_game_IDs(team)

  ### Get Play by Play Data
  base_url <- "http://www.espn.com/mens-college-basketball/playbyplay?gameId="
  summary_url <- "http://www.espn.com/mens-college-basketball/game?gameId="
  j <- 0

  for(i in 1:length(gameIDs)) {
    if(is.nit(gameIDs[i])) {
      print("NIT Game--Play by Play Data Not Available at this time")
      next
    }
    print(paste("Getting ", team, " Game: ", i, "/", length(gameIDs), sep = ""))
    url <- paste(base_url, gameIDs[i], sep = "")
    tmp <- try(XML::readHTMLTable(url), silent = T)

    ### Check if PBP Data is Available
    if(length(tmp) < ncol(tmp[[1]]) | length(tmp) == 0) {
      print("Play by Play Data Not Available")
      next
    }
    else{
      t1 <- as.numeric(unlist(strsplit(as.character(tmp[[2]][2,1]), ":")))
      t2 <- as.numeric(unlist(strsplit(as.character(tmp[[2]][5,1]), ":")))
      if(60 * t1[1] + t1[2] < 60 * t2[1] + t2[2]) {
        print("Game In Progress--Play by Play Data Not Available. Please Check Back After the Game")
        next
      }
      j <- j + 1
    }

    ### 0 OT
    if(ncol(tmp[[1]]) == 4) {
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 0)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 0)
      pbp <- rbind(half_1, half_2)
    }

    ### 1 OT
    else if(ncol(tmp[[1]]) == 5 & ((length(tmp) == 6 & ncol(tmp[[5]]) == 4) | (length(tmp) == 5 & ncol(tmp[[4]]) == 5))) {
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 1)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 1)
      half_3 <- clean(as.data.frame(tmp[[4]]), 3, 1)
      pbp <- rbind(half_1, half_2, half_3)
    }

    ### 2 OT
    else if(ncol(tmp[[1]]) == 5 & ((length(tmp) == 7 & ncol(tmp[[6]]) == 4) | (length(tmp) == 6 & ncol(tmp[[5]]) == 5))) {
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 2)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 2)
      half_3 <- clean(as.data.frame(tmp[[4]]), 3, 2)
      half_4 <- clean(as.data.frame(tmp[[5]]), 4, 2)
      pbp <- rbind(half_1, half_2, half_3, half_4)
    }

    ### 3 OT
    else if(ncol(tmp[[1]]) == 5 & ((length(tmp) == 8 & ncol(tmp[[7]]) == 4) | (length(tmp) == 7 & ncol(tmp[[6]]) == 5))){
      half_1 <- clean(as.data.frame(tmp[[2]]), 1, 3)
      half_2 <- clean(as.data.frame(tmp[[3]]), 2, 3)
      half_3 <- clean(as.data.frame(tmp[[4]]), 3, 3)
      half_4 <- clean(as.data.frame(tmp[[5]]), 4, 3)
      half_5 <- clean(as.data.frame(tmp[[6]]), 5, 3)
      pbp <- rbind(half_1, half_2, half_3, half_4, half_5)
    }

    ### 4 OT
    else if(ncol(tmp[[1]]) == 5 & ((length(tmp) == 9 & ncol(tmp[[8]]) == 4) | (length(tmp) == 8 & ncol(tmp[[7]]) == 5))) {
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
    url2 <- paste(summary_url, gameIDs[i], sep = "")
    tmp <- XML::readHTMLTable(url2)
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
    }
    else {
      line <- NA
    }

    pbp$home_favored_by <- line
    pbp$play_id <- 1:nrow(pbp)
    pbp$game_id <- gameIDs[i]

    ### Get Date
    url <- paste("http://www.espn.com/mens-college-basketball/playbyplay?gameId=", gameIDs[i], sep = "")
    y <- scan(url, what = "", sep = "\n")[8]
    y <- unlist(strsplit(y, "-"))
    date <-  stripwhite(y[length(y) - 1])
    pbp$date <- date

    if(j == 1) {
      pbp_season <- pbp
    }else{
      pbp_season <- rbind(pbp_season, pbp)
    }
  }

  if(!exists("pbp_season")) {
    pbp_all <- NULL
  }

  return(pbp_season)
}

############ Function to get PBP Data for a set of ESPN Game IDs ###############
#' Get Game Play-by-Play Data
#'
#' Scrapes ESPN Play-by-Play data for the desired games.
#'
#' @param gameIDs Vector of ESPN game-IDs
#' @param win_prob Logical whether to return win probability from home team perspective
#' on each play of the game. Default = F.
#' @return A data-frame of the Play-by-Play data fror desired games.
#' @export
get_pbp_game <- function(gameIDs, win_prob = F) {
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  ### Get Play by Play Data
  base_url <- "http://www.espn.com/mens-college-basketball/playbyplay?gameId="
  summary_url <- "http://www.espn.com/mens-college-basketball/game?gameId="
  j <- 0

  for(i in 1:length(gameIDs)) {
    print(paste("Game: ", i, "/", length(gameIDs), sep = ""))
    if(is.nit(gameIDs[i])) {
      print("NIT Game--Play by Play Data Not Available at this time")
      next
    }
    url <- paste(base_url, gameIDs[i], sep = "")
    tmp <- try(XML::readHTMLTable(url), silent = T)

    ### Check if PBP Data is Available
    if(length(tmp) < ncol(tmp[[1]]) | length(tmp) == 0) {
      print("Play by Play Data Not Available")
      next
    }else{
      t1 <- as.numeric(unlist(strsplit(as.character(tmp[[2]][2,1]), ":")))
      t2 <- as.numeric(unlist(strsplit(as.character(tmp[[2]][5,1]), ":")))
      if(60 * t1[1] + t1[2] < 60 * t2[1] + t2[2]) {
        print("Game In Progress--Play by Play Data Not Available. Please Check Back After the Game")
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
    url2 <- paste(summary_url, gameIDs[i], sep = "")
    tmp <- XML::readHTMLTable(url2)
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
    }
    else {
      line <- NA
    }

    pbp$home_favored_by <- line
    pbp$play_id <- 1:nrow(pbp)
    pbp$game_id <- gameIDs[i]

    url <- paste("http://www.espn.com/mens-college-basketball/playbyplay?gameId=", gameIDs[i], sep = "")
    y <- scan(url, what = "", sep = "\n")[8]
    y <- unlist(strsplit(y, "-"))
    date <-  stripwhite(y[length(y) - 1])
    pbp$date <- date

    ### Win Probability by Play
    if(win_prob) {
      ### Cleaning
      pbp$scorediff <- pbp$home_score - pbp$away_score
      if(is.na(pbp$home_favored_by[1])) {
        pbp$home_favored_by <- get_line(pbp)
      }
      if(!is.na(pbp$home_favored_by[1])){
        pbp$pre_game_prob <- predict(prior, newdata = data.frame(predscorediff = pbp$home_favored_by),
                                     type = "response")
      }else{
        pbp$pre_game_prob <- 0.5
      }

      ### Compute Win Prob
      pbp$winprob <- NA
      msec <- max(pbp$secs_remaining)
      for(k in 1:nrow(pbp)) {
        m <- secs_to_model(pbp$secs_remaining[k], msec)
        model <- wp_hoops[m,]
        log_odds <- model$intercept + pbp$scorediff[k]*model$scorediff +
          pbp$pre_game_prob[k]*model$pre_game_prob
        odds <- exp(log_odds)
        pbp$winprob[k] <- odds/(1 + odds)
      }

      ### Hardcode to 50-50 if Line = 0 or NA
      if(is.na(pbp$home_favored_by[1]) | pbp$home_favored_by[1] == 0) {
        pbp$winprob[1] <- 0.5
      }
      pbp <- select(pbp, -pre_game_prob)
    }

    if(i == 1) {
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

################################  Get Schedule #################################
#' Get Team Schedule
#'
#' Gets team schedule for current season.
#'
#' @param team Team to get schedule for
#' @return A data-frame of the team's schedule for current season
#' @export
get_schedule <- function(team) {
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }

  base_url <- "http://www.espn.com/mens-college-basketball/team/schedule/_/id/"
  url <- paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  schedule <- XML::readHTMLTable(url)[[1]][-1,]
  schedule <- schedule[,1:4]
  names(schedule) <- c("date", "opponent", "result", "record")
  schedule <- schedule[!is.na(schedule$opponent) & schedule$opponent != "Opponent",]
  schedule$location <- ifelse(grepl("[*]", schedule$opponent), "N",
                              ifelse(grepl("^vs", schedule$opponent), "H", "A"))
  schedule$opponent <- gsub("^vs", "", schedule$opponent)
  schedule$opponent <- gsub("[@#*()]", "", schedule$opponent)
  schedule$opponent <- gsub("[0-9]*", "", schedule$opponent)
  schedule$opponent <- gsub("^ ", "", schedule$opponent)
  schedule$result[grep(":", schedule$result)] <- NA
  schedule$result[grep("TBD", schedule$result)] <- NA
  scores <- unlist(sapply(gsub("[A-z]*", "", schedule$result), strsplit, "-"))
  team_scores <- as.numeric(scores[seq(1, length(scores), 2)])
  opp_scores <- as.numeric(scores[seq(2, length(scores), 2)])
  schedule <- dplyr::mutate(schedule, team_score = NA, opp_score = NA)
  schedule$team_score[1:length(team_scores)] <- team_scores
  schedule$opp_score[1:length(opp_scores)] <- opp_scores
  index <- grepl("L", schedule$result)
  tmp <- schedule$team_score[index]
  schedule$team_score[index] <- schedule$opp_score[index]
  schedule$opp_score[index] <- tmp
  schedule$day <- as.numeric(gsub("[^0-9]*", "", schedule$date))
  schedule$month <- substring(schedule$date, 6, 8)
  schedule$month[schedule$month == "Nov"] <- 11
  schedule$month[schedule$month == "Dec"] <- 12
  schedule$month[schedule$month == "Jan"] <- 1
  schedule$month[schedule$month == "Feb"] <- 2
  schedule$month[schedule$month == "Mar"] <- 3
  schedule$month[schedule$month == "Apr"] <- 4
  schedule$month <- as.numeric(schedule$month)
  schedule$year <- ifelse(schedule$month <= 4, 19, 18)
  schedule$date <- paste(schedule$month, schedule$day, schedule$year, sep = "/")
  schedule$game_id <- get_game_IDs(team)
  schedule$date <- as.Date(schedule$date, "%m/%d/%y")
  return(schedule[,c("game_id", "date", "opponent", "location",
                     "team_score", "opp_score", "record" )])
}

######################### Get Game IDs ########################################
#' Get Team GameIDs
#'
#' Gets team gameIDs for current season.
#'
#' @param team Team to get gameIDs
#' @return A vector of the team's ESPN gameIDs for current season
#' @export
get_game_IDs <- function(team) {
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

  gameIDs <- substring(x, 1, 9)
  if(length(reg_flag) > 0) {
    gameIDs <- c(gameIDs[-c(1:reg_flag)], gameIDs[1:reg_flag])
  }
  gameIDs <- unique(gameIDs)

  return(gameIDs)
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
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }
  base_url <- "http://www.espn.com/mens-college-basketball/team/roster/_/id/"
  url <-  paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  tmp <- try(XML::readHTMLTable(url))
  if(class(tmp) == "try-error") {
    return("Unable to get roster. ESPN is updating CBB files. Check back again soon")
  }
  tmp <- as.data.frame(tmp[[3]])
  names(tmp) <- c("Number", "Name", "Position", "Height", "Weight", "Class", "Hometown")
  return(tmp)
}

get_date <- function(gameID) {
  url <- paste("http://www.espn.com/mens-college-basketball/playbyplay?gameId=", gameID, sep = "")
  y <- scan(url, what = "", sep = "\n")[8]
  y <- unlist(strsplit(y, "-"))
  date <-  stripwhite(y[length(y) - 1])
  return(date)
}

################################# Checks if Game is in NIT #####################
is.nit <- function(gameID) {
  url <- paste("http://www.espn.com/mens-college-basketball/playbyplay?gameId=", gameID, sep = "")
  y <- scan(url, what = "", sep = "\n")
  return(sum(grepl("NIT", y)) > 1)
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
  date <- paste0(year, ifelse(nchar(month) == 1, paste0("0", month), month),
                 ifelse(nchar(day) == 1, paste0("0", day), day))
  url <-paste0("http://www.espn.com/mens-college-basketball/schedule/_/date/", date, "/group/50")
  z <- XML::readHTMLTable(url)
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

  ### Extract Ranking
  ranking <- function(team) {
    rank <- gsub("[^0-9]", "", team)
    return(ifelse(rank == "", NA, rank))
  }

  ### Clean Team Name
  clean <- function(team) {
    team <- gsub("[#0-9]", "", team)
    team <- gsub("\\s[A-Z]*-*[A-Z]*$", "", team)
    team <- gsub("\\s*$", "", gsub("^\\s*", "", team))
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
  x <- x[279]
  x <- gsub("[A-z]", "", x)
  x <- strsplit(x, "\\?=")[[1]]
  x <- suppressWarnings(as.numeric(unname(sapply(x, function(y){ substring(y, 1, 9) }))))
  x <- x[!is.na(x) & !duplicated(x)]

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
  winning_scores <- scores[seq(1, length(scores) - 1, 2)]
  losing_scores <- scores[seq(2, length(scores), 2)]

  index <- sapply(completed$away_anchor, function(y) { y %in% winners })
  completed$home_score[index] <- losing_scores[index]
  completed$home_score[!index] <- winning_scores[!index]
  completed$away_score[!index] <- losing_scores[!index]
  completed$away_score[index] <- winning_scores[index]

  if(any(!is.na(schedule[1]))) {
    schedule <- rbind(schedule, dplyr::select(completed, -away_anchor, -result))
  }else{
    schedule <- completed
  }

  schedule <- dplyr::mutate(schedule, "game_id" = x)
  schedule <- dplyr::select(schedule, game_id, away, home, away_rank, home_rank, away_score, home_score)

  return(schedule)
}
