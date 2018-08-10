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

  return(pbp_season)
}

############ Function to get PBP Data for a set of ESPN Game IDs ###############
#' Get Game Play-by-Play Data
#'
#' Scrapes ESPN Play-by-Play data for the desired games.
#'
#' @param gameIDs Vector of ESPN game-IDs
#' @return A data-frame of the Play-by-Play data fror desired games.
#' @export
get_pbp_game <- function(gameIDs) {

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
    }
    else{
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

    if(i == 1) {
      pbp_all <- pbp
    }
    else{
      pbp_all <- rbind(pbp_all, pbp)
    }
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
  base_url <- "http://www.espn.com/mens-college-basketball/team/schedule/_/id/"
  url <- paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  schedule <- XML::readHTMLTable(url)[[1]][-1,]
  names(schedule) <- c("date", "opponent", "result", "record")
  schedule <- schedule[!is.na(schedule$opponent),]
  schedule$opponent <- gsub("^vs", "", schedule$opponent)
  schedule$opponent <- gsub("[@#*()]", "", schedule$opponent)
  schedule$opponent <- gsub("[0-9]*", "", schedule$opponent)
  schedule$opponent <- gsub("^ ", "", schedule$opponent)
  schedule$day <- as.numeric(gsub("[^0-9]*", "", schedule$date))
  schedule$month <- substring(schedule$date, 6, 8)
  schedule$month[schedule$month == "Nov"] <- 11
  schedule$month[schedule$month == "Dec"] <- 12
  schedule$month[schedule$month == "Jan"] <- 1
  schedule$month[schedule$month == "Feb"] <- 2
  schedule$month[schedule$month == "Mar"] <- 3
  schedule$month <- as.numeric(schedule$month)
  schedule$year <- ifelse(schedule$month <= 3, 18, 17)
  schedule$date <- paste(schedule$month, schedule$day, schedule$year, sep = "/")
  schedule$game_id <- get_game_IDs(team)
  return(schedule[,c("date", "opponent", "game_id")])
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
  print(paste("Getting Roster: ", team, sep = ""))
  base_url <- "http://www.espn.com/mens-college-basketball/team/roster/_/id/"
  url <-  paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  tmp <- try(XML::readHTMLTable(url))
  if(class(tmp) == "try-error") {
    return("Unable to get roster. ESPN is updating CBB files. Check back again soon")
  }
  tmp <- as.data.frame(tmp[[1]][-1,])
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
  return(any(grepl("NIT", y)))
}

