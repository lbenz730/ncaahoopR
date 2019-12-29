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
    if(class(tmp) == "try-error") {
      message("Play by Play Data Not Available")
      next
    } else if(length(tmp) == 0) {
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
    pbp$naive_win_prob <- wp_compute(dplyr::mutate(pbp, "home_favored_by" = 0))

    ### Hardcode to 50-50 if Line = 0 or NA
    if(is.na(pbp$home_favored_by[1]) | pbp$home_favored_by[1] == 0) {
      pbp$win_prob[1] <- 0.5
    }

    ### Time Outs
    timeout <- dplyr::filter(pbp, sapply(pbp$description, grepl, pattern = "Timeout")) %>%
      dplyr::filter(description != "Official TV Timeout") %>%
      dplyr::filter(description != "Official TV Timeout.")

    timeout$team <- sapply(timeout$description, function(z) gsub("\\s* Timeout", "", z))
    timeout$team <- sapply(timeout$team, function(z) gsub("\\s* Official TV.", "", z))
    timeout$team <- sapply(timeout$team, function(z) gsub("\\s* 30 Second.", "", z))
    timeout$team <- sapply(timeout$team, function(z) gsub("\\s* 20 Second.", "", z))
    timeout$team <- stripwhite(sapply(timeout$team, function(z) gsub("\\s* Full.", "", z)))
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
    nplay <- nrow(pbp)
    if(nrow(timeout) > 0) {
      for(j in 1:nrow(timeout)) {
        play_id <- timeout$play_id[j]
        secs_remaining <- timeout$secs_remaining_relative[j]
        half <- timeout$half[j]

        if(timeout$team[j] == home) {
          pbp$home_time_out_remaining[play_id:nplay] <- pbp$home_time_out_remaining[play_id:nplay] - 1

        }else {
          pbp$away_time_out_remaining[play_id:nplay] <- pbp$away_time_out_remaining[play_id:nplay] - 1

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
                         win_prob, naive_win_prob, home, away, home_time_out_remaining,
                         away_time_out_remaining, home_favored_by, game_id, date) %>%
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
