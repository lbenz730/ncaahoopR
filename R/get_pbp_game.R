############ Function to get PBP Data for a set of ESPN Game IDs ###############
#' Get Game Play-by-Play Data
#'
#' Scrapes ESPN Play-by-Play data for the desired games.
#'
#' @param game_ids Vector of ESPN game-IDs
#' @param extra_parse Logical whether to link shot variables and possesion parsing
#' (Default = TRUE).
#' @return A data-frame of the Play-by-Play data fror desired games.
#' @export
get_pbp_game <- function(game_ids, extra_parse = T) {
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

  for(g in 1:length(game_ids)) {
    message(paste0("Scraping Data for Game: ", g, " of ", length(game_ids)))
    if(is.nit(game_ids[g])) {
      message("NIT Game--Play by Play Data Not Available at this time")
      next
    }
    url <- paste(base_url, game_ids[g], sep = "")
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
    url2 <- paste(summary_url, game_ids[g], sep = "")
    tmp <- XML::readHTMLTable(RCurl::getURL(url2))
    pbp$away <- as.character(as.data.frame(tmp[[2]])[1,1])
    pbp$home <- as.character(as.data.frame(tmp[[2]])[2,1])
    away_abv <- as.character(as.data.frame(tmp[[1]])[1,1])
    home_abv <- as.character(as.data.frame(tmp[[1]])[2,1])

    ### Get Game Line
    y <- scan(url2, what = "", sep = "\n", quiet = T)
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
    pbp$game_id <- game_ids[g]
    pbp$date <- get_date(game_ids[g])
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
    pbp$play_length[2:nrow(pbp)] <-
      pbp$secs_remaining[1:(nrow(pbp)-1)] - pbp$secs_remaining[2:nrow(pbp)]

    if(extra_parse & (pbp$date[1] >= "2007-11-01")) {
      ### Rosters for Player and Team Matching
      year <- lubridate::year(pbp$date[1])
      if(lubridate::month(pbp$date[1]) <= 5) {
        year <- paste(year - 1, year - 2000, sep = "-")
      } else {
        year <- paste(year, year - 1999, sep = "-")
      }

      home_roster <- NULL
      away_roster <- NULL

      if(pbp$home[1] %in% dict$ESPN_PBP) {
        home_roster <- get_roster(dict$ESPN[dict$ESPN_PBP == pbp$home[1]], year)$name
      }
      if(pbp$away[1] %in% dict$ESPN_PBP) {
        away_roster <- get_roster(dict$ESPN[dict$ESPN_PBP == pbp$away[1]], year)$name
      }

      ### Link Shot Data
      pbp <- dplyr::mutate(pbp, "shot_x" = NA, "shot_y" = NA, "shot_team" = NA,
                           "shot_outcome" = NA, "three_pt" = NA, "free_throw" = NA,
                           "shooter" = NA, "assist" = NA, "possession_before" = NA,
                           "possession_after" = NA)

      shots <- get_shot_locs(game_ids[g])

      if(class(shots) != "NULL") {
        ### Break Each Team's Shots Down Individually
        df1 <- dplyr::filter(shots, team_name == unique(shots$team_name)[1])
        df2 <- dplyr::filter(shots, team_name == unique(shots$team_name)[2])
        n1 <- nrow(df1)
        n2 <- nrow(df2)
        ix1 <- 1
        ix2 <- 1

        # Make sure that when you link shots you flip the shots from full court
        # coordinates to half court coordinates
        shots$x[shots$y > 47] <- 50 - shots$x[shots$y > 47]
        shots$y[shots$y > 47] <- 94 - shots$y[shots$y > 47]

        ### Match Shots w/ PBP Data
        for(i in 1:nrow(pbp)) {
          if((ix1 <= n1) & (pbp$description[i] == df1$shot_text[ix1])) {
            pbp$shot_x[i] <- df1$x[ix1]
            pbp$shot_y[i] <- df1$y[ix1]
            pbp$shot_team[i] <- df1$team_name[ix1]
            pbp$shot_outcome[i] <- df1$outcome[ix1]
            pbp$three_pt[i] <- df1$three_pt[ix1]
            pbp$shooter[i] <- df1$shooter[ix1]
            pbp$assist[i] <- df1$assisted[ix1]
            ix1 <- ix1 + 1
          } else if((ix2 <= n2) & (pbp$description[i] == df2$shot_text[ix2])) {
            pbp$shot_x[i] <- df2$x[ix2]
            pbp$shot_y[i] <- df2$y[ix2]
            pbp$shot_team[i] <- df2$team_name[ix2]
            pbp$shot_outcome[i] <- df2$outcome[ix2]
            pbp$three_pt[i] <- df2$three_pt[ix2]
            pbp$shooter[i] <- df2$shooter[ix2]
            pbp$assist[i] <- df2$assisted[ix2]
            ix2 <- ix2 + 1
          }
        }
      } else { ### Manually Annotate what we can
        made_shots <- grepl("Made|made", pbp$description)
        missed_shots <- grepl("Missed|missed", pbp$description)
        pbp$shot_outcome[made_shots] <- "made"
        pbp$shot_outcome[missed_shots] <- "missed"
        pbp$three_pt[made_shots | missed_shots] <-
          grepl("Three Point Jumper", pbp$description[made_shots | missed_shots])
        pbp$shooter[made_shots]  <- gsub(" made.*", "", pbp$description[made_shots])
        pbp$shooter[missed_shots]  <- gsub(" missed.*", "", pbp$description[missed_shots])
        ix_ast <- made_shots & grepl("Assisted", pbp$description)
        pbp$assist[ix_ast] <- gsub("\\.", "", gsub(".*Assisted by ", "", pbp$description[ix_ast]))

        pbp$shot_team[(made_shots | missed_shots) & tolower(pbp$shooter) %in% tolower(home_roster)] <- pbp$home[1]
        pbp$shot_team[(made_shots | missed_shots) & tolower(pbp$shooter) %in% tolower(away_roster)] <- pbp$away[1]
        if(is.null(home_roster[1]) & !is.null(away_roster[1])) {
          pbp$shot_team[(made_shots | missed_shots) & !tolower(pbp$shooter) %in% tolower(away_roster)] <- pbp$home[1]
        } else if(!is.null(home_roster[1]) & is.null(away_roster[1])) {
          pbp$shot_team[(made_shots | missed_shots) & !tolower(pbp$shooter) %in% tolower(home_roster)] <- pbp$away[1]
        }

      }

      ### Tag Free Throws
      pbp$free_throw[!is.na(pbp$shooter)] <- grepl("Free Throw", pbp$description[!is.na(pbp$shooter)])


      ########################## Possession Parsing ############################
      home <- pbp$home[1]
      away <- pbp$away[1]
      message("Parsing Possessions")

      ### Assumptions:
      ###   1) Shooting teams have possesion on the play before they shoot
      ###   2) On made shots (non-free throws), the possession after the play
      ###      switches to the other team
      ###   3) For missed shots, the correct rebound is the first possible
      ###      rebound that is:
      ###       - In the same half
      ###       - Before any future shot
      ###   4) All actions between a missed shot and rebound maintain the same
      ###      before/after possession team
      ###   5) Turnovers/Steals:
      ###      - If steal, possesion go to stealing team from other team
      ###      - If turnover, possesion goes from turnover team to other team
      ###   6) Fouls: Foul gives possession to non-foul committing team
      ###   7) Missed Free Throws are rebounded by the next unmapped rebound before
      ###      next shot. Then proceed as assumption 4
      ###   8) Deadball Team Rebounds implies possesion after for the deadball
      ###      rebounding team
      ###   9) All non-Free Throw sequences occuring at the same time not yet tagged
      ###      get the same before possesion mapping as the first event at that
      ###      time period and the same after possesion mapping as the last event
      ###      at that time period.
      ###   10) All Remaining sequences don't involve change of possession


      ### Assumption 1
      ix_home_shots <- which(pbp$shot_team == home)
      ix_away_shots <- which(pbp$shot_team == away)

      pbp$possession_before[ix_home_shots] <- home
      pbp$possession_before[ix_away_shots] <- away

      ### Assumption 2
      ix_home_makes <- which(pbp$shot_team == home & pbp$shot_outcome == "made" & !pbp$free_throw)
      ix_away_makes <- which(pbp$shot_team == away & pbp$shot_outcome == "made" & !pbp$free_throw)

      pbp$possession_after[ix_home_makes] <- away
      pbp$possession_after[ix_away_makes] <- home

      ### Assumption 3
      ix_all_shot <- which(!is.na(pbp$shot_outcome))
      ix_missed_shot <- which(pbp$shot_outcome == "missed" & !pbp$free_throw)
      ix_all_rebounds <- which(grepl("Rebound", pbp$description))


      find_rebound <- function(i) {
        next_shot <- max(c(ix_all_shot[ix_all_shot > i][1], nrow(pbp)), na.rm = T)
        rebound <- ix_all_rebounds[ix_all_rebounds > i][1]

        if(is.na(rebound)) {
          return(NA)
        } else if(rebound < next_shot & pbp$half[i] == pbp$half[rebound]) {
          return(rebound)
        } else {
          return(NA)
        }
      }

      # Match Rebound to Missed Shots
      rebound_pairs <- sapply(ix_missed_shot, find_rebound)

      ### Assumption 4
      for(i in 1:length(ix_missed_shot)) {
        missed_shot <- ix_missed_shot[i]
        rebound <- rebound_pairs[i]

        if(!is.na(rebound)) {
          pbp$possession_before[missed_shot:rebound] <- pbp$possession_before[missed_shot]
          if(grepl("Offensive", pbp$description[rebound])) {
            pbp$possession_after[missed_shot:rebound] <- pbp$possession_before[missed_shot]
          } else if(grepl("Defensive", pbp$description[rebound])) {
            pbp$possession_after[missed_shot:rebound] <-
              ifelse(pbp$possession_before[missed_shot] == home, away, home)
          } else if(grepl("Deadball", pbp$description[rebound])) {
            pbp$possession_after[missed_shot:rebound] <-
              ifelse(grepl(home, pbp$description[missed_shot]), home, away)
          }
        }
      }

      ### Assumption 5
      steals <- which(grepl("Steal|steal", pbp$description))
      turnovers <- which(grepl("Turnover|turnover", pbp$description))

      # Turnovers
      for(t in turnovers) {
        turnover <- t
        home_to <- sapply(c(home_roster, home), grepl, pbp$description[turnover])
        away_to <- sapply(c(away_roster, away), grepl, pbp$description[turnover])

        if(any(home_to) & !any(away_to)) {
          pbp$possession_before[turnover] <- home
          pbp$possession_after[turnover] <- away
        } else if(any(away_to) & !any(home_to)) {
          pbp$possession_before[turnover] <- away
          pbp$possession_after[turnover] <- home
        }
      }

      # Steals
      for(s in steals) {
        steal <- s
        home_steal <- sapply(home_roster, grepl, pbp$description[steal])
        away_steal <- sapply(away_roster, grepl, pbp$description[steal])

        if(any(home_steal) & !any(away_steal)) {
          pbp$possession_before[steal] <- away
          pbp$possession_after[steal] <- home
        } else if(any(away_steal) & !any(home_steal)) {
          pbp$possession_before[steal] <- home
          pbp$possession_after[steal] <- away
        }
      }


      ### Assumption 6
      fouls <- which(grepl("Foul on", pbp$description))
      for(f in fouls) {
        foul <- f
        home_f <- sapply(c(home_roster, home), grepl, pbp$description[foul])
        away_f <- sapply(c(away_roster, away), grepl, pbp$description[foul])

        if(any(home_f) & !any(away_f)) {
          pbp$possession_after[foul] <- away
        } else if(any(away_f) & !any(home_f)) {
          pbp$possession_before[foul] <- home
        }
      }

      ### Assumption 7
      ix_non_ft <- which(!is.na(pbp$shot_outcome) & !pbp$free_throw)
      ix_missed_ft <- which(pbp$shot_outcome == "missed" & pbp$free_throw)
      ix_unmapped_rebounds <- which(grepl("Rebound", pbp$description) & is.na(pbp$possession_after))

      find_rebound_ft <- function(i) {
        next_shot <- max(c(nrow(pbp), ix_non_ft[pbp$secs_remaining[ix_non_ft] <
                                                  pbp$secs_remaining[i]][1]), na.rm = T)

        rebound <- ix_unmapped_rebounds[pbp$secs_remaining[ix_unmapped_rebounds] <=
                                          pbp$secs_remaining[i]][1]

        if(is.na(rebound)) {
          return(NA)
        } else if(rebound < next_shot & pbp$half[i] == pbp$half[rebound]) {
          return(rebound)
        } else {
          return(NA)
        }
      }

      # Match Rebound to Missed Free Throws
      rebound_pairs_ft <- sapply(ix_missed_ft, find_rebound_ft)

      for(i in 1:length(ix_missed_ft)) {
        missed_shot <- ix_missed_ft[i]
        rebound <- rebound_pairs_ft[i]

        if(!is.na(rebound)) {
          pbp$possession_before[missed_shot:rebound] <- pbp$possession_before[missed_shot]
          if(grepl("Offensive", pbp$description[rebound])) {
            pbp$possession_after[missed_shot:rebound] <- pbp$possession_before[missed_shot]
          } else if(grepl("Defensive", pbp$description[rebound])) {
            pbp$possession_after[missed_shot:rebound] <-
              ifelse(pbp$possession_before[missed_shot] == home, away, home)
          } else if(grepl("Deadball", pbp$description[rebound])) {
            pbp$possession_after[missed_shot:rebound] <-
              ifelse(grepl(home, pbp$possession_before[missed_shot]), home, away)
          }
        }
      }

      ### Assumption 8
      pbp$possession_after[pbp$free_throw & pbp$shot_outcome == "made"] <- NA
      ix_deadball <- which(grepl("Deadball Team", pbp$description))
      for(i in ix_deadball) {
        pbp$possession_after[i] <- ifelse(grepl(home, pbp$description[i]), home, away)
      }

      ### Assumption 9
      secs_u <- unique(pbp$secs_remaining[duplicated(pbp$secs_remaining)])
      for(t in secs_u) {
        ix <- which(pbp$secs_remaining == t & !grepl("Free Throw", pbp$description))
        if(any(is.na(pbp$possession_before[ix])) & any(!is.na(pbp$possession_before[ix]))) {
          pbp$possession_before[ix] <- pbp$possession_before[ix][!is.na(pbp$possession_before[ix])][1]
        }
        if(any(is.na(pbp$possession_after[ix])) & any(!is.na(pbp$possession_after[ix]))) {
          vec <- pbp$possession_after[ix][!is.na(pbp$possession_after[ix])]
          pbp$possession_after[ix] <- vec[length(vec)]
        }

      }

      ### Assumption 10
      bad_ix_before <- which(pbp$description == "PLAY")
      bad_ix_after <- which(grepl("End of", pbp$description))
      pbp$possession_before[c(bad_ix_before, bad_ix_after)] <- NA
      pbp$possession_after[c(bad_ix_before, bad_ix_after)] <- NA
      n <- nrow(pbp)

      for(i in 1:n) {
        if(is.na(pbp$possession_before[i]) & !(i %in% bad_ix_before)) {
          ix <- which(!is.na(pbp$possession_after))
          if(any(ix < i)) {
            m <- max(ix[ix < i])
            M <- i
            while(M < n && is.na(pbp$possession_before[M+1]) && !(M+1 %in% bad_ix_before)) {
              M <- M+1
            }
            pbp$possession_before[i:M] <- pbp$possession_after[m]
          }
        }

        if(is.na(pbp$possession_after[i]) & !(i %in% bad_ix_after)) {
          ix <- which(!is.na(pbp$possession_before))
          if(any(ix > i)) {
            m <- min(ix[ix > i])
            M <- i
            while(M > 1 && is.na(pbp$possession_after[M-1]) && !(M-1 %in% bad_ix_after)) {
              M <- M-1
            }
            pbp$possession_after[M:i] <- pbp$possession_before[m]
          }
        }
      }

      ### Final Selection of Columns
      pbp <- dplyr::select(pbp, -pre_game_prob)
      pbp <- dplyr::select(pbp, game_id, date, home, away, play_id, half, time_remaining_half,
                           secs_remaining_relative, secs_remaining, description,
                           home_score, away_score, score_diff, play_length,
                           win_prob, naive_win_prob, home_time_out_remaining,
                           away_time_out_remaining, home_favored_by, shot_x,
                           shot_y, shot_team, shot_outcome, shooter, assist,
                           three_pt, free_throw, possession_before, possession_after) %>%
        dplyr::rename("secs_remaining_absolute" = secs_remaining,
                      "secs_remaining" = secs_remaining_relative)
    } else {
      ### Final Selection of Columns
      pbp <- dplyr::select(pbp, -pre_game_prob)
      pbp <- dplyr::select(pbp, game_id, date, home, away, play_id, half, time_remaining_half,
                           secs_remaining_relative, secs_remaining, description,
                           home_score, away_score, score_diff, play_length,
                           win_prob, naive_win_prob, home_time_out_remaining,
                           away_time_out_remaining, home_favored_by) %>%
        dplyr::rename("secs_remaining_absolute" = secs_remaining,
                      "secs_remaining" = secs_remaining_relative)

    }

    ### Remove buggy score rows
    rm <- which(pbp$score_diff != dplyr::lag(pbp$score_diff) &
                  !grepl("made", pbp$description) &
                  pbp$secs_remaining_absolute > 0)
    if(length(rm) > 1) {
      pbp <- pbp[-rm,]
    }
    pbp$home_score[pbp$secs_remaining_absolute == 0] <- max(pbp$home_score, na.rm = T)
    pbp$away_score[pbp$secs_remaining_absolute == 0] <- max(pbp$away_score, na.rm = T)
    pbp$score_diff[pbp$secs_remaining_absolute == 0] <-
      pbp$home_score[pbp$secs_remaining_absolute == 0] - pbp$away_score[pbp$secs_remaining_absolute == 0]

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
