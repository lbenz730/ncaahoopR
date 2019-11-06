################################ Helper Functions #############################
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

### Function to clean PBP data
clean <- function(data, half, OTs) {
  cleaned <- data %>% dplyr::mutate(play_id = 1:nrow(data),
                                    half = half,
                                    time_remaining_half = as.character(V1),
                                    description = as.character(V3),
                                    away_score = suppressWarnings(as.numeric(gsub("-.*", "", V4))),
                                    home_score = suppressWarnings(as.numeric(gsub(".*-", "", V4))))
  cleaned$time_remaining_half[1] <- ifelse(half <= 2, "20:00", "5:00")
  mins <- suppressWarnings(as.numeric(gsub(":.*","", cleaned$time_remaining_half)))
  secs <- suppressWarnings(as.numeric(gsub(".*:","", cleaned$time_remaining_half)))
  cleaned$secs_remaining <- max(20 * (2 - half), 0) * 60 +
    5 * 60 * max((OTs * as.numeric(half <= 2)), ((OTs + 2 - half) * as.numeric(half > 2))) + 60 * mins + secs
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

################################# Checks if Game is in NIT #####################
is.nit <- function(game_id) {
  url <- paste("http://www.espn.com/mens-college-basketball/playbyplay?gameId=", game_id, sep = "")
  y <- scan(url, what = "", sep = "\n")
  if(any(grepl("NIT SEASON TIP-OFF", y))) {
    return(F)
  }
  return(sum(grepl("NIT", y)) > 1)
}

######################## Loading for Win Prob + Related Charts #################
history <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/History/history.csv", as.is = T)
games_2016 <-
  read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2016-17/NCAA_Hoops_Results_2017_Final.csv", as.is = T) %>%
  dplyr::rename("pred_score_diff" = predscorediff) %>%
  dplyr::mutate("date" = as.Date(paste(year, month, day, sep = "-")))
games_2017 <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2017-18/training.csv", as.is = T)
games_2018 <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2018-19/2019_Final.csv", as.is = T)
train <- rbind(select(games_2016, pred_score_diff, wins),
               select(games_2017, pred_score_diff, wins))
prior <- glm(wins ~ pred_score_diff, data = train, family = binomial)

### Set Coefficients to achieve deterministic relationship at max_time = 0
coeffs <- read.csv("https://raw.githubusercontent.com/lbenz730/Senior-Thesis/master/model_coefficients/model_0_coeffs.csv", as.is = T)
coeffs$estimate[coeffs$max_time <= 2 & coeffs$coefficient == "favored_by"] <- 0

### Fit Loess Models to get smooth functions of coefficient estimate over time
score_diff_smooth <-
  loess(estimate ~ max_time,
        data = filter(coeffs, coefficient == "score_diff"),
        span = 0.5)

favored_by_smooth <-
  loess(estimate ~ max_time,
        data = filter(coeffs, coefficient == "favored_by"),
        span = 0.5)

### Win Probability Function
wp_compute <- function(x) {
  if(is.na(x$home_favored_by[1])) {
    x$home_favored_by <- 0
  }
  ### Get Coefficient Values for Current Game
  sc_diff <- predict(score_diff_smooth, newdata = x$secs_remaining_relative)
  fb <- predict(favored_by_smooth, newdata = x$secs_remaining_relative)

  ### Capture Game Determinism
  index <- x$secs_remaining == 0 & (x$home_score != x$away_score)
  sc_diff[index] <- 20
  fb[index] <- predict(favored_by_smooth, newdata = 1)

  ### Compute log odds of winning
  log_odds <-
    sc_diff * x$score_diff  +
    fb * x$home_favored_by


  return(logit(log_odds))
}

### Get Approiate Model for Time Remaining
secs_to_model <- function(sec, msec) {
  offset <- msec - 2400
  if(offset == 300 & sec > offset) {
    sec <- sec - offset
  }
  if(offset == 600) {
    if(sec > 600) {
      sec <- sec - offset
    }
    else if (sec < 600 & sec > 300) {
      sec <- sec - 300
    }
  }
  else if(offset == 900) {
    if(sec > 900) {
      sec <- sec - offset
    }
    else if (sec <= 900 & sec > 600) {
      sec <- sec - 600
    }
    else if (sec <= 600 & sec > 300) {
      sec <- sec - 300
    }
  }
  else if(offset == 1200) {
    if(sec > 1200) {
      sec <- sec - offset
    }
    else if (sec <= 1200 & sec > 900) {
      sec <- sec - 900
    }
    else if (sec <= 900 & sec > 600) {
      sec <- sec - 600
    }
    else if (sec <= 600 & sec > 300) {
      sec <- sec - 300
    }
  }

  if(sec == 0) {
    m <- 1
  }
  else if(sec >= 1 & sec < 5) {
    m <- 5
  }
  else if(sec >= 5 & sec <= 10) {
    m <- 10
  }
  else if(sec > 10 & sec <= 30) {
    m <- sec + 1
  }
  else if(sec > 30 & sec <= 60) {
    m <- 31 + floor((sec - 30)/2)
  }
  else if(sec > 60 & sec < 2400) {
    m <- 46 + floor((sec - 60)/10)
  }
  else{
    m <- 279
  }
  return(c(m, sec))
}

### Impute Line for Games
get_line <- function(data) {
  game_date <- data$date[1]
  away <- data$away[1]
  home <- data$home[1]

  ### Convert to NCAA Names
  away <- dict$NCAA[dict$ESPN_PBP == away]
  home <- dict$NCAA[dict$ESPN_PBP == home]

  ### Get Predicted Line
  if(length(home) == 0 | length(away) == 0) {
    return(NA)
  }

  ### Don't have Imputed Lines Before 2016-17
  if(game_date < "2016-11-01") {
    return(NA)
  }

  ### Impute from 2016-17 Season
  if(game_date >= "2016-11-01" & game_date <= "2017-05-01") {
    game <- dplyr::filter(games_2016, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }

  ### Impute from 2017-18 Season
  if(game_date >= "2017-11-01" & game_date <= "2018-05-01") {
    game <- dplyr::filter(games_2017, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }

  ### Impute from 2018-19 Season
  if(game_date >= "2018-11-01" & game_date <= "2019-05-01") {
    game <- dplyr::filter(games_2018, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }

  return(NA)
}

### Get Date of Given Game
get_date <- function(game_id) {
  url <- paste("http://www.espn.com/mens-college-basketball/playbyplay?gameId=", game_id, sep = "")
  y <- scan(url, what = "", sep = "\n")[9]
  y <- unlist(strsplit(y, "-"))
  date <-  stripwhite(y[length(y) - 1])
  date <- as.Date(date, "%B %d, %Y")
  return(date)
}

### Recreate ggplot2 colors
### Copied from https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


### Define Logit Function
logit <- function(x) {
  tmp <- exp(x)
  case_when(
    tmp == Inf ~ 1,
    tmp == -Inf ~ 0,
    T ~ tmp/(1 + tmp)
  )
}
