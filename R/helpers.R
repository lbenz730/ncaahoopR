################################ Helper Functions #############################
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

### Make ids df (only if package not loaded in memory)
create_ids_df <- function() {
  ids <- ncaahoopR::ids
  return(ids)
}

################################# Checks if Game is in NIT #####################
is.nit <- function(game_id) {
  url <- paste("http://www.espn.com/mens-college-basketball/playbyplay?gameId=", game_id, sep = "")
  y <- scan(url, what = "", sep = "\n", quiet = TRUE)
  if(any(grepl("NIT SEASON TIP-OFF", y))) {
    return(FALSE)
  }
  return(sum(grepl("NIT", y)) > 1)
}

######################## Loading for Win Prob + Related Charts #################
history <- read.csv(
  "https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/History/history.csv",
  as.is = TRUE
)
games_2016 <-
  read.csv(
    "https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2016-17/NCAA_Hoops_Results_2017_Final.csv",
    as.is = TRUE
  ) %>%
  dplyr::rename("pred_score_diff" = predscorediff) %>%
  dplyr::mutate("date" = as.Date(paste(year, month, day, sep = "-")))
games_2017 <- read.csv(
  "https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2017-18/training.csv",
  as.is = TRUE
)
games_2018 <- read.csv(
  "https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2018-19/2019_Final.csv",
  as.is = TRUE
)
games_2019 <- read.csv(
  "https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/6a40b6c37c8b888f5d01add9b68b60747e1953c1/3.0_Files/Predictions/predictions.csv",
  as.is = TRUE
)
games_2020 <- read.csv(
  "https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/2ee90d21234392649cefd2bf9d23a4926aa9ed64/3.0_Files/Predictions/predictions.csv",
  as.is = TRUE
)
games_2021 <- read.csv(
  "https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/d7cf98801b93190a24e8528a70a1edb97a4df16e/3.0_Files/Predictions/predictions.csv",
  as.is = TRUE
)
game_2022 <- 
  read.csv(
    'https://github.com/lbenz730/NCAA_Hoops/raw/67cb4cacdb5d5fe214fa71be2dda0774d1fb1d09/3.0_Files/Predictions/predictions.csv',
    as.is = TRUE
  )
games_2024 <- read.csv(
  "https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Predictions/predictions.csv",
  as.is = TRUE
)

train <- rbind(dplyr::select(games_2016, pred_score_diff, wins),
               dplyr::select(games_2017, pred_score_diff, wins))
prior <- glm(wins ~ pred_score_diff, data = train, family = binomial)

### Set Coefficients to achieve deterministic relationship at max_time = 0
coeffs <- read.csv(
  "https://raw.githubusercontent.com/lbenz730/Senior-Thesis/master/model_coefficients/model_0_coeffs.csv",
  as.is = TRUE
)
coeffs$estimate[coeffs$max_time <= 2 & coeffs$coefficient == "favored_by"] <- 0

### Fit Loess Models to get smooth functions of coefficient estimate over time
score_diff_smooth <-
  loess(estimate ~ max_time,
        data = dplyr::filter(coeffs, coefficient == "score_diff"),
        span = 0.5)

favored_by_smooth <-
  loess(estimate ~ max_time,
        data = dplyr::filter(coeffs, coefficient == "favored_by"),
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

### Get Appropriate Model for Time Remaining
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
  else if(offset == 1500) {
    if(sec > 1500) {
      sec <- sec - offset
    }
    else if (sec <= 1500 & sec > 1200) {
      sec <- sec - 1200
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
  
  else if(offset == 1800) {
    if(sec > 1800) {
      sec <- sec - offset
    }
    else if (sec <= 1800 & sec > 1500) {
      sec <- sec - 1500
    }
    else if (sec <= 1500 & sec > 1200) {
      sec <- sec - 1200
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
  away <- dict$NCAA[which(dict$ESPN_PBP == away)]
  home <- dict$NCAA[which(dict$ESPN_PBP == home)]
  
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
  
  ### Impute from 2019-20 Season
  if(game_date >= "2019-11-01" & game_date <= "2020-05-01") {
    game <- dplyr::filter(games_2019, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  ### Impute from 2020-21 Season
  if(game_date >= "2020-11-01" & game_date <= "2021-05-01") {
    game <- dplyr::filter(games_2020, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  ### Impute from 2021-22 Season
  if(game_date >= "2021-11-01" & game_date <= "2022-05-01") {
    game <- dplyr::filter(games_2021, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  ### Impute from 2022-23 Season
  if(game_date >= "2022-11-01" & game_date <= "2023-05-01") {
    game <- dplyr::filter(games_2022, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  if(game_date >= "2023-11-01" & game_date <= "2024-05-01") {
    game <- dplyr::filter(games_2023, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  if(game_date >= "2024-11-01" & game_date <= "2024-05-01") {
    game <- dplyr::filter(games_2023, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  return(NA)
}

### Get Date of Given Game
get_date <- function(game_id) {
  url <- paste0("https://www.espn.com/mens-college-basketball/playbyplay?gameId=", game_id)
  txt <- try(RCurl::getURL(url), silent = TRUE)
  x <- strsplit(txt, 'Men&#x27;s College Basketball Play-By-Play')[[1]]
  date <- as.Date(stripwhite(gsub('^.*-\\s+', '', gsub('\\|.*$', '', x[2]))), '%b %d, %Y')
  return(date)
}

### Define Logit Function
logit <- function(x) {
  tmp <- exp(x)
  dplyr::case_when(
    tmp == Inf ~ 1,
    tmp == -Inf ~ 0,
    TRUE ~ tmp/(1 + tmp)
  )
}

### Current Season Constant
current_season <- "2024-25"

### Recreate ggplot2 colors
### Copied from https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
