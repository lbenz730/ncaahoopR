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
y <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2018-19/NCAA_Hoops_Results_11_8_2018.csv",
              as.is = T)
x <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Power_Rankings/power_rankings.csv",
              as.is = T)
z <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops/master/3.0_Files/Results/2016-17/NCAA_Hoops_Results_2017_Final.csv", as.is = T)
prior <- glm(wins ~ predscorediff, data = z, family = binomial)

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
  away <- data$away[1]
  home <- data$home[1]

  ### Convert to NCAA Names
  away <- dict$NCAA[dict$ESPN_PBP == away]
  home <- dict$NCAA[dict$ESPN_PBP == home]

  ### Get Predicted Line
  if(length(home) == 0 | length(away) == 0) {
    return(NA)
  }
  game <- y %>% filter(team == home, opponent == away, location == "H")
  HCA <- 3.4
  if(nrow(game) == 0) {
    game <- y %>% filter(team == home, opponent == away, location == "N")
    HCA <- 0
    if(nrow(game) == 0) {
      return(0)
    }
  }
  line <- x$yusag_coeff[x$team == home] - x$yusag_coeff[x$team == away] + HCA
  return(line)
}
