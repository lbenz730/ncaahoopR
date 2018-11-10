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
  return(m)
}

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


#' Win Probability Chart
#'
#' Renders win proability chart for desired game
#'
#' @param gameID ESPN gameID for which to create chart
#' @param home_col Color of home team for chart
#' @param home_col Color of away team for chart
#' @param show_legend Logical indicating whether or not to display legend and min win probability
#' on the chart. Default = TRUE
#' @export
wp_chart <- function(gameID, home_col, away_col, show_legend = T) {
  ### Scrape Data from ESPN
  data <- get_pbp_game(gameID)
  if(is.null(data)) {
    print("PBP Data Not Available for Win Probability Chart")
    return(NA)
  }
  date <- data$date

  ### Cleaning
  data$scorediff <- data$home_score - data$away_score
  if(is.na(data$home_favored_by[1])) {
    data$home_favored_by <- get_line(data)
  }
  if(!is.na(data$home_favored_by[1])){
    data$pre_game_prob <- predict(prior, newdata = data.frame(predscorediff = data$home_favored_by),
                                  type = "response")
  }else{
    data$pre_game_prob <- 0.5
  }

  ### Compute Win Prob
  data$winprob <- NA
  msec <- max(data$secs_remaining)
  for(i in 1:nrow(data)) {
    m <- secs_to_model(data$secs_remaining[i], msec)
    model <- wp_hoops[m,]
    log_odds <- model$intercept + data$scorediff[i]*model$scorediff +
      data$pre_game_prob[i]*model$pre_game_prob
    odds <- exp(log_odds)
    data$winprob[i] <- odds/(1 + odds)
  }

  ### Hardcode to 50-50 if Line = 0 or NA
  if(is.na(data$home_favored_by[1]) | data$home_favored_by[1] == 0) {
    data$winprob[1] <- 0.5
  }

  ### Game Excitemant Index
  data$wp_delta <- 0
  for(i in 2:nrow(data)) {
    data$wp_delta[i] <- abs(data$winprob[i] - data$winprob[i-1])
  }
  gei <- sum(data$wp_delta, na.rm = T) * 2400/msec
  gei <- paste("Game Excitement Index:", round(gei, 2))
  gap <- 0.08


  ### Plot Results
  data$secs_elapsed <- max(data$secs_remaining) - data$secs_remaining
  title <- paste("Win Probability Chart for", data$away[1], "vs.", data$home[1],"\n", date[1])
  if(data$scorediff[nrow(data)] < 0) {
    plot(winprob ~ secs_elapsed, data = data, col = home_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "Seconds Elapsed", ylab = "Win Probability", main = title)
    par(new = T)
    plot((1 - winprob) ~ secs_elapsed, data = data, col = away_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "", ylab = "", main = "")
    abline(h = 0.5, lty = 2)
  }
  else{
    plot((1 - winprob) ~ secs_elapsed, data = data, col = away_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "Seconds Elapsed", ylab = "Win Probability", main = title)
    par(new = T)
    plot(winprob ~ secs_elapsed, data = data, col = home_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "", ylab = "", main = "")
    abline(h = 0.5, lty = 2)
  }
  if(show_legend) {
    gap <- 0.02
    if(data$winprob[1] < 0.85) {
      legend("topleft", col = c(home_col, away_col), legend = c(data$home[1], data$away[1]), lty = 1,
             cex = 0.5)
    }
    else{
      legend("left", col = c(home_col, away_col), legend = c(data$home[1], data$away[1]), lty = 1,
             cex = 0.5)
    }
  }

  ### Min Win Prob
  if(data$scorediff[nrow(data)] > 0) {
    min_prob <- min(data$winprob, na.rm = T)
    if(min_prob < 0.01) {
      min_prob <- paste("Minimum Win Probability for", data$home[1], "< 1", "%")
    }
    else{
      min_prob <- paste("Minimum Win Probability for", data$home[1], round(100 * min_prob, 1), "%")
    }
  }
  else{
    min_prob <- min(1 - data$winprob, na.rm = T)
    if(min_prob < 0.01) {
      min_prob <- paste("Minimum Win Probability for", data$away[1], "< 1", "%")
    }
    else{
      min_prob <- paste("Minimum Win Probability for", data$away[1], round(100 * min_prob, 1), "%")
    }
  }
  if(show_legend) {
    text(600, 2 * gap, min_prob, cex = 0.8)
    text(600, gap, gei, cex = 0.8)
    text(10, 0, "Luke Benz\n@recspecs730\nncaahoopR", cex = 0.5)
  }

  ### Returns Game Excitement Index
  gei <- sum(data$wp_delta, na.rm = T) * 2400/msec
  return(gei)
}

