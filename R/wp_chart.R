#' Win Probability Chart
#'
#' Renders win proability chart for desired game
#'
#' @param game_id ESPN game_id for which to create chart
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart
#' @param include_spread Logical, whether to include pre-game spread in Win Probability
#' calculations. Default = True.
#' @param show_legend Logical indicating whether or not to display legend and min win probability
#' on the chart. Default = TRUE
#' @export
wp_chart <- function(game_id, home_col, away_col, include_spread = T, show_legend = T) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }
  if(is.na(home_col)) {
    stop("home_col is missing with no default")
  }
  if(is.na(away_col)) {
    stop("away_col is missing with no default")
  }

  ### Scrape Data from ESPN
  data <- get_pbp_game(game_id, extra_parse = F)
  if(is.null(data)) {
    print("PBP Data Not Available for Win Probability Chart")
    return(NA)
  }
  date <- format(as.Date(data$date[1]), "%B %d, %Y")
  msec <- max(data$secs_remaining_absolute)

  ### Cleaning
  data$scorediff <- data$home_score - data$away_score
  if(is.na(data$home_favored_by[1])) {
    data$home_favored_by <- get_line(data)
  }
  if(!is.na(data$home_favored_by[1])){
    data$pre_game_prob <- predict(prior, newdata = data.frame(pred_score_diff = data$home_favored_by),
                                  type = "response")
  }else{
    data$pre_game_prob <- 0.5
  }

  if(!include_spread) {
    data$win_prob <- data$naive_win_prob
  }

  ### Game Excitement Index
  data$wp_delta <- 0
  for(i in 2:nrow(data)) {
    data$wp_delta[i] <- abs(data$win_prob[i] - data$win_prob[i-1])
  }
  gei <- sum(data$wp_delta, na.rm = T)
  gei <- paste("Game Excitement Index:", round(gei, 2))
  gap <- 0.08

  ### Plot Results
  data$secs_elapsed <- max(data$secs_remaining_absolute) - data$secs_remaining_absolute
  title <- paste("Win Probability Chart for", data$away[1], "vs.", data$home[1],"\n", date[1])
  if(!include_spread) {
    title <- paste("Naive", title)
  }
  if(data$scorediff[nrow(data)] < 0) {
    plot(win_prob ~ secs_elapsed, data = data, col = home_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "Seconds Elapsed", ylab = "Win Probability", main = title)
    par(new = T)
    plot((1 - win_prob) ~ secs_elapsed, data = data, col = away_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "", ylab = "", main = "")
    abline(h = 0.5, lty = 2)
  }
  else{
    plot((1 - win_prob) ~ secs_elapsed, data = data, col = away_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "Seconds Elapsed", ylab = "Win Probability", main = title)
    par(new = T)
    plot(win_prob ~ secs_elapsed, data = data, col = home_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "", ylab = "", main = "")
    abline(h = 0.5, lty = 2)
  }
  if(show_legend) {
    gap <- 0.02
    if(data$win_prob[1] < 0.85) {
      legend("topleft", col = c(home_col, away_col), legend = c(data$home[1], data$away[1]), lty = 1,
             cex = 0.5)
    }
    else{
      legend("left", col = c(home_col, away_col), legend = c(data$home[1], data$away[1]), lty = 1,
             cex = 0.5)
    }
  }

  ### Min Win Prob
  if(data$score_diff[nrow(data)] > 0) {
    min_prob <- min(data$win_prob, na.rm = T)
    if(min_prob < 0.01) {
      min_prob <- paste("Minimum Win Probability for", data$home[1], "< 1", "%")
    }
    else{
      min_prob <- paste("Minimum Win Probability for", data$home[1], round(100 * min_prob, 1), "%")
    }
  }
  else{
    min_prob <- min(1 - data$win_prob, na.rm = T)
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
}
