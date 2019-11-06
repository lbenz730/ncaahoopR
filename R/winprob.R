#' Win Probability Chart
#'
#' Renders win proability chart for desired game
#'
#' @param game_id ESPN game_id for which to create chart
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart
#' @param show_legend Logical indicating whether or not to display legend and min win probability
#' on the chart. Default = TRUE
#' @export
wp_chart <- function(game_id, home_col, away_col, show_legend = T) {
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
  data <- get_pbp_game(game_id)
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

  ### Game Excitemant Index
  data$wp_delta <- 0
  for(i in 2:nrow(data)) {
    data$wp_delta[i] <- abs(data$win_prob[i] - data$win_prob[i-1])
  }
  gei <- sum(data$wp_delta, na.rm = T) * 2400/msec
  gei <- paste("Game Excitement Index:", round(gei, 2))
  gap <- 0.08

  ### Plot Results
  data$secs_elapsed <- max(data$secs_remaining_absolute) - data$secs_remaining_absolute
  title <- paste("Win Probability Chart for", data$away[1], "vs.", data$home[1],"\n", date[1])
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

#' Game Excitement Index
#'
#' Computes Game Excitement Index for Desired Game
#'
#' @param game_id ESPN game_id for which to compute GEI
#' @return GEI--Game Exictement Index
#' @export
game_excitement_index <- function(game_id) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }

  data <- get_pbp_game(game_id)
  if(is.null(data)) {
    return(NA)
  }

  ### Compute Game Excitemant Index
  msec <- max(data$secs_remaining_absolute)
  data$wp_delta <- 0
  for(i in 2:nrow(data)) {
    data$wp_delta[i] <- abs(data$win_prob[i] - data$win_prob[i-1])
  }
  gei <- sum(data$wp_delta, na.rm = T) * 2400/msec
  return(gei)
}

#' Average Win Probability
#'
#' Computes Time-Based Average Win Probability for the Home Team
#'
#' @param game_id ESPN game_id for which to compute average win probability
#' @return Average Win Probability
#' @export
average_win_prob <- function(game_id) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }

  data <- get_pbp_game(game_id)
  if(is.null(data)) {
    return(NA)
  }

  avg_wp <- sum(data$play_length * data$win_prob/max(data$secs_remaining_absolute), na.rm = T)
  return(avg_wp)
}

#' Average Score Differential
#'
#' Computes Time-Based Average Score Differential for the Home Team
#'
#' @param game_id ESPN game_id for which to compute average score differential.
#' @return Average score differential
#' @export
average_score_diff <- function(game_id) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }

  data <- get_pbp_game(game_id)
  if(is.null(data)) {
    return(NA)
  }

  avg_sd <- sum(data$play_length * data$score_diff/max(data$secs_remaining_absolute), na.rm = T)
  return(avg_sd)
}

#' Win Probability Charts in ggplot
#'
#' Renders Win Probability Charts in ggplot
#'
#' @param game_id ESPN game_id for which to render chart
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart
#' @param show_labels Logical whether Game Exictement Index and Minimum
#' Win Probability metrics should be displayed on the plot. Default = TRUE.
#' @export
gg_wp_chart <- function(game_id, home_col, away_col, show_labels = T) {
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

  ### Get Data
  data <- get_pbp_game(game_id)
  if(is.null(data)) {
    warning("PBP Data Not Available for Win Probability Chart")
    return(NULL)
  }
  home_team <- data$home[1]
  away_team <- data$away[1]
  plot_lines <- 1200
  msec <- max(data$secs_remaining_absolute)
  sec <- msec - 2400
  ot_counter <- 0
  while(sec > 0) {
    sec <- sec - 300
    plot_lines <- c(plot_lines, 2400 + ot_counter * 300)
    ot_counter <- ot_counter + 1
  }
  date <- format(as.Date(data$date[1]), "%B %d, %Y")

  ### Get in to Appropropriate Format
  x <- rbind(
    dplyr::select(data, secs_remaining_absolute, win_prob) %>%
      dplyr::mutate(team = "home"),
    dplyr::select(data, secs_remaining_absolute, win_prob) %>%
      dplyr::mutate("win_prob" = 1 - win_prob,
                    team = "away")
  ) %>%
    dplyr::mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute)

  ### Game Excitemant Index
  data$wp_delta <- 0
  for(i in 2:nrow(data)) {
    data$wp_delta[i] <- abs(data$win_prob[i] - data$win_prob[i-1])
  }
  gei <- sum(data$wp_delta, na.rm = T) * 2400/msec
  gei <- paste("Game Excitement Index:", round(gei, 2))

  ### Minimum Win Probability
  if(data$score_diff[nrow(data)] > 0) {
    min_prob <- min(data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", home_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  }else {
    min_prob <- min(1 - data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", away_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  }

  ### Make Plot
  p <- ggplot2::ggplot(x, aes(x = secs_elapsed/60, y = win_prob, group = team, col = team)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "Minutes Elapsed",
                  y = "Win Probability",
                  col = "",
                  title = paste("Win Probability Chart for", home_team, "vs.", away_team),
                  subtitle = date,
                  caption = "Luke Benz (@recspecs730) Data Accessed via ncaahoopR") +
    ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5),
                   plot.subtitle = element_text(size = 12, hjust = 0.5),
                   axis.title = element_text(size = 14),
                   plot.caption = element_text(size = 8, hjust = 0),
                   legend.position = "bottom",) +
    ggplot2::scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    ggplot2::scale_y_continuous(labels = function(x) {paste(100 * x, "%")}) +
    ggplot2::scale_color_manual(values = c(away_col, home_col),
                                labels = c(away_team, home_team))
  if(show_labels) {
    p <- p +
      ggplot2::annotate("text", x = 5, y = 0.05, label = gei) +
      ggplot2::annotate("text", x = 5, y = 0.025, label = min_prob)
  }

  p
}

#' Game Flow Chart
#'
#' Renders Game Flow Chart
#'
#' @param game_id ESPN game_id for which to render chart
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart
#' @export
game_flow <- function(game_id, home_col, away_col) {
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

  ### Get Data
  data <- get_pbp_game(game_id)
  if(is.null(data)) {
    warning("PBP Data Not Available for Game Flow Chart")
    return(NULL)
  }
  home_team <- data$home[1]
  away_team <- data$away[1]
  plot_lines <- 1200
  msec <- max(data$secs_remaining_absolute)
  sec <- msec - 2400
  ot_counter <- 0
  while(sec > 0) {
    sec <- sec - 300
    plot_lines <- c(plot_lines, 2400 + ot_counter * 300)
    ot_counter <- ot_counter + 1
  }
  date <- format(as.Date(data$date[1]), "%B %d, %Y")

  ### Get in to Appropropriate Format
  x <- rbind(
    dplyr::select(data, secs_remaining_absolute, home_score) %>%
      dplyr::mutate("score" = home_score, team = "home") %>%
      dplyr::select(-home_score),
    dplyr::select(data, secs_remaining_absolute, away_score) %>%
      dplyr::mutate("score" = away_score,
                    "team" = "away") %>%
      dplyr::select(-away_score)
  ) %>%
    dplyr::mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute)


  ### Message
  avg_sd <- round(sum(data$play_length * data$score_diff/max(data$secs_remaining_absolute)), 2)
  home_win <- data$home_score[nrow(data)] > data$away_score[nrow(data)]
  avg_sd <- ifelse(home_win, avg_sd, -avg_sd)
  avg_sd <- paste0("Average Score Differential for ",
                  ifelse(home_win, home_team, away_team), ": ", avg_sd)
  max_score <- max(c(data$home_score, data$away_score))

  ### Make Plot
  ggplot2::ggplot(x, aes(x = secs_elapsed/60, y = score, group = team, col = team)) +
    ggplot2::geom_step(size = 1) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "Minutes Elapsed",
                  y = "Score",
                  col = "",
                  title = paste("Game Flow Chart for", home_team, "vs.", away_team),
                  subtitle = date,
                  caption = "Luke Benz (@recspecs730) Data Accessed via ncaahoopR") +
    ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5),
                   plot.subtitle = element_text(size = 12, hjust = 0.5),
                   axis.title = element_text(size = 14),
                   plot.caption = element_text(size = 8, hjust = 0),
                   legend.position = "bottom",) +
    ggplot2::scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    ggplot2::scale_color_manual(values = c(away_col, home_col),
                                labels = c(away_team, home_team)) +
    ggplot2::annotate("text", x = 10, y = max_score - 10, label = avg_sd)
}
