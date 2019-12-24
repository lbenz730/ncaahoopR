#' Win Probability Charts in ggplot
#'
#' Renders Win Probability Charts in ggplot
#'
#' @param game_id ESPN game_id for which to render chart
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart#'
#' @param include_spread Logical, whether to include pre-game spread in Win Probability
#' calculations. Default = True.
#' @param show_labels Logical whether Game Exictement Index and Minimum
#' Win Probability metrics should be displayed on the plot. Default = TRUE.
#' @export
#'
gg_wp_chart <- function(game_id, home_col, away_col, include_spread = T, show_labels = T) {
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
  
  ### Naive WP if Spread Not Included
  if(!include_spread) {
    data$win_prob <- data$naive_win_prob
  }
  
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
  gei <- sum(data$wp_delta, na.rm = T)
  gei <- paste("Game Excitement Index:", round(gei, 2))
  
  ### Minimum Win Probability
  if(data$score_diff[nrow(data)] > 0) {
    min_prob <- min(data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", home_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  } else {
    min_prob <- min(1 - data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", away_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  }
  
  
  
  p <- ggplot2::ggplot(x, aes(x = secs_elapsed/60, y = win_prob, group = team, col = team)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "Minutes Elapsed",
                  y = "Win Probability",
                  col = "",
                  title = paste0(ifelse(include_spread, "", "Naive "), "Win Probability Chart for ", home_team, " vs. ", away_team),
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