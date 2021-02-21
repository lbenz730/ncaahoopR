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
  data <- 
    get_pbp_game(game_id, extra_parse = F) %>% 
    filter(!wrong_time)
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

  ### Get into Appropriate Format
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
