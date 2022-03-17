#' Win Probability Chart
#'
#' Renders Win Probability Charts in ggplot
#'
#' @param game_id ESPN game_id for which to render chart
#' @param home_col Color of home team for chart. Defaults to primary color if NULL
#' @param away_col Color of away team for chart. Defaults to primary color if NULL
#' @param include_spread Logical, whether to include pre-game spread in Win Probability
#' calculations. Default = True.
#' @param show_labels Logical whether Game Exictement Index and Minimum
#' Win Probability metrics should be displayed on the plot. Default = TRUE.
#' @export
#'
wp_chart_new <- function(game_id, home_col = NULL, away_col = NULL, include_spread = T, show_labels = T) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }
  
  ### Get Data
  data <- 
    get_pbp_game(game_id, extra_parse = F) %>% 
    dplyr::filter(!wrong_time)
  
  if(is.null(data)) {
    warning("PBP Data Not Available for Win Probability Chart")
    return(NULL)
  }
  
  home_team <- data$home[1]
  away_team <- data$away[1]
  
  if(is.null(home_col)) {
    home_col <- ncaa_colors$primary_color[ ncaa_colors$espn_name == dict$ESPN[dict$ESPN_PBP == home_team  | dict$ESPN_PBP == gsub('State', 'St', home_team)][1] ]
  }
  if(is.null(away_col)) {
    away_col <- ncaa_colors$primary_color[ ncaa_colors$espn_name == dict$ESPN[dict$ESPN_PBP == away_team | dict$ESPN_PBP == gsub('State', 'St', away_team)][1] ] 
  }
  
  home_url <- ncaa_colors$logo_url[ ncaa_colors$espn_name == dict$ESPN[dict$ESPN_PBP == home_team][1] ]
  away_url <- ncaa_colors$logo_url[ ncaa_colors$espn_name == dict$ESPN[dict$ESPN_PBP == away_team][1] ]
  
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
  
  ### Get into Appropriate Format
  x <- rbind(
    dplyr::select(data, secs_remaining_absolute, win_prob) %>%
      dplyr::mutate(team = "home"),
    dplyr::select(data, secs_remaining_absolute, win_prob) %>%
      dplyr::mutate("win_prob" = 1 - win_prob,
                    team = "away")
  ) %>%
    dplyr::mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute)
  
  ### Game Excitement Index
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
  
  home_score <- data$home_score[nrow(data)]
  away_score <- data$away_score[nrow(data)]
  st <- paste0(home_team, ": ", home_score, "  ", away_team, ": ", away_score, "\n", date)
  
  if(home_score > away_score) {
    winning_col <- home_col
    winning_url <- home_url
    losing_col <- away_col
    losing_url <- away_url
    x <- dplyr::filter(x, team == 'home')
  } else {
    winning_col <- away_col
    winning_url <- away_url
    losing_col <- home_col
    losing_url <- home_url
    x <- dplyr::filter(x, team == 'away')
  }
  
  x$favored <- x$win_prob >= 0.5
  
  ix_switch <- which(x$favored != dplyr::lag(x$favored, 1))
  if(length(ix_switch) > 0) {
    add <- 
      dplyr::bind_rows(
        dplyr::slice(x, ix_switch) %>% dplyr::mutate(id = 1:length(ix_switch)),
        dplyr::slice(x, ix_switch - 1) %>% dplyr::mutate(id = 1:length(ix_switch))) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise('win_prob' = 0.5,
                       'favored' = favored[1],
                       'secs_elapsed' = mean(secs_elapsed))
    
    
    x <- dplyr::bind_rows(x, add)
  }
  
  
  x$winning_upper <- pmax(x$win_prob, 0.5)
  x$losing_lower <- pmin(x$win_prob, 0.5)
  
  cols <- c(losing_col, winning_col)
  if(all(x$favored)) {
   cols <- cols[2] 
  }
  
  p <-
    ggplot2::ggplot(x, aes(x = secs_elapsed/60, y = win_prob)) +
    ggplot2::geom_line(size = 1, aes(col = favored, group = 1), lineend = 'round') +
    ggplot2::geom_ribbon(ymin = 0.5,
                         aes(ymax = winning_upper),
                         fill = winning_col,
                         alpha = 0.2) + 
    ggplot2::geom_ribbon(ymax = 0.5,
                         aes(ymin = losing_lower),
                         fill = losing_col,
                         alpha = 0.2) + 
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "Minutes Elapsed",
                  y = "Win Probability",
                  col = "",
                  title = paste0(ifelse(include_spread, "", "Naive "), "Win Probability Chart for ", home_team,
                                 " vs. ", away_team),
                  subtitle = st,
                  caption = "Luke Benz (@recspecs730) Data Accessed via ncaahoopR") +
    ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5),
                   plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),
                   axis.title = element_text(size = 14),
                   plot.caption = element_text(size = 8, hjust = 0),
                   panel.grid.minor = element_blank(),
                   legend.position = "none") +
    ggplot2::scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    ggplot2::scale_y_continuous(limits = c(0,1), labels = function(x) {paste(100 * pmax(x, 1 - x), "%")}) +
    ggplot2::scale_color_manual(values = cols) + 
    ggimage::geom_image(x = 5, y = 0.1, image = losing_url, asp = 16/9, size = 0.08) + 
    ggimage::geom_image(x = 5, y = 0.9, image = winning_url, asp = 16/9, size = 0.08) + 
    scale_size_identity() + 
    ggplot2::annotate("text", x = 35, y = 0.05, label = gei, size = 3.5) +
    ggplot2::annotate("text", x = 35, y = 0.025, label = min_prob, size = 3.5)
  
  p
}
