#' Win Probability Charts in ggplot
#'
#' Renders Win Probability Charts in ggplot
#'
#' @param game_id ESPN game_id for which to render chart
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart
#' @param include_spread Logical, whether to include pre-game spread in Win Probability
#' calculations. Default = True.
#' @param show_labels Logical whether Game Exictement Index and Minimum
#' Win Probability metrics should be displayed on the plot. Default = TRUE.
#' @export
#'
gg_wp_chart <- function(game_id, home_col, away_col, include_spread = T, show_labels = T) {
  wp_chart(game_id, home_col, away_col, include_spread, show_labels) 
}
