#' Average Win Probability
#'
#' Computes Time-Based Average Win Probability for the Home Team
#'
#' @param game_id ESPN game_id for which to compute average win probability
#' @param include_spread Logical, whether to include pre-game spread in Win Probability
#' calculations. Default = True.
#' @return Average Win Probability
#' @export
average_win_prob <- function(game_id, include_spread = T) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }
  
  data <- 
    get_pbp_game(game_id) %>% 
    filter(!wrong_time)
  
  if(is.null(data)) {
    return(NA)
  }
  
  if(include_spread) {
    avg_wp <- sum(data$play_length * data$win_prob/max(data$secs_remaining_absolute), na.rm = T)
  } else {
    avg_wp <- sum(data$play_length * data$naive_win_prob/max(data$secs_remaining_absolute), na.rm = T)
  }
  return(avg_wp)
}