#' Game Excitement Index
#'
#' Computes Game Excitement Index for Desired Game
#'
#' @param game_id ESPN game_id for which to compute GEI
#' @param include_spread Logical, whether to include pre-game spread in Win Probability
#' calculations. Default = True.
#' @return GEI--Game Exictement Index
#' @export
game_excitement_index <- function(game_id, include_spread = T) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }

  data <- 
    get_pbp_game(game_id, extra_parse = F) %>% 
    filter(!wrong_time)
  if(is.null(data)) {
    return(NA)
  }

  ### Compute Game Excitemant Index
  msec <- max(data$secs_remaining_absolute)
  data$wp_delta <- 0
  if(include_spread) {
    for(i in 2:nrow(data)) {
      data$wp_delta[i] <- abs(data$win_prob[i] - data$win_prob[i-1])
    }
  } else {
    for(i in 2:nrow(data)) {
      data$wp_delta[i] <- abs(data$naive_win_prob[i] - data$naive_win_prob[i-1])
    }
  }
  gei <- sum(data$wp_delta, na.rm = T)
  return(gei)
}
