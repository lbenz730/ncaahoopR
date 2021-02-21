#' Average Score Differential
#'
#' Computes Time-Based Average Score Differential for the Home Team
#'
#' @param game_id ESPN game_id for which to compute average score differential.
#' @export
average_score_diff <- function(game_id) {
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

  avg_sd <- sum(data$play_length * data$score_diff/max(data$secs_remaining_absolute), na.rm = T)
  return(avg_sd)
}
