#' Get ESPN PBP
#' @author Saiem Gilani
#' @param game_id Game ID
#' @keywords CBB PBP
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr filter select rename bind_cols
#' @importFrom tidyr unnest
#' @export
#' @return A list of three data frames:
#' \describe{
#'   \item{Play-by-Play data frame}
#'   \item{Player Box Score}
#'   \item{Team Box Score}
#' }
#' @examples
#'
#'  cbb_pbp_data(game_id = 401256760)
#'

cbb_pbp_data <- function(game_id){
  options(stringsAsFactors = FALSE)
  options(scipen = 999)

  play_base_url <- "http://cdn.espn.com/mens-college-basketball/playbyplay?render=false&userab=1&xhr=1&"

  ## Inputs
  ## game_id
  full_url <- paste0(play_base_url,
                     "gameId=", game_id)
  raw_play_df <- jsonlite::fromJSON(full_url)
  raw_play_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df[["gamepackageJSON"]]),flatten=TRUE)
  plays <- raw_play_df[["plays"]]
  plays_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df[["plays"]]),flatten=TRUE)
  teams_box_score_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df[["boxscore"]][["teams"]]),flatten=TRUE)
  teams_box_score_df_2 <- teams_box_score_df[[1]][[2]] %>%
    dplyr::select(.data$displayValue, .data$label) %>%
    dplyr::rename(Home = .data$displayValue)
  teams_box_score_df_1 <- teams_box_score_df[[1]][[1]] %>%
    dplyr::select(.data$displayValue) %>%
    dplyr::rename(Away = .data$displayValue)

  team_box_score = dplyr::bind_cols(teams_box_score_df_2, teams_box_score_df_1)
  players_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df[["boxscore"]][["players"]]), flatten=TRUE) %>%
    tidyr::unnest(.data$statistics) %>%
    tidyr::unnest(.data$athletes)
  stat_cols <- players_df$names[[1]]
  stats <- players_df$stats

  stats_df <- as.data.frame(do.call(rbind,stats))
  colnames(stats_df) <- stat_cols

  players_df <- players_df %>%
    dplyr::filter(!.data$didNotPlay)

  player_box <- dplyr::bind_cols(players_df, stats_df)
  pbp <- c(list(plays_df), list(team_box_score),list(player_box))

  return(pbp)
}



