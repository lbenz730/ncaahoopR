###################################Get Season Long PBP Data ####################
#' Get Team Play-by-Play Data
#'
#' Scrapes the current season's Play-by-Play data for desired team. Team
#' is assumed to be the ESPN team name, which can be looked up in the ids
#' dataframe.
#'
#' @param team Team to get Play-by-Play data for
#' @param season Season to get schedule for in form "2019-20". Default equals current season.
#' @param extra_parse Logical whether to link shot variables and possesion parsing
#' (Default = TRUE).
#' @return A data-frame of the team's Play-by-Play data for the specified season.
#' @export
get_pbp <- function(team, season = current_season, extra_parse = T) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }

  message(paste("Getting Game IDs: ", team, sep = ""))

  ### Get Game IDs
  if(season == current_season) {
    game_ids <- get_schedule(team) %>%
      dplyr::filter(date < Sys.Date()) %>%
      pull(game_id)
  } else {
    game_ids <- get_game_ids(team, season)
  }


  ### Get PBP Data
  pbp_season <- get_pbp_game(game_ids, extra_parse)

  return(pbp_season)
}
