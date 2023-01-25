################################  Get Season Box Score #########################
#' Get Season Box Score
#'
#' Gets team schedule for requested season.
#'
#' @param team Team to get schedule for
#' @param season Season to get schedule for. In form "2020-21". Default equals current season.
#' @param aggregate One of 'average' (per-game average statistics), 'total' (sums of season stats) or 'raw' 
#' (just return all box scores binded together). 'average' is the default.
#' @return A data frame of the team's schedule for requested season
#' @export
season_boxscore <- function(team, season = current_season, aggregate = 'average') {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!aggregate %in% c('average', 'total', 'raw')) {
    stop("aggregate argument must be one of c('average', 'total', 'raw')")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }
  
  ### Team Schedule
  schedule <- get_schedule(team, season)
  schedule <- dplyr::filter(schedule, date <= Sys.Date())
  game_ids <- schedule$game_id
  
  ### Team PBP Name
  box_team <- ifelse(team == "UConn", team, dict$ESPN_PBP[dict$ESPN == team])
  
  season_box <- NULL
  for(i in 1:length(game_ids)) {
    message(paste0("Pulling Box Score Data for Game: ", i, " of ", length(game_ids)))
    box <- get_boxscore(game_id = game_ids[i])
    box <- box[[box_team]]
    box$game_id <- game_ids[i]
    
    season_box <- dplyr::bind_rows(season_box, box)
  }
  
  season_box <- dplyr::filter(season_box, player != 'TEAM')
  

  ### totals
  if(aggregate == 'total') {
    season_box <- 
      season_box %>% 
      dplyr::mutate('GP' = 1,
                    'GS' = as.numeric(starter)) %>% 
      dplyr::group_by(player_id, player, position) %>% 
      dplyr::summarise_if(is.numeric, sum) %>% 
      dplyr::ungroup()
  }
  
  ### Return Averages
  if(aggregate == 'average') {
    season_box <- 
      season_box %>% 
      dplyr::group_by(player_id, player, position) %>% 
      dplyr::mutate('GP' = n(),
                    'GS' = sum(starter)) %>% 
      dplyr::summarise_if(is.numeric, mean) %>% 
      dplyr::ungroup()
  }
  
  return(season_box)

}
