######################### Get Game IDs ########################################
#' Get Team game_ids
#'
#' Gets team game_ids for current season.
#'
#' @param team Team to get game_ids
#' @param season Season to get schedule for in form "2019-20". Default equals current season.
#' @return A vector of the team's ESPN game_ids for current season
#' @export
get_game_ids <- function(team, season = current_season) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }

  ### Current Season (get game_ids from team clubhouse)
  if(season == current_season) {
    base_url <- "https://www.espn.com/mens-college-basketball/team/_/id/"
    url <- paste0(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team])

    x <- scan(url, what = "", sep = "\n")
    x <- x[grep("club-schedule", x)]
    x <- unlist(strsplit(x, "gameId="))
    x <- x[-1]
    x <- x[1:(floor(length(x)/2))]
    reg_flag <- grep("<h2>Regular Season</h2>", x)

    ### Move Postseason Games to End of Schedule
    game_ids <- substring(x, 1, 9)
    if(length(reg_flag) > 0) {
      game_ids <- c(game_ids[-c(1:reg_flag)], game_ids[1:reg_flag])
    }
    game_ids <- unique(game_ids)
  } else { ### Old Season (Get Them from Schedule)
    base_url <- "https://www.espn.com/mens-college-basketball/team/schedule/_/id/"
    url <- paste0(base_url, ids$id[ids$team == team], "/season/", as.numeric(substring(season, 1, 4)) + 1)
    x <- scan(url, what = "", sep = "\n")
    y <- unlist(strsplit(x[65], "href=\"http://www.espn.com/mens-college-basketball/game\\?gameId="))
    game_ids <- gsub("\".*", "", y)[-1]

    ### Postseason Games First
    reg_flag <- grep("Regular Season", y)
    if(reg_flag > 1) {
      game_ids <- c(game_ids[reg_flag:length(game_ids)], game_ids[1:(reg_flag - 1)])
    }

  }

  return(game_ids)
}




