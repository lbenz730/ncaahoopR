######################### Get Game IDs ########################################
#' Get Team game_ids
#'
#' Gets team game_ids for current season.
#'
#' @param team Team to get game_ids
#' @return A vector of the team's ESPN game_ids for current season
#' @export
get_game_ids <- function(team) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  base_url <- "http://www.espn.com/mens-college-basketball/team/_/id/"
  url <- paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  
  x <- scan(url, what = "", sep = "\n")
  x <- x[grep("club-schedule", x)]
  x <- unlist(strsplit(x, "gameId="))
  x <- x[-1]
  x <- x[1:(floor(length(x)/2))]
  reg_flag <- grep("<h2>Regular Season</h2>", x)
  
  game_ids <- substring(x, 1, 9)
  if(length(reg_flag) > 0) {
    game_ids <- c(game_ids[-c(1:reg_flag)], game_ids[1:reg_flag])
  }
  game_ids <- unique(game_ids)
  
  return(game_ids)
}