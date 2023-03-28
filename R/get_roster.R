################################  Get Roster #################################
#' Get Team Roster
#'
#' Gets team Roster for requested season.
#'
#' @param team Team to get roster for
#' @param season Season to get roster for. In form "2019-20". Default equals current season.
#' @return A data frame of the team's roster for requested season
#' @export
get_roster <- function(team, season = current_season) {
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
  
  if(season == current_season) {
    base_url <- "https://www.espn.com/mens-college-basketball/team/roster/_/id/"
    url <-  paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
    content <- RCurl::getURL(url)
    tmp <- try(XML::readHTMLTable(content))
    if(class(tmp) == "try-error") {
      warning("Unable to get roster. ESPN is updating CBB files. Check back again soon")
      return(NULL)
    }
    tmp <- as.data.frame(tmp[[1]])
    names(tmp) <- c("number", "name", "position", "height", "weight", "class", "hometown")
    # links for player images, extract each player id
    player_ids <- XML::getHTMLLinks(content) %>%
      # keep only links with this pattern
      stringr::str_subset(., "mens-college-basketball/player/_/id") %>%
      # there are always two of each
      unique() %>%
      # extract the 7 digit player id
      stringr::str_extract(., "[0-9]{7}")
    
    for(i in 1:ncol(tmp)) {
      tmp[,i] <- as.character(tmp[,i])
    }
    tmp$number <- as.numeric(gsub("[^0-9]", "", tmp$name))
    tmp$name <- gsub("[0-9]*", "", tmp$name)
    # player image is found at this link
    tmp$player_image <- paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/", player_ids, ".png")
    tmp$player_id <- suppressWarnings(as.numeric(player_ids))
    tmp <- dplyr::arrange(tmp, number)
    tmp <- dplyr::select(tmp, player_id, number, name, position, height, weight, class, hometown, player_image)
    
    return(tmp)
  } else {
    roster <-
      suppressWarnings(try(readr::read_csv(paste0("https://raw.githubusercontent.com/lbenz730/ncaahoopR_data/master/",
                                                  season,"/rosters/", gsub(" ", "_", team), "_roster.csv"), show_col_types = F)))
    if(any(class(roster) == 'try-error')) {
      warning('No Roster Available')
      return(NULL)
    }
    
    return(roster)
  }
}
