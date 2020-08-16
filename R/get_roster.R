####################### Function To Get a Team's Roster ########################
#' Get Team Roster
#'
#' Gets team roster for current season.
#'
#' @param team Team to get roster for

#' @return A data frame of the team's roster
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
    tmp <- try(XML::readHTMLTable(RCurl::getURL(url)))
    if(class(tmp) == "try-error") {
      warning("Unable to get roster. ESPN is updating CBB files. Check back again soon")
      return(NULL)
    }
    tmp <- as.data.frame(tmp[[1]])
    names(tmp) <- c("number", "name", "position", "height", "weight", "class", "hometown")
    for(i in 1:ncol(tmp)) {
      tmp[,i] <- as.character(tmp[,i])
    }
    tmp$number <- as.numeric(gsub("[^0-9]", "", tmp$name))
    tmp$name <- gsub("[0-9]*", "", tmp$name)
    tmp <- dplyr::arrange(tmp, number)
    return(tmp)
  } else {
    season <- as.numeric(substring(season, 1, 4)) + 1
    x <- rjson::fromJSON(file = paste0("http://barttorvik.com/", season, "_rosters.json"))

    numbers <- rep(NA, length(x))
    players <- rep(NA, length(x))
    teams <- rep(NA, length(x))
    positions <- rep(NA, length(x))
    heights <- rep(NA, length(x))
    weights <- rep(NA, length(x))
    classes <- rep(NA, length(x))
    hometowns <- rep(NA, length(x))

    for(i in 1:length(x)) {
      numbers[i] <- unlist(x[[i]])[1]
      players[i] <- unlist(x[[i]])[2]
      teams[i] <- unlist(x[[i]])[3]
      positions[i] <- unlist(x[[i]])[4]
      heights[i] <- unlist(x[[i]])[5]
      weights[i] <- unlist(x[[i]])[6]
      classes[i] <-unlist(x[[i]])[7]
      hometowns[i] <- unlist(x[[i]])[8]
    }


    y <- dplyr::tibble("number" = as.numeric(numbers),
                       "name" = players,
                       "position" = positions,
                       "height" = paste0(gsub("-", "' ", heights), '"'),
                       "weight" = paste(weights, "lbs"),
                       "class" = toupper(classes),
                       "hometown" = as.character(hometowns),
                       "team" = teams)

    y$weight[y$weight %in% c("0 lbs", " lbs")] <- NA
    y$height[y$height %in% c("\' \"", "Fr\"", "So\"", "Jr\"" , "Sr\"", "\"")] <- NA
    y$hometown[y$hometown %in% c("0", "Fr", "So", "Jr", "Sr", "")] <- NA

    trank_team <- dict$Trank[dict$ESPN == team]

    roster <- dplyr::filter(y, team == trank_team) %>%
      dplyr::select(-team) %>%
      dplyr::arrange(number)

    return(roster)
  }
}
