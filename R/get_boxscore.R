#' Get Boxscore
#'
#' Gets boxscores for each team for a given game.
#'
#' @param game_id ESPN game_id for which to scrape boxscore.
#'
#' @return A named list containing two dataframes with box score for each team.
#'   First team in list is away team, second is home team.
#' @export
get_boxscore <- function(game_id) {
	url <- paste0("https://www.espn.com/mens-college-basketball/boxscore?gameId=", game_id)
	webpage <- xml2::read_html(url)

	# Grab team names. Away team is always listed first.
	pagetext <- rvest::html_text(webpage)
	matchup <- unlist(strsplit(pagetext, "-"))[[1]][1]
	away_name <- unlist(strsplit(matchup, " vs. "))[1]
	away_name <- stringr::str_trim(away_name)
	home_name <- unlist(strsplit(matchup, " vs. "))[2]
	home_name <- stringr::str_trim(home_name)

	# General tidying and splitting of columns.
	away <- rvest::html_table(webpage)[[2]]
	away <- away[1:(nrow(away) - 1),]
	away <- away[-6,]
	away <- tidyr::separate(away, 'FG', c("FGM", "FGA"), sep = "-")
	away <- tidyr::separate(away, '3PT', c("3PTM", "3PTA"), sep = "-")
	away <- tidyr::separate(away, 'FT', c("FTM", "FTA"), sep = "-")
	away_totals <- away[nrow(away):nrow(away),]
	away_totals$Position <- NA
	away <- head(away, -1)
	away$Position <- substr(away$Starters, nchar(away$Starters), nchar(away$Starters))
	away$Starters <- substr(away$Starters, 0, (nchar(away$Starters)-1)/2)
	away <- rbind(away, away_totals)
	rownames(away) <- NULL
	colnames(away)[1] <- "player"
	colnames(away)[18] <- "position"
	away <- away[, c(1, 18, 2:(ncol(away)-1))]
	away$starter <- F
	away$starter[1:5] <- T

	home <- rvest::html_table(webpage)[[3]]
	home <- home[1:(nrow(home) - 1),]
	home <- home[-6,]
	home <- tidyr::separate(home, 'FG', c("FGM", "FGA"), sep = "-")
	home <- tidyr::separate(home, '3PT', c("3PTM", "3PTA"), sep = "-")
	home <- tidyr::separate(home, 'FT', c("FTM", "FTA"), sep = "-")
	home_totals <- home[nrow(home):nrow(home),]
	home_totals$Position <- NA
	home <- head(home, -1)
	home$Position <- substr(home$Starters, nchar(home$Starters), nchar(home$Starters))
	home$Starters <- substr(home$Starters, 0, (nchar(home$Starters)-1)/2)
	home <- rbind(home, home_totals)
	rownames(home) <- NULL
	colnames(home)[1] <- "player"
	colnames(home)[18] <- "position"
	home <- home[, c(1, 18, 2:(ncol(home)-1))]
	home$starter <- F
	home$starter[1:5] <- T

	for(i in 3:18) {
	  home[,i] <- as.numeric(home[,i])
	  away[,i] <- as.numeric(away[,i])
	}


	results <- list(away, home)
	names(results) <- c(away_name, home_name)


	return(results)
}
