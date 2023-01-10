#' Get Boxscore
#'
#' Gets boxscores for each team for a given game.
#'
#' @param game_id ESPN game_id for which to scrape boxscore.
#'
#' @return A named list containing two dataframes with boxscore for each team.
#'   First team in list is away team, second is home team.
#' @export
get_boxscore <- function(game_id) {
  url <- paste0("https://www.espn.com/mens-college-basketball/boxscore?gameId=", game_id)
  txt <- RCurl::getURL(url)
  
  x <- strsplit(gsub(',"sbpg".*$', '', txt), 'bxscr')[[1]]
  x <- x[3]
  x <- gsub('^\":', '', x)
  stats <- jsonlite::fromJSON(x, flatten = T)
  
  info <- 
    stats %>% 
    dplyr::select(-stats)
  
  away_labs <- stats$stats[[1]]$lbls[[1]]
  home_labs <- stats$stats[[2]]$lbls[[2]]
  away_ttls <- stats$stats[[1]]$ttls[[3]]
  home_ttls <- stats$stats[[2]]$ttls[[3]]
  
  if(any(unlist(purrr::map(stats$stats[[1]]$athlts, class)) == 'list')) {
    message('Boxscore not available')
    return(NULL) 
  }
  
  if(any(unlist(purrr::map(stats$stats[[2]]$athlts, class)) == 'list')) {
    message('Boxscore not available')
    return(NULL) 
  }
  
  away <- 
    stats$stats[[1]]$athlts %>% 
    dplyr::bind_rows() %>% 
    dplyr::select('player_id' = athlt.id,
                  'player' = athlt.shrtNm,
                  'position' = athlt.pos,
                  'stat_values' = stats) %>% 
    tidyr::unnest(cols = 'stat_values') %>% 
    dplyr::mutate('stat_category' = rep(away_labs, n()/length(away_labs))) %>% 
    dplyr::bind_rows(dplyr::tibble('player' = 'TEAM',
                                   'stat_category' = away_labs,
                                   'stat_values' = away_ttls)) %>% 
    tidyr::pivot_wider(names_from = 'stat_category',
                       values_from = 'stat_values') %>% 
    tidyr::separate('FG', c("FGM", "FGA"), sep = "-") %>% 
    tidyr::separate('3PT', c("3PTM", "3PTA"), sep = "-") %>% 
    tidyr::separate('FT', c("FTM", "FTA"), sep = "-") %>% 
    dplyr::mutate('team' = info$tm.nm[1],
                  'opponent' = info$tm.nm[2],
                  'home' = info$tm.hm[1]) %>% 
    dplyr::mutate('starter' = ifelse(1:n() <= 5, T, F)) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::any_of(c(away_labs, 'FGA', 'FGM', 'FTA', 'FTM', '3PTM', '3PTA'))), ~as.numeric(.x))
  
  home <- 
    stats$stats[[2]]$athlts %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate('starter' = ifelse(1:n() <= 5, T, F)) %>% 
    dplyr::select('player_id' = athlt.id,
                  'player' = athlt.shrtNm,
                  'position' = athlt.pos,
                  'stat_values' = stats) %>% 
    tidyr::unnest(cols = 'stat_values') %>% 
    dplyr::mutate('stat_category' = rep(home_labs, n()/length(home_labs))) %>% 
    dplyr::bind_rows(dplyr::tibble('player' = 'TEAM',
                                   'stat_category' = home_labs,
                                   'stat_values' = home_ttls)) %>% 
    tidyr::pivot_wider(names_from = 'stat_category',
                       values_from = 'stat_values') %>% 
    tidyr::separate('FG', c("FGM", "FGA"), sep = "-") %>% 
    tidyr::separate('3PT', c("3PTM", "3PTA"), sep = "-") %>% 
    tidyr::separate('FT', c("FTM", "FTA"), sep = "-") %>% 
    dplyr::mutate('team' = info$tm.nm[2],
                  'opponent' = info$tm.nm[1],
                  'home' = info$tm.hm[2]) %>% 
    dplyr::mutate('starter' = ifelse(1:n() <= 5, T, F)) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::any_of(c(home_labs, 'FGA', 'FGM', 'FTA', 'FTM', '3PTM', '3PTA'))), ~as.numeric(.x))
  
  
  results <- list(away, home)
  names(results) <- info$tm.nm
  
  return(results)
}
