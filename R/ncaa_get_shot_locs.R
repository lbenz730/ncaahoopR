source("R/helpers.R")
#' Get Shot Locations
#'
#' Extracts Shot location data for a specific game
#'
#' @param game_ids Vector of ESPN game-IDs
#' @return A data-frame with shot details (including location) for given game_ids
#' @import stringr
#' @import rvest
#' @import xml2
#' @import dplyr
#'
#'
#' @examples
#' get_shot_locs(401083556)
#' @export
get_shot_locs <- function(gameId){
  url = paste0('http://www.espn.com/mens-college-basketball/playbyplay?gameId=',gameId)
  y <- scan(url, what = "", sep = "\n")[8]
  y <- unlist(strsplit(y, "-"))
  date <-  stripwhite(y[length(y) - 1])

  away_team_name = str_replace_all(read_html(url) %>% html_nodes(".away h3") %>% html_text(), "[\r\n\t]" , "")
  ## if not equal to 1, then print this
  if(length(away_team_name)==0){
    message("No shot location data available for this game.")
    return(NULL)
  }
  away_shot_text = read_html(url) %>% html_nodes(".away-team li") %>% html_text()
  ## Style, get shot location data from here
  away_shot_style = read_html(url) %>% html_nodes(".away-team li") %>% xml_attr("style")
  away_color  = gsub("^.*border-color:\\s*|\\s*;.*$","",away_shot_style[1])
  ### home text
  home_team_name = str_replace_all(read_html(url) %>% html_nodes(".home h3") %>% html_text(), "[\r\n\t]" , "")
  home_shot_text = read_html(url) %>% html_nodes(".home-team li") %>% html_text()
  ## Style, get shot location data from here
  home_shot_style = read_html(url) %>% html_nodes(".home-team li") %>% xml_attr("style")
  home_color = gsub("^.*border-color:\\s*|\\s*;.*$","",home_shot_style[1])


  away_df = data.frame(
    team_name = away_team_name,
    shot_text = away_shot_text,
    shot_style = away_shot_style,
    color = away_color
  )
  home_df = data.frame(
    team_name = home_team_name,
    shot_text = home_shot_text,
    shot_style = home_shot_style,
    color = home_color
  )

  total_df = rbind(away_df,home_df)

  total_df = total_df %>% mutate(
    date = date,
    outcome = ifelse(grepl("made",shot_text),"made","missed"),
    shooter = gsub("made.*","",shot_text),
    shooter = gsub("missed.*","",shooter),
    asissted = gsub(".*Assisted by","",shot_text),
    asissted = ifelse(grepl("made",asissted) | grepl("missed",asissted),NA,asissted),
    three_pt = grepl("Three Point",shot_text),
    x = as.numeric(gsub('^.*top:\\s*|\\s*%;.*$', '', total_df$shot_style)) * 0.5,
    y = as.numeric(gsub('^.*left:\\s*|\\s*%;top.*$', '', total_df$shot_style)) * .94
  ) %>% select(-shot_style)
  return(total_df)
}

