source("R/helpers.R")

#' Get Shot Locations
#'
#' Extracts Shot location data for a specific game
#'
#' @param game_ids Vector of ESPN game_ids
#' @return A data-frame with shot details (including location) for given game_ids
#' @export
get_shot_locs <- function(game_ids) {
  if(any(is.na(game_ids))) {
    error("game_ids missing with no default")
  }
  n <- length(game_ids)
  for(i in 1:n) {
    message(paste("Getting Shots for Game", i, "of", n))
    url = paste0('http://www.espn.com/mens-college-basketball/playbyplay?gameId=', game_ids[i])
    date <- get_date(game_ids[i])

    away_team_name <-
      stringr::str_replace_all(xml2::read_html(url) %>% rvest::html_nodes(".away h3") %>% rvest::html_text(), "[\r\n\t]" , "")

    ## if not equal to 1, then print this
    if(length(away_team_name) == 0){
      message("No shot location data available for this game.")
    }else{
      away_shot_text <- xml2::read_html(url) %>% rvest::html_nodes(".away-team li") %>% rvest::html_text()

      ## Style, get shot location data from here
      away_shot_style <- xml2::read_html(url) %>% rvest::html_nodes(".away-team li") %>% xml2::xml_attr("style")
      away_color  <- gsub("^.*border-color:\\s*|\\s*;.*$", "", away_shot_style[1])

      ### home text
      home_team_name <- stringr::str_replace_all(xml2::read_html(url) %>% rvest::html_nodes(".home h3") %>% rvest::html_text(), "[\r\n\t]" , "")
      home_shot_text <- xml2::read_html(url) %>% rvest::html_nodes(".home-team li") %>% rvest::html_text()

      ## Style, get shot location data from here
      home_shot_style <- xml2::read_html(url) %>% rvest::html_nodes(".home-team li") %>% xml2::xml_attr("style")
      home_color <- gsub("^.*border-color:\\s*|\\s*;.*$", "", home_shot_style[1])


      away_df <- data.frame(
        team_name = away_team_name,
        shot_text = away_shot_text,
        shot_style = away_shot_style,
        color = away_color,
        stringsAsFactors = F
      )

      home_df <- data.frame(
        team_name = home_team_name,
        shot_text = home_shot_text,
        shot_style = home_shot_style,
        color = home_color,
        stringsAsFactors = F
      )

      total_df = rbind(away_df, home_df)

      total_df <-total_df %>%
        mutate(
          "date" = date,
          "outcome" = ifelse(grepl("made", shot_text), "made", "missed"),
          "shooter" = stripwhite(gsub("made.*", "", shot_text)),
          "shooter" = stripwhite(gsub("missed.*", "", shooter)),
          "asissted" = stripwhite(gsub(".{1}$", "", gsub(".*Assisted by", "", shot_text))),
          "asissted" = stripwhite(ifelse(grepl("made", asissted) |
                                           grepl("missed", asissted), NA, asissted)),
          "three_pt" = grepl("Three Point", shot_text),
          "x" = as.numeric(gsub('^.*top:\\s*|\\s*%;.*$', '', total_df$shot_style)) * 0.5,
          "y" = as.numeric(gsub('^.*left:\\s*|\\s*%;top.*$', '', total_df$shot_style)) * .94
        ) %>% select(-shot_style)

      if(!exists("total_df_all")) {
        total_df_all <- total_df
      }else{
        total_df_all <- rbind(total_df_all, total_df)
      }
    }
  }

  if(!exists("total_df_all")) {
    return(NULL)
  }
  return(total_df_all)
}


#' Plot all shots from one game
#'
#' Plots game shot locations from one game.
#'
#' @param game_id ESPN game_id
#' @param heatmap Use a density type heatmap
#' @export
#'
#'
game_shot_chart <- function(game_id, heatmap = F){
  if(any(is.na(game_id))) {
    error("game_id missing with no default")
  }
  if(length(game_id) > 1) {
    error("game_shot_chart only takes in a single game_id")
  }

  shot_loc_df <- get_shot_locs(game_id)
  if(!is.null(shot_loc_df)) {
    teams <- sort(unique(shot_loc_df$team_name))
    game_title <- paste0(teams[1]," vs. ", teams[2])
    color <- c(shot_loc_df$color[shot_loc_df$team_name == teams[1]][1],
               shot_loc_df$color[shot_loc_df$team_name == teams[2]][1])

    date <- format(as.Date(shot_loc_df$date[1]), "%B %d, %Y")

    if(heatmap){
      full_title <- paste0(game_title," \n ",date)
      title <- cowplot::ggdraw() +
        cowplot::draw_label(full_title,
                            fontface = 'bold')
      p <- team_shot_chart(game_id, teams[1], heatmap = T) +
        theme(legend.position = "bottom")
      p2 <- team_shot_chart(game_id, teams[2], heatmap=T) +
        labs(caption="") +
        theme(legend.position = "bottom")
      p3 <- cowplot::plot_grid(p,p2)
      p1 <- cowplot::plot_grid(title, p3, ncol = 1, rel_heights = c(0.1, 1))
      return(p1)
    }

    p1 <- suppressMessages(ggplot2::ggplot() +
                             ggplot2::geom_point(data = shot_loc_df,
                                                 aes(
                                                   x = x,
                                                   y = y,
                                                   shape = outcome,
                                                   color = team_name),
                                                 size = 3) +
                             ggplot2::geom_polygon(data = court, aes(x = x, y = y, group = group), col = "gray") +
                             ggplot2::geom_point(alpha = 0.2, size = 1.5) +
                             ggplot2::scale_color_manual(values = color) +
                             ggplot2::xlab("") +
                             ggplot2::ylab("")  +
                             ggplot2::coord_flip() +
                             ggplot2::scale_x_reverse() +
                             ggplot2::theme_void() +
                             ggplot2::theme(
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks.x = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title = element_blank(),
                               legend.position = "bottom",
                               legend.direction = 'vertical',
                               plot.title = element_text(size = 16, hjust = 0.5),
                               plot.subtitle = element_text(size = 12, hjust = 0.5),
                               plot.caption = element_text(size = 8, hjust = 0),
                               plot.background = element_rect(fill = 'cornsilk')) +
                             ggplot2::labs(
                               title = game_title,
                               subtitle = date,
                               color = 'Team',
                               shape = 'Made',
                               caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR"))
    return(p1)
  }
}


#' Plot all shots for a team
#'
#' Plots team shot locations from one game or multiple games.
#'
#' @param game_ids Vector of ESPN game_ids
#' @param team Team Name
#' @param heatmap Use a density type heatmap (Default = FALSE)
#' @export
team_shot_chart <- function(game_ids, team, heatmap = F) {
  if(any(is.na(game_ids))) {
    error("game_ids missing with no default")
  }
  if(any(is.na(team))) {
    error("team missing with no default")
  }
  df <- get_shot_locs(game_ids)

  if(!is.null(df)) {
    side_one <- court %>% filter(side == 1)
    team_shots <- df %>% filter(team_name %in% c(team, dict$ESPN_PBP[dict$ESPN == team]))

    ### flip shots if they are on the wrong side
    team_shots[team_shots$y > 47, "x"] <- 50 - team_shots[team_shots$y > 47, "x"]
    team_shots[team_shots$y > 47, "y"] <- 94 - team_shots[team_shots$y > 47, "y"]

    ### only pick one color
    color <- as.character(unique(team_shots$color))[1]

    if(heatmap){
      p1 <-
        ggplot2::ggplot() +
        ggplot2::stat_density_2d(data = team_shots,
                                 aes(x = x, y = y, fill = stat(density / max(density))),
                                 geom = "raster", contour = FALSE, interpolate = TRUE, n = 200) +
        ggplot2::geom_polygon(data = side_one, aes(x = x, y = y, group = group), col = "gray") +
        ggplot2::geom_point(alpha = 0.2, size = 1.5) +
        ggplot2::coord_equal() +
        ggplot2::xlab("") +
        ggplot2::ylab("")  +
        ggplot2::scale_fill_viridis_c("Shot Frequency",
                                      limits = c(0, 1),
                                      breaks = c(0, 1),
                                      labels = c("Lower", "Higher"),
                                      option = "plasma") +
        ggplot2::theme_void() +
        ggplot2::theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 0)) +
        ggplot2::labs(
          title = paste0(team," Shots"),
          shape = "Shot Outcome",
          caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR")
      return(p1)
    }

    p1 <-
      suppressMessages(ggplot2::ggplot() +
                         ggplot2::geom_point(data = team_shots,
                                             aes(
                                               x = x,
                                               y = y,
                                               shape = outcome),
                                             color = color,
                                             size = 3) +
                         ggplot2::geom_polygon(data = side_one, aes(x = x, y = y, group = group), col = "gray") +
                         ggplot2::geom_point(alpha = 0.2, size = 1.5) +
                         ggplot2::scale_color_manual(values = color) +
                         ggplot2:: xlab("") +
                         ggplot2::ylab("")  +
                         ggplot2::theme_void() +
                         ggplot2::theme(
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title = element_blank(),
                           plot.title = element_text(size = 16, hjust = 0.5),
                           plot.subtitle = element_text(size = 12, hjust = 0.5),
                           plot.caption = element_text(size = 8, hjust = 0),
                           plot.background = element_rect(fill = 'cornsilk')) +
                         ggplot2::labs(
                           title = paste(team, "Shots"),
                           shape = "Shot Outcome",
                           caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR"))
    return(p1)
  }
}

#' Plot all shots allowed by a team
#'
#' Plots team shot locations from one game or multiple games.
#'
#' @param game_ids Vector of ESPN game_ids
#' @param team Team Name
#' @param heatmap Use a density type heatmap (Default = FALSE)
#' @export
opp_shot_chart <- function(game_ids, team, heatmap = F) {
  if(any(is.na(game_ids))) {
    error("game_ids missing with no default")
  }
  if(any(is.na(team))) {
    error("team missing with no default")
  }
  df <- get_shot_locs(game_ids)

  if(!is.null(df)) {
    side_one <- court %>% filter(side == 1)
    team_shots <- df %>% filter(!team_name %in% c(team, dict$ESPN_PBP[dict$ESPN == team]))

    ### flip shots if they are on the wrong side
    team_shots[team_shots$y > 47, "x"] <- 50 - team_shots[team_shots$y > 47, "x"]
    team_shots[team_shots$y > 47, "y"] <- 94 - team_shots[team_shots$y > 47, "y"]

    ### only pick one color
    color <- as.character(unique(team_shots$color))[1]

    if(heatmap){
      p1 <-
        ggplot2::ggplot() +
        ggplot2::stat_density_2d(data = team_shots,
                                 aes(x = x, y = y, fill = stat(density / max(density))),
                                 geom = "raster", contour = FALSE, interpolate = TRUE, n = 200) +
        ggplot2::geom_polygon(data = side_one, aes(x = x, y = y, group = group), col = "gray") +
        ggplot2::geom_point(alpha = 0.2, size = 1.5) +
        ggplot2::coord_equal() +
        ggplot2::xlab("") +
        ggplot2::ylab("")   +
        ggplot2::scale_fill_viridis_c("Shot Frequency",
                                      limits = c(0, 1),
                                      breaks = c(0, 1),
                                      labels = c("Lower", "Higher"),
                                      option = "plasma") +
        ggplot2::theme_void() +
        ggplot2::theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 0)) +
        ggplot2::labs(
          title = paste(team, "Shots Against"),
          shape = "Shot Outcome",
          caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR")
      return(p1)
    }

    p1 <-
      suppressMessages(ggplot2::ggplot() +
                         ggplot2::geom_point(data = team_shots,
                                             aes(
                                               x = x,
                                               y = y,
                                               shape = outcome),
                                             color = color,
                                             size = 3) +
                         ggplot2::geom_polygon(data = side_one, aes(x = x, y = y, group = group), col = "gray") +
                         ggplot2::geom_point(alpha = 0.2, size = 1.5) +
                         ggplot2::scale_color_manual(values = color) +
                         ggplot2:: xlab("") +
                         ggplot2::ylab("")  +
                         ggplot2::theme_void() +
                         ggplot2::theme(
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title = element_blank(),
                           plot.title = element_text(size = 16, hjust = 0.5),
                           plot.subtitle = element_text(size = 12, hjust = 0.5),
                           plot.caption = element_text(size = 8, hjust = 0),
                           plot.background = element_rect(fill = 'cornsilk')) +
                         ggplot2::labs(
                           title = paste0(team, " Shots Against"),
                           shape = "Shot Outcome",
                           caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR"))
    return(p1)
  }
}

