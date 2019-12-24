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
