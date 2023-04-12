#' Plot all shots from one game
#'
#' Plots game shot locations from one game.
#'
#' @param game_id ESPN game_id for which to render shot chart
#' @param heatmap Logical, Use a density-type heatmap, Default = False
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
        ggplot2::theme(legend.position = "bottom")
      p2 <- team_shot_chart(game_id, teams[2], heatmap=T) +
        ggplot2::labs(caption="") +
        ggplot2::theme(legend.position = "bottom")
      p3 <- cowplot::plot_grid(p,p2)
      p1 <- cowplot::plot_grid(title, p3, ncol = 1, rel_heights = c(0.1, 1))
      return(p1)
    }

    p1 <- suppressMessages(
      sportyR::geom_basketball(
        league = "ncaa",
        color_updates = list(
          plot_background = NULL,
          defensive_half_court = "#d2ab6f00",
          offensive_half_court = "#d2ab6f00",
          court_apron = "#d2ab6f00",
          center_circle_outline = "gray",
          center_circle_fill = "#d2ab6f00",
          division_line = "gray",
          endline = "gray",
          sideline = "gray",
          two_point_range = c("#d2ab6f00", "#d2ab6f00"),
          three_point_line = c("gray", "#d2ab6f00"),
          painted_area = c("#d2ab6f00", "#d2ab6f00"),
          lane_boundary = c("gray", "#ff552e"),
          free_throw_circle_outline = "gray",
          free_throw_circle_fill = "#d2ab6f00",
          free_throw_circle_dash = "#d2ab6f00",
          lane_space_mark = "gray",
          inbounding_line = "gray",
          substitution_line = "gray",
          baseline_lower_defensive_box = "gray",
          lane_lower_defensive_box = "gray",
          team_bench_line = "gray",
          restricted_arc = "gray",
          backboard = "gray",
          basket_ring = "gray",
          net = "#ffffff80"
        ),
      )  +
        ggplot2::geom_point(
          data = shot_loc_df,
         ggplot2::aes(
            x = .data$x,
            y = .data$y,
            shape = .data$outcome,
            color = .data$team_name
          ),
          size = 3
        ) +
        ggplot2::geom_point(alpha = 0.2, size = 1.5) +
        ggplot2::scale_color_manual(values = .data$color) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.direction = 'vertical',
          plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
          plot.caption = ggplot2::element_text(size = 8, hjust = 0),
          plot.background =ggplot2::element_rect(fill = 'cornsilk')
        ) +
        ggplot2::labs(
          title = game_title,
          subtitle = date,
          color = 'Team',
          shape = 'Made',
          caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR")
    )
    return(p1)
  }
}
