#' Circle Assist Network
#'
#' Renders circlized assist network for game or entire season and allows for highlight
#' of a single player if desired.
#'
#' @param team Team to create network for
#' @param season Season, as a character,  (e.g. "2018-19"), or vector of ESPN game_ids.
#' for which data to use in network.
#' @param highlight_player Name of player to highlight in assist network. `NA` yields full-team assist
#' network with no player highlighting. Default = `NA`.
#' @param highlight_color Color of player links to be highlighted. `NA` if ```highlight_player``` is `NA`.
#' @param three_weights Logical indicating whether to give extra weight for assisted three-point shots.
#' If TRUE, assisted three-point shots will be given weight 1.5 (as opposed to weight 1). Default = `TRUE`.
#' @param threshold Number between 0-1 indicating minimum percentage of team assists/baskets a player needs to exceed to be included in network. Default = 0.
#' @param message User-supplied plot title to overwrite default plot title, if desired. Default = `NA`.
#' @param return_stats Return Assist Network-related statistics (default = `TRUE`)
#' @return List with network statistics
#' \itemize{
#'  \item{"clust_coeff"} - Network Clustering Coefficient
#'  \item{"page_ranks"} - Player Page Ranks in network
#'  \item{"hub_scores"} - Player Hub Scores in network
#'  \item{"auth_scores"} - Player Authority Scores in network
#'  \item{"ast_freq"} - Player percentage of team's assists
#'  \item{"shot_freq"} - Player percenatge of scoring on team's assisted baskets
#'  }
#' @export
circle_assist_net <- function(team, season, highlight_player = NA, highlight_color = NA,
                              three_weights = T, threshold = 0, message = NA, return_stats = T) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(is.na(season[1])) {
    stop("season is missing with no default")
  }
  if(is.na(highlight_color) & !is.na(highlight_player)) {
    warning("Please provide highlight color")
  }

  text_team <- dict$ESPN_PBP[dict$ESPN == team]
  text_team <- text_team[!is.na(text_team)]

  ### Warnings
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    warning("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
    return(NULL)
  }
  if(threshold < 0 | threshold > 1) {
    warning("Threshold for display must be between 0 and 1")
    return(NULL)
  }

  ### Read Play-by-Play File
  if(grepl("-", season[1])) {
    x <- suppressWarnings(try(get_pbp(team, season[1], extra_parse = F)))
    if(class(x) == "try-error" | class(x) == "NULL") {
      warning("Play-by-Play Data Not Available for Assist Network")
      return(NULL)
    }
    text <- paste0(" Assist Network\n", season[1], " Season")
    year <- season[1]
    factor <- 0.75
  }else {
    x <- suppressWarnings(try(get_pbp_game(season, extra_parse = F), silent = T))
    if(class(x) == "try-error" | class(x) == "NULL") {
      warning("Play-by-Play Data Not Available for Assist Network")
      return(NULL)
    }
    date <- x$date[1]
    year <- lubridate::year(date)
    if(lubridate::month(date) <= 5) {
      year <- paste(year - 1, year - 2000, sep = "-")
    } else {
      year <- paste(year, year - 1999, sep = "-")
    }

    opp <- setdiff(c(x$away, x$home), text_team)
    if(length(season) == 1 & is.na(message)){
      text <- paste(" Assist Network vs. ", opp, sep = "")
    } else{
      text <- message
    }
    factor <- 1.25
  }

  ### Override plot title
  if(!is.na(message)) {
    text <- message
  }


  ### Get Roster
  roster <- try(get_roster(team, year))
  if(class(roster)[1] == "try-error") {
    warning("Unable to get roster. ESPN is updating CBB files. Check back again soon")
    return(NULL)
  }
  roster$name <- gsub("Jr.", "Jr", roster$name)
  games <- unique(x$game_id)
  ast <- grep("Assisted", x$description)
  x <- x[ast, ]

  ### Get Ast/Shot from ESPN Play Description
  splitplay <- function(description) {
    tmp <- strsplit(strsplit(description, "Assisted")[[1]], " ")
    n1 <- grep("made", tmp[[1]])
    n1 <- n1[length(n1)]
    n2 <- length(tmp[[2]])
    tmp[[2]][n2] <- substring(tmp[[2]][n2], 1, nchar(tmp[[2]][n2]) - 1)
    shot_maker <- paste(tmp[[1]][1:(n1-1)], collapse = " ")
    assister <- paste(tmp[[2]][3:n2], collapse = " ")
    return(list("shot_maker" = shot_maker, "assister" = assister))
  }

  x <- dplyr::mutate(x, "ast" = NA, "shot" = NA)
  for(i in 1:nrow(x)) {
    play <- splitplay(x$description[i])
    x$ast[i] <- play$assister
    x$shot[i] <- play$shot_maker
  }

  ### Get only shots made by the team in question
  x$ast <- gsub("Jr.", "Jr", x$ast)
  x$shot <- gsub("Jr.", "Jr", x$shot)
  x <- x[is.element(x$ast, roster$name), ]

  sets <- 2 * choose(nrow(roster), 2)
  network <- data.frame("ast" = rep(NA, sets),
                        "shot" = rep(NA, sets),
                        "num" = rep(NA, sets))

  ### Adjust Three Point Weights in Network
  x$weights <- 1
  if(three_weights){
    threes <- grep("Three Point", x$description)
    x$weights[threes] <- 1.5
  }

  ### Aggregate Assists
  for(i in 1:nrow(roster)) {
    ast <- roster$name[i]
    tmp <- roster[roster$name != ast,]
    for(j in 1:nrow(tmp)) {
      index <- j + (i - 1) * nrow(tmp)
      network$ast[index] <- ast
      network$shot[index] <- tmp$name[j]
      network$num[index] <- sum(x$weights[x$ast == ast & x$shot == tmp$name[j]])
    }
  }

  network$a_freq <- network$num/sum(network$num)
  network <- dplyr::filter(network, a_freq > 0)
  player_asts <-
    sapply(roster$name, function(name) { sum(network$a_freq[network$ast == name | network$shot == name]) })

  ### Team Ast/Shot Distributions
  ast_data <- aggregate(a_freq ~ ast, data = network, sum)
  shot_data <- aggregate(a_freq ~ shot, data = network, sum)

  ### Create Temporary Directed Network For Stat Aggregation
  net <- igraph::graph.data.frame(network, directed = T)
  deg <- igraph::degree(net, mode="all")
  igraph::E(net)$weight <- network$num

  ### Compute Clustering Coefficient
  clust_coeff <- round(igraph::transitivity(net, type = "global"), 3)

  ### Compute Page Rank
  pagerank <- sort(igraph::page_rank(net)$vector, decreasing = T)

  ### Compute Hub Score
  hubscores <- sort(igraph::hub_score(net, scale = F)$vector, decreasing = T)

  ### Compute Authority Scores
  auth_scores <- sort(igraph::authority_score(net, scale = F)$vector, decreasing = T)

  ### Compute Assist Frequency Data
  ast_freq <- ast_data$a_freq
  names(ast_freq) <- ast_data$ast

  ### Compute Shot Frequency Data
  shot_freq <- shot_data$a_freq
  names(shot_freq) <- shot_data$shot

  ### Create/Plot Undirected Network
  if(max(player_asts) < threshold) {
    warning("Threshold is too large--no players exceed threshold")
    ### Return Results
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
  keep <- names(player_asts)[player_asts > threshold]
  network <- dplyr::filter(network, shot %in% keep, ast %in% keep)


  if(any(season %in% c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21"))) {
    labs <- NA
  }
  else{
    labs <- as.character(network$num)
  }

  plot_title <-
    ifelse(is.na(message), paste0(text_team, ifelse(three_weights, " Weighted", ""), text), text)
  if(length(unique(x$game_id)) == 1 & is.na(message)) {
    plot_title <- paste(plot_title, format(as.Date(x$date[1]), "%B %d, %Y"), sep = "\n")
  }

  players <- dplyr::group_by(network, ast) %>%
    dplyr::summarise("count" = sum(num)) %>%
    dplyr::rename("player" = ast) %>%
    rbind(
      dplyr::group_by(network, shot) %>%
        dplyr::summarise("count" = sum(num)) %>%
        dplyr::rename("player" = shot)
    ) %>%
    dplyr::group_by(player) %>%
    dplyr::summarise("count" = sum(count)) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::pull(player)

  if(is.na(highlight_player)) {
    cols <- gg_color_hue(length(players))
    names(cols) <- players
    circlize::chordDiagram(network[,-4],
                           order = players,
                           grid.col = cols,
                           annotationTrack = "grid",
                           preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(network))))))
  }else {
    cols <- rep("grey", length(players))
    if(!gsub("Jr.", "Jr", highlight_player) %in% players) {
      warning(paste("Selected highlight_player not in given network.",
                    "Please select a player from the following list"))
      return(sort(players))
    }
    cols[grepl(highlight_player, players)] <- highlight_color
    names(cols) <- players
    borders <- dplyr::filter(network, ast == highlight_player) %>%
      dplyr::select(ast, shot) %>%
      dplyr::mutate(graphical = 1)
    circlize::chordDiagram(network[,-4],
                           order = players,
                           grid.col = cols,
                           link.lwd = 2,
                           link.border = borders,
                           annotationTrack = "grid",
                           preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(network))))))
  }

  for(si in circlize::get.all.sector.index()) {
    circlize::circos.axis(h = "top", labels.cex = 0.3, sector.index = si, track.index = 2)
  }
  par(cex = 0.6)
  circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
    circlize::circos.text(circlize::CELL_META$xcenter, circlize::CELL_META$ylim[1], circlize::CELL_META$sector.index,
                          facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)
  par(cex = 1)
  title(paste("\n\n", plot_title))

  ### Return Results
  if(return_stats) {
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
}
