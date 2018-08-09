#' Assist Network
#'
#' Renders assist network for game or entire season
#'
#' @param team Team to create network for
#' @param node_col Color of nodes in network
#' @param season Season, as a character,  (e.g. "2018-19"), or vector of ESPN gameIDS
#' for which data to use in network. Currently only handles the current season worth of data.
#' @param rmv_bench Logical indicating whether to remove players who do not factor in the network.
#' @param tree Logicial indicating whether to draw the network as a tree (recommended for single games).
#' If FALSE, the network will be drawn as a circle (recommended for entire seasons).
#' @param three_weights Logical indicating whether to give extra weight for assisted three point shots.
#' If TRUE, assisted three-point shots will be given weight 1.5 (as opposed to weight 1).
#' @param message User supplied plot title to overwrite default plot title, if desired. Default = NA.
#' @return List with network statistics
#' \itemize{
#'  \item{"clust_coeff"} - Network Clustering Coefficient
#'  \item{"page_ranks"} - Player Page Ranks in network
#'  \item{"hub_scores"} - Player Hub Scores in network
#'  \item{"auth_scores"} - Player Authority Scores in network
#'  \item{"ast_freq"} - Player percentage of team's assists
#'  \item{"shot_freq"} - Player percenatge of scoring on team's assisted baskets
#' @export

assist_net <- function(team, node_col, season, rmv_bench, tree, three_weights, message = NA) {
  text_team <- dict$ESPN_PBP[dict$ESPN == team]
  ### Read Play-by-Play File
  if(season[1] == "2018-19") {
    x <- get_pbp(team)
    text <- " Assist Network for 2018-19 Season"
    factor <- 0.75
    x$description <- as.character(x$description)
  }else {
    x <- suppressWarnings(try(get_pbp_game(season), silent = T))
    if(class(x) == "try-error") {
      return("Play-by-Play Data Not Available")
    }
    opp <- setdiff(c(x$away, x$home), text_team)
    if(length(season) == 1){
      text <- paste(" Assist Network vs. ", opp, sep = "")
    }
    else{
      text <- paste(" Assist Network vs. ", message, sep = "")
    }
    x$description <- as.character(x$description)
    factor <- 3
  }

  ### Get Roster
  team <- gsub(" ", "_", team)
  roster <- try(get_roster(team))
  if(class(roster) == "try-error") {
      return("Unable to get roster. ESPN is updating CBB files. Check back again soon")
  }
  roster$Name <- gsub("Jr.", "Jr", roster$Name)
  games <- unique(x$game_id)
  ast <- grep("Assisted", x$description)
  x <- x[ast, ]

  ### Get Ast/Shot from ESPN Play Description
  splitplay <- function(description) {
    tmp <- strsplit(strsplit(description, "Assisted")[[1]], " ")
    n1 <- grep("made", tmp[[1]])
    n2 <- length(tmp[[2]])
    tmp[[2]][n2] <- substring(tmp[[2]][n2], 1, nchar(tmp[[2]][n2]) - 1)
    shot_maker <- paste(tmp[[1]][1:(n1-1)], collapse = " ")
    assister <- paste(tmp[[2]][3:n2], collapse = " ")
    return(list("shot_maker" = shot_maker, "assister" = assister))
  }

  x <- mutate(x, "ast" = NA, "shot" = NA)
  for(i in 1:nrow(x)) {
    play <- splitplay(x$description[i])
    x$ast[i] <- play$assister
    x$shot[i] <- play$shot_maker
  }

  ### Get only shots made by the team in question
  x$ast <- gsub("Jr.", "Jr", x$ast)
  x$shot <- gsub("Jr.", "Jr", x$shot)
  x <- x[is.element(x$ast, roster$Name), ]

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
    ast <- roster$Name[i]
    tmp <- roster[roster$Name != ast,]
    for(j in 1:nrow(tmp)) {
      index <- j + (i - 1) * nrow(tmp)
      network$ast[index] <- ast
      network$shot[index] <- tmp$Name[j]
      network$num[index] <- sum(x$weights[x$ast == ast & x$shot == tmp$Name[j]])
    }
  }

  network$a_freq <- network$num/sum(network$num)

  ### Remove Bench
  if(rmv_bench) {
    network <- network[network$a_freq > 0,]
  }

  ### Team Ast/Shot Distributions
  ast_data <- aggregate(a_freq ~ ast, data = network, sum)
  shot_data <- aggregate(a_freq ~ shot, data = network, sum)

  ### Create Temporary Directed Network
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
  net <- igraph::graph.data.frame(network, directed = T)
  deg <- igraph::degree(net, mode="all")
  igraph::E(net)$weight <- network$num
  igraph::E(net)$arrow.size <- 0.7
  igraph::E(net)$edge.color <- "white"
  igraph::E(net)$width <- E(net)$weight * factor
  igraph::V(net)$color <- node_col

  if(season %in% c("2016-17", "2017-18")) {
    labs <- NA
  }
  else{
    labs <- as.character(network$num)
  }


  plot(net, vertex.label.color= "black", vertex.label.cex = 1,
       edge.curved = 0.3, edge.label = labs, edge.label.cex = 1.2,
       edge.label.color = "black",
       layout = ifelse(tree, layout_as_tree,layout_in_circle),
       vertex.label.family = "Arial Black",
       main = paste(text_team, ifelse(three_weights, " Weighted", ""), text, sep = ""))


  ### Add Text to Network
  text(-1.5, 1.0, paste(ifelse(three_weights, "Weighted ", ""), "Assist Frequency Leader: ",
                        ast_data$ast[which.max(ast_data$a_freq)], " (",
                        round(100 * max(ast_data$a_freq), 1), "%)", sep = ""),
       cex = ifelse(three_weights, 0.8, 0.6))
  text(-1.5, 0.9, paste(ifelse(three_weights, "Weighted ", ""), "(Assisted) Shot Frequency Leader: ",
                        shot_data$shot[which.max(shot_data$a_freq)], " (",
                        round(100 * max(shot_data$a_freq), 1), "%)", sep = ""),
       cex = ifelse(three_weights, 0.8, 0.6))
  text(-1.5, 0.8, paste("PageRank MVP: ", names(which.max(pagerank)), " (",
                        round(max(pagerank), 3), ")", sep = ""))
  text(-1.5, 0.7, paste("Hub Score MVP: ", names(which.max(hubscores)), " (",
                        round(max(hubscores), 3), ")", sep = ""))
  text(-1.5, 0.6, paste("Authority Score MVP: ", names(which.max(auth_scores)), " (",
                        round(max(auth_scores), 3), ")", sep = ""))
  text(-1.5, 0.5, paste("Team Clustering Coefficient: ", clust_coeff, sep = ""))

  if(three_weights){
    text(0, -1.4, cex = 0.7,
         paste("Weighted Assist Network: Assisted 2 point shots are given weight 1, ",
               "Assisted 3 point shots are given weight 1.5", sep = ""))
  }


  ### Return Results
  return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
              "hub_scores" = hubscores, "auth_scores" = auth_scores,
              "ast_freq" = ast_freq, "shot_freq" = shot_freq))
}

