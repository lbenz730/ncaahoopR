# ncaahoopR <img src="figures/logo.png" align="right" />
`ncaahoopR` is an R package for working with NCAA Basketball Play-by-Play Data. It scrapes play by play data 
and returns it to the user in a tidy format and allows the user to explore the data with assist networks and in game win-probability charts.

__Note:__ `ncaahoopR` scrapes data from ESPN. Since ESPN is currently updating many college basketball pages, such as schedules and rosters,
some functionality may be temporarily unavailable. Once we approach the beginning of the season, all functionality should work as desired.


## Installation
You can install `ncaahoopR` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("lbenz730/ncaahoopR")
```

## Functions
Several functions use ESPN gameIDs. You can find the gameID in the URL for the game summary, 
as shown in the url for the summary of the UMBC-Virginia game below.
![gameid](figures/espn.png)

### Scraping Data
* ```get_pbp(team)```: Game entire 2017-18 Season's worth of play-by-play data for a given team.
* ```get_pbp_game(gameIDS)```:  Get play-by-play data for a specific vector of ESPN game ids. 
* ```get_roster(team)```: Get a particular team's roster. 
* ```get_schedule(team)```: Get a team's schedule.
* ```get_game_IDs(team)```: Get a vector of ESPN Game IDs for all games in which ```team``` plays in.

### Assist Networks
Used to create college basketball assist networks. 

Usage: ```assist_net(team, node_col, season, rmv_bench, tree, three_weights, message = NA)```

* ```team``` is the ESPN team name, as listed in the `ids` dataframe.
* ```node_col``` is the node color for the graph
* ```season```: Options include "2018-19" (for entire season), or a vector of ESPN game IDs. 
* ```rmv_bench```: Logical. If TRUE, removes all players who aren't in the network. 
* ```tree```: Logical. If TRUE, draws graph in tree structure. If FALSE, draws graph in circle. Tree structure is recommended for single game networks, while circles are recommended for entire season networks.
* ```three_weights```: Logical. If TRUE, assisted three point shots are given 1.5 weight. If FALSE, assisted three point shots are given weight 1. In both cases, assisted 2 point shots are given weight 1. 
* ```message``` (default = ```NA```) Option for custom message to replace graph title when using a subset of the season (e.g. conference play).

### Win-Probability Charts
__Plots win probability chart for given game.__
Usage:```wp_chart(gameID, home_color, away_color, show_legend = T)```
* ```gameID``` ESPN gameID for the desired win probability chart.
* ```home_col``` Chart color for home team.
* ```season```: Chart color for away team.
* ```show_legend```: Logical, whether or not to show legend/text on chart. Default = `TRUE`.

For more information about how these win-probability charts are fit, check out the below links

* [Model Methodology](https://sports.sites.yale.edu/ncaa-basketball-win-probability-model)
* [Game Excitement Index](https://sports.sites.yale.edu/game-excitement-index-part-ii)

## Datasets

```dict``` A dataframe for converting between team names from various sites.
 
 * ```NCAA```: the name of the team, as listed on the NCAA website
 * ```ESPN```: the name of the team, as listed on the ESPN URLs
 * ```ESPN_PBP```: the name of the team, as listed on the ESPN Play-By-Play logs
 * ```Warren_Nolan```: the name of the team, as listed on WarrenNolan.com
 * ```Trank```: the name of the team, as listed on barttorvik.com
 * ```name_247```: the name of the team, as listed on 247Sports.com

```ids``` A dataframe for converting between team names from various sites.
 
 * ```team```: the name of the team to be suplied to function in ncaahoopR package
 * ```id```: team id; used in ESPN URLs
 * ```link```: link; used in ESPN URLs
 
 These datasets can be loaded with ```data("ids")``` or ```data("dict")``` respectively.

## Examples
__Single Game Assist Network__
![Assist Single](figures/oklahoma.png)
```assist_net("Oklahoma", "firebrick4", 400989185, rmv_bench = T, tree = F, three_weights = T)```

__Season Long Assist Network__
![Assist All](figures/yale.png)
```assist_net("Yale", "royalblue4", "2017-18", rmv_bench = T, tree = F, three_weights = T)```

__NOTE:__ The use ```season = "2017-18"``` would be replaced with the current season. Backdated charts are currently not avaiable.

__Win Probability Chart---2018 NCAA Championship Game__
![2018 NCAA Championship Game](figures/wp_chart.png)
```wp_chart(401025888, "navy", "goldenrod1")```
