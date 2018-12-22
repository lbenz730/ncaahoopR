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
Several functions use ESPN game_ids. You can find the game_id in the URL for the game summary, 
as shown in the url for the summary of the UMBC-Virginia game below.
![game_id](figures/espn.png)

### Scraping Data
* ```get_pbp(team)```: Game entire 2017-18 Season's worth of play-by-play data for a given team.
* ```get_pbp_game(game_ids, win_prob = F)```:  Get play-by-play data for a specific vector of ESPN game ids. ```win_prob``` parameter allows users to specify whether win probability (from the home team's perspective) should be appended to the play-by-play data. Default = ```FALSE```.
* ```get_roster(team)```: Get a particular team's roster. 
* ```get_schedule(team)```: Get a team's schedule.
* ```get_game_ids(team)```: Get a vector of ESPN Game IDs for all games in which ```team``` plays in.
* ```get_master_schedule(year, month, day)```: Get schedule of all games for given date.

### Assist Networks

```assist_net(team, node_col, season, rmv_bench, tree, three_weights, message = NA)```

* ```team``` is the ESPN team name, as listed in the `ids` dataframe.
* ```node_col``` is the node color for the graph
* ```season```: Options include "2018-19" (for entire season), or a vector of ESPN game IDs. 
* ```rmv_bench``` (default = ```TRUE```): Logical. If TRUE, removes all players who aren't in the network. 
* ```tree``` (default = ```FALSE```): Logical. If TRUE, draws graph in tree structure. If FALSE, draws graph in circle. Tree structure is recommended for single game networks, while circles are recommended for entire season networks.
* ```three_weights``` (default = ```TRUE```): Logical. If TRUE, assisted three point shots are given 1.5 weight. If FALSE, assisted three point shots are given weight 1. In both cases, assisted 2 point shots are given weight 1. 
* ```message``` (default = ```NA```) Option for custom message to replace graph title when using a subset of the season (e.g. conference play).

### Win-Probability and Game-Flow Charts
__Win Probability Charts__

There are two functions for plotting win probability charts, one that uses base graphics (`wp_chart`), and the other which uses the ```ggplot2``` library (```gg_wp_chart```). Both are maintainted, as graphics in base R have some nice concatenation principles.

```wp_chart(game_id, home_col, away_col, show_legend = T)```

* ```game_id``` ESPN game_id for the desired win probability chart.
* ```home_col``` Chart color for home team.
* ```away_col```: Chart color for away team.
* ```show_legend```: Logical, whether or not to show legend/text on chart. Default = `TRUE`.


```gg_wp_chart(game_id, home_col, away_col)```

* ```game_id``` ESPN game_id for the desired win probability chart.
* ```home_col``` Chart color for home team.
* ```away_col```: Chart color for away team.

__Game Flow Charts__

```game_flow(game_id, home_col, away_col)```

* ```game_id``` ESPN game_id for the desired game flow chart.
* ```home_col``` Chart color for home team.
* ```away_col```: Chart color for away team.

__Game Excitement Index__

```game_exciment_index(game_id)```

Returns ```GEI``` (Game Excitement Index) for given espn. For more information about how these win-probability charts are fit and how Game Excitemnet Index is calculated, check out the below links

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
 
These datasets can be loaded by typing ```data("ids")``` or ```data("dict")``` respectively, followed by typing the typing the name of the desired dataset into the R console.

## Examples
#### Single Game Assist Network
![Assist Single](figures/oklahoma.png)
```assist_net(team = "Oklahoma", node_col = "firebrick4", season = 400989185, rmv_bench = T, tree = F, three_weights = T)```



#### Season Long Assist Network
![Assist All](figures/yale.png)
```assist_net(team = "Yale", node_col = "royalblue4", season = "2017-18", rmv_bench = T, tree = F, three_weights = T)```

__NOTE:__ The use ```season = "2017-18"``` would be replaced with the current season. Backdated charts are currently not avaiable.

#### Win Probability Charts
![2018 NCAA Championship Game](figures/wp_chart.png)
```wp_chart(game_id = 401025888, home_col = "navy", away_col = "goldenrod1")```

![ggwp](figures/gg_wp_chart.png)
```gg_wp_chart(game_id = 401082978, home_col = "gray", away_col = "orange")```
#### Game Flow Chart
![game_flow](figures/game_flow.png)
```game_flow(game_id = 401082669, home_col = "blue", away_col = "navy")```


## Glossary
Play-by-Play files contain the following variables:

* ```play_id```: Unique identifier of play/event in sequence of game events.
* ```half```: Period of action in the game. 1 and 2 denote the first and second halves of play, while 3 denotes OT1, 4 denotes OT2 etc.
* ```time_remaining_half```: Time remaining in the peroid as it would appear on a scoreboard.
* ```secs_remaining```: Time remaining in regulation, in seconds.
* ```secs_remaining_absolute```: The time remaining until the game is over, in seconds. For example a game that goes to overtime would begin with 2700 seconds remaining (2400 for regulation and 300 for overtime), and regulation would end with 300 seconds remaining.
* ```description```: A description of the play/game event.
* ```home_score```: Home team's score.
* ```away_score```: Away team's score.
* ```score_diff```: Score differential from the home team's perspective (```home_score``` - ```away_score```)
* ```win_prob```: Win probability for the home team.
* ```home```: Name of the home team.
* ```away```: Name of the away team.
* ```home_timeout_remaining```: Number of timeouts remaining for the home team.
* ```away_timeout_remaining```: Number of timeouts remaining for the away team.
* ```home_timeout_ind```: Binary indicator of whether home team took a timeout in the previous 60 seconds of game time.
* ```away_timeout_ind```: Binary indicator of whether away team took a timeout in the previous 60 seconds of game time.
* ```home_favored_by```: Number of points by which the home team is favored, prior to tip-off.
* ```game_id```: ESPN game_id for the game in question.
* ```date```: Date of game.
