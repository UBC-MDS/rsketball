## Dataset Description of Scraped ESPN NBA data

Below is the dataset description for any scraped data from the ESPN NBA website. For reference, please refer to the [ESPN NBA Regular Season 2018/19 website](https://www.espn.com/nba/stats/player/_/season/2019/seasontype/2) and check out the available data.

This guide will provide some guide to users on the available column formats that can be used with the following `rsketball` functions:
- `nba_boxplot()`
- `nba_ranking()`
- `nba_team_stats()`

Below is a list of available columns in the scraped data using `nba_scraper()`. The column names have been kept the same as what is found in the tabular data in the [ESPN NBA Regular Season website](https://www.espn.com/nba/stats/player/_/season/2019/seasontype/2), and are fully capitalised in its abbreviations.

__NAME__: Player names

__TEAM__: NBA Team names that the player belongs to

__POS__: Player position type

__GP__: Games played 

__MIN__: Minutes per game 

__PTS__: Points per game 

__FGM__: Field goals made per game

__FGA__: Field goals attempts per game

__FG%__: Field goals percentage success rate

__3PM__: Three pointers made per game

__3PA__: Three pointers attempts per game

__3P%__: Three pointers percentage success rate

__FTM__: Free throws made per game

__FTA__: Free throws attempts per game

__REB__: Rebounds per game

__AST__: Assists per game

__STL__: Steals per game

__BLK__: Blocks per game

__TO__: Turnovers per game

__DD__: Accumulated Double Doubles per season. A Double Double is when a player achieves double digits in 2 main metrics (such as Points, Assists, Rebounds, Steals) in a single game.

__TD3__: Accumulated Triple Doubles per season. A Triple Double is when a player achieves double digits in 3 main metrics (such as Points, Assists, Rebounds, Steals) in a single game.

__PER__: Player Efficiency Rating. Composite metrics based on a formula to calculate the efficiency of a player in both offensive and defensive skillsets.

## Player Positions

Player positions that can be used as a categorical basis for analysis.

__PG__: Point Guard

__SG__: Shooting Guard

__G__: Guard. Typically players who play either PG or SG.

__SF__: Small Forward. 

__PF__: Power Forward.

__C__: Center

## Teams

Here are the list of teams and their abbrievations used in the ESPN NBA dataset. This list is based on the recent teams in the ESPN NBA and may not account for teams in the past that have been dropped out of the league (such as Seattle Super Sonics, Vancouver Grizzlies).

__ATL__: Atlanta Hawks

__BKN__: Brooklyn Nets

__BOS__: Boston Celtics

__CHA__: Charlotte Bobcats

__CHI__: Chicago Bulls

__CLE__: Cleveland Cavaliers

__DAL__: Dallas Mavericks

__DEN__: Denver Nuggets

__DET__: Detroit Pistons

__GS__: Golden State Warriors

__HOU__: Houston Rockets

__IND__: Indiana Pacers

__LAC__: Los Angeles Clippers

__LAL__: Los Angeles Lakers

__MEM__: Memphis Grizzlies

__MIA__: Miami Heat

__MIN__: Minnesota Timberwolves

__NO__: New Orleans Pelicans

__NY__: New York Knicks

__OKC__: Oklahoma City Thunder

__ORL__: Orlando Magic

__PHI__: Philadelphia Sixers

__PHX__: Phoenix Suns

__POR__: Portland Trailblazers

__SA__: San Antonio Spurs

__SAC__: Sacramento Kings

__TOR__: Toronto Raptors

__UTAH__: Utah Jazz

__WSH__: Washington Wizards

Some players may have been traded within the season, and thus their allocated teams may be mentioned as a hybrid. An example are two players Jimmy Butler and Dario Saric that were traded in the regular season of 2018/2019:

__MIN/PHI__: Minnesota Timberwolves and Philadelphia Sixers
