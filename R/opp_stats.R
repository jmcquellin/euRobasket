#'Get stats for lineups from stints
#'
#'This function calculate stats for each lineup in stints.
#'Stints can be downloaded using get_stints_fibalivestats() or get_stints_livefibaeurope().
#'
#'@param stints.df data.frame, stints downloaded with get_stints_fibalivestats() or get_stints_livefibaeurope()
#'
#'@param min.possessions numeric, set to 1 by default, minimum number of possessions for
#'lineups to be included in function results
#'
#'@param ratings.only logical, set to FALSE by default, if set to TRUE for
#'each lineup function results will only show number of possessions, offensive rating,
#'defensive rating and net rating
#'
#'@return data.frame with stats for each lineup
#'
#'@seealso \code{\link[euRobasket]{get_stints_fibalivestats}}, \code{\link[euRobasket]{get_stints_livefibaeurope}}
#'
#'@examples
#'#get lineups stats for basketball champions league
#'#firstly get the stints from bcl using data()
#'data("cl_stints")
#'
#'#use lineup_stats() to get stats for each lineup that played more than 10 possessions
#'
#'lineups_stats(cl_stints, min.possessions = 10)

opp_stats = function(stints.df, min.possessions = 1, ratings.only = FALSE) {

#get home lineups sorted alphabetically
home_lineups = sapply(1:nrow(stints.df), function(x)
               paste(sort(c(stints.df$home_P1[x],
                            stints.df$home_P2[x],
                            stints.df$home_P3[x],
                            stints.df$home_P4[x],
                            stints.df$home_P5[x]))))

home_lineups = t(home_lineups)
home_lineups = paste(home_lineups[,1], home_lineups[,2], home_lineups[,3], home_lineups[,4], home_lineups[,5])

#add counting stats to lineups
home_counting = data.frame(team = stints.df$home_team,
                           team_pts = stints.df$away_pts,
                           team_poss = stints.df$away_possesions,
                           opp_pts = stints.df$home_pts,
                           opp_poss = stints.df$home_possesions,
                           team_fga2 = stints.df$away_2pt_fga,
                           team_fgm2 = stints.df$away_2pt_fgm,
                           team_pfga = stints.df$away_patr_a,
                           team_pfgm = stints.df$away_patr_m,
                           opp_pfga = stints.df$home_patr_a,
                           opp_pfgm = stints.df$home_patr_m,
                           team_fga3 = stints.df$away_3pt_fga,
                           team_fgm3 = stints.df$away_3pt_fgm,
                           team_fta = stints.df$away_fta,
                           team_ftm = stints.df$away_ftm,
                           team_drebs = stints.df$away_drebs,
                           team_orebs = stints.df$away_orebs,
                           opp_drebs = stints.df$home_drebs,
                           opp_orebs = stints.df$home_orebs,
                           team_ast = stints.df$away_assists,
                           team_stls = stints.df$away_steals,
                           team_blks = stints.df$away_blocks,
                           team_tovs = stints.df$away_tovs,
                           opp_fga = stints.df$home_2pt_fga+stints.df$home_3pt_fga,
                           team_ballhandling = stints.df$away_ballhandling,
                           team_badpass = stints.df$away_badpass,
                           team_oFoul = stints.df$away_oFoul,
                           team_3sec = stints.df$away_3sec,
                           team_8sec = stints.df$away_8sec,
                           team_24sec = stints.df$away_24sec,
                           opp_fga2 = stints.df$home_2pt_fga,
                           opp_fga3 = stints.df$home_3pt_fga,
                           opp_fgm2 = stints.df$home_2pt_fgm,
                           opp_fgm3 = stints.df$home_3pt_fgm)

home_lineups = data.frame(cbind(home_lineups, home_counting, stringsAsFactors = FALSE))
names(home_lineups)[1] = 'lineup'


#get away lineups sorted alphabetically
away_lineups = sapply(1:nrow(stints.df), function(x)
                      paste(sort(c(stints.df$away_P1[x],
                                   stints.df$away_P2[x],
                                   stints.df$away_P3[x],
                                   stints.df$away_P4[x],
                                   stints.df$away_P5[x]))))

away_lineups = t(away_lineups)
away_lineups = paste(away_lineups[,1], away_lineups[,2], away_lineups[,3], away_lineups[,4], away_lineups[,5])

#add counting stats to lineups
away_counting = data.frame(team = stints.df$away_team,
                           team_pts = stints.df$home_pts,
                           team_poss = stints.df$home_possesions,
                           opp_pts = stints.df$away_pts,
                           opp_poss = stints.df$away_possesions,
                           team_fga2 = stints.df$home_2pt_fga,
                           team_fgm2 = stints.df$home_2pt_fgm,
                           team_pfga = stints.df$home_patr_a,
                           team_pfgm = stints.df$home_patr_m,
                           opp_pfga = stints.df$away_patr_a,
                           opp_pfgm = stints.df$away_patr_m,
                           team_fga3 = stints.df$home_3pt_fga,
                           team_fgm3 = stints.df$home_3pt_fgm,
                           team_fta = stints.df$home_fta,
                           team_ftm = stints.df$home_ftm,
                           team_drebs = stints.df$home_drebs,
                           team_orebs = stints.df$home_orebs,
                           opp_drebs = stints.df$away_drebs,
                           opp_orebs = stints.df$away_orebs,
                           team_ast = stints.df$home_assists,
                           team_stls = stints.df$home_steals,
                           team_blks = stints.df$home_blocks,
                           team_tovs = stints.df$home_tovs,
                           opp_fga = stints.df$away_2pt_fga+stints.df$away_3pt_fga,
                           team_ballhandling = stints.df$home_ballhandling,
                           team_badpass = stints.df$home_badpass,
                           team_oFoul = stints.df$home_oFoul,
                           team_3sec = stints.df$home_3sec,
                           team_8sec = stints.df$home_8sec,
                           team_24sec = stints.df$home_24sec,
                           opp_fga2 = stints.df$away_2pt_fga,
                           opp_fga3 = stints.df$away_3pt_fga,
                           opp_fgm2 = stints.df$away_2pt_fgm,
                           opp_fgm3 = stints.df$away_3pt_fgm)

away_lineups = data.frame(cbind(away_lineups, away_counting, stringsAsFactors = FALSE))
names(away_lineups)[1] = 'lineup'

#rbind home_lineups and away_lineups
lineups_df = rbind(home_lineups, away_lineups)

#calculate stats for each lineup
all_lineups_df = data.frame()

for(i in 1:length(unique(lineups_df$team))) {
  #get team name
  team = unique(lineups_df$team)[i]

  #subset team lineups
  team_dat = lineups_df[which(lineups_df$team == team),]

  #get unique team lineups
  team_lineups = unique(team_dat$lineup)

  team_lineup_dat = data.frame()

  for(j in 1:length(team_lineups)) {

    lineup = team_lineups[j]

    lineup_dat = team_dat[which(team_dat$lineup == lineup),]

    #calculate ortg, drtg and netrtg
    team_ortg = round(100*(sum(lineup_dat$opp_pts)/sum(lineup_dat$opp_poss)),2)
    team_drtg = round(100*(sum(lineup_dat$team_pts)/sum(lineup_dat$team_poss)),2)
    team_netrtg = team_drtg-team_ortg

    #calculate shooting stats
    team_fga = sum(lineup_dat$team_fga2) + sum(lineup_dat$team_fga3)
    
    team_fgm = sum(lineup_dat$team_fgm2) + sum(lineup_dat$team_fgm3)
    
    opp_fga = sum(lineup_dat$opp_fga2) + sum(lineup_dat$opp_fga3)
    
    opp_fgm = sum(lineup_dat$opp_fgm2) + sum(lineup_dat$opp_fgm3)
    
   `team_fg%` = round(100*((sum(lineup_dat$team_fgm2) +  sum(lineup_dat$team_fgm3))/team_fga),2)

   `team_paint_fg%` = round(100*((sum(lineup_dat$team_pfgm)/sum(lineup_dat$team_pfga))))
    
   `team_3p%` = round(100*(sum(lineup_dat$team_fgm3)/sum(lineup_dat$team_fga3)))

   `team_ft%` = round(100*(sum(lineup_dat$team_ftm)/sum(lineup_dat$team_fta)))

   `team_ts%` = round(100*(sum(lineup_dat$team_pts)/(2*(team_fga + 0.44*sum(lineup_dat$team_fta)))),2)

   `%fgm_as` = round(100*(sum(lineup_dat$team_ast)/(sum(lineup_dat$team_fgm2) +  sum(lineup_dat$team_fgm3))),2)

    #calculate playmaking stats
    ast_rate = round(100*(sum(lineup_dat$team_ast)/sum(lineup_dat$team_poss)),2)

    tov_rate = round(100*(sum(lineup_dat$team_tovs)/sum(lineup_dat$team_poss)),2)

    #calculate rebounding stats

   `team_drb%` = round(100*(sum(lineup_dat$team_drebs) / (sum(lineup_dat$team_drebs)+sum(lineup_dat$opp_orebs))))
   `team_orb%` = round(100*(sum(lineup_dat$team_orebs) / (sum(lineup_dat$team_orebs)+sum(lineup_dat$opp_drebs))))
   `team_trb` = sum(lineup_dat$team_drebs) + sum(lineup_dat$team_orebs)
   `team_trb%` = round(100*((sum(lineup_dat$team_drebs) + sum(lineup_dat$team_orebs)) /
                            (sum(lineup_dat$team_drebs) + sum(lineup_dat$team_orebs) + sum(lineup_dat$opp_drebs) + sum(lineup_dat$opp_orebs))),2)

    #calculate steals and blocks stats

    blk_rate = round(100*(sum(lineup_dat$team_blks)/sum(lineup_dat$team_poss)),2)
    stl_rate = round(100*(sum(lineup_dat$team_stls)/sum(lineup_dat$team_poss)),2)

    `%opp_fga_blocked` = round(100*(sum(lineup_dat$team_blks)/sum(lineup_dat$opp_fga)),2)

    stats_df = data.frame(lineup,
                          team,
                          team_poss = sum(lineup_dat$team_poss),
                          team_ortg,
                          team_drtg,
                          team_netrtg,
                          team_fga,
                          team_fgm,
                          opp_fga,
                          opp_fgm,
                          opp_pts = sum(lineup_dat$opp_pts),
                          team_pts = sum(lineup_dat$team_pts),
                          team_pfga = sum(lineup_dat$team_pfga),
                          team_pfgm = sum(lineup_dat$team_pfgm),
                          `team_paint_fg%`,
                          `team_3p` = sum(lineup_dat$team_fga3),
                          `team_3pm` = sum(lineup_dat$team_fgm3),
                          `team_3p%`,
                          team_fta = sum(lineup_dat$team_fta),
                          team_ftm = sum(lineup_dat$team_ftm),
                          `team_ft%`,
                          `team_ts%`,
                          `%fgm_as`,
                          team_ast = sum(lineup_dat$team_ast),
                          ast_rate,
                          team_tovs = sum(lineup_dat$team_tovs),
                          tov_rate,
                          team_drb = sum(lineup_dat$team_drebs),
                          `team_drb%`,
                          team_orb = sum(lineup_dat$team_orebs),
                          `team_orb%`,
                          `team_trb%`,
                          `team_trb`,
                          team_blks = sum(lineup_dat$team_blks),
                          blk_rate,
                          `%opp_fga_blocked`,
                          team_stls = sum(lineup_dat$team_stls),
                          stl_rate,
                          team_ballhandling = sum(lineup_dat$team_ballhandling),
                          team_badpass = sum(lineup_dat$team_badpass),
                          team_oFoul = sum(lineup_dat$team_oFoul),
                          team_3sec = sum(lineup_dat$team_3sec),
                          team_8sec = sum(lineup_dat$team_8sec),
                          team_24sec = sum(lineup_dat$team_24sec))

    names(stats_df) = c('Lineup',
                        'Team',
                        'Possessions',
                        'ORtg',
                        'DRtg',
                        'NetRtg',
                        'FGA',
                        'FGM',
                        'Opp FGA',
                        'Opp FGM',
                        'Opp Points',
                        'Points',
                        'Shots at the rim',
                        'Makes at the rim',
                        'Rim FG%',
                        '3PA',
                        '3PM',
                        '3P%',
                        'FTA',
                        'FTM',
                        'FT%',
                        'TS%',
                        '%FGM Assisted',
                        'AST',
                        'AST Rate',
                        'TOV',
                        'TOV Rate',
                        'DRB',
                        'DRB%',
                        'ORB',
                        'ORB%',
                        'TRB%',
                        'TRB',
                        'BLK',
                        'BLk Rate',
                        '%Opponent FGA Blocked',
                        'STL',
                        'STL Rate',
                        'TO - Ballhandling',
                        'TO - Bad Pass',
                        'TO - Offensive Foul',
                        'TO - 3 Sec',
                        'TO - 8 Sec',
                        'TO - 24 Sec')

  team_lineup_dat = rbind(team_lineup_dat, stats_df)
  }

all_lineups_df = rbind(all_lineups_df, team_lineup_dat)

}

#NaN to 0
all_lineups_df[all_lineups_df == 'NaN'] <- 0

#remove lineups based on min.possessions
all_lineups_df = all_lineups_df[which(all_lineups_df$Possessions >= min.possessions),]


if(ratings.only == TRUE) {
  all_lineups_df = all_lineups_df[,1:6]
}


print(all_lineups_df)
}
