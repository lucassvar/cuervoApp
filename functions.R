library(fst)
library(worldfootballR)
library(tidyverse)
get_mode <- function(x) {
	ux <- unique(x)
	tab <- tabulate(match(x, ux))
	max_tab <- max(tab)
	modes <- ux[tab == max_tab]
	paste(modes, collapse = ",")
}

team_replacements <- {c(
	"Leverkusen" = "Bayer Leverkusen",
	"Betis" = "Real Betis",
	"Brighton" = "Brighton & Hove Albion",
	"Eint Frankfurt" = "Eintracht Frankfurt",
	"Inter" = "Internazionale",
	"M'Gladbach" = "Mönchengladbach",
	"Manchester Utd" = "Manchester United",
	"Newcastle Utd" = "Newcastle United",
	"Nott'ham Forest" = "Nottingham Forest",
	"Paris S-G" = "Paris Saint-Germain",
	"Sheffield Utd" = "Sheffield United",
	"Tottenham" = "Tottenham Hotspur",
	"West Ham" = "West Ham United",
	"Wolves" = "Wolverhampton Wanderers",
	"Cen. Córdoba–SdE" = "Central Córdoba (SdE)",
	"Defensa y Just" = "Defensa y Justicia",
	"Sheffield Weds" = "Sheffield Wednesday"
)}
player_replacements <- {c(
	"Marciano Sanca" = "Marciano Tchami",
	"Peter González" = "Peter Federico",
	"Abde Ezzalzouli" = "Abdessamad Ezzalzouli"
)}



get_links <- function(end_years_selected){
	# Countries that don't have data stored
	countries_noDB <- c("ENG", "GER", "ESP", "ITA", "FRA", "ARG", "BRA", "NED", "POR", "BEL", "USA", "MEX")
	countries_2nd_tier_noDB <- c("ENG", "GER", "ESP", "ITA", "FRA")
	male_non_dom_cups <- {c("https://fbref.com/en/comps/8/history/Champions-League-Seasons",
													"https://fbref.com/en/comps/19/history/Europa-League-Seasons",
													"https://fbref.com/en/comps/882/history/Conference-League-Seasons",
													"https://fbref.com/en/comps/14/history/Copa-Libertadores-Seasons",
													"https://fbref.com/en/comps/1/history/World-Cup-Seasons",
													"https://fbref.com/en/comps/676/history/UEFA-Euro-Seasons",
													"https://fbref.com/en/comps/685/history/Copa-America-Seasons")}
	
	
	# Extract URLs
	print("Male")
	male_urls <- {fb_match_urls(country = countries_noDB,
															gender = "M",
															tier = "1st",
															season_end_year = end_years_selected,
															time_pause = 5)}
	
	print("Second")
	second_urls <- {fb_match_urls(country = countries_2nd_tier_noDB,
																gender = "M",
																tier = "2nd",
																season_end_year = end_years_selected,
																time_pause = 5)}
	
	print("Male Cups")
	male_cups <- c()
	for (link in male_non_dom_cups) {
		male_cups <- c(male_cups, fb_match_urls(country = "",
																						gender = "M",
																						season_end_year = end_years_selected,
																						non_dom_league_url = link,
																						time_pause = 5)
		)
	}
	
	
	# Save urls
	all_match_urls <- list("male_urls" = male_urls,
												 "second_urls" = second_urls,
												 "male_cups" = male_cups)
	save(all_match_urls, file = "rda//all_match_urls.rda")
}

extract_extra_data <- function(extraction_quantites = c(5, 1, 1)){
	load("rda//all_match_urls.rda")
	
	all_stat_types <- c("passing", "passing_types", "possession", "misc", "defense")
	columns_merge_by <- c("Player", "Team", "League", "Match_Date", "Min", "First_Pos")
	
	
	# Remove used links (if they exist)
	sel_urls <- c()
	if (file.exists("rda//used_links.rda")) {
		load("rda//used_links.rda")
		sel_urls$male_urls <- setdiff(all_match_urls$male_urls, used_links)[1:extraction_quantites[1]]
		sel_urls$second_urls <- setdiff(all_match_urls$second_urls, used_links)[1:extraction_quantites[2]]
		sel_urls$male_cups <- setdiff(all_match_urls$male_cups, used_links)[1:extraction_quantites[3]]
	} else {
		sel_urls$male_urls <- all_match_urls$male_urls[1:extraction_quantites[1]]
		sel_urls$second_urls <- all_match_urls$second_urls[1:extraction_quantites[2]]
		sel_urls$male_cups <- all_match_urls$male_cups[1:extraction_quantites[3]]
	}
	
	
	# Extract data
	new_outf_data <- c()
	for (stat_type in all_stat_types) {
		merged_data <- c()
		for (current_urls in names(sel_urls)) {
			print(paste(stat_type, "-", current_urls))
			
			if (length(current_urls) > 0) {
				# Extract data
				new_data <- {fb_advanced_match_stats(match_url = sel_urls[[current_urls]],
																						 stat_type = stat_type,
																						 team_or_player = "player",
																						 time_pause = 4) %>%
						separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
						mutate(
							Team_Formation = if_else(Team == Home_Team, Home_Formation, Away_Formation),
							Team_Score = if_else(Team == Home_Team, Home_Score, Away_Score),
							Team_xG = if_else(Team == Home_Team, Home_xG, Away_xG),
							Team_Goals = if_else(Team == Home_Team, Home_Goals, Away_Goals),
							Team_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Home_Yellow_Cards), as.numeric(Away_Yellow_Cards)),
							Team_Red_Cards = if_else(Team == Home_Team, as.numeric(Home_Red_Cards), as.numeric(Away_Red_Cards)),
							Rival_Team = if_else(Team == Home_Team, Away_Team, Home_Team),
							Rival_Formation = if_else(Team == Home_Team, Away_Formation, Home_Formation),
							Rival_Score = if_else(Team == Home_Team, Away_Score, Home_Score),
							Rival_xG = if_else(Team == Home_Team, Away_xG, Home_xG),
							Rival_Goals = if_else(Team == Home_Team, Away_Goals, Home_Goals),
							Rival_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Away_Yellow_Cards), as.numeric(Home_Yellow_Cards)),
							Rival_Red_Cards = if_else(Team == Home_Team, as.numeric(Away_Red_Cards), as.numeric(Home_Red_Cards)),
							
							Gender = "M",
							Tier = case_when(current_urls == "second_urls" ~ "2nd",
															 current_urls == "male_cups" ~ "Cup",
															 TRUE ~ "1st")
						) %>%
						select(-Home_Formation, -Home_Score, -Home_xG, -Home_Goals, -Home_Yellow_Cards, -Home_Red_Cards,
									 -Away_Formation, -Away_Score, -Away_xG, -Away_Goals, -Away_Yellow_Cards, -Away_Red_Cards)}
				
				# Merge the gender-based data with others from same stat_type
				if (length(merged_data) <= 1) {
					merged_data <- new_data
				} else {
					merged_data <- bind_rows(merged_data, new_data)
				}
			}
			
		}
		
		# Merge the stat_type-based data with the other data from other stat_types
		if (length(new_outf_data) <= 1) {
			new_outf_data <- merged_data
		} else {
			common_cols <- intersect(names(new_outf_data), names(merged_data))
			remove_columns <- setdiff(common_cols, columns_merge_by)
			
			new_outf_data <- new_outf_data %>%
				merge(merged_data[, !names(merged_data) %in% remove_columns], by = columns_merge_by, all = T)
		}
		
	}
	
	
	merged_shots <- c()
	for (current_urls in names(sel_urls)) {
		print(paste("Shots -", current_urls))
		
		if (length(current_urls) > 0) {
			# Extract data
			new_shots <- {fb_match_shooting(sel_urls[[current_urls]],
																			time_pause = 4) %>%
					mutate(xG = as.numeric(xG),
								 PSxG = as.numeric(PSxG),
								 SVA = case_when(is.na(PSxG) ~ 0 - xG,
								 								TRUE ~ PSxG - xG),
								 Half = case_when(
								 	as.numeric(str_extract(Minute, "^[0-9]+")) <= 45 ~ "first",
								 	as.numeric(str_extract(Minute, "^[0-9]+")) > 45 & as.numeric(str_extract(Minute, "^[0-9]+")) <= 90 ~ "second",
								 	as.numeric(str_extract(Minute, "^[0-9]+")) > 90 ~ "et"
								 ),
								 Minute = str_replace_all(Minute, "\\+", " + ") %>%
								 	purrr::map_dbl(~ eval(parse(text = .))),
								 Distance = as.numeric(Distance)*0.9144,
								 
								 Gender = "M",
								 Tier = case_when(current_urls == "second_urls" ~ "2nd",
								 								 current_urls == "male_cups" ~ "Cup",
								 								 TRUE ~ "1st"))}
			
			
			# Merge the gender-based data with others from same stat_type
			if (length(merged_shots) <= 1) {
				merged_shots <- new_shots
			} else {
				merged_shots <- bind_rows(merged_shots, new_shots)
			}
		}
		
	}
	
	
	
	# Bind with previous extra data (if exists)
	if (file.exists("rda//extra_data//new_outf_data.fst")) {
		data <- new_outf_data
		new_outf_data <- bind_rows(new_outf_data, data)
		write.fst(new_outf_data, "rda//extra_data//new_outf_data.fst")
		
	} else {
		write.fst(new_outf_data, "rda//extra_data//new_outf_data.fst")
	}
	
	
	if (file.exists("rda//extra_data//merged_shots.fst")) {
		data <- merged_shots
		merged_shots <- bind_rows(merged_shots, data)
		write.fst(merged_shots, "rda//extra_data//merged_shots.fst")
		
	} else {
		write.fst(merged_shots, "rda//extra_data//merged_shots.fst")
	}
	
	
	# Save used links
	used_links <- unique(new_outf_data$Game_URL)
	save(used_links, file = "rda//used_links.rda")
}

load_data <- function(){
	# Load outfield data
	print("Passing")
	pass_stats <- {load_fb_advanced_match_stats(
		country = c("ITA", "ESP", "GER", "ENG", "FRA"),
		gender = "M",
		tier = "1st",
		stat_type = "passing",
		team_or_player = "player"
	) %>%
			select(-Tkl_Tackles, -TklW_Tackles, -Tkl_Challenges, -`Tkl+Int`, -Tkl_percent_Challenges,
						 -`Def 3rd_Tackles`, -`Mid 3rd_Tackles`, -`Att 3rd_Tackles`, -Att_Challenges,
						 -Lost_Challenges, -Blocks_Blocks, -Sh_Blocks, -Pass_Blocks, -Int, -Clr, -Err) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(
				Team_Formation = if_else(Team == Home_Team, Home_Formation, Away_Formation),
				Team_Score = if_else(Team == Home_Team, Home_Score, Away_Score),
				Team_xG = if_else(Team == Home_Team, Home_xG, Away_xG),
				Team_Goals = if_else(Team == Home_Team, Home_Goals, Away_Goals),
				Team_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Home_Yellow_Cards), as.numeric(Away_Yellow_Cards)),
				Team_Red_Cards = if_else(Team == Home_Team, as.numeric(Home_Red_Cards), as.numeric(Away_Red_Cards)),
				Rival_Team = if_else(Team == Home_Team, Away_Team, Home_Team),
				Rival_Formation = if_else(Team == Home_Team, Away_Formation, Home_Formation),
				Rival_Score = if_else(Team == Home_Team, Away_Score, Home_Score),
				Rival_xG = if_else(Team == Home_Team, Away_xG, Home_xG),
				Rival_Goals = if_else(Team == Home_Team, Away_Goals, Home_Goals),
				Rival_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Away_Yellow_Cards), as.numeric(Home_Yellow_Cards)),
				Rival_Red_Cards = if_else(Team == Home_Team, as.numeric(Away_Red_Cards), as.numeric(Home_Red_Cards))
			) %>%
			select(-Home_Formation, -Home_Score, -Home_xG, -Home_Goals, -Home_Yellow_Cards, -Home_Red_Cards,
						 -Away_Formation, -Away_Score, -Away_xG, -Away_Goals, -Away_Yellow_Cards, -Away_Red_Cards)}
	
	
	print("Passing Types")
	pass_types_stats <- {load_fb_advanced_match_stats(
		country = c("ITA", "ESP", "GER", "ENG", "FRA"),
		gender = "M",
		tier = "1st",
		stat_type = "passing_types",
		team_or_player = "player"
	) %>%
			select(-Tkl_Tackles, -TklW_Tackles, -Tkl_Challenges, -`Tkl+Int`, -Tkl_percent_Challenges,
						 -`Def 3rd_Tackles`, -`Mid 3rd_Tackles`, -`Att 3rd_Tackles`, -Att_Challenges,
						 -Lost_Challenges, -Blocks_Blocks, -Sh_Blocks, -Pass_Blocks, -Int, -Clr, -Err) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(
				Team_Formation = if_else(Team == Home_Team, Home_Formation, Away_Formation),
				Team_Score = if_else(Team == Home_Team, Home_Score, Away_Score),
				Team_xG = if_else(Team == Home_Team, Home_xG, Away_xG),
				Team_Goals = if_else(Team == Home_Team, Home_Goals, Away_Goals),
				Team_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Home_Yellow_Cards), as.numeric(Away_Yellow_Cards)),
				Team_Red_Cards = if_else(Team == Home_Team, as.numeric(Home_Red_Cards), as.numeric(Away_Red_Cards)),
				Rival_Team = if_else(Team == Home_Team, Away_Team, Home_Team),
				Rival_Formation = if_else(Team == Home_Team, Away_Formation, Home_Formation),
				Rival_Score = if_else(Team == Home_Team, Away_Score, Home_Score),
				Rival_xG = if_else(Team == Home_Team, Away_xG, Home_xG),
				Rival_Goals = if_else(Team == Home_Team, Away_Goals, Home_Goals),
				Rival_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Away_Yellow_Cards), as.numeric(Home_Yellow_Cards)),
				Rival_Red_Cards = if_else(Team == Home_Team, as.numeric(Away_Red_Cards), as.numeric(Home_Red_Cards))
			) %>%
			select(-Home_Formation, -Home_Score, -Home_xG, -Home_Goals, -Home_Yellow_Cards, -Home_Red_Cards,
						 -Away_Formation, -Away_Score, -Away_xG, -Away_Goals, -Away_Yellow_Cards, -Away_Red_Cards)}
	
	
	print("Possession")
	possession_stats <- {
		load_fb_advanced_match_stats(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			stat_type = "possession",
			team_or_player = "player"
		)  %>%
			select(-Tkl_Tackles, -TklW_Tackles, -Tkl_Challenges, -`Tkl+Int`, -Tkl_percent_Challenges,
						 -`Def 3rd_Tackles`, -`Mid 3rd_Tackles`, -`Att 3rd_Tackles`, -Att_Challenges,
						 -Lost_Challenges, -Blocks_Blocks, -Sh_Blocks, -Pass_Blocks, -Int, -Clr, -Err) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(
				Team_Formation = if_else(Team == Home_Team, Home_Formation, Away_Formation),
				Team_Score = if_else(Team == Home_Team, Home_Score, Away_Score),
				Team_xG = if_else(Team == Home_Team, Home_xG, Away_xG),
				Team_Goals = if_else(Team == Home_Team, Home_Goals, Away_Goals),
				Team_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Home_Yellow_Cards), as.numeric(Away_Yellow_Cards)),
				Team_Red_Cards = if_else(Team == Home_Team, as.numeric(Home_Red_Cards), as.numeric(Away_Red_Cards)),
				Rival_Team = if_else(Team == Home_Team, Away_Team, Home_Team),
				Rival_Formation = if_else(Team == Home_Team, Away_Formation, Home_Formation),
				Rival_Score = if_else(Team == Home_Team, Away_Score, Home_Score),
				Rival_xG = if_else(Team == Home_Team, Away_xG, Home_xG),
				Rival_Goals = if_else(Team == Home_Team, Away_Goals, Home_Goals),
				Rival_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Away_Yellow_Cards), as.numeric(Home_Yellow_Cards)),
				Rival_Red_Cards = if_else(Team == Home_Team, as.numeric(Away_Red_Cards), as.numeric(Home_Red_Cards))
			) %>%
			select(-Home_Formation, -Home_Score, -Home_xG, -Home_Goals, -Home_Yellow_Cards, -Home_Red_Cards,
						 -Away_Formation, -Away_Score, -Away_xG, -Away_Goals, -Away_Yellow_Cards, -Away_Red_Cards)}
	
	
	print("Defense")
	defense_stats <- {
		load_fb_advanced_match_stats(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			stat_type = "defense",
			team_or_player = "player"
		) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(
				Team_Formation = if_else(Team == Home_Team, Home_Formation, Away_Formation),
				Team_Score = if_else(Team == Home_Team, Home_Score, Away_Score),
				Team_xG = if_else(Team == Home_Team, Home_xG, Away_xG),
				Team_Goals = if_else(Team == Home_Team, Home_Goals, Away_Goals),
				Team_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Home_Yellow_Cards), as.numeric(Away_Yellow_Cards)),
				Team_Red_Cards = if_else(Team == Home_Team, as.numeric(Home_Red_Cards), as.numeric(Away_Red_Cards)),
				Rival_Team = if_else(Team == Home_Team, Away_Team, Home_Team),
				Rival_Formation = if_else(Team == Home_Team, Away_Formation, Home_Formation),
				Rival_Score = if_else(Team == Home_Team, Away_Score, Home_Score),
				Rival_xG = if_else(Team == Home_Team, Away_xG, Home_xG),
				Rival_Goals = if_else(Team == Home_Team, Away_Goals, Home_Goals),
				Rival_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Away_Yellow_Cards), as.numeric(Home_Yellow_Cards)),
				Rival_Red_Cards = if_else(Team == Home_Team, as.numeric(Away_Red_Cards), as.numeric(Home_Red_Cards))
			) %>%
			select(-Home_Formation, -Home_Score, -Home_xG, -Home_Goals, -Home_Yellow_Cards, -Home_Red_Cards,
						 -Away_Formation, -Away_Score, -Away_xG, -Away_Goals, -Away_Yellow_Cards, -Away_Red_Cards)}
	
	
	print("Miscellaneous")
	misc_stats <- {
		load_fb_advanced_match_stats(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			stat_type = "misc",
			team_or_player = "player"
		) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(
				Team_Formation = if_else(Team == Home_Team, Home_Formation, Away_Formation),
				Team_Score = if_else(Team == Home_Team, Home_Score, Away_Score),
				Team_xG = if_else(Team == Home_Team, Home_xG, Away_xG),
				Team_Goals = if_else(Team == Home_Team, Home_Goals, Away_Goals),
				Team_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Home_Yellow_Cards), as.numeric(Away_Yellow_Cards)),
				Team_Red_Cards = if_else(Team == Home_Team, as.numeric(Home_Red_Cards), as.numeric(Away_Red_Cards)),
				Rival_Team = if_else(Team == Home_Team, Away_Team, Home_Team),
				Rival_Formation = if_else(Team == Home_Team, Away_Formation, Home_Formation),
				Rival_Score = if_else(Team == Home_Team, Away_Score, Home_Score),
				Rival_xG = if_else(Team == Home_Team, Away_xG, Home_xG),
				Rival_Goals = if_else(Team == Home_Team, Away_Goals, Home_Goals),
				Rival_Yellow_Cards = if_else(Team == Home_Team, as.numeric(Away_Yellow_Cards), as.numeric(Home_Yellow_Cards)),
				Rival_Red_Cards = if_else(Team == Home_Team, as.numeric(Away_Red_Cards), as.numeric(Home_Red_Cards))
			) %>%
			select(-Home_Formation, -Home_Score, -Home_xG, -Home_Goals, -Home_Yellow_Cards, -Home_Red_Cards,
						 -Away_Formation, -Away_Score, -Away_xG, -Away_Goals, -Away_Yellow_Cards, -Away_Red_Cards)}
	
	
	
	# Common columns between dfs, to merge by and to remove
	common_cols <- {intersect(names(possession_stats),
													 intersect(names(defense_stats),
													 					intersect(names(misc_stats),
													 										intersect(names(pass_stats),
													 															names(pass_types_stats)))))}
	columns_merge_by <- c("Player", "Team", "League", "Match_Date", "Min", "First_Pos")
	remove_columns <- setdiff(common_cols, columns_merge_by)
	
	
	# Merge data into one dataframe and save it
	outfield_data <- possession_stats %>%
		merge(pass_stats[, !names(pass_stats) %in% remove_columns], by = columns_merge_by, all = T) %>%
		merge(pass_types_stats[, !names(pass_types_stats) %in% remove_columns], by = columns_merge_by, all = T) %>%
		merge(defense_stats[, !names(defense_stats) %in% remove_columns], by = columns_merge_by, all = T) %>%
		merge(misc_stats[, !names(misc_stats) %in% c(remove_columns, "Int")], by = columns_merge_by, all = T) %>%
		distinct(across(all_of(columns_merge_by)), .keep_all = TRUE) %>%
		select(-Live_Touches, -Att, -Cmp_Outcomes, -Crs_Pass_Types, -TklW)
	
	
	
	# Load fbref Shots
	print("FB Shots")
	fbref_shots <- {
		load_fb_match_shooting(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			season_end_year = 2024
		) %>%
			mutate(xG = as.numeric(xG),
						 PSxG = as.numeric(PSxG),
						 SVA = case_when(is.na(PSxG) ~ 0 - xG,
						 								TRUE ~ PSxG - xG),
						 Half = case_when(
						 	as.numeric(str_extract(Minute, "^[0-9]+")) <= 45 ~ "first",
						 	as.numeric(str_extract(Minute, "^[0-9]+")) > 45 & as.numeric(str_extract(Minute, "^[0-9]+")) <= 90 ~ "second",
						 	as.numeric(str_extract(Minute, "^[0-9]+")) > 90 ~ "et"
						 ),
						 Minute = str_replace_all(Minute, "\\+", " + ") %>%
						 	purrr::map_dbl(~ eval(parse(text = .))),
						 Distance = as.numeric(Distance)*0.9144)
	}
	
	
	# If there's extra outfield data, combine with the loaded one
	if (file.exists("rda//extra_data//new_outf_data.fst")) {
		# Load extra outfield
		new_outf_data <- read.fst("rda//extra_data//new_outf_data.fst")
		
		# Combine extra + loaded outfield data and save it
		outfield_data <- bind_rows(outfield_data, new_outf_data)
		write.fst(outfield_data, "rda//outfield_data.fst")
		
	} else {
		write.fst(outfield_data, "rda//outfield_data.fst")
	}
	
	
	# If there's extra shooting data, combine with the loaded one
	if (file.exists("rda//extra_data//merged_shots.fst")) {
		# Load extra shot data
		merged_shots <- read.fst("rda//extra_data//merged_shots.fst")
		
		# Combine extra + loaded shot data and save it
		fbref_shots <- bind_rows(fbref_shots, merged_shots)
		write.fst(fbref_shots, "rda//fbref_shots.fst")
		
	} else {
		write.fst(fbref_shots, "rda//fbref_shots.fst")
	}
	
	
	
	# Shooting dataframes
	shots_with_mins <- {fbref_shots %>%
			filter(!str_detect(Player, "\\(pen\\)$")) %>%
			rename(
				Team = Squad,
				Match_Date = Date,
				League = Competition_Name
			) %>%
			mutate(
				Team = coalesce(team_replacements[Team], Team),
				Player = coalesce(player_replacements[Player], Player)
			) %>%
			left_join(possession_stats %>% 
									select(Player, Team, Match_Date, League, Min, First_Pos),
								by = c("Player", "Team", "Match_Date", "League"),
								relationship = "many-to-many") %>%
			filter(!is.na(Min), !is.na(First_Pos))}
	shooting_distances <- {shots_with_mins %>%
			select(Player, Team, Min, xG, Outcome, Distance, League, First_Pos, Match_Date) %>%
			mutate(
				outcome_level = match(Outcome, c("Goal", "Saved", "Saved off Target", "Off Target", "Blocked", "Woodwork")),
				Outcome = recode(Outcome,
												 "Goal" = "Gol",
												 "Saved" = "Atajado",
												 "Saved off Target" = "Atajado Fuera",
												 "Off Target" = "Fuera",
												 "Blocked" = "Bloqueado",
												 "Woodwork" = "Palo")
			)}
	per_match_shots <- {shots_with_mins %>%
			group_by(Player, Team, League, Match_Date) %>%
			summarise(
				Min = mean(Min, na.rm = TRUE),
				xG = sum(xG, na.rm = TRUE),
				PSxG = sum(PSxG, na.rm = TRUE),
				SVA = sum(SVA, na.rm = TRUE),
				`xG/Tiro` = mean(xG, na.rm = TRUE),
				Distance = mean(Distance, na.rm = TRUE),
				First_Pos = get_mode(First_Pos),  # Assuming you have a fast Mode function
				Tiros = n(), 
				SoT = sum(Outcome %in% c("Goal", "Saved"), na.rm = TRUE),
				Goals = sum(Outcome == "Goal", na.rm = TRUE),
				Shots_from_FK = sum(Notes == "Free kick", na.rm = TRUE),
				Goals_minus_xG = Goals - xG
			)}
	
	
	# SCA dataframes
	per_match_SCA <- {fbref_shots %>%
			filter(!str_detect(Player, "\\(pen\\)$")) %>%
			select(-Player) %>%
			rename(
				Player = Player_SCA_1,
				Team = Squad,
				Match_Date = Date,
				League = Competition_Name
			) %>%
			mutate(
				Team = coalesce(team_replacements[Team], Team),
				Player = coalesce(player_replacements[Player], Player)
			) %>%
			left_join(possession_stats %>% 
									select(Player, Team, Match_Date, League, Min, First_Pos),
								by = c("Player", "Team", "Match_Date", "League"),
								relationship = "many-to-many") %>%
			filter(!is.na(Min), !is.na(First_Pos)) %>%
			group_by(Player, Team, League, Match_Date) %>%
			summarise(
				Min = mean(Min, na.rm = TRUE),
				First_Pos = get_mode(First_Pos),
				SCA = n(),
				`xG (SCA)` = sum(xG, na.rm = TRUE),
				`Pases en Juego` = sum(Event_SCA_1 == "Pass (Live)", na.rm = TRUE),
				`xG (Pases en Juego)` = sum(xG[Event_SCA_1 == "Pass (Live)"], na.rm = TRUE),
				`Pases a Pelota Parada` = sum(Event_SCA_1 == "Pass (Dead)", na.rm = TRUE),
				`xG (Pases a Pelota Parada)` = sum(xG[Event_SCA_1 == "Pass (Dead)"], na.rm = TRUE),
				`Regates` = sum(Event_SCA_1 == "Take-On", na.rm = TRUE),
				`xG (Regates)` = sum(xG[Event_SCA_1 == "Take-On"], na.rm = TRUE),
				`Tiros` = sum(Event_SCA_1 == "Shot", na.rm = TRUE),
				`xG (Tiros)` = sum(xG[Event_SCA_1 == "Shot"], na.rm = TRUE),
				`Faltas Recibidas` = sum(Event_SCA_1 == "Fouled", na.rm = TRUE),
				`xG (Faltas Recibidas)` = sum(xG[Event_SCA_1 == "Fouled"], na.rm = TRUE),
				`Acciones Defensivas` = sum(Event_SCA_1 %in% c("Tackle", "Interception"), na.rm = TRUE),
				`xG (Acciones Defensivas)` = sum(xG[Event_SCA_1 %in% c("Tackle", "Interception")], na.rm = TRUE)
			)}
	
	
	# GCA dataframes
	per_match_GCA <- {fbref_shots %>%
			filter(!str_detect(Player, "\\(pen\\)$")) %>%
			select(-Player) %>%
			rename(
				Player = Player_SCA_1,
				Team = Squad,
				Match_Date = Date,
				League = Competition_Name
			) %>%
			mutate(
				Team = coalesce(team_replacements[Team], Team),
				Player = coalesce(player_replacements[Player], Player)
			) %>%
			left_join(possession_stats %>% 
									select(Player, Team, Match_Date, League, Min, First_Pos),
								by = c("Player", "Team", "Match_Date", "League"),
								relationship = "many-to-many") %>%
			filter(!is.na(Min), !is.na(First_Pos)) %>%
			group_by(Player, Team, League, Match_Date) %>%
			summarise(
				Min = mean(Min, na.rm = TRUE),
				First_Pos = get_mode(First_Pos),
				GCA = n(),
				`xG (GCA)` = sum(xG, na.rm = TRUE),
				`Pases en Juego` = sum(Event_SCA_1 == "Pass (Live)" & Outcome == "Goal", na.rm = TRUE),
				`xG (Pases en Juego)` = sum(xG[Event_SCA_1 == "Pass (Live)" & Outcome == "Goal"], na.rm = TRUE),
				`Pases a Pelota Parada` = sum(Event_SCA_1 == "Pass (Dead)" & Outcome == "Goal", na.rm = TRUE),
				`xG (Pases a Pelota Parada)` = sum(xG[Event_SCA_1 == "Pass (Dead)" & Outcome == "Goal"], na.rm = TRUE),
				`Regates` = sum(Event_SCA_1 == "Take-On" & Outcome == "Goal", na.rm = TRUE),
				`xG (Regates)` = sum(xG[Event_SCA_1 == "Take-On" & Outcome == "Goal"], na.rm = TRUE),
				`Tiros` = sum(Event_SCA_1 == "Shot" & Outcome == "Goal", na.rm = TRUE),
				`xG (Tiros)` = sum(xG[Event_SCA_1 == "Shot" & Outcome == "Goal"], na.rm = TRUE),
				`Faltas Recibidas` = sum(Event_SCA_1 == "Fouled" & Outcome == "Goal", na.rm = TRUE),
				`xG (Faltas Recibidas)` = sum(xG[Event_SCA_1 == "Fouled" & Outcome == "Goal"], na.rm = TRUE),
				`Acciones Defensivas` = sum(Event_SCA_1 %in% c("Tackle", "Interception") & Outcome == "Goal", na.rm = TRUE),
				`xG (Acciones Defensivas)` = sum(xG[Event_SCA_1 %in% c("Tackle", "Interception") & Outcome == "Goal"], na.rm = TRUE)
			)}
	
	
	# Save Shooting, SCA and GCA dataframes
	write.fst(shooting_distances, "rda//shooting_distances.fst")
	write.fst(per_match_shots, "rda//per_match_shots.fst")
	write.fst(per_match_SCA, "rda//per_match_SCA.fst")
	write.fst(per_match_GCA, "rda//per_match_GCA.fst")
	
}

