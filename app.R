library(bslib)
library(shiny)
library(dplyr)
library(gt)
library(gtExtras)
library(ggplot2)
library(shinyjs)
library(rlang)
library(waffle)
library(hexbin)
library(ggsoccer)
understat_shots <- read.fst("rda//understat_shots.fst")
outfield_data <- read.fst("rda//outfield_data.fst")
shooting_distances <- read.fst("rda//shooting_distances.fst")
per_match_shots <- read.fst("rda//per_match_shots.fst")
per_match_SCA <- read.fst("rda//per_match_SCA.fst")
per_match_GCA <- read.fst("rda//per_match_GCA.fst")

# League codes
top_5_europe_leagues <- c("La Liga", "Premier League", "Ligue 1", "Serie A", "Fußball-Bundesliga")

# Function to get the mode (most common value)
get_mode <- function(x) {
	ux <- unique(x)
	tab <- tabulate(match(x, ux))
	max_tab <- max(tab)
	modes <- ux[tab == max_tab]
	paste(modes, collapse = ",")
}

# Column names
negative_columns <- {c("Controles Fallidos", "Pérdidas", "Regates Tackleados",
											 "% Regates Tackleados", "1v1 Perdidos", "Errores")}
passing_columns <- {c("Pases Completados", "Pases Intentados", "% Pases Completados", "Distancia Total (Pases)", "Distancia Progresiva (Pases)",
											"Pases Cortos Completados", "Pases Cortos Intentados", "% Pases Cortos Completados", 
											"Pases Medios Completados", "Pases Medios Intentados", "% Pases Medios Completados",
											"Pases Largos Completados", "Pases Largos Intentados", "% Pases Largos Completados",
											"Asistencias", "xAG", "xA", "Pases Clave", "Pases al Últ 1/3", 
											"Pases al Área", "Centros al Área", "Pases Progresivos", "Pases Filtrados")}
possession_columns <- {c("Toques", "Toques Def. Pen", "Toques 1/3 Def", 
												 "Toques 1/3 Med", "Toques 1/3 Att", "Toques Att. Pen", "Regates Intentados", 
												 "Regates Completados", "% Regates Completados", "Regates Tackleados", "% Regates Tackleados",
												 "Acarreos", "Distancia Total (Acarreos)", "Distancia Prog. (Acarreos)", 
												 "Acarreos Progresivos", "Acarreos al Últ 1/3", "Acarreos al Área", "Controles Fallidos", "Pérdidas", 
												 "Pases Recibidos", "Pases Prog. Recibidos")}
defense_columns <- {c("Entradas", "Entradas Ganadas", "Entradas 1/3 Def", "Entradas 1/3 Med", 
											"Entradas 1/3 Att", "Regateadores Tacleados", "Regateadores Desafiados", "1v1 Perdidos", "% Regateadores Tacleados",
											"Bloqueos", "Bloqueos (Tiros)", "Bloqueos (Pases)",
											"Intercepciones", "Entradas + Intercepcions", "% Duelos Aéreos Ganados", "Despejes", "Errores")}
shooting_columns <- {c("Goles", "Tiros", "Tiros al Arco", "% Tiros al Arco", "Goles/Tiro",
											 "Goles/Tiro al Arco", "Distancia Promedio", "Tiros de Tiro Libre",
											 "xG", "PSxG", "SVA (PSxG - xG)", "xG/Tiro", "Goles - xG")}
sca_columns <- {c(
	"SCA", "xG (SCA)",
	"Pases en Juego", "xG (Pases en Juego)",
	"Pases a Pelota Parada", "xG (Pases a Pelota Parada)",
	"Regates", "xG (Regates)",
	"Tiros", "xG (Tiros)",
	"Faltas Recibidas", "xG (Faltas Recibidas)",
	"Acciones Defensivas", "xG (Acciones Defensivas)"
)}
gca_columns <- {c(
	"GCA", "xG (GCA)",
	"Pases en Juego", "xG (Pases en Juego)",
	"Pases a Pelota Parada", "xG (Pases a Pelota Parada)",
	"Regates", "xG (Regates)",
	"Tiros", "xG (Tiros)",
	"Faltas Recibidas", "xG (Faltas Recibidas)",
	"Acciones Defensivas", "xG (Acciones Defensivas)"
)}

# Per 90 and mean columns
p90_columns <- {c(
	"Cmp_Total", "Att_Total", "TotDist_Total", "PrgDist_Total",
	"Cmp_Short", "Att_Short", "Cmp_Medium", "Att_Medium", "Cmp_Long", "Att_Long",
	"Ast", "xAG", "xA", "KP", "Final_Third", "PPA", "CrsPA", "PrgP",
	"Live_Pass_Types", "Dead_Pass_Types", 
	"FK_Pass_Types", "TB_Pass_Types", "Sw_Pass_Types", 
	"TI_Pass_Types", "CK_Pass_Types", 
	"In_Corner_Kicks", "Out_Corner_Kicks", "Str_Corner_Kicks", 
	"Off_Outcomes", "Blocks_Outcomes",
	"Tkl_Tackles", "TklW_Tackles", "Def 3rd_Tackles",
	"Mid 3rd_Tackles", "Att 3rd_Tackles", "Tkl_Challenges", 
	"Att_Challenges", "Lost_Challenges", "Blocks_Blocks", 
	"Sh_Blocks", "Pass_Blocks", "Tkl+Int", "Clr", "Err",
	"Touches_Touches", "Def Pen_Touches", "Def 3rd_Touches", 
	"Mid 3rd_Touches", "Att 3rd_Touches", "Att Pen_Touches", 
	"Att_Take_Ons", "Succ_Take_Ons", "Tkld_Take_Ons", 
	"Carries_Carries", "TotDist_Carries", "PrgDist_Carries", 
	"PrgC_Carries", "Final_Third_Carries", "CPA_Carries", 
	"Mis_Carries", "Dis_Carries", "Rec_Receiving", "PrgR_Receiving",
	"CrdY", "CrdR", "2CrdY", "Fls", "Fld", "Off", 
	"Crs", "Int", "PKwon", "PKcon", "OG", 
	"Recov", "Won_Aerial_Duels", "Lost_Aerial_Duels"
)}
mean_columns <- {c(
	"Cmp_percent_Total", "Cmp_percent_Short", "Cmp_percent_Medium", "Cmp_percent_Long",
	"Tkl_percent_Challenges",
	"Succ_percent_Take_Ons", "Tkld_percent_Take_Ons",
	"Won_percent_Aerial_Duels"
)}





ui <- page_sidebar(
	title = "Cuervo App",
	useShinyjs(),
	fillable = F,
	
	sidebar = sidebar(
		selectizeInput("player_sel", "Jugador:", 
									 choices = NULL,  # Start with no choices
									 multiple = FALSE),
		selectInput("team_sel", "Equipo:", choices = unique(outfield_data$Team), multiple = TRUE, selected = "Chelsea"),
		selectInput("comp_positions", "Posiciones Comp:", choices = c("Portero" = "GK", "Def. Central" = "CB", "Lateral Der." = "RB", "Lateral Izq." = "LB",
																														 "Carrilero" = "WB", "Medio. Defensivo" = "DM", "Medio. Central" = "CM", "Medio. Atacante" = "AM",
																														 "Medio Izq." = "LM", "Medio Der." = "RM", "Extremo Der." = "RW", "Extremo Izq." =  "LW", "Delantero" = "FW"),
								multiple = TRUE, selected = c("CM", "DM")),
		selectInput("comp_leagues", "Ligas Comp:", choices = c("Top 5 EUROPA" = "top_5_europa",
																													 "La Liga" = "La Liga", "Premier League" = "Premier League",
																													 "Ligue 1" = "Ligue 1", "Serie A" = "Serie A", "Bundesliga" = "Fußball-Bundesliga"),
								multiple = TRUE, selected = "top_5_europa"),
		dateRangeInput("date_range", "Fechas Data:", start = Sys.Date()-365, end = Sys.Date()),
		selectizeInput("selected_compared_players", "Jugadores Comp:", 
									 choices = NULL,  # Start with no choices
									 multiple = TRUE),
		numericInput("min_90s", "Min 90s:", value = 10),
		checkboxInput("remove_outliers", "Ocultar Outliers", value = FALSE),
		actionButton("submit_btn", "Enviar")
	),
	
	
	layout_columns(
		col_widths = c(4, 8),
		navset_card_underline(
			title = "Percentiles",
			nav_panel("Pases", gt_output("gtout")),
			nav_panel("Pos.", gt_output("gtout_possession")),
			nav_panel("Defensa", gt_output("gtout_defense")),
			nav_panel("Tiros", gt_output("gtout_shooting")),
			nav_panel("SCA", gt_output("gtout_sca")),
			nav_panel("GCA", gt_output("gtout_gca")),
			height = 1000
			),
		layout_column_wrap(
			width = 1, heights_equal = "row", height = 1000,
			card(
				full_screen = TRUE,
				height = 660,
				fill = FALSE,
				card_header("Variables X-Y"),
				
				layout_columns(
					width = 1,
					heights_equal = "row",
					col_widths = c(6, 6),
					height = 15,
					selectInput("x_axis", "X-axis:", choices = c(passing_columns, defense_columns, possession_columns) %>% sort(), selected = "Pases Intentados"),
					selectInput("y_axis", "Y-axis:", choices = c(passing_columns, defense_columns, possession_columns) %>% sort(), selected = "% Pases Completados")
				),
				
				plotOutput("scatter_plot")
			),
			
			layout_column_wrap(
				width = 1/3, height = 325,
				card(full_screen = TRUE, card_header("Pases Cortos"), plotOutput("short_passes")),
				card(full_screen = TRUE, card_header("Pases Medios"), plotOutput("medium_passes")),
				card(full_screen = TRUE, card_header("Pases Largos"), plotOutput("long_passes"))
			)
		)
	),
	
	
	
	layout_column_wrap(
		width = NULL,
		style = css(grid_template_columns = "1.5fr 2fr"),
		heights_equal = "row",
		layout_column_wrap(
			width = 1/2,
			card(fill = FALSE, full_screen = TRUE, card_header("Duelos Aéreos"), plotOutput("aerial_duels"), height = 325),
			card(fill = FALSE, full_screen = TRUE, card_header("Regateadores Tacleados"), plotOutput("challenges_tkld"), height = 325),
			card(fill = FALSE, full_screen = TRUE, card_header("Regates Completados"), plotOutput("succ_take_ons"), height = 325)
		),
		layout_column_wrap(
			height = 700,
			width = 1,
			card(full_screen = TRUE, card_header("Entradas por Zona"), plotOutput("tackles_zones")),
			card(full_screen = TRUE, card_header("Toques por Zona"), plotOutput("touches_zones"))
		)
	),
	
	
	
	layout_column_wrap(
		width = NULL,
		style = css(grid_template_columns = "1fr 3fr"),
		height = 800,
		card(full_screen = TRUE, card_header("Distancia de Tiros"), plotOutput("sh_distances")),
		navset_card_underline(
			title = "Tiros",
			nav_panel("Distribución", plotOutput("sh_distribution")),
			nav_panel("Ubicación", plotOutput("sh_locations"))
		)
	)
	
	
	
	
)






server <- function(input, output, session) {
	# Populate the player selector
	updateSelectizeInput(session, "player_sel", 
											 choices = unique(outfield_data$Player), 
											 selected = "Enzo Fernández",
											 server = TRUE)
	
	# Populate the compared players selector
	updateSelectizeInput(session, "selected_compared_players", 
											 choices = unique(outfield_data$Player),
											 selected = c("Declan Rice", "Rodri", "Moisés Caicedo"),
											 server = TRUE)
	
	
	# Filter data based on inputs
	filtered_player_data <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$team_sel, input$date_range)
		
		ply_data <- outfield_data %>%
			filter(Player == input$player_sel,
						 Team %in% input$team_sel,
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2]) %>%
			select(Player, Team, Min, First_Pos, League,
						 all_of(p90_columns), all_of(mean_columns)) %>%
			group_by(Player, Team, League) %>%
			summarise(
				Min = sum(Min, na.rm = T) / 90,
				First_Pos = get_mode(First_Pos),
				across(all_of(p90_columns), ~ round(sum(.x, na.rm = TRUE) / Min, 2)),
				across(all_of(mean_columns), ~ round(mean(.x, na.rm = TRUE), 1))
			)
		
		colnames(ply_data) <- {c("Player", "Team", "League", "First_Pos", "Min",
															 "Pases Completados", "Pases Intentados",
															 "Distancia Total (Pases)", "Distancia Progresiva (Pases)",
															 "Pases Cortos Completados", "Pases Cortos Intentados", 
															 "Pases Medios Completados", "Pases Medios Intentados",
															 "Pases Largos Completados", "Pases Largos Intentados",
															 "Asistencias", "xAG", "xA", "Pases Clave", "Pases al Últ 1/3", 
															 "Pases al Área", "Centros al Área", "Pases Progresivos",
															 "Pases en Juego", "Pases a Pelota Parada", "Pases de Tiro Libre",
															 "Pases Filtrados", "Pases Cambio de Banda", "Laterales", "Corners",
															 "Corners hacia Dentro", "Corners hacia Fuera", "Corners Derechos",
															 "Pases en Offside", "Pases Bloqueados",
															 "Entradas", "Entradas Ganadas", "Entradas 1/3 Def", "Entradas 1/3 Med", 
															 "Entradas 1/3 Att", "Regateadores Tacleados", "Regateadores Desafiados", "1v1 Perdidos",
															 "Bloqueos", "Bloqueos (Tiros)", "Bloqueos (Pases)",
															 "Entradas + Intercepcions", "Despejes", "Errores",
															 "Toques", "Toques Def. Pen", "Toques 1/3 Def", 
															 "Toques 1/3 Med", "Toques 1/3 Att", "Toques Att. Pen", "Regates Intentados", 
															 "Regates Completados", "Regates Tackleados",
															 "Acarreos", "Distancia Total (Acarreos)", "Distancia Prog. (Acarreos)", 
															 "Acarreos Progresivos", "Acarreos al Últ 1/3", "Acarreos al Área",
															 "Controles Fallidos", "Pérdidas", 
															 "Pases Recibidos", "Pases Prog. Recibidos",
															 "Amarillas", "Rojas", "Doble Amarillas", "Faltas", "Faltas Recibidas", "Offsides", "Centros",
															 "Intercepciones", "Penales Ganados", "Penales Cometidos",
															 "Goles En Contra", "Pelotas Recuperadas", "Duelos Aéreos Ganados",
															 "Duelos Aéreos Perdidos", "% Pases Completados", "% Pases Cortos Completados",
															 "% Pases Medios Completados", "% Pases Largos Completados", "% Regateadores Tacleados",
															 "% Regates Completados", "% Regates Tackleados", "% Duelos Aéreos Ganados")}
		
		ply_data
	})
	
	filtered_comp_pool_data <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$comp_positions, input$min_90s, input$comp_leagues, input$date_range)
		
		cp_data <- outfield_data %>%
			filter(Player != input$player_sel,
						 str_detect(First_Pos, str_c(input$comp_positions, collapse = "|")),
						 League %in% case_when(
						 	input$comp_leagues == "top_5_europa" ~ top_5_europe_leagues,
						 	TRUE ~ input$comp_leagues
						 ),
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2]) %>%
			select(Player, Team, Min, First_Pos, League,
						 all_of(p90_columns), all_of(mean_columns)) %>%
			group_by(Player, Team, League) %>%
			summarise(
				Min = sum(Min, na.rm = T) / 90,
				First_Pos = get_mode(First_Pos),
				across(all_of(p90_columns), ~ round(sum(.x, na.rm = TRUE) / Min, 2)),
				across(all_of(mean_columns), ~ round(mean(.x, na.rm = TRUE), 1))
			) %>%
			filter(Min >= input$min_90s)
		
		colnames(cp_data) <- {c("Player", "Team", "League", "First_Pos", "Min",
															 "Pases Completados", "Pases Intentados",
															 "Distancia Total (Pases)", "Distancia Progresiva (Pases)",
															 "Pases Cortos Completados", "Pases Cortos Intentados", 
															 "Pases Medios Completados", "Pases Medios Intentados",
															 "Pases Largos Completados", "Pases Largos Intentados",
															 "Asistencias", "xAG", "xA", "Pases Clave", "Pases al Últ 1/3", 
															 "Pases al Área", "Centros al Área", "Pases Progresivos",
															 "Pases en Juego", "Pases a Pelota Parada", "Pases de Tiro Libre",
															 "Pases Filtrados", "Pases Cambio de Banda", "Laterales", "Corners",
															 "Corners hacia Dentro", "Corners hacia Fuera", "Corners Derechos",
															 "Pases en Offside", "Pases Bloqueados",
															 "Entradas", "Entradas Ganadas", "Entradas 1/3 Def", "Entradas 1/3 Med", 
															 "Entradas 1/3 Att", "Regateadores Tacleados", "Regateadores Desafiados", "1v1 Perdidos",
															 "Bloqueos", "Bloqueos (Tiros)", "Bloqueos (Pases)",
															 "Entradas + Intercepcions", "Despejes", "Errores",
															 "Toques", "Toques Def. Pen", "Toques 1/3 Def", 
															 "Toques 1/3 Med", "Toques 1/3 Att", "Toques Att. Pen", "Regates Intentados", 
															 "Regates Completados", "Regates Tackleados",
															 "Acarreos", "Distancia Total (Acarreos)", "Distancia Prog. (Acarreos)", 
															 "Acarreos Progresivos", "Acarreos al Últ 1/3", "Acarreos al Área",
															 "Controles Fallidos", "Pérdidas", 
															 "Pases Recibidos", "Pases Prog. Recibidos",
															 "Amarillas", "Rojas", "Doble Amarillas", "Faltas", "Faltas Recibidas", "Offsides", "Centros",
															 "Intercepciones", "Penales Ganados", "Penales Cometidos",
															 "Goles En Contra", "Pelotas Recuperadas", "Duelos Aéreos Ganados",
															 "Duelos Aéreos Perdidos", "% Pases Completados", "% Pases Cortos Completados",
															 "% Pases Medios Completados", "% Pases Largos Completados", "% Regateadores Tacleados",
															 "% Regates Completados", "% Regates Tackleados", "% Duelos Aéreos Ganados")}
		
		cp_data
	})
	
	
	# Filter shooting data based on inputs
	filtered_shooting_player_data <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$team_sel, input$date_range)
		
		per_match_shots %>%
			filter(Player == input$player_sel,
						 Team %in% input$team_sel,
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2]) %>%
			select(Player, Team, League, First_Pos, Min, xG, PSxG, SVA, Distance,
						 `xG/Tiro`, Tiros, SoT, Goals, Shots_from_FK, Goals_minus_xG) %>%
			group_by(Player, Team, League) %>%
			summarise(
				Min = sum(Min, na.rm = T) / 90,
				First_Pos = get_mode(First_Pos),
				
				xG = round(sum(xG, na.rm = T) / Min, 2),
				PSxG = round(sum(PSxG, na.rm = T) / Min, 2),
				`SVA (PSxG - xG)` = round(sum(SVA, na.rm = T) / Min, 2),
				`xG/Tiro` = round(mean(`xG/Tiro`, na.rm = T), 2),
				`Distancia Promedio` = round(mean(Distance, na.rm = T), 2),
				Tiros = round(sum(Tiros) / Min, 2), 
				`Tiros al Arco` = round(sum(SoT, na.rm = T) / Min, 2),
				Goles = round(sum(Goals, na.rm = T) / Min, 2),
				`Tiros de Tiro Libre` = round(sum(Shots_from_FK, na.rm = T) / Min, 2),
				`Goles - xG` = round(sum(Goals_minus_xG, na.rm = T) / Min, 2),
				
				`Goles/Tiro` = round(Goles / Tiros, 2),
				`Goles/Tiro al Arco` = round(Goles / `Tiros al Arco`, 2),
				`% Tiros al Arco` = round(`Tiros al Arco` / Tiros * 100, 1)
			)
	})
	
	filtered_shooting_comp_pool_data <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$comp_positions, input$min_90s, input$comp_leagues)
		
		per_match_shots %>%
			filter(Player != input$player_sel,
						 str_detect(First_Pos, str_c(input$comp_positions, collapse = "|")),
						 League %in% case_when(
						 	input$comp_leagues == "top_5_europa" ~ top_5_europe_leagues,
						 	TRUE ~ input$comp_leagues
						 ),
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2]) %>%
			select(Player, Team, League, First_Pos, Min, xG, PSxG, SVA, Distance,
						 `xG/Tiro`, Tiros, SoT, Goals, Shots_from_FK, Goals_minus_xG) %>%
			group_by(Player, Team, League) %>%
			summarise(
				Min = sum(Min, na.rm = T) / 90,
				First_Pos = get_mode(First_Pos),
				
				xG = round(sum(xG, na.rm = T) / Min, 2),
				PSxG = round(sum(PSxG, na.rm = T) / Min, 2),
				`SVA (PSxG - xG)` = round(sum(SVA, na.rm = T) / Min, 2),
				`xG/Tiro` = round(mean(`xG/Tiro`, na.rm = T), 2),
				`Distancia Promedio` = round(mean(Distance, na.rm = T), 2),
				Tiros = round(sum(Tiros) / Min, 2), 
				`Tiros al Arco` = round(sum(SoT, na.rm = T) / Min, 2),
				Goles = round(sum(Goals, na.rm = T) / Min, 2),
				`Tiros de Tiro Libre` = round(sum(Shots_from_FK, na.rm = T) / Min, 2),
				`Goles - xG` = round(sum(Goals_minus_xG, na.rm = T) / Min, 2),
				
				`Goles/Tiro` = round(Goles / Tiros, 2),
				`Goles/Tiro al Arco` = round(Goles / `Tiros al Arco`, 2),
				`% Tiros al Arco` = round(`Tiros al Arco` / Tiros * 100, 1)
			) %>%
			filter(Min >= input$min_90s)
	})
	
	
	# Filter SCA data based on inputs
	filtered_SCA_player_data <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$team_sel, input$date_range)
		
		per_match_SCA %>%
			filter(Player == input$player_sel,
						 Team %in% input$team_sel,
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2]) %>%
			group_by(Player, Team, League) %>%
			summarise(
				Min = sum(Min, na.rm = TRUE) / 90,
				First_Pos = get_mode(First_Pos),
				
				SCA = round(sum(SCA, na.rm = T) / Min, 2),
				`xG (SCA)` = round(sum(`xG (SCA)`, na.rm = T) / Min, 2),
				
				`Pases en Juego` = round(sum(`Pases en Juego`, na.rm = T) / Min, 2),
				`xG (Pases en Juego)` = round(sum(`xG (Pases en Juego)`, na.rm = T) / Min, 2),
				
				`Pases a Pelota Parada` = round(sum(`Pases a Pelota Parada`, na.rm = T) / Min, 2),
				`xG (Pases a Pelota Parada)` = round(sum(`xG (Pases a Pelota Parada)`, na.rm = T) / Min, 2),
				
				`Regates` = round(sum(`Regates`, na.rm = T) / Min, 2),
				`xG (Regates)` = round(sum(`xG (Regates)`, na.rm = T) / Min, 2),
				
				`Tiros` = round(sum(`Tiros`, na.rm = T) / Min, 2),
				`xG (Tiros)` = round(sum(`xG (Tiros)`, na.rm = T) / Min, 2),
				
				`Faltas Recibidas` = round(sum(`Faltas Recibidas`, na.rm = T) / Min, 2),
				`xG (Faltas Recibidas)` = round(sum(`xG (Faltas Recibidas)`, na.rm = T) / Min, 2),
				
				`Acciones Defensivas` = round(sum(`Acciones Defensivas`, na.rm = T) / Min, 2),
				`xG (Acciones Defensivas)` = round(sum(`xG (Acciones Defensivas)`, na.rm = T) / Min, 2)
			)
	})
	
	filtered_SCA_comp_pool_data <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$comp_positions, input$min_90s, input$comp_leagues)
		
		per_match_SCA %>%
			filter(Player != input$player_sel,
						 str_detect(First_Pos, str_c(input$comp_positions, collapse = "|")),
						 League %in% case_when(
						 	input$comp_leagues == "top_5_europa" ~ top_5_europe_leagues,
						 	TRUE ~ input$comp_leagues
						 )) %>%
			group_by(Player, Team, League) %>%
			summarise(
				Min = sum(Min, na.rm = TRUE) / 90,
				First_Pos = get_mode(First_Pos),
				
				SCA = round(sum(SCA, na.rm = T) / Min, 2),
				`xG (SCA)` = round(sum(`xG (SCA)`, na.rm = T) / Min, 2),
				
				`Pases en Juego` = round(sum(`Pases en Juego`, na.rm = T) / Min, 2),
				`xG (Pases en Juego)` = round(sum(`xG (Pases en Juego)`, na.rm = T) / Min, 2),
				
				`Pases a Pelota Parada` = round(sum(`Pases a Pelota Parada`, na.rm = T) / Min, 2),
				`xG (Pases a Pelota Parada)` = round(sum(`xG (Pases a Pelota Parada)`, na.rm = T) / Min, 2),
				
				`Regates` = round(sum(`Regates`, na.rm = T) / Min, 2),
				`xG (Regates)` = round(sum(`xG (Regates)`, na.rm = T) / Min, 2),
				
				`Tiros` = round(sum(`Tiros`, na.rm = T) / Min, 2),
				`xG (Tiros)` = round(sum(`xG (Tiros)`, na.rm = T) / Min, 2),
				
				`Faltas Recibidas` = round(sum(`Faltas Recibidas`, na.rm = T) / Min, 2),
				`xG (Faltas Recibidas)` = round(sum(`xG (Faltas Recibidas)`, na.rm = T) / Min, 2),
				
				`Acciones Defensivas` = round(sum(`Acciones Defensivas`, na.rm = T) / Min, 2),
				`xG (Acciones Defensivas)` = round(sum(`xG (Acciones Defensivas)`, na.rm = T) / Min, 2)
			) %>%
			filter(Min >= input$min_90s)
	})
	
	
	# Filter GCA data based on inputs
	filtered_GCA_player_data <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$team_sel, input$date_range)
		
		per_match_GCA %>%
			filter(Player == input$player_sel,
						 Team %in% input$team_sel,
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2]) %>%
			group_by(Player, Team, League) %>%
			summarise(
				Min = sum(Min, na.rm = TRUE) / 90,
				First_Pos = get_mode(First_Pos),
				
				GCA = round(sum(GCA, na.rm = T) / Min, 2),
				`xG (GCA)` = round(sum(`xG (GCA)`, na.rm = T) / Min, 2),
				
				`Pases en Juego` = round(sum(`Pases en Juego`, na.rm = T) / Min, 2),
				`xG (Pases en Juego)` = round(sum(`xG (Pases en Juego)`, na.rm = T) / Min, 2),
				
				`Pases a Pelota Parada` = round(sum(`Pases a Pelota Parada`, na.rm = T) / Min, 2),
				`xG (Pases a Pelota Parada)` = round(sum(`xG (Pases a Pelota Parada)`, na.rm = T) / Min, 2),
				
				`Regates` = round(sum(`Regates`, na.rm = T) / Min, 2),
				`xG (Regates)` = round(sum(`xG (Regates)`, na.rm = T) / Min, 2),
				
				`Tiros` = round(sum(`Tiros`, na.rm = T) / Min, 2),
				`xG (Tiros)` = round(sum(`xG (Tiros)`, na.rm = T) / Min, 2),
				
				`Faltas Recibidas` = round(sum(`Faltas Recibidas`, na.rm = T) / Min, 2),
				`xG (Faltas Recibidas)` = round(sum(`xG (Faltas Recibidas)`, na.rm = T) / Min, 2),
				
				`Acciones Defensivas` = round(sum(`Acciones Defensivas`, na.rm = T) / Min, 2),
				`xG (Acciones Defensivas)` = round(sum(`xG (Acciones Defensivas)`, na.rm = T) / Min, 2)
			)
	})
	
	filtered_GCA_comp_pool_data <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$comp_positions, input$min_90s, input$comp_leagues, input$date_range)
		
		per_match_GCA %>%
			filter(Player != input$player_sel,
						 str_detect(First_Pos, str_c(input$comp_positions, collapse = "|")),
						 League %in% case_when(
						 	input$comp_leagues == "top_5_europa" ~ top_5_europe_leagues,
						 	TRUE ~ input$comp_leagues
						 ),
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2]) %>%
			group_by(Player, Team, League) %>%
			summarise(
				Min = sum(Min, na.rm = TRUE) / 90,
				First_Pos = get_mode(First_Pos),
				
				GCA = round(sum(GCA, na.rm = T) / Min, 2),
				`xG (GCA)` = round(sum(`xG (GCA)`, na.rm = T) / Min, 2),
				
				`Pases en Juego` = round(sum(`Pases en Juego`, na.rm = T) / Min, 2),
				`xG (Pases en Juego)` = round(sum(`xG (Pases en Juego)`, na.rm = T) / Min, 2),
				
				`Pases a Pelota Parada` = round(sum(`Pases a Pelota Parada`, na.rm = T) / Min, 2),
				`xG (Pases a Pelota Parada)` = round(sum(`xG (Pases a Pelota Parada)`, na.rm = T) / Min, 2),
				
				`Regates` = round(sum(`Regates`, na.rm = T) / Min, 2),
				`xG (Regates)` = round(sum(`xG (Regates)`, na.rm = T) / Min, 2),
				
				`Tiros` = round(sum(`Tiros`, na.rm = T) / Min, 2),
				`xG (Tiros)` = round(sum(`xG (Tiros)`, na.rm = T) / Min, 2),
				
				`Faltas Recibidas` = round(sum(`Faltas Recibidas`, na.rm = T) / Min, 2),
				`xG (Faltas Recibidas)` = round(sum(`xG (Faltas Recibidas)`, na.rm = T) / Min, 2),
				
				`Acciones Defensivas` = round(sum(`Acciones Defensivas`, na.rm = T) / Min, 2),
				`xG (Acciones Defensivas)` = round(sum(`xG (Acciones Defensivas)`, na.rm = T) / Min, 2)
			) %>%
			filter(Min >= input$min_90s)
	})
	
	
	# Filter shooting distances data based on inputs
	filtered_player_sh_distances <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$team_sel, input$date_range)
		
		shooting_distances %>%
			filter(Player == input$player_sel,
						 Team %in% input$team_sel,
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2])
	})
	
	filtered_comp_pool_sh_distances <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$comp_positions, input$comp_leagues, input$date_range)
		
		shooting_distances %>%
			filter(Player != input$player_sel,
						 str_detect(First_Pos, str_c(input$comp_positions, collapse = "|")),
						 League %in% case_when(
						 	input$comp_leagues == "top_5_europa" ~ top_5_europe_leagues,
						 	TRUE ~ input$comp_leagues
						 ),
						 Match_Date >= input$date_range[1] & Match_Date <= input$date_range[2])
	})
	
	
	# Filter understat shots
	filtered_understat_shots <- eventReactive(input$submit_btn, {
		req(input$player_sel, input$team_sel, input$date_range)
		
		understat_shots %>%
			filter(player == input$player_sel,
						 team %in% input$team_sel,
						 date >= input$date_range[1] & date <= input$date_range[2],
						 situation != "Penalty")
	})
	
	
	
	# Calculate percentiles for all tables
	passing_percs <- eventReactive(input$submit_btn, {
		req(filtered_player_data(), filtered_comp_pool_data())
		tibble(Stat = passing_columns) %>%
			rowwise() %>%
			mutate(Value = filtered_player_data()[[Stat]],
						 Percentile = round(mean(mean(filtered_comp_pool_data()[[Stat]] < filtered_player_data()[[Stat]], na.rm = TRUE),
						 												mean(filtered_comp_pool_data()[[Stat]] <= filtered_player_data()[[Stat]], na.rm = TRUE)) * 100)) %>%
			mutate(Percentile = case_when(Stat %in% negative_columns ~ 100 - Percentile,
																		TRUE ~ Percentile),
						 perc_value = Percentile,
						 Stat = factor(Stat, levels = passing_columns))
	})
	
	possession_percs <- eventReactive(input$submit_btn, {
		req(filtered_player_data(), filtered_comp_pool_data())
		tibble(Stat = possession_columns) %>%
			rowwise() %>%
			mutate(Value = filtered_player_data()[[Stat]],
						 Percentile = round(mean(mean(filtered_comp_pool_data()[[Stat]] < filtered_player_data()[[Stat]], na.rm = TRUE),
						 												mean(filtered_comp_pool_data()[[Stat]] <= filtered_player_data()[[Stat]], na.rm = TRUE)) * 100)) %>%
			mutate(Percentile = case_when(Stat %in% negative_columns ~ 100 - Percentile,
																		TRUE ~ Percentile),
						 perc_value = Percentile,
						 Stat = factor(Stat, levels = possession_columns))
	})
	
	defense_percs <- eventReactive(input$submit_btn, {
		req(filtered_player_data(), filtered_comp_pool_data())
		tibble(Stat = defense_columns) %>%
			rowwise() %>%
			mutate(Value = filtered_player_data()[[Stat]],
						 Percentile = round(mean(mean(filtered_comp_pool_data()[[Stat]] < filtered_player_data()[[Stat]], na.rm = TRUE),
						 												mean(filtered_comp_pool_data()[[Stat]] <= filtered_player_data()[[Stat]], na.rm = TRUE)) * 100)) %>%
			mutate(Percentile = case_when(Stat %in% negative_columns ~ 100 - Percentile,
																		TRUE ~ Percentile),
						 perc_value = Percentile,
						 Stat = factor(Stat, levels = defense_columns))
	})
	
	shooting_percs <- eventReactive(input$submit_btn, {
		req(filtered_shooting_player_data(), filtered_shooting_comp_pool_data())
		tibble(Stat = shooting_columns) %>%
			rowwise() %>%
			mutate(Value = filtered_shooting_player_data()[[Stat]],
						 Percentile = round(mean(mean(filtered_shooting_comp_pool_data()[[Stat]] < filtered_shooting_player_data()[[Stat]], na.rm = TRUE),
						 												mean(filtered_shooting_comp_pool_data()[[Stat]] <= filtered_shooting_player_data()[[Stat]], na.rm = TRUE)) * 100)) %>%
			mutate(Percentile = case_when(Stat %in% negative_columns ~ 100 - Percentile,
																		TRUE ~ Percentile),
						 perc_value = Percentile,
						 Stat = factor(Stat, levels = shooting_columns))
	})
	
	sca_percs <- eventReactive(input$submit_btn, {
		req(filtered_SCA_player_data(), filtered_SCA_comp_pool_data())
		tibble(Stat = sca_columns) %>%
			rowwise() %>%
			mutate(Value = filtered_SCA_player_data()[[Stat]],
						 Percentile = round(mean(mean(filtered_SCA_comp_pool_data()[[Stat]] < filtered_SCA_player_data()[[Stat]], na.rm = TRUE),
						 												mean(filtered_SCA_comp_pool_data()[[Stat]] <= filtered_SCA_player_data()[[Stat]], na.rm = TRUE)) * 100)) %>%
			mutate(Percentile = case_when(Stat %in% negative_columns ~ 100 - Percentile,
																		TRUE ~ Percentile),
						 perc_value = Percentile,
						 Stat = factor(Stat, levels = sca_columns))
	})
	
	gca_percs <- eventReactive(input$submit_btn, {
		req(filtered_GCA_player_data(), filtered_GCA_comp_pool_data())
		tibble(Stat = gca_columns) %>%
			rowwise() %>%
			mutate(Value = filtered_GCA_player_data()[[Stat]],
						 Percentile = round(mean(mean(filtered_GCA_comp_pool_data()[[Stat]] < filtered_GCA_player_data()[[Stat]], na.rm = TRUE),
						 												mean(filtered_GCA_comp_pool_data()[[Stat]] <= filtered_GCA_player_data()[[Stat]], na.rm = TRUE)) * 100)) %>%
			mutate(Percentile = case_when(Stat %in% negative_columns ~ 100 - Percentile,
																		TRUE ~ Percentile),
						 perc_value = Percentile,
						 Stat = factor(Stat, levels = gca_columns))
	})
	
	
	# Function to apply JavaScript to the rendered table with delay
	apply_js_to_gt <- function(output_id) {
		shinyjs::delay(500, runjs(sprintf('
      var tds = $("#%s td[headers=\'Percentile\'] > div > div");
      if (tds.length > 0) {
        tds.each(function() {
          var p = $(this).width() / $(this).parent().width();
          var b = "#FFFFFF";
          if (p <= 0.1) { b = "#65010c"; }
          else if (p <= 0.2) { b = "#cb1b16"; }
          else if (p <= 0.3) { b = "#ef3c2d"; }
          else if (p <= 0.4) { b = "#f26a4f"; }
          else if (p <= 0.5) { b = "#f29479"; }
          else if (p <= 0.6) { b = "#fedfd4"; }
          else if (p <= 0.7) { b = "#9dcee2"; }
          else if (p <= 0.8) { b = "#4091c9"; }
          else if (p <= 0.9) { b = "#1368aa"; }
          else if (p <= 1.0) { b = "#033270"; }
          $(this).css("background", b);
        });
      }
    ', output_id)))
	}
	
	# Generate and render GT tables, followed by JavaScript application with delay
	output$gtout <- render_gt({
		req(passing_percs())
		apply_js_to_gt("gtout")  # Apply JS after rendering with delay
		passing_percs() %>%
			gt() %>%
			gt_plt_bar_pct(Percentile, labels = FALSE, fill = "black", scaled = T) %>%
			cols_width(Percentile ~ px(200)) %>%
			opt_row_striping() %>%
			cols_align(align = "left", columns = Stat) %>%
			cols_align(align = "center", columns = Percentile) %>%
			opt_stylize(style = 1, color = "gray") %>%
			cols_label(perc_value = "") %>%
			tab_style(
				style = cell_borders(sides = "bottom", color = "darkgrey", weight = px(2)),
				locations = cells_body(rows = c(5, 8, 11, 14))
			) %>%
			tab_style(style = cell_text(color = "black"),
								locations = cells_body(columns = perc_value))
	})
	
	output$gtout_possession <- render_gt({
		req(possession_percs())
		apply_js_to_gt("gtout_possession")  # Apply JS after rendering with delay
		possession_percs() %>%
			gt() %>%
			gt_plt_bar_pct(Percentile, labels = FALSE, fill = "black", scaled = T) %>%
			cols_width(Percentile ~ px(200)) %>%
			opt_row_striping() %>%
			cols_align(align = "left", columns = Stat) %>%
			cols_align(align = "center", columns = Percentile) %>%
			opt_stylize(style = 1, color = "gray") %>%
			cols_label(perc_value = "") %>%
			tab_style(
				style = cell_borders(sides = "bottom", color = "darkgrey", weight = px(2)),
				locations = cells_body(rows = c(6, 11, 19))
			) %>%
			tab_style(style = cell_text(color = "black"),
								locations = cells_body(columns = perc_value))
	})
	
	output$gtout_defense <- render_gt({
		req(defense_percs())
		apply_js_to_gt("gtout_defense")  # Apply JS after rendering with delay
		defense_percs() %>%
			gt() %>%
			gt_plt_bar_pct(Percentile, labels = FALSE, fill = "black", scaled = T) %>%
			cols_width(Percentile ~ px(200)) %>%
			opt_row_striping() %>%
			cols_align(align = "left", columns = Stat) %>%
			cols_align(align = "center", columns = Percentile) %>%
			opt_stylize(style = 1, color = "gray") %>%
			cols_label(perc_value = "") %>%
			tab_style(
				style = cell_borders(sides = "bottom", color = "darkgrey", weight = px(2)),
				locations = cells_body(rows = c(5, 9, 12))
			) %>%
			tab_style(style = cell_text(color = "black"),
								locations = cells_body(columns = perc_value))
	})
	
	output$gtout_shooting <- render_gt({
		req(shooting_percs())
		apply_js_to_gt("gtout_shooting")  # Apply JS after rendering with delay
		shooting_percs() %>%
			gt() %>%
			gt_plt_bar_pct(Percentile, labels = FALSE, fill = "black", scaled = T) %>%
			cols_width(Percentile ~ px(200)) %>%
			opt_row_striping() %>%
			cols_align(align = "left", columns = Stat) %>%
			cols_align(align = "center", columns = Percentile) %>%
			opt_stylize(style = 1, color = "gray") %>%
			cols_label(perc_value = "") %>%
			tab_style(
				style = cell_borders(sides = "bottom", color = "darkgrey", weight = px(2)),
				locations = cells_body(rows = c(8))
			) %>%
			tab_style(style = cell_text(color = "black"),
								locations = cells_body(columns = perc_value))
	})
	
	output$gtout_sca <- render_gt({
		req(sca_percs())
		apply_js_to_gt("gtout_sca")  # Apply JS after rendering with delay
		sca_percs() %>%
			gt() %>%
			gt_plt_bar_pct(Percentile, labels = FALSE, fill = "black", scaled = T) %>%
			cols_width(Percentile ~ px(200)) %>%
			opt_row_striping() %>%
			cols_align(align = "left", columns = Stat) %>%
			cols_align(align = "center", columns = Percentile) %>%
			opt_stylize(style = 1, color = "gray") %>%
			cols_label(perc_value = "") %>%
			tab_style(
				style = cell_borders(sides = "bottom", color = "darkgrey", weight = px(2)),
				locations = cells_body(rows = c(2, 4, 6, 8, 10, 12, 14))
			) %>%
			tab_style(style = cell_text(color = "black"),
								locations = cells_body(columns = perc_value))
	})
	
	output$gtout_gca <- render_gt({
		req(gca_percs())
		apply_js_to_gt("gtout_gca")  # Apply JS after rendering with delay
		gca_percs() %>%
			gt() %>%
			gt_plt_bar_pct(Percentile, labels = FALSE, fill = "black", scaled = T) %>%
			cols_width(Percentile ~ px(200)) %>%
			opt_row_striping() %>%
			cols_align(align = "left", columns = Stat) %>%
			cols_align(align = "center", columns = Percentile) %>%
			opt_stylize(style = 1, color = "gray") %>%
			cols_label(perc_value = "") %>%
			tab_style(
				style = cell_borders(sides = "bottom", color = "darkgrey", weight = px(2)),
				locations = cells_body(rows = c(2, 4, 6, 8, 10, 12, 14))
			) %>%
			tab_style(style = cell_text(color = "black"),
								locations = cells_body(columns = perc_value))
	})
	
	
	
	
	
	# Scatter plot rendering remains unchanged...
	output$scatter_plot <- renderPlot({
		req(filtered_player_data(), filtered_comp_pool_data(), input$x_axis, input$y_axis, input$selected_compared_players)
		
		# If remove_outliers is TRUE, filter out the outliers
		comp_pool_data <- filtered_comp_pool_data()
		if (input$remove_outliers) {
			comp_pool_data <- comp_pool_data %>%
				filter(
					between(.data[[input$x_axis]], mean(.data[[input$x_axis]], na.rm = TRUE) - 3 * sd(.data[[input$x_axis]], na.rm = TRUE),
									mean(.data[[input$x_axis]], na.rm = TRUE) + 3 * sd(.data[[input$x_axis]], na.rm = TRUE)),
					between(.data[[input$y_axis]], mean(.data[[input$y_axis]], na.rm = TRUE) - 3 * sd(.data[[input$y_axis]], na.rm = TRUE),
									mean(.data[[input$y_axis]], na.rm = TRUE) + 3 * sd(.data[[input$y_axis]], na.rm = TRUE))
				)
		}
		
		comp_pool_data <- comp_pool_data %>% ungroup()
		player_data <- filtered_player_data() %>% ungroup()
		
		ggplot() +
			# Add horizontal and vertical dashed lines
			geom_hline(aes(yintercept = mean(comp_pool_data[[input$y_axis]], na.rm = TRUE)), linetype = "dashed", alpha = 0.5) +
			geom_vline(aes(xintercept = mean(comp_pool_data[[input$x_axis]], na.rm = TRUE)), linetype = "dashed", alpha = 0.5) +
			
			# Add points for comp_pool_data data
			geom_point(data = comp_pool_data %>%
								 	select(all_of(c(input$x_axis, input$y_axis))) %>%
								 	filter(.data[[input$y_axis]] > 0 & !is.na(.data[[input$y_axis]]) & .data[[input$x_axis]] > 0 & !is.na(.data[[input$x_axis]])),
								 aes(x = .data[[input$x_axis]], y = .data[[input$y_axis]]), alpha = 0.20, size = 3) +
			
			# Add points for player_data data (larger red points)
			geom_point(data = player_data %>%
								 	select(all_of(c(input$x_axis, input$y_axis))),
								 aes(x = .data[[input$x_axis]], y = .data[[input$y_axis]]), color = "red", size = 7, alpha = 0.25) +
			
			# Add points for player_data data (smaller red points)
			geom_point(data = player_data %>%
								 	select(all_of(c(input$x_axis, input$y_axis))),
								 aes(x = .data[[input$x_axis]], y = .data[[input$y_axis]]), color = "red", size = 3) +
			
			ggrepel::geom_text_repel(data = comp_pool_data %>%
															 	filter(.data[[input$y_axis]] > 0 & !is.na(.data[[input$y_axis]]) & .data[[input$x_axis]] > 0 & !is.na(.data[[input$x_axis]])) %>%
															 	arrange(desc(.data[[input$x_axis]])) %>%
															 	slice_head(n = 5) %>%
															 	bind_rows(comp_pool_data %>% filter(Player %in% input$selected_compared_players)) %>%
															 	distinct(Player, .keep_all = TRUE) %>%
															 	select(Player, all_of(c(input$x_axis, input$y_axis))),
															 aes(x = .data[[input$x_axis]], y = .data[[input$y_axis]], label = Player),
															 min.segment.length = 0, 
															 seed = 42, 
															 box.padding = 0.5,
															 max.overlaps = Inf,
															 nudge_x = .01,
															 nudge_y = .01,
															 color = "black",
															 size = rel(6.2)) +
			theme_minimal() +
			theme(
				plot.title = element_text(hjust = 0.5, size = rel(2)),
				axis.text = element_text(size = rel(1.4)),
				axis.title = element_text(size = rel(1.5))
			) +
			labs(title = paste(input$x_axis, "vs.", input$y_axis, "(p90)")) +
			xlab(input$x_axis) + ylab(input$y_axis)
	})
	
	
	
	
	# Passing completion percentage plots
	output$short_passes <- renderPlot({
		req(filtered_player_data())
		
		pass_completion <- data.frame(
			category = c("Success", "Missed"),
			count = c(filtered_player_data()$`% Pases Cortos Completados`, 100 - filtered_player_data()$`% Pases Cortos Completados`)
		) %>%
				mutate(fraction = count / 100,
							 ymax = cumsum(fraction),
							 ymin = c(0, head(ymax, n = 1)),
							 color = case_when(
							 	count >= 90 ~ "#20CE60",
							 	count < 90 & count >= 80 ~ "#66FF66",
							 	count < 80 & count >= 70 ~ "#B3FFB3",
							 	count < 70 & count >= 60 ~ "#D9F29B",
							 	count < 60 & count >= 50 ~ "#FFD966",
							 	count < 50 & count >= 40 ~ "#FFB366",
							 	count < 40 & count >= 30 ~ "#FF9966",
							 	count < 30 & count >= 20 ~ "#FF5956",
							 	count < 20 ~ "#DF3E3B"
							 ),
							 color = c(color[1], "#FFFFFF"))
		
		
		ggplot(pass_completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
				geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
				coord_polar(theta="y") +
				scale_fill_identity() +
				xlim(c(-1, 4)) +
				theme_void() +
				theme(legend.position = "none") +
				annotate(geom = 'text', x = -0.98, y = 0,
								 label = paste0(pass_completion$count[1], "%", sep = ""), size = rel(12))
	})
	
	output$medium_passes <- renderPlot({
		req(filtered_player_data())
		
		pass_completion <- data.frame(
			category = c("Success", "Missed"),
			count = c(filtered_player_data()$`% Pases Medios Completados`, 100 - filtered_player_data()$`% Pases Medios Completados`)
		) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))
		
		
		ggplot(pass_completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(pass_completion$count[1], "%", sep = ""), size = rel(12))
	})
	
	output$long_passes <- renderPlot({
		req(filtered_player_data())
		
		pass_completion <- data.frame(
			category = c("Success", "Missed"),
			count = c(filtered_player_data()$`% Pases Largos Completados`, 100 - filtered_player_data()$`% Pases Largos Completados`)
		) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))
		
		
		ggplot(pass_completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(pass_completion$count[1], "%", sep = ""), size = rel(12))
	})
	
	
	
	
	# Aerials-Tackles-1v1 percentage plots
	output$aerial_duels <- renderPlot({
		req(filtered_player_data())
		
		completion <- data.frame(
			category = c("Success", "Missed"),
			count = c(filtered_player_data()$`% Duelos Aéreos Ganados`, 100 - filtered_player_data()$`% Duelos Aéreos Ganados`)
		) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))
		
		
		ggplot(completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(completion$count[1], "%", sep = ""), size = rel(12))
	})
	
	output$challenges_tkld <- renderPlot({
		req(filtered_player_data())
		
		completion <- data.frame(
			category = c("Success", "Missed"),
			count = c(filtered_player_data()$`% Regateadores Tacleados`, 100 - filtered_player_data()$`% Regateadores Tacleados`)
		) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))
		
		
		ggplot(completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(completion$count[1], "%", sep = ""), size = rel(12))
	})
	
	output$succ_take_ons <- renderPlot({
		req(filtered_player_data())
		
		completion <- data.frame(
			category = c("Success", "Missed"),
			count = c(filtered_player_data()$`% Regates Completados`, 100 - filtered_player_data()$`% Regates Completados`)
		) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))
		
		
		ggplot(completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(completion$count[1], "%", sep = ""), size = rel(12))
	})
	
	
	
	
	# Tackles zones plot
	output$tackles_zones <- renderPlot({
		req(filtered_player_data(), filtered_comp_pool_data())
		
		as_tibble(data.frame(
			Zone = c("1/3 Def", "1/3 Med", "1/3 Att"),
			Tackles = c(filtered_player_data()$`Entradas 1/3 Def`, filtered_player_data()$`Entradas 1/3 Med`, filtered_player_data()$`Entradas 1/3 Att`),
			Average = c(mean(filtered_comp_pool_data()$`Entradas 1/3 Def`, na.rm = T),
									mean(filtered_comp_pool_data()$`Entradas 1/3 Med`, na.rm = T),
									mean(filtered_comp_pool_data()$`Entradas 1/3 Att`, na.rm = T)),
			x1 = c(0.5, 1.5, 2.5),
			x2 = c(1.5, 2.5, 3.5)
		)) %>%
			ggplot(aes(x = Zone, y = Tackles)) +
			scale_x_discrete(limits = c("1/3 Def", "1/3 Med", "1/3 Att")) +
			scale_fill_manual(values = c("1/3 Def" = "#1b9e77",
																	 "1/3 Med" = "#7570b3",
																	 "1/3 Att" = "#e7298a")) + 
			geom_col(aes(fill = Zone), width = 0.3) +
			geom_segment(aes(x = x1, y = Average, xend = x2, yend = Average), linetype = "dashed", color = "black") +
			theme_minimal() +
			theme(
				legend.position = "none",
				axis.text = element_text(size = rel(1.4)),
				axis.title.y = element_text(size = rel(1.5)),
				axis.title.x = element_blank(),
				plot.title = element_blank()
			) +
			ylab(label = "Entradas")
	})
	
	
	
	# Waffle touches zones plot
	output$touches_zones <- renderPlot({
		req(filtered_player_data())
		
		touches_data <- {as.data.frame(t(c(filtered_player_data()$`Toques 1/3 Def`, filtered_player_data()$`Toques Def. Pen`,
																			 filtered_player_data()$`Toques 1/3 Med`, filtered_player_data()$`Toques 1/3 Att`, filtered_player_data()$`Toques Att. Pen`)))}
		colnames(touches_data) <- c("Def 3rd", "Def Pen", "Mid 3rd", "Att 3rd", "Att Pen")
		touches_vector <- c(filtered_player_data()$`Toques Def. Pen`,
												filtered_player_data()$`Toques 1/3 Def`,
												filtered_player_data()$`Toques 1/3 Med`,
												filtered_player_data()$`Toques 1/3 Att`,
												filtered_player_data()$`Toques Att. Pen`)
		names(touches_vector) <- c("Def Pen", "1/3 Def", "1/3 Med", "1/3 Att", "Att Pen")
		waffle(touches_vector,
					 rows = 5,
					 colors = c("#d95f02", "#1b9e77", "#7570b3", "#e7298a", "#66a61e"),
					 legend_pos = "bottom",
					 keep = T) + theme(legend.text = element_text(size = rel(1.4)))
	})
	
	
	# Shooting distances vertical scatterplot
	output$sh_distances <- renderPlot({
		req(filtered_player_sh_distances(), filtered_comp_pool_sh_distances())
		
		ggplot() +
			geom_point(data = filtered_player_sh_distances(), aes(x = Outcome, y = Distance, color = Outcome, size = xG), alpha = 0.2) +
			scale_y_reverse(breaks = seq(0, max(filtered_player_sh_distances()$Distance), by = 5)) +
			scale_x_discrete(limits = c("Gol", "Atajado", "Atajado Fuera", "Fuera", "Bloqueado", "Palo")) +
			scale_color_manual(values = c("Gol" = "#005826", 
																		"Atajado" = "#003B73", 
																		"Atajado Fuera" = "#538DC2", 
																		"Fuera" = "#DB162F", 
																		"Bloqueado" = "#8C00FF", 
																		"Palo" = "#C45B00")) +
			scale_size_continuous(breaks = c(0.05, 0.15, 0.25, 0.55),
														labels = c(1, 2, 3.5, 5),
														range = c(2.5, 7)) +
			geom_segment(data = {filtered_comp_pool_sh_distances() %>%
					mutate(outcome_level = case_when(Outcome == "Gol" ~ 1, 
																					 Outcome == "Atajado" ~ 2, 
																					 Outcome == "Atajado Fuera" ~ 3, 
																					 Outcome == "Fuera" ~ 4, 
																					 Outcome == "Bloqueado" ~ 5, 
																					 Outcome == "Palo" ~ 6)) %>%
					group_by(Outcome) %>%
					summarise(
						outcome_level = mean(outcome_level),
						y = mean(Distance),
						x1 = outcome_level - 0.5,
						x2 = outcome_level + 0.5
					)},
					aes(x = x1, y = y, xend = x2, yend = y, color = Outcome), linetype = "dashed") +
			theme_minimal() +
			theme(
				legend.position = "none",
				axis.text.x = element_text(size = rel(1.8), margin = margin(t = 2), angle = 90, vjust = 0.5, hjust = 1),
				axis.text.y = element_text(size = rel(1.6)),
				axis.title.y = element_text(size = rel(1.8)),
				axis.title.x = element_blank()
			) +
			ylab(label = "Distancia")
	})
	
	
	# Shooting locations and distribution
	output$sh_distribution <- renderPlot({
		req(filtered_understat_shots())
		
		filtered_understat_shots() %>%
			ggplot(aes(x = X, y = Y)) + 
			annotate_pitch(colour = "darkgrey", fill = "white") +
			geom_hex(aes(alpha = after_stat(count)), fill = "#012a4a", bins = 20, color = "white", linewidth = 0.2) +
			scale_alpha_continuous(range = c(0.2, 0.8)) +
			guides(alpha = guide_none()) +
			coord_flip(xlim = c(55, 105), ylim = c(105, 0)) +
			theme_pitch() +
			theme(panel.background = element_rect(fill = "white"))
	})
	
	output$sh_locations <- renderPlot({
		req(filtered_understat_shots())
		
		filtered_understat_shots() %>%
			ggplot(aes(x = X, y = Y)) +
			annotate_pitch(colour = "darkgrey", fill = "white") +
			geom_point(alpha = 0.5, aes(size = xG, color = result, shape = result, fill = result), stroke = 2) +
			
			scale_color_manual(
				name = NULL,
				values = c(
					"Gol" = "#005826",
					"Aajado" = "#003B73",
					"Fuera/Bloqueado" = "#DB162F",
					"Palo" = "black",
					"En Contra" = "darkgrey"
				)) +
			scale_size_continuous(breaks = c(0.05, 0.15, 0.25, 0.55),
														range = c(2.5, 8)) +
			scale_shape_manual(
				values = c(
					"Gol" = 21,
					"Aajado" = 21,
					"Fuera/Bloqueado" = 1,
					"Palo" = 1,
					"En Contra" = 1
				)) +
			scale_fill_manual(
				name = NULL,
				values = c(
					"Gol" = "#005826",
					"Aajado" = "#003B73",
					"Fuera/Bloqueado" = "#DB162F",
					"Palo" = "black",
					"En Contra" = "darkgrey"
				)) +
			
			guides(size = guide_none(), shape = guide_none(), fill = guide_none(), color = guide_legend(override.aes = list(size = 5))) +
			
			coord_flip(xlim = c(55, 105), ylim = c(105, 0)) +  # Flip the plot
			theme_pitch() +
			theme(panel.background = element_rect(fill = "white"),
						legend.position = "bottom",            # Move legend to bottom
						legend.justification = "center",
						legend.text = element_text(size = rel(1.6)))       # Center the legend
	})
	
}

shinyApp(ui, server)
