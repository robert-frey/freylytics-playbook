statcast_search_milb <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
           playerid = NULL, player_type = "batter", ...) {
    # Check for other user errors.
    if (start_date <= "2021-03-28") { 
      message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2021 at the Minor League Level.")
    }
    if (start_date < "2021-03-25") { 
      stop("Minor League Statcast tracking is available since the 2021 season for certain levels and ballparks. 
           Data is available for: 
           All Triple-A games starting with the 2023 season, as well as Pacific Coast League games and Charlotte home games for the 2022 season
           Florida State League (Single-A) games starting with the 2021 season")
      return(NULL)
    }
    if (start_date == Sys.Date()) {
      message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
    }
    if (start_date > as.Date(end_date)) {
      stop("The start date is later than the end date.")
      return(NULL)
    }
    
    playerid_var <- ifelse(player_type == "pitcher",
                           "pitchers_lookup%5B%5D", "batters_lookup%5B%5D")
    
    
    url <- paste0("https://baseballsavant.mlb.com/statcast-search-minors/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C=&hfSea=&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&minors=true&")

    tryCatch(
      { 
        suppressMessages(
          suppressWarnings(
            payload <- baseballr:::csv_from_url(url, encoding ="UTF-8")
          )
        )
      },
      error = function(cond) {
        message(cond)
        stop("No payload acquired")
      },
      # this will never run??
      warning = function(cond) {
        message(cond)
      }
    )
    # returns 0 rows on failure but > 1 columns
    if (nrow(payload) > 1) {

      names(payload) <- c("pitch_type", "game_date", "release_speed", "release_pos_x",
                          "release_pos_z", "player_name", "batter", "pitcher", "events",
                          "description", "spin_dir", "spin_rate_deprecated", "break_angle_deprecated",
                          "break_length_deprecated", "zone", "des", "game_type", "stand",
                          "p_throws", "home_team", "away_team", "type", "hit_location",
                          "bb_type", "balls", "strikes", "game_year", "pfx_x", "pfx_z",
                          "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up",
                          "inning", "inning_topbot", "hc_x", "hc_y", "tfs_deprecated",
                          "tfs_zulu_deprecated", #"fielder_2"
                          "umpire", "sv_id", "vx0",
                          "vy0", "vz0", "ax", "ay", "az", "sz_top", "sz_bot", "hit_distance_sc",
                          "launch_speed", "launch_angle", "effective_speed", "release_spin_rate",
                          "release_extension", "game_pk", #"pitcher_1", 
                          "fielder_2","fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                          "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle",
                          "estimated_woba_using_speedangle", "woba_value", "woba_denom",
                          "babip_value", "iso_value", "launch_speed_angle", "at_bat_number",
                          "pitch_number", "pitch_name", "home_score", "away_score", "bat_score",
                          "fld_score", "post_away_score", "post_home_score", "post_bat_score",
                          "post_fld_score", "if_fielding_alignment", "of_fielding_alignment",
                          "spin_axis", "delta_home_win_exp", "delta_run_exp", "bat_speed", "swing_length",
                          "estimated_slg_using_speedangle","delta_pitcher_run_exp","hyper_speed","home_score_diff","bat_score_diff","home_win_exp",
                          "bat_win_exp","age_pit_legacy","age_bat_legacy","age_pit","age_bat","n_thruorder_pitcher",
                          "n_priorpa_thisgame_player_at_bat","pitcher_days_since_prev_game",
                          "batter_days_since_prev_game","pitcher_days_until_next_game",
                          "batter_days_until_next_game","api_break_z_with_gravity","api_break_x_arm","api_break_x_batter_in",
                          "arm_angle","attack_angle","attack_direction","swing_path_tilt",
                          "intercept_ball_minus_batter_pos_x_inches","intercept_ball_minus_batter_pos_y_inches")
       #payload <- process_statcast_payload(payload) %>%
       return(payload)
     } else {
       warning("No valid data found")
     
      names(payload) <- c("pitch_type", "game_date", "release_speed", "release_pos_x",
                          "release_pos_z", "player_name", "batter", "pitcher", "events",
                          "description", "spin_dir", "spin_rate_deprecated", "break_angle_deprecated",
                          "break_length_deprecated", "zone", "des", "game_type", "stand",
                          "p_throws", "home_team", "away_team", "type", "hit_location",
                          "bb_type", "balls", "strikes", "game_year", "pfx_x", "pfx_z",
                          "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up",
                          "inning", "inning_topbot", "hc_x", "hc_y", "tfs_deprecated",
                          "tfs_zulu_deprecated", #"fielder_2"
                          "umpire", "sv_id", "vx0",
                          "vy0", "vz0", "ax", "ay", "az", "sz_top", "sz_bot", "hit_distance_sc",
                          "launch_speed", "launch_angle", "effective_speed", "release_spin_rate",
                          "release_extension", "game_pk", #"pitcher_1", 
                          "fielder_2","fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                          "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle",
                          "estimated_woba_using_speedangle", "woba_value", "woba_denom",
                          "babip_value", "iso_value", "launch_speed_angle", "at_bat_number",
                          "pitch_number", "pitch_name", "home_score", "away_score", "bat_score",
                          "fld_score", "post_away_score", "post_home_score", "post_bat_score",
                          "post_fld_score", "if_fielding_alignment", "of_fielding_alignment",
                          "spin_axis", "delta_home_win_exp", "delta_run_exp", "bat_speed", "swing_length",
                          "estimated_slg_using_speedangle","delta_pitcher_run_exp","hyper_speed","home_score_diff","bat_score_diff","home_win_exp",
                          "bat_win_exp","age_pit_legacy","age_bat_legacy","age_pit","age_bat","n_thruorder_pitcher",
                          "n_priorpa_thisgame_player_at_bat","pitcher_days_since_prev_game",
                          "batter_days_since_prev_game","pitcher_days_until_next_game",
                          "batter_days_until_next_game","api_break_z_with_gravity","api_break_x_arm","api_break_x_batter_in",
                          "arm_angle","attack_angle","attack_direction","swing_path_tilt",
                          "intercept_ball_minus_batter_pos_x_inches","intercept_ball_minus_batter_pos_y_inches")

       return(payload)
     }
    
  }
