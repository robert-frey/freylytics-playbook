library(httr)
library(jsonlite)
library(tidyverse)
library(nflreadr)
library(xml2)
library(rvest)

players <- nflreadr::load_players() %>%
  dplyr::select(gsis_id, display_name, nfl_id, espn_id, position)

scrape_espn <- function(pos = c("QB", "RB", "WR", "TE", "K"), season = 2025, week = NA) {
  
# Define the base URL for the given season
base_url <- paste0("https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/",season,"/segments/0/leaguedefaults/3")

message("\nScraping ESPN data. Please be patient...")

query_params <- list(
  scoringPeriodId = 0,
  view = "kona_player_info"
)

filter_list <- list(
  players = list(
    limit = 2000,
    sortPercOwned = list(
      sortPriority = 4,
      sortAsc = FALSE
    )
  )
)

filter_json <- jsonlite::toJSON(filter_list, auto_unbox = TRUE)


request_headers <- httr::add_headers(
  `X-Fantasy-Filter` = filter_json
)

response <- httr::GET(url = base_url, query = query_params, config = request_headers)

content <- httr::content(response, "text", encoding = "UTF-8")
data <- jsonlite::fromJSON(content, flatten = TRUE)[['players']]

filter_max <- function(stats_df) {
  filtered <- stats_df %>%
    dplyr::filter(seasonId == season) %>%
    dplyr::select(appliedTotal)
  
  if (nrow(filtered) == 0) {
    return(tibble(seasonId = integer(), appliedTotal = numeric()))
  }
  
  max_total <- max(filtered$appliedTotal)
  
  result <- filtered %>%
    dplyr::filter(appliedTotal == max_total)
  
  return(result)
}

data <- data %>%
  dplyr::mutate(position = dplyr::case_when(player.defaultPositionId == 1 ~ "QB",
                                            player.defaultPositionId == 2 ~ "RB",
                                            player.defaultPositionId == 3 ~ "WR",
                                            player.defaultPositionId == 4 ~ "TE",
                                            player.defaultPositionId == 5 ~ "K",
                                            TRUE ~ "D/ST"),
                site = "ESPN",
                year = 2025) %>%
  dplyr::left_join(.,players %>% dplyr::select(gsis_id,espn_id) %>% dplyr::mutate(espn_id = as.integer(espn_id)),by=c("id"="espn_id")) %>%
  dplyr::select(year,site,espn_id=id,gsis_id, player = player.fullName,pos=position,
                team_id = player.proTeamId,
                espn_season_outlook = player.seasonOutlook,
                espn_adp = player.ownership.averageDraftPosition,
                espn_adp_percent_change = player.ownership.averageDraftPositionPercentChange,
                espn_standard_rank = player.draftRanksByRankType.STANDARD.rank,
                espn_ppr_rank = player.draftRanksByRankType.PPR.rank,
                espn_ppr_auction_value = player.draftRanksByRankType.PPR.auctionValue,
                espn_rank_overall = ratings.0.totalRanking,
                espn_rank_positional = ratings.0.positionalRanking,
                player_ppr_points_2024 = ratings.0.totalRating,
                player.stats, site)

l_pos <- data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(player.stats = list(filter_max(player.stats))) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(player.stats) %>%
  dplyr::rename(espn_projected_fantasy_points_ppr=appliedTotal) %>%
  dplyr::filter(pos %in% pos)


return(l_pos)

}

### OTHER PROJS ####

get_scrape_year <- function(date) {
  if(missing(date)) {
    date = Sys.Date()
  }
  date = as.POSIXlt(date)
  cal_year = date$year + 1900L
  cal_month = date$mon + 1L
  
  if(cal_month %in% 1:3) {
    cal_year - 1L
  } else {
    cal_year
  }
}

get_scrape_week = function(scrape_start_date) {
  if(missing(scrape_start_date)) {
    scrape_start_date = ffanalytics:::scrape_start_date
  }
  sum(Sys.Date() >= scrape_start_date)
}

rename_vec = function(x, new_names, old_names = NULL) {
  if(is.null(old_names)) {
    old_names = names(new_names)
    if(is.null(names(new_names))) {
      
      message = paste0("Must supply old_names argument, or "
                       , deparse(substitute(new_names))
                       , " needs to be a named vector with the "
                       , "old names  as the named portion")
      stop(message)
    }
  }
  
  idx = match(x, old_names)
  x[!is.na(idx)] = new_names[omit_NA(idx)]
  x
}

#### CBS ####
draft_cbs = function(metric = "adp") {
  
  draft_url <- "https://www.cbssports.com/fantasy/football/draft/averages/ppr/both/h2h/all"
  
  draft_page <- rvest::read_html(draft_url)
  
  cbs_id = draft_page %>%
    rvest::html_elements("span.CellPlayerName--long > span > a") %>%
    rvest::html_attr("href") %>%
    dirname() %>%
    dirname() %>%
    basename()
  
  out_df = draft_page %>%
    rvest::html_element("#TableBase > div > div > table") %>%
    rvest::html_table() %>%
    tidyr::extract(
      Player, c("player", "pos", "team"),
      "\\n\\s+(.*?)\\n\\s+([A-Z]{1,3})\\s+([A-Z]{2,3})") %>%
    dplyr::transmute(
      cbs_id = cbs_id,
      cbs_player_adp_change = as.integer(replace(Trend, Trend == "—", 0)),
      cbs_player_adp = as.numeric(`Avg Pos`),
      cbs_player_high_adp = as.integer(sub("/\\d+", "", `Hi/Lo`)),
      cbs_player_low_adp = as.integer(sub("\\d+/", "", `Hi/Lo`)),
      cbs_percent_drafted = Pct
    )
  
  out_df
  
}

scrape_cbs = function(pos = c("QB", "RB", "WR", "TE", "K"), season = 2025, week = NA,
                      draft = TRUE, weekly = TRUE) {
  
  if(is.null(season)) {
    szn = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }
  
  if(week %in% c(0, "ros")) {
    scrape_week = "restofseason"
  } else {
    scrape_week = week
  }
  
  message("\nScraping CBS data. Please be patient...")
  
  base_link = paste0("https://www.cbssports.com/fantasy/football/")
  site_session = rvest::session(base_link)
  
  draft_data <- draft_cbs()
  
  l_pos = lapply(pos, function(pos) {
    scrape_link = paste0("https://www.cbssports.com/fantasy/football/stats/", pos, "/",
                         season, "/", scrape_week, "/projections/ppr/")
    
    Sys.sleep(sample(runif(25,min = 2, max = 3.1415926535897932384626),1))
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")
    
    html_page = site_session %>%
      rvest::session_jump_to(scrape_link) %>%
      xml2::read_html()
    
    col_names = html_page %>%
      rvest::html_element("#TableBase > div > div > table > thead > tr.TableBase-headTr") %>%
      rvest::html_text2() %>%
      strsplit("\\\n|\\\t")
    
    col_names = grep("[A-Z]", col_names[[1]], value = TRUE)

    src_id = html_page %>%
        rvest::html_elements("table > tbody > tr > td:nth-child(1) > span.CellPlayerName--long > span > a") %>%
        rvest::html_attr("href") %>%
        sub(".*?([0-9]+).*", "\\1", .)

    
   out_df = html_page %>%
      rvest::html_element("#TableBase > div > div > table > tbody") %>%
      rvest::html_table() %>%
      `names<-`(col_names)

  out_df <- dplyr::mutate(out_df, year = season, 
                          week = ifelse(is.na(week),NA_real_,week),
                          site = "CBS", Opp = NA_character_)
  
  out_df <- out_df %>%
     tidyr::extract(Player, c("player", "pos", "team"),
     ".*?\\s{2,}[A-Z]{1,3}\\s{2,}[A-Z]{2,3}\\s{2,}(.*?)\\s{2,}(.*?)\\s{2,}(.*)") %>%
     dplyr::mutate(cbs_id = src_id
     ) %>%
    dplyr::left_join(.,players %>% dplyr::select(gsis_id,player=display_name,pos=position),by=c("player","pos"))
  
  if(pos %in% c("RB","WR")) {
  out_df <- out_df %>%    
    dplyr::left_join(.,draft_data,by="cbs_id") %>%
    dplyr::select(year,site,cbs_id, gsis_id,player, pos, team, opp = Opp, GP = `Games Played`,
                  cbs_player_adp, cbs_player_adp_change, cbs_player_high_adp,
                  cbs_player_low_adp, cbs_percent_drafted,
                  cbs_projected_rushing_attempts = `Rushing Attempts`,
                  cbs_projected_rushing_yards = `Rushing Yards`,
                  cbs_projected_rushing_tds = `Rushing Touchdowns`,
                  cbs_projected_targets = Targets,
                  cbs_projected_receptions = Receptions,
                  cbs_projected_receiving_yards = `Receiving Yards`,
                  cbs_projected_receiving_tds = `Receiving Touchdowns`,
                  cbs_projected_fum_lost = `Fumbles Lost`,
                  cbs_projected_fantasy_points_ppr = `Fantasy Points`
    )
  
  } else if(pos == "TE") {
    out_df <- out_df %>%    
      dplyr::left_join(.,draft_data,by="cbs_id") %>% 
      dplyr::mutate(`Rushing Yards` = 0, `Rushing Touchdowns` = 0,`Rushing Attempts`=0) %>%
      dplyr::select(year,site,cbs_id, gsis_id,player, pos, team, opp = Opp, GP = `Games Played`,
                    cbs_player_adp, cbs_player_adp_change, cbs_player_high_adp,
                    cbs_player_low_adp, cbs_percent_drafted,
                    cbs_projected_targets = Targets,
                    cbs_projected_receptions = Receptions,
                    cbs_projected_receiving_yards = `Receiving Yards`,
                    cbs_projected_receiving_tds = `Receiving Touchdowns`,
                    cbs_projected_fum_lost = `Fumbles Lost`,
                    cbs_projected_rushing_attempts = `Rushing Attempts`,
                    cbs_projected_rushing_yards = `Rushing Yards`,
                    cbs_projected_rushing_tds = `Rushing Touchdowns`,
                    cbs_projected_fantasy_points_ppr = `Fantasy Points`
      ) 
  } else if(pos == "QB") {
    out_df <- out_df %>%    
      dplyr::left_join(.,draft_data,by="cbs_id") %>% 
      dplyr::select(year,site,cbs_id,gsis_id,player,pos,team,opp = Opp,GP = `Games Played`,
                    cbs_player_adp, cbs_player_adp_change, cbs_player_high_adp,
                    cbs_player_low_adp, cbs_percent_drafted,
                    cbs_projected_completions = `Pass Completions`,
                    cbs_projected_passing_yards = `Passing Yards`,
                    cbs_projected_passing_tds = `Touchdowns Passes`,
                    cbs_projected_passing_ints = `Interceptions Thrown`,
                    cbs_projected_rushing_attempts = `Rushing Attempts`,
                    cbs_projected_rushing_yards = `Rushing Yards`,
                    cbs_projected_rushing_tds = `Rushing Touchdowns`,
                    cbs_projected_fum_lost = `Fumbles Lost`,
                    cbs_projected_fantasy_points_ppr = `Fantasy Points`)
  }
  
  })
  
  l_pos

}

#### NFL.COM ####
draft_nfl = function() {
  year = get_scrape_year()
  
  
  nfl_url = paste0("https://fantasy.nfl.com/draftcenter/breakdown?leagueId=&offset=1&count=200&position=all&season=",
                   year, "&sort=draftAveragePosition")
  
  html_page = rvest::read_html(nfl_url)
  
  nfl_table = html_page %>%
    rvest::html_elements("tbody") %>%
    rvest::html_table() %>%
    base::`[[`(1) %>%
    extract(X1, c("player", "pos", "team"), "(.*?)\\s+([A-Z]{2,3}).*?([A-Z]{2,3}).*") %>%
    rename(adp = X2, avg_round = X3, average_salary = X4)
  
  nfl_id = html_page %>%
    rvest::html_elements("tbody > tr > td > div > a") %>%
    rvest::html_attr("href") %>%
    unique() %>%
    sub(".*playerId=", "", .)
  
  out_df = nfl_table %>%
    dplyr::mutate(
      nfl_id = !!nfl_id) %>%
    dplyr::select(nfl_id, 
                  nfl_adp = adp,
                  nfl_avg_round = avg_round,
                  nfl_avg_salary = average_salary)
  out_df
}

scrape_nfl = function(pos = c("QB", "RB", "WR", "TE", "K"), season = 2025, week = NA,
                      draft = TRUE, weekly = TRUE) {
  message("\nScraping NFL.com data. Please be patient...")
  
  if(is.null(season)) {
    season = get_scrape_year()
  }
  if(is.null(week)) {
    week = get_scrape_week()
  }
  
  draft_data <- draft_nfl()
  
  l_pos = lapply(pos, function(pos) {
    
    pos_scrape = dplyr::case_when(
      pos == "QB" ~ 1,
      pos == "RB" ~ 2,
      pos == "WR" ~ 3,
      pos == "TE" ~ 4,
      pos == "K" ~ 7
    )
    
    base_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                       "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                       "&statType=seasonProjectedStats")
    
    site_session = rvest::session(base_link)
    
    n_records = dplyr::case_when(
      pos == "QB" ~ 64,
      pos == "RB" ~ 96,
      pos == "WR" ~ 160,
      pos == "TE" ~ 64,
      pos == "K" ~ 64
      #pos == "DST" ~ 32
    )
    
    if(is.na(week)) {
      scrape_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape,
                           "&count=", n_records,
                           "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                           "&statType=seasonProjectedStats")
    } else {
      scrape_link = paste0("https://fantasy.nfl.com/research/projections?position=", pos_scrape[1],
                           "&count=", n_records,
                           "&sort=projectedPts&statCategory=projectedStats&statSeason=", season,
                           "&statType=weekProjectedStats&statWeek=", week)
    }
    
    cat(paste0("Scraping ", pos, " projections from"), scrape_link, sep = "\n  ")
    
    html_page = site_session %>%
      rvest::session_jump_to(scrape_link) %>%
      xml2::read_html()
    
    # Get PID
    site_id = html_page %>%
      rvest::html_elements("table td:first-child a.playerName") %>%
      rvest::html_attr("href") %>%
      sub(".*=", "",  .)
    
    # Getting column names
    col_names = html_page %>%
      rvest::html_element("table > thead") %>%
      rvest::html_table(header = FALSE)
    
    col_names = trimws(paste(col_names[1, ], col_names[2, ]))
    #col_names = nfl_columns[col_names]
    
    # Creating and cleaning table
    out_df = html_page %>%
      rvest::html_element("table > tbody") %>%
      rvest::html_table(header = FALSE) %>%
      `names<-`(col_names)
    
    # Breaking out first column / cleaning (for DST)
    # if(pos != "DST") {
    #   out_df = out_df %>%
    #     extract(player, c("player", "pos", "team"),
    #             "(.*?)\\s+\\b(QB|RB|WR|TE|K)\\b.*?([A-Z]{2,3})")
    # } else {
    #   out_df$team = sub("\\s+DEF$", "", out_df$team)
    #   out_df$pos = "DST"
    # }
    
    if(pos %in% c("RB", "WR", "TE") && "pass_int" %in% names(out_df)) {
      out_df$pass_int = NA_real_
    }
    
    out_df <- dplyr::mutate(out_df,year = season, week = ifelse(is.na(week),NA_real_,week),
                            site = "NFL.com",
                            nfl_id = as.character(site_id),
                            Opp = NA_character_)
    
    if (pos %in% c("QB","RB","WR","TE")) {
    out_df <- out_df %>%
      tidyr::extract(Player, c("player", "pos", "team"),
              "(.*?)\\s+\\b(QB|RB|WR|TE|K)\\b.*?([A-Z]{2,3})") %>%
      dplyr::left_join(.,draft_data,by="nfl_id") %>%
      dplyr::left_join(.,players %>% dplyr::select(gsis_id,nfl_id),by=c("nfl_id")) %>%
      dplyr::select(year,site,nfl_id, gsis_id,player, pos, team, opp = Opp, GP,
                    nfl_adp, nfl_avg_round, nfl_avg_salary,
                    nfl_projected_passing_yards = `Passing Yds`,
                    nfl_projected_passing_tds = `Passing TD`,
                    nfl_projected_passing_ints = `Passing Int`,
                    nfl_projected_rushing_yards = `Rushing Yds`,
                    nfl_projected_rushing_tds = `Rushing TD`,
                    nfl_projected_receptions = `Receiving Rec`,
                    nfl_projected_receiving_yards = `Receiving Yds`,
                    nfl_projected_receiving_tds = `Receiving TD`,
                    nfl_projected_return_tds = `Ret TD`,
                    nfl_projected_misc_td = `Misc FumTD`,
                    nfl_projected_misc_2pt = `Misc 2PT`,
                    nfl_projected_fum_lost = `Fum Lost`,
                    nfl_projected_fantasy_points_ppr = `Fantasy Points`
      )
    } 
    
    Sys.sleep(sample(runif(25,min = 2, max = 3.1415926535897932384626),1)) 
    
    # Removing all NA columns
    Filter(function(x) any(!is.na(x)), out_df)
  })
  
  l_pos

}
