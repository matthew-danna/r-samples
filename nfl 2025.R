library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)

# ------------------------------
# Stadium lookup table
# ------------------------------
nfl_stadiums <- tribble(
  ~Team.Home, ~Stadium, ~City, ~State, ~Lat, ~Lon, ~StadiumType,
  "Arizona Cardinals", "State Farm Stadium", "Glendale", "AZ", 33.5276, -112.2626, "Retractable",
  "Atlanta Falcons", "Mercedes-Benz Stadium", "Atlanta", "GA", 33.7554, -84.4009, "Retractable",
  "Baltimore Ravens", "M&T Bank Stadium", "Baltimore", "MD", 39.2780, -76.6227, "Outdoor",
  "Buffalo Bills", "Highmark Stadium", "Orchard Park", "NY", 42.7738, -78.7864, "Outdoor",
  "Carolina Panthers", "Bank of America Stadium", "Charlotte", "NC", 35.2251, -80.8526, "Outdoor",
  "Chicago Bears", "Soldier Field", "Chicago", "IL", 41.8623, -87.6167, "Outdoor",
  "Cincinnati Bengals", "Paycor Stadium", "Cincinnati", "OH", 39.0954, -84.5160, "Outdoor",
  "Cleveland Browns", "Cleveland Browns Stadium", "Cleveland", "OH", 41.5061, -81.6995, "Outdoor",
  "Dallas Cowboys", "AT&T Stadium", "Arlington", "TX", 32.7473, -97.0945, "Retractable",
  "Denver Broncos", "Empower Field at Mile High", "Denver", "CO", 39.7439, -105.0201, "Outdoor",
  "Detroit Lions", "Ford Field", "Detroit", "MI", 42.3400, -83.0456, "Indoor",
  "Green Bay Packers", "Lambeau Field", "Green Bay", "WI", 44.5013, -88.0622, "Outdoor",
  "Houston Texans", "NRG Stadium", "Houston", "TX", 29.6847, -95.4107, "Retractable",
  "Indianapolis Colts", "Lucas Oil Stadium", "Indianapolis", "IN", 39.7601, -86.1639, "Retractable",
  "Jacksonville Jaguars", "EverBank Stadium", "Jacksonville", "FL", 30.3239, -81.6373, "Outdoor",
  "Kansas City Chiefs", "GEHA Field at Arrowhead Stadium", "Kansas City", "MO", 39.0490, -94.4840, "Outdoor",
  "Las Vegas Raiders", "Allegiant Stadium", "Las Vegas", "NV", 36.0908, -115.1830, "Indoor",
  "Los Angeles Chargers", "SoFi Stadium", "Inglewood", "CA", 33.9535, -118.3392, "Indoor",
  "Los Angeles Rams", "SoFi Stadium", "Inglewood", "CA", 33.9535, -118.3392, "Indoor",
  "Miami Dolphins", "Hard Rock Stadium", "Miami Gardens", "FL", 25.9580, -80.2389, "Outdoor",
  "Minnesota Vikings", "U.S. Bank Stadium", "Minneapolis", "MN", 44.9736, -93.2575, "Indoor",
  "New England Patriots", "Gillette Stadium", "Foxborough", "MA", 42.0909, -71.2643, "Outdoor",
  "New Orleans Saints", "Caesars Superdome", "New Orleans", "LA", 29.9509, -90.0815, "Indoor",
  "New York Giants", "MetLife Stadium", "East Rutherford", "NJ", 40.8136, -74.0742, "Outdoor",
  "New York Jets", "MetLife Stadium", "East Rutherford", "NJ", 40.8136, -74.0742, "Outdoor",
  "Philadelphia Eagles", "Lincoln Financial Field", "Philadelphia", "PA", 39.9008, -75.1675, "Outdoor",
  "Pittsburgh Steelers", "Acrisure Stadium", "Pittsburgh", "PA", 40.4468, -80.0158, "Outdoor",
  "San Francisco 49ers", "Levi's Stadium", "Santa Clara", "CA", 37.4030, -121.9700, "Outdoor",
  "Seattle Seahawks", "Lumen Field", "Seattle", "WA", 47.5952, -122.3316, "Outdoor",
  "Tampa Bay Buccaneers", "Raymond James Stadium", "Tampa", "FL", 27.9759, -82.5033, "Outdoor",
  "Tennessee Titans", "Nissan Stadium", "Nashville", "TN", 36.1665, -86.7713, "Outdoor",
  "Washington Commanders", "FedExField", "Landover", "MD", 38.9077, -76.8645, "Outdoor"
)

# ------------------------------
# Scrape function
# ------------------------------
scrape_season <- function(season) {
  url <- paste0("https://www.pro-football-reference.com/years/", season, "/games.htm")
  
  page <- read_html(url)
  
  # Pull the table
  games <- page %>%
    html_element("table#games") %>%
    html_table()
  
  # Replace any blank/NA column name (the between-team column) with "at"
  nms <- names(games)
  nms[is.na(nms) | nms == ""] <- "at"
  names(games) <- make.names(nms, unique = TRUE)
  
  # Keep regular season only
  regular_season <- games %>%
    filter(str_detect(Week, "^[0-9]+$")) %>%
    filter(!is.na(Date) & Date != "")
  
  # Add home/away
  regular_season <- regular_season %>%
    mutate(
      HomeTeam = if_else(at == "@", `Loser.tie`, `Winner.tie`),
      AwayTeam = if_else(at == "@", `Winner.tie`, `Loser.tie`)
    )
  
  # Select + rename columns
  regular_season_cleaned <- regular_season[c(1:5,7,9:16)]
  regular_season_cleaned$season <- season
  
  names(regular_season_cleaned) <- c(
    "Week", "Day", "Date", "Time", "Winner", "Loser",
    "Winner.Points", "Loser.Points",
    "Winner.Yards", "Winner.Turnovers",
    "Loser.Yards", "Loser.Turnovers",
    "Team.Home", "Team.Away", "Season"
  )
  
  # ---- CLEAN NUMERIC COLUMNS HERE ----
  regular_season_cleaned <- regular_season_cleaned %>%
    mutate(
      Winner.Points     = suppressWarnings(as.numeric(Winner.Points)),
      Loser.Points      = suppressWarnings(as.numeric(Loser.Points)),
      Winner.Yards      = suppressWarnings(as.numeric(Winner.Yards)),
      Loser.Yards       = suppressWarnings(as.numeric(Loser.Yards)),
      Winner.Turnovers  = suppressWarnings(as.numeric(Winner.Turnovers)),
      Loser.Turnovers   = suppressWarnings(as.numeric(Loser.Turnovers))
    ) %>%
    mutate(
      GameID = paste0(
        Season, "_W", Week, "_",
        gsub(" ", "", Team.Home), "_vs_", gsub(" ", "", Team.Away)
      )
    )
  
  return(regular_season_cleaned)
}

# ------------------------------
# Weather fetcher (Open-Meteo)
# ------------------------------
get_weather <- function(lat, lon, datetime) {
  # Round to nearest hour
  datetime <- floor_date(datetime, "hour")
  url <- paste0("https://archive-api.open-meteo.com/v1/archive?",
                "latitude=", lat,
                "&longitude=", lon,
                "&start_date=", as.Date(datetime),
                "&end_date=", as.Date(datetime),
                "&hourly=temperature_2m,precipitation",
                "&timezone=America/New_York")
  
  resp <- try(GET(url), silent = TRUE)
  if(inherits(resp, "try-error") || status_code(resp) != 200) {
    return(tibble(TempC = NA, PrecipMM = NA))
  }
  
  dat <- content(resp, "parsed")
  
  if (is.null(dat$hourly)) return(tibble(TempC = NA, PrecipMM = NA))
  
  hours <- dat$hourly$time
  temps <- dat$hourly$temperature_2m
  precs <- dat$hourly$precipitation
  
  match_idx <- which(hours == format(datetime, "%Y-%m-%dT%H:00"))
  if(length(match_idx) == 0) return(tibble(TempC = NA, PrecipMM = NA))
  
  tibble(
    TempC = temps[[match_idx]],
    PrecipMM = precs[[match_idx]]
  )
}

# ------------------------------
# GET DATA
# ------------------------------
all_seasons <- map_df(2015:2024, scrape_season)

# Mapping of old names -> current names
team_name_map <- c(
  "San Diego Chargers"    = "Los Angeles Chargers",
  "St. Louis Rams"        = "Los Angeles Rams",
  "Oakland Raiders"       = "Las Vegas Raiders",
  "Washington Redskins"   = "Washington Commanders",
  "Washington Football Team" = "Washington Commanders"
)

# Function to normalize names
normalize_team_name <- function(name) {
  ifelse(name %in% names(team_name_map),
         team_name_map[name],
         name)
}

# Apply normalization
all_seasons <- all_seasons %>%
  mutate(
    Team.Home = normalize_team_name(Team.Home),
    Team.Away = normalize_team_name(Team.Away),
    Winner    = normalize_team_name(Winner),
    Loser     = normalize_team_name(Loser)
  )

# Join stadium info
games_with_stadium <- all_seasons %>%
  left_join(nfl_stadiums, by = c("Team.Home" = "Team.Home"))

games_with_stadium <- games_with_stadium %>%
  mutate(
    # Build full date with year
    Date_full = paste(Date),
    
    # Normalize time: remove "Final", uppercase AM/PM
    Time_clean = ifelse(str_detect(Time, "^[0-9]"),
                        str_to_upper(Time),
                        NA_character_),
    
    # Build combined string
    DateTime_str = ifelse(!is.na(Time_clean),
                          paste(Date_full, Time_clean),
                          Date_full),
    
    # Parse datetime (handles both with and without time)
    GameDateTime = parse_date_time(
      DateTime_str,
      orders = c("Y-m-d I:Mp"),
      tz = "America/New_York"
    )
  )

# Fetch weather only for outdoor/retractable
games_with_weather <- games_with_stadium %>%
  mutate(
    Weather = pmap(list(Lat, Lon, GameDateTime, StadiumType), function(lat, lon, dt, type) {
      if (is.na(lat) | type == "Indoor") return(tibble(TempC = NA, PrecipMM = NA))
      get_weather(lat, lon, dt)
    })
  ) %>%
  unnest(Weather)

# ------------------------------
# 2025 schedule
# ------------------------------

# Scrape 2025 schedule
url_2025 <- "https://www.pro-football-reference.com/years/2025/games.htm"
page_2025 <- read_html(url_2025)

games_2025 <- page_2025 %>%
  html_element("table#games") %>%
  html_table()

# Replace blank/NA headers
nms <- names(games_2025)
nms[is.na(nms) | nms == ""] <- c("Date", "at")  # 3rd col = Date, 6th col = "@"
names(games_2025) <- make.names(nms, unique = TRUE)

# Keep only regular season (Week is numeric)
regular_2025 <- games_2025 %>%
  filter(str_detect(Week, "^[0-9]+$")) %>%
  filter(!is.na(Date) & Date != "")

# Build cleaned table
schedule_2025 <- regular_2025 %>%
  mutate(
    # Add season year to the date and parse to yyyy-mm-dd
    Date = mdy(paste(Date, 2025))
  ) %>%
  transmute(
    Week,
    Day,
    Date = format(Date, "%Y-%m-%d"),
    Time,
    Winner = VisTm,
    Loser = HomeTm,  # placeholder until games are played
    Winner.Points = Pts,
    Loser.Points = Pts.1,
    Winner.Yards = NA,
    Winner.Turnovers = NA,
    Loser.Yards = NA,
    Loser.Turnovers = NA,
    Team.Home = HomeTm,
    Team.Away = VisTm,
    Season = 2025
  )

# join stadium info
games_with_stadium_2025 <- schedule_2025 %>%
  left_join(nfl_stadiums, by = c("Team.Home" = "Team.Home"))

games_with_stadium_2025 <- games_with_stadium_2025 %>%
  mutate(
    # Build full date with year
    Date_full = paste(Date),
    
    # Normalize time: remove "Final", uppercase AM/PM
    Time_clean = ifelse(str_detect(Time, "^[0-9]"),
                        str_to_upper(Time),
                        NA_character_),
    
    # Build combined string
    DateTime_str = ifelse(!is.na(Time_clean),
                          paste(Date_full, Time_clean),
                          Date_full),
    
    # Parse datetime (handles both with and without time)
    GameDateTime = parse_date_time(
      DateTime_str,
      orders = c("Y-m-d I:Mp"),
      tz = "America/New_York"
    )
  )