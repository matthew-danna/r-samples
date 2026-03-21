# =============================================================================
# Marvel Strike Force — Character Tag + Stats Scraper
# =============================================================================
# SETUP (run once):
#   install.packages(c("chromote", "rvest", "dplyr", "tidyr", "readr", "stringr"))
#
# If Chrome isn't found automatically, set its path first:
#   Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/Google/Chrome/Application/chrome.exe")
#   Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
#   Sys.setenv(CHROMOTE_CHROME = "/usr/bin/google-chrome")
# =============================================================================

library(chromote)
library(rvest)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# =============================================================================
# !! PASTE YOUR FULL TAG LIST HERE !!
# =============================================================================

TRAIT_SLUGS <- c(
  "HERO", "VILLAIN", "LEGENDARY", "MYTHIC", "MINION", "CITY", "GLOBAL", "COSMIC", "BIO", "MUTANT", 
  "MYSTIC", "SKILL", "TECH", "BLASTER", "BRAWLER", "CONTROLLER", "PROTECTOR", "SUPPORT", "AForce", 
  "Aim", "AbsoluteAForce", "Accursed", "AlphaFlight", "Annihilator", "Asgard", "Astonishing", 
  "Astral", "Avenger", "Bifrost", "BionicAvenger", "BlackOrder", "Brimstone", "Brotherhood", "Cabal", 
  "Champion", "ChaosTeam", "Harbingers", "Conqueror", "Stormbound", "AnnihilationWave", "Couples", 
  "DaringWarrior", "DarkHunter", "DARK_PROMOTION", "Darkhold", "Deathseed", "Defender", "Epic", 
  "Eternal", "Exposed", "FantasticFour", "FantasticFourMCU", "GalacticCouncil", "Gamma", 
  "GotG", "Hand", "HellfireClub", "HeroesForHire", "HiveMind", "Horseman", "Hydra", 
  "Illuminati", "ImmortalWeapon", "ImmortalXMen", "Infestation", "InfinityWatch", "Inhuman", 
  "InsidiousSix", "Invader", "Knowhere", "KnowhereHeist", "KnullChallengers", "Kree", "Liberty", 
  "Marauders", "MastersOfEvil", "Mercenary", "MercsForMoney", "MightyAvenger", "Military", 
  "MSFOriginal", "NewAvenger", "NewMutant", "NewWarrior", "Nightstalker", "Orchis", "OutOfTime", 
  "Pegasus", "PhoenixForce", "PowerArmor", "PymTech", "Ravager", "Rebirth", "Retcon", 
  "Shield", "SecretAvenger", "SecretDefender", "SecretWarrior", "Shadowland", "SinisterSix", 
  "SpiderSociety", "SpiderVerse", "Starjammer", "SuperiorSix", "Supernatural", "Symbiote", 
  "TangledWeb", "Thunderbolt", "UncannyAvenger", "Uncanny", "Underworld", "Undying", 
  "Unlimited", "Vampire", "Vigilante", "Wakanda", "WarDog", "Wave1Avenger", "WeaponX", 
  "WebSlinger", "WebWarrior", "WinterGuard", "XFactor", "Xforce", "Xmen", "Xtreme", 
  "YoungAvenger"
)

# =============================================================================
# MANUAL ID → DISPLAY NAME OVERRIDES
# Maps character_id slugs (from the URL) to their exact display name in the
# stats table. Add any new mismatches to the bottom of this list.
# =============================================================================

ID_NAME_OVERRIDES <- c(
  # Individuals with abbreviated / alternate slugs
  "Coulson"             = "Agent Coulson",
  "AmadeusCho"          = "Brawn",
  "SamWilson"           = "Captain America (Sam)",
  "CaptainAmericaWW2"   = "Captain America (WWII)",
  "TChalla"             = "Black Panther",
  "BlackPantherBC"      = "Black Panther (1MM)",
  "KamalaKhan"          = "Ms. Marvel",
  "JaneThor"            = "Mighty Thor",
  "Xavier"              = "Professor Xavier",
  "MrFantastic"         = "Mister Fantastic",
  "MrFantasticMCU"      = "Mister Fantastic (MCU)",
  "MrNegative"          = "Mister Negative",
  "MrSinister"          = "Mister Sinister",
  "IronFistOrson"       = "Iron Fist (WWII)",
  "IronheartMCU"        = "Ironheart (MKII)",
  "Sybil"               = "Scarlet Witch",
  "UltSpiderMan"        = "Spider-Man (Miles)",
  "SymbioteSpiderMan"   = "Spider-Man (Symbiote)",
  "SymbioteSilverSurfer"= "Void Knight",
  "Thing"               = "The Thing",
  "Yelena"              = "Yelena Belova",
  "Zemo"                = "Baron Zemo",
  "Doom"                = "Doom",
  "UltGreenGoblin"      = "Green Goblin",
  "GreenGoblinGlider"   = "Green Goblin (Classic)",
  "Korath"              = "Korath the Pursuer",
  "Minerva"             = "Minn-Erva",
  "BaronMordo"          = "Mordo",
  "OmegaRedPhoenix"     = "Omega Red (Phoenix Force)",
  "Ronan"               = "Ronan the Accuser",
  "StrangeSupreme"      = "Strange (Heartless)",
  "ZombieIronMan"       = "Iron Man (Zombie)",
  "ZombieJuggernaut"    = "Juggernaut (Zombie)",
  "ZombieKestrel"       = "Kestrel (Zombie)",
  "ZombieScarletWitch"  = "Scarlet Witch (Zombie)",

  # A.I.M. minions
  "AimDmg_Speed"        = "A.I.M. Assaulter",
  "AimDmg_Offense"      = "A.I.M. Monstrosity",
  "AimControl_Infect"   = "A.I.M. Infector",
  "AimSupport_Heal"     = "A.I.M. Researcher",
  "AimTank_Taunt"       = "A.I.M. Security",

  # S.H.I.E.L.D. minions
  "ShieldDmg_AoE"       = "S.H.I.E.L.D. Assault",
  "ShieldDmg_Defense"   = "S.H.I.E.L.D. Trooper",
  "ShieldSupport_Heal"  = "S.H.I.E.L.D. Medic",
  "ShieldSupport_Stealth"= "S.H.I.E.L.D. Operative",
  "ShieldTank_Stun"     = "S.H.I.E.L.D. Security",

  # Hand minions
  "HandDmg_Unbuff"      = "Hand Archer",
  "HandDmg_Bonus"       = "Hand Blademaster",
  "HandControl_HealBlock"= "Hand Assassin",
  "HandTank_Stealth"    = "Hand Sentry",
  "HandSupport_Heal"    = "Hand Sorceress",

  # Hydra minions
  "HydraTank_Taunt"     = "Hydra Armored Guard",
  "HydraDmg_AoE"        = "Hydra Grenadier",
  "HydraDmg_Buff"       = "Hydra Rifle Trooper",
  "HydraSupport_Heal"   = "Hydra Scientist",
  "HydraDmg_Single"     = "Hydra Sniper",

  # Kree minions
  "KreeDmg_Speed"       = "Kree Cyborg",
  "KreeControl_Assist"  = "Kree Noble",
  "KreeSupport_HoT"     = "Kree Oracle",
  "KreeDmg_Offense"     = "Kree Reaper",
  "KreeTank_Counter"    = "Kree Royal Guard",

  # Mercenary minions
  "MercSupport_Buffs"   = "Mercenary Lieutenant",
  "MercTank_Debuff"     = "Mercenary Riot Guard",
  "MercDmg_Single"      = "Mercenary Sniper",
  "MercDmg_General"     = "Mercenary Soldier",

  # Ravager minions
  "RavagerDmg_AoE"      = "Ravager Boomer",
  "RavagerTank_Taunt"   = "Ravager Bruiser",
  "RavagerSupport_Heal" = "Ravager Stitcher"

  # ── Add any new mismatches here as you find them ──
)

# =============================================================================
# CONFIG
# =============================================================================

OUTPUT_FILE  <- "msf_characters.csv"
BASE_URL     <- "https://marvelstrikeforce.com"
STATS_URL    <- paste0(BASE_URL, "/en/hero-total-stats")

PAGE_WAIT    <- 8
SCROLL_PAUSE <- 1.0
SCROLL_STEPS <- 30

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

if (length(TRAIT_SLUGS) == 0) {
  stop("TRAIT_SLUGS is empty — paste your tag list into the section above first.")
}

# =============================================================================
# 1. Start Chrome
# =============================================================================

message("\n── Starting Chrome ...")
chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
if (is.null(chrome_path) || !nzchar(chrome_path)) {
  stop(
    "\nChrome not found. Set the path first, e.g.:\n",
    '  Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/Google/Chrome/Application/chrome.exe")\n',
    '  Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")\n',
    '  Sys.setenv(CHROMOTE_CHROME = "/usr/bin/google-chrome")\n'
  )
}
message("   Found Chrome at: ", chrome_path)

b <- ChromoteSession$new()
on.exit(try(b$close(), silent = TRUE), add = TRUE)

get_html <- function() {
  src <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  read_html(src)
}

scroll_down <- function() {
  for (i in seq_len(SCROLL_STEPS)) {
    b$Runtime$evaluate(sprintf(
      "window.scrollTo(0, document.body.scrollHeight * %.4f);", i / SCROLL_STEPS))
    Sys.sleep(SCROLL_PAUSE)
  }
  b$Runtime$evaluate("window.scrollTo(0, 0);")
  Sys.sleep(1)
}

normalise <- function(x) str_to_lower(str_replace_all(x, "[^a-zA-Z0-9]", ""))

# =============================================================================
# 2. Scrape stats table
# =============================================================================

message("\n── Scraping stats table: ", STATS_URL)
b$Page$navigate(STATS_URL)
Sys.sleep(PAGE_WAIT)
scroll_down()

stats_html <- get_html()

stats_df <- tryCatch({
  tables <- stats_html |> html_elements("table")
  if (length(tables) == 0) stop("no <table> found")
  tbls <- lapply(tables, function(t) tryCatch(html_table(t, fill = TRUE), error = function(e) NULL))
  tbls <- Filter(Negate(is.null), tbls)
  tbls <- Filter(function(t) nrow(t) > 5, tbls)
  if (length(tbls) == 0) stop("no usable tables")
  tbls[[which.max(sapply(tbls, nrow))]]
}, error = function(e) {
  message("   <table> parse failed: ", conditionMessage(e), " — trying div layout ...")
  row_els   <- stats_html |> html_elements("[class*='row'], [class*='Row'], tr")
  rows_text <- lapply(row_els, function(el) {
    cells <- el |>
      html_elements("[class*='cell'], [class*='Cell'], td, th, span, div") |>
      html_text(trim = TRUE) |>
      (\(x) x[nzchar(x)])()
    if (length(cells) >= 3) cells else NULL
  })
  rows_text <- Filter(Negate(is.null), rows_text)
  if (length(rows_text) < 2) return(NULL)
  header    <- rows_text[[1]]
  data_rows <- rows_text[-1]
  n         <- length(header)
  data_rows <- lapply(data_rows, function(r) { length(r) <- n; r })
  tbl <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
  names(tbl) <- header
  tbl
})

if (is.null(stats_df) || nrow(stats_df) == 0) {
  stop("Could not parse the stats table. Check ", STATS_URL, " in your browser.")
}

# Clean up stats
name_col <- names(stats_df)[
  str_detect(names(stats_df), regex("name|character|hero", ignore_case = TRUE))
][1]
if (is.na(name_col)) name_col <- names(stats_df)[1]

stats_clean <- stats_df |>
  rename(character_name = all_of(name_col)) |>
  mutate(character_name = str_trim(character_name)) |>
  filter(nzchar(character_name), character_name != name_col) |>
  mutate(across(-character_name,
                ~ as.numeric(str_replace_all(as.character(.), "[^0-9.]", "")))) |>
  select(where(~ !all(is.na(.)))) |>
  distinct(character_name, .keep_all = TRUE) |>
  mutate(norm_key = normalise(character_name))

stat_cols <- setdiff(names(stats_clean), c("character_name", "norm_key"))
message(sprintf("   %d characters, stat columns: %s",
                nrow(stats_clean), paste(stat_cols, collapse = ", ")))

# =============================================================================
# 3. Scrape trait filter pages → character_id slugs
# =============================================================================

message(sprintf("\n── Scraping %d trait filter pages ...", length(TRAIT_SLUGS)))

trait_rows <- list()

for (slug in TRAIT_SLUGS) {
  url <- sprintf("%s/en/characters?trait=%s", BASE_URL, slug)
  message(sprintf("   [%s]", slug))

  b$Page$navigate(url)
  Sys.sleep(PAGE_WAIT)
  scroll_down()

  hrefs <- get_html() |>
    html_elements("a[href*='/characters/']") |>
    html_attr("href") |>
    unique()

  hrefs <- hrefs[
    !is.na(hrefs) &
    !grepl("^/?(/en)?/characters$", hrefs) &
    !grepl("/(abilities|stats|gear|costumes|trait)(/|$)", hrefs)
  ]

  char_ids <- str_extract(hrefs, "/characters/([^/?#]+)$", group = 1) |> na.omit()

  if (length(char_ids) > 0) {
    trait_rows[[slug]] <- data.frame(
      character_id = char_ids,
      trait        = slug,
      stringsAsFactors = FALSE
    )
    message(sprintf("      → %d characters", length(char_ids)))
  } else {
    message("      → 0 (check slug matches site filter value)")
  }
  Sys.sleep(0.5)
}

chars_long <- bind_rows(trait_rows) |>
  distinct(character_id, trait, .keep_all = TRUE)

if (nrow(chars_long) == 0) {
  stop("No trait data collected. Check that ?trait=Hero loads correctly in your browser.")
}

message(sprintf("   %d character×trait rows, %d unique characters.",
                nrow(chars_long), n_distinct(chars_long$character_id)))

# =============================================================================
# 4. Resolve character_id → display name
#
# Priority order:
#   1. Manual override table (ID_NAME_OVERRIDES)
#   2. Normalised string match against stats table names
#   3. Humanised slug fallback (CamelCase → spaced words)
# =============================================================================

message("\n── Resolving display names ...")

all_ids <- unique(chars_long$character_id)

resolve_name <- function(cid) {
  # 1. Manual override
  if (cid %in% names(ID_NAME_OVERRIDES)) return(ID_NAME_OVERRIDES[[cid]])

  # 2. Normalised match against stats table
  norm <- normalise(cid)
  match_idx <- which(stats_clean$norm_key == norm)
  if (length(match_idx) > 0) return(stats_clean$character_name[match_idx[1]])

  # 3. Humanise CamelCase slug: "AdamWarlock" → "Adam Warlock"
  str_trim(str_replace_all(cid, "(?<=[a-z])(?=[A-Z])|[-_]", " "))
}

name_lookup <- setNames(sapply(all_ids, resolve_name), all_ids)

chars_long <- chars_long |>
  mutate(character_name = name_lookup[character_id])

# Report anything still unmatched vs stats table
matched_to_stats <- chars_long |>
  distinct(character_id, character_name) |>
  mutate(in_stats = character_name %in% stats_clean$character_name)

n_matched   <- sum(matched_to_stats$in_stats)
n_unmatched <- sum(!matched_to_stats$in_stats)
message(sprintf("   Matched to stats: %d  |  Unmatched: %d", n_matched, n_unmatched))

if (n_unmatched > 0) {
  miss <- matched_to_stats$character_id[!matched_to_stats$in_stats]
  message("   Still unmatched (add to ID_NAME_OVERRIDES if they exist in stats table):")
  message("   ", paste(miss, collapse = ", "))
}

# =============================================================================
# 5. Pivot tags to wide Y/N
# =============================================================================

message("\n── Building wide tag table ...")

all_traits <- sort(unique(chars_long$trait))

chars_wide <- chars_long |>
  mutate(value = "Y") |>
  pivot_wider(
    id_cols     = c(character_name, character_id),
    names_from  = trait,
    values_from = value,
    values_fill = "N"
  )

for (tr in all_traits) {
  if (!tr %in% names(chars_wide)) chars_wide[[tr]] <- "N"
}

id_cols    <- c("character_name", "character_id")
trait_cols <- sort(setdiff(names(chars_wide), id_cols))
chars_wide <- chars_wide |>
  select(all_of(id_cols), all_of(trait_cols)) |>
  arrange(character_name)

# =============================================================================
# 6. Join stats
# =============================================================================

message("── Joining stats ...")

chars_final <- chars_wide |>
  left_join(stats_clean |> select(-norm_key), by = "character_name")

n_with_stats <- sum(!is.na(chars_final[[stat_cols[1]]]))
message(sprintf("   %d / %d characters have stats.", n_with_stats, nrow(chars_final)))

# =============================================================================
# 7. Save
# =============================================================================

write_csv(chars_final, "/Users/matthewdanna/Documents/GitHub/r-samples/msf/msf_characters.csv")

message(sprintf("Done! %d characters", nrow(chars_final)))
