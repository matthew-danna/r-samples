library(tidyverse)
library(googlesheets4)
library(googledrive)

# Get Roster
rosters <- read.csv("/Users/matthewdanna/Downloads/roster.csv", stringsAsFactors = FALSE)
unique(rosters$Name)

#####
##### Zones
#####

# Zone 1
bw.z1m1 <- data.frame(Zone = "1",
                         Mission = "1",
                         Requirement = "3YS",
                         Character.Id = c("OldManLogan", "HankPym", "Sasquatch", "Guardian", "Daken", "IronPatriot"))
bw.z1m2 <- data.frame(Zone = "1",
                      Mission = "2",
                      Requirement = "5YS",
                      Character.Id = c("BlackBolt", "BlackPantherShuri", "Northstar", "PandaPool", "TheLeader", "Sunfire"))
bw.z1m3 <- data.frame(Zone = "1",
                      Mission = "3",
                      Requirement = "7YS",
                      Character.Id = c("IronMan", "Deathpool", "Namor", "MrFantastic", "Deadpool", "Wolverine"))
bw.z1m4 <- data.frame(Zone = "1",
                      Mission = "4",
                      Requirement = "GT15",
                      Character.Id = c("CosmicGhostRider", "SpiderSlayer", "SpiderManNoir", "SilverSurfer", "MoonKnight", "Venom"))
bw.z1m5 <- data.frame(Zone = "1",
                      Mission = "5",
                      Requirement = "GT17",
                      Character.Id = c("SuperSkrull", "BlackCat", "PeterBParker", "RedGoblin", "Kraven", "Ultimus"))
bw.z1m6 <- data.frame(Zone = "1",
                      Mission = "6",
                      Requirement = "6RS",
                      Character.Id = c("Nova", "DoctorOctopus", "Gorr", "SpiderManPavitr", "Mysterio", "Carnage"))
bw.z1m7 <- data.frame(Zone = "1",
                      Mission = "7",
                      Requirement = "GT19",
                      Character.Id = c("GreenGoblinGlider", "ThanosEndgame", "PeniParker", "VoidKnight", "SpiderManBigTime", "Vahl"))
bw.z1m8 <- data.frame(Zone = "1",
                      Mission = "8",
                      Requirement = "1D",
                      Character.Id = c("Gladiator", "Gwenom", "Lizard", "Kang", "GhostSpider", "Vulture"))

# Zone 2
bw.z2m1 <- data.frame(Zone = "2",
                      Mission = "1",
                      Requirement = "3YS",
                      Character.Id = c("CaptainBritain", "ThanosEndgame", "Gorr", "BlackPantherShuri", "PeniParker", "PeterBParker"))
bw.z2m2 <- data.frame(Zone = "2",
                      Mission = "2",
                      Requirement = "5YS",
                      Character.Id = c("Gladiator", "HankPym", "SpiderManPavitr", "Starbrand", "MrFantastic", "Ultimus"))
bw.z2m3 <- data.frame(Zone = "2",
                      Mission = "3",
                      Requirement = "7YS",
                      Character.Id = c("BlackBolt", "CosmicGhostRider", "BlackKnight", "SpiderManNoir", "SilverSurfer", "GhostSpider"))
bw.z2m4 <- data.frame(Zone = "2",
                      Mission = "4",
                      Requirement = "GT15",
                      Character.Id = c("DoctorDoom", "ZombieJuggernaut", "OmegaSentinel", "Northstar", "Daken", "Sunspot"))
bw.z2m5 <- data.frame(Zone = "2",
                      Mission = "5",
                      Requirement = "GT17",
                      Character.Id = c("Rogue", "Sentinel", "Ares", "Forge", "Sunfire", "Deathpool"))
bw.z2m6 <- data.frame(Zone = "2",
                      Mission = "6",
                      Requirement = "6RS",
                      Character.Id = c("ZombieIronMan", "PandaPool", "IronPatriot", "Cyclops", "ScientistSupreme", "Wolverine"))
bw.z2m7 <- data.frame(Zone = "2",
                      Mission = "7",
                      Requirement = "GT19",
                      Character.Id = c("Apocalypse", "OldManLogan", "Nimrod", "Sasquatch", "TheLeader", "Gambit"))
bw.z2m8 <- data.frame(Zone = "2",
                      Mission = "8",
                      Requirement = "1D",
                      Character.Id = c("Guardian", "Nightcrawler", "AgathaHarkness", "LadyDeathstrike", "Namor", "Deadpool"))

# Zone 3
bw.z3m1 <- data.frame(Zone = "3",
                      Mission = "1",
                      Requirement = "3YS",
                      Character.Id = c("OmegaSentinel", "Nimrod", "Northstar", "PandaPool", "Gwenom", "Deadpool"))
bw.z3m2 <- data.frame(Zone = "3",
                      Mission = "2",
                      Requirement = "5YS",
                      Character.Id = c("OldManLogan", "Sentinel", "Sasquatch", "RedGoblin", "Deathpool", "Wolverine"))
bw.z3m3 <- data.frame(Zone = "3",
                      Mission = "3",
                      Requirement = "7YS",
                      Character.Id = c("Guardian", "Daken", "VoidKnight", "Sunfire", "LadyDeathstrike", "ScientistSupreme"))
bw.z3m4 <- data.frame(Zone = "3",
                      Mission = "4",
                      Requirement = "GT15",
                      Character.Id = c("BlackCat", "IronMan", "PeterBParker", "Starbrand", "Quicksilver", "Ultimus"))
bw.z3m5 <- data.frame(Zone = "3",
                      Mission = "5",
                      Requirement = "GT17",
                      Character.Id = c("GreenGoblinGlider", "CaptainBritain", "Gorr", "SpiderManPavitr", "SpiderManBigTime", "CaptainCarter"))
bw.z3m6 <- data.frame(Zone = "3",
                      Mission = "6",
                      Requirement = "6RS",
                      Character.Id = c("SuperSkrull", "BlackBolt", "Ares", "CosmicGhostRider", "Sunspot", "SilverSurfer"))
bw.z3m7 <- data.frame(Zone = "3",
                      Mission = "7",
                      Requirement = "GT19",
                      Character.Id = c("Mephisto", "Nova", "Gladiator", "BlackPantherShuri", "BlackKnight", "Nightcrawler"))
bw.z3m8 <- data.frame(Zone = "3",
                      Mission = "8",
                      Requirement = "1D",
                      Character.Id = c("Apocalypse", "ThanosEndgame", "HankPym", "PeniParker", "Forge", "CaptainAmerica"))

# Zone 4
bw.z4m1 <- data.frame(Zone = "4",
                      Mission = "1",
                      Requirement = "3YS",
                      Character.Id = c("ManThing", "Oath", "Blade", "Sentinel", "AgathaHarkness", "Ultimus"))
bw.z4m2 <- data.frame(Zone = "4",
                      Mission = "2",
                      Requirement = "5YS",
                      Character.Id = c("DoctorDoom", "CaptainBritain", "OmegaSentinel", "Nimrod", "ThanosEndgame", "Guardian"))
bw.z4m3 <- data.frame(Zone = "4",
                      Mission = "3",
                      Requirement = "7YS",
                      Character.Id = c("Dormammu", "HankPym", "BlackPantherShuri", "Northstar", "PeniParker", "MoonKnight"))
bw.z4m4 <- data.frame(Zone = "4",
                      Mission = "4",
                      Requirement = "GT15",
                      Character.Id = c("Archangel", "Phoenix", "ZombieScarletWitch", "SpiderManPavitr", "SpiderWeaver", "Cyclops"))
bw.z4m5 <- data.frame(Zone = "4",
                      Mission = "5",
                      Requirement = "GT17",
                      Character.Id = c("Rogue", "MorganLeFay", "ZombieJuggernaut", "Gladiator", "Sunspot", "X23"))
bw.z4m6 <- data.frame(Zone = "4",
                      Mission = "6",
                      Requirement = "6RS",
                      Character.Id = c("Odin", "OldManLogan", "RedHulk", "PeterBParker", "RedGoblin", "Nightcrawler"))
bw.z4m7 <- data.frame(Zone = "4",
                      Mission = "7",
                      Requirement = "GT19",
                      Character.Id = c("SuperSkrull", "Gorr", "Gwenom", "Forge", "Deathpool", "Wolverine"))
bw.z4m8 <- data.frame(Zone = "4",
                      Mission = "8",
                      Requirement = "1D",
                      Character.Id = c("ZombieIronMan", "Sasquatch", "VoidKnight", "Vahl", "Sunfire", "Gambit"))

# Zone 5
bw.z5m1 <- data.frame(Zone = "5",
                      Mission = "1",
                      Requirement = "3YS",
                      Character.Id = c("ZombieScarletWitch", "Gladiator", "Guardian", "Sunfire", "SilverSurfer", "Wolverine"))
bw.z5m2 <- data.frame(Zone = "5",
                      Mission = "2",
                      Requirement = "5YS",
                      Character.Id = c("ZombieIronMan", "Gorr", "Ares", "Northstar", "Daken", "Deadpool"))
bw.z5m3 <- data.frame(Zone = "5",
                      Mission = "3",
                      Requirement = "7YS",
                      Character.Id = c("OldManLogan", "ZombieJuggernaut", "ThanosEndgame", "Sasquatch", "Pandpool", "Ultimus"))
bw.z5m4 <- data.frame(Zone = "5",
                      Mission = "4",
                      Requirement = "GT15",
                      Character.Id = c("ManThing", "PeniParker", "VoidKnight", "GhostRiderRobbie", "LadyDeathstrike", "Mysterio"))
bw.z5m5 <- data.frame(Zone = "5",
                      Mission = "5",
                      Requirement = "GT17",
                      Character.Id = c("Apocalypse", "Oath", "Gwenom", "SpiderSlayer", "Quicksilver", "ScientistSupreme"))
bw.z5m6 <- data.frame(Zone = "5",
                      Mission = "6",
                      Requirement = "6RS",
                      Character.Id = c("GreenGoblinGlider", "CaptainBritain", "Sentinel", "HankPym", "BlackPantherShuri", "Venom"))
bw.z5m7 <- data.frame(Zone = "5",
                      Mission = "7",
                      Requirement = "GT19",
                      Character.Id = c("Blade", "OmegaSentinel", "PeterBParker", "CosmicGhostRider", "RedGoblin", "Vulture"))
bw.z5m8 <- data.frame(Zone = "5",
                      Mission = "8",
                      Requirement = "1D",
                      Character.Id = c("DoctorOctopus", "Nimrod", "SpiderManPavitr", "Deathpool", "MoonKnight", "Carnage"))

battleworld <- rbind(bw.z1m1, bw.z1m2, bw.z1m3, bw.z1m4, bw.z1m5, bw.z1m6, bw.z1m7, bw.z1m8,
                     bw.z2m1, bw.z2m2, bw.z2m3, bw.z2m4, bw.z2m5, bw.z2m6, bw.z2m7, bw.z2m8,
                     bw.z3m1, bw.z3m2, bw.z3m3, bw.z3m4, bw.z3m5, bw.z3m6, bw.z3m7, bw.z3m8,
                     bw.z4m1, bw.z4m2, bw.z4m3, bw.z4m4, bw.z4m5, bw.z4m6, bw.z4m7, bw.z4m8,
                     bw.z5m1, bw.z5m2, bw.z5m3, bw.z5m4, bw.z5m5, bw.z5m6, bw.z5m7, bw.z5m8)

roster.bw <- rosters %>% inner_join(battleworld, by = "Character.Id")

#####
##### Season Rules
#####

roster.m1 <- subset(roster.bw, roster.bw$Mission == 1 & 
                       roster.bw$Stars > 2)
roster.m2 <- subset(roster.bw, roster.bw$Mission == 2 & 
                        roster.bw$Stars > 4)
roster.m3 <- subset(roster.bw, roster.bw$Mission == 3 & 
                        roster.bw$Stars > 6)
roster.m4 <- subset(roster.bw, roster.bw$Mission == 4 & 
                        roster.bw$Gear.Tier > 14)
roster.m5 <- subset(roster.bw, roster.bw$Mission == 5 & 
                        roster.bw$Stars > 16)
roster.m6 <- subset(roster.bw, roster.bw$Mission == 6 & 
                        roster.bw$Red.Stars > 5)
roster.m7 <- subset(roster.bw, roster.bw$Mission == 7 & 
                        roster.bw$Gear.Tier > 18)
roster.m8 <- subset(roster.bw, roster.bw$Mission == 8 & 
                        roster.bw$Red.Stars > 7)

roster.battleworld <- rbind(roster.m1, roster.m2, roster.m3, roster.m4, 
                            roster.m5, roster.m6, roster.m7, roster.m8)

#####
##### Summary Tables
#####

bw.sum1 <- roster.battleworld %>%
  group_by(Zone, Mission, Requirement, Character.Id, Name) %>%
  summarise(Match = "yes")

bw.sum2 <- roster.battleworld %>%
  group_by(Zone, Mission, Requirement, Character.Id) %>%
  summarise(Player.Count = n())

bw.sum3 <- roster.battleworld %>%
  group_by(Zone, Mission, Requirement, Name) %>%
  summarise(Character.Count = n())

#####
##### Export
#####

battleworld.summary <- gs4_create("MSF Gods of Knowhere Battlejam", sheets = list(summary = bw.sum2,
                                                                                  player = bw.sum3,
                                                                                  data = bw.sum1))
