# HW 3
#Created by Hissah Altamimi

# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ/
rm(list=ls())
library(tidyverse)
library(sf)
library(magrittr)
library(dplyr)
library(tools)  
library(stargazer)
library(cowplot)

# Part1:
countypres <- read.csv("countypres_2000-2020.csv")
head(countypres)
#   year   state state_po county_name county_fips    office      candidate      party candidatevotes totalvotes  version  mode
# 1 2000 ALABAMA       AL     AUTAUGA        1001 PRESIDENT        AL GORE   DEMOCRAT           4942      17208 20191203 TOTAL
# 2 2000 ALABAMA       AL     AUTAUGA        1001 PRESIDENT GEORGE W. BUSH REPUBLICAN          11993      17208 20191203 TOTAL
# 3 2000 ALABAMA       AL     AUTAUGA        1001 PRESIDENT    RALPH NADER      GREEN            160      17208 20191203 TOTAL
# 4 2000 ALABAMA       AL     AUTAUGA        1001 PRESIDENT          OTHER      OTHER            113      17208 20191203 TOTAL
# 5 2000 ALABAMA       AL     BALDWIN        1003 PRESIDENT        AL GORE   DEMOCRAT          13997      56480 20191203 TOTAL
# 6 2000 ALABAMA       AL     BALDWIN        1003 PRESIDENT GEORGE W. BUSH REPUBLICAN          40872      56480 20191203 TOTAL

states <- c("colorado","wyoming","utah","new-mexico","nebraska")
states <- c("CO", "WY", "UT", "NM", "NE")
countypres %>%
  filter(state_po %in% states  & year == 2020 ) %>%
  write.csv('CO_WY_UT_NM_NE_countypres_2020.csv')

countypres %>%
  filter(state_po %in% states & year %in% c(2016, 2020) & party %in% c("DEMOCRAT", "REPUBLICAN") ) %>%
  write.csv('CO_WY_UT_NM_NE_countypres_2016_2020.csv')

D_VOTES <- countypres %>%
  filter(state_po %in% states & year %in% c(2016, 2020) & party %in% c("DEMOCRAT", "REPUBLICAN") & mode == "TOTAL" ) %>%
  select(year, state_po, county_name, party, candidatevotes)  %>%
  spread(key = year, value = candidatevotes) %>%
  mutate(d_votes = `2020` - `2016`) %>%
  rename("County"="county_name", "State"="state_po") %>%
  mutate(County = toTitleCase(tolower(County))) %>%
  select(State, County, party, d_votes) %>%
  spread(key = party, value = d_votes)

head(D_VOTES)
dim(D_VOTES)
save(D_VOTES, file="D_VOTES.RData")
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Part2:
load("D_VOTES.RData")
load("CENSUS.2.RData")
CENSUS.2 <- CENSUS.2 %>%
  mutate(County = toTitleCase(tolower(County))) %>%
  mutate(State = case_when(State == "Colorado" ~ "CO",
                           State == "Nebraska" ~ "NE",
                           State == "New Mexico" ~ "NM",
                           State == "Utah" ~ "UT",
                           State == "Wyoming" ~ "WY"))

D_VOTES$County[which(D_VOTES$County == "Dona Ana")] <- "Doña Ana"

D_VOTES <- D_VOTES %>%
  arrange(State, County)

CENSUS.2 <- CENSUS.2 %>%
  arrange(State, County)

D_VOTES$County == CENSUS.2$County
all(D_VOTES$County == CENSUS.2$County)

# merge D_VOTES and CENSUS.2
core <- merge(CENSUS.2, D_VOTES, 
              by.x=c("State","County"),
              by.y=c("State","County"),
              all=TRUE)

core$area <- st_area(core) #Command for maps in ggplot later

# acs with state border
library(tidycensus)
census_api_key("2f358eb210d5db4d9b725eafda61e2f76d75e99c", overwrite = TRUE, install = TRUE)
options(tigris_use_cache =TRUE)
readRenviron("~/.Renviron")
Sys.getenv("2f358eb210d5db4d9b725eafda61e2f76d75e99c")
vars<-c("B01001_001",
        "B01001_002",
        "B02001_001",
        "B02001_002", 
        "B02001_003", 
        "B05001_001", 
        "B05001_006", 
        "B07001_001",
        "B07001_017",
        "B07001_033",
        "B07001_049", 
        "B07001_065", 
        "B07001_081")
my_states <- c("CO", "WY", "UT", "NM", "NE")
for (state in my_states) {
  acs <- get_acs(geography = "county", 
                 geometry = TRUE, 
                 variables = vars, 
                 state = state, 
                 year = 2016)
  acs <- acs %>%
    mutate(variable2 = case_when(variable == "B01001_001" ~ "TotPop",
                                 variable == "B01001_002" ~ "Male",
                                 variable == "B02001_001" ~ "TotRace",
                                 variable == "B02001_002" ~ "White",
                                 variable == "B02001_003" ~ "Black",
                                 variable == "B05001_001" ~ "TotCit",
                                 variable == "B05001_006" ~ "NonCit",
                                 variable == "B07001_001" ~ "TotMob",
                                 variable == "B07001_017" ~ "Stay",
                                 variable == "B07001_033" ~ "SameCounty",
                                 variable == "B07001_049" ~ "SameSt",
                                 variable == "B07001_065" ~ "OthState",
                                 variable == "B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe, variable)) %>%
    spread(key = variable2, value = estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           PerCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID", "NAME", starts_with("per"), "geometry") %>%
    mutate(state = state)
  
  assign(paste0(state, "census"), acs)
  
  acs$area <- st_area(acs)
  map <- acs %>%
    summarise(area = sum(area)) %>%
    mutate(state = state)
  assign(paste0(state, "map"), map)
  rm(acs, map)
}

COcensus
WYcensus
UTcensus
NMcensus
NEcensus
COmap
WYmap
UTmap
NMmap
NEmap

census <- rbind(COcensus,
                WYcensus,
                UTcensus,
                NMcensus,
                NEcensus)

states <- rbind(COmap,
                WYmap,
                UTmap,
                NMmap,
                NEmap)

census$NAME <- as.data.frame(str_split_fixed(census$NAME, ",", 2))[,1]
census$NAME <- trimws(gsub(" County","",census$NAME))
census <- census %>%
  rename("County" = "NAME") %>%
  mutate(County = tolower(County))
save(census, file = "census.RData")

# Map with Republican candidate
rmap <- ggplot(core) +
  geom_sf(aes(fill = REPUBLICAN)) +
  scale_fill_gradient(low="yellow",
                      high="blue",
                      aes(name="Change in Republican votes")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(
    data = states,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE
  )
ggsave(rmap, file="rmap.eps", device="eps")

# Map with Democrat candidate
dmap <- ggplot(core) +
  geom_sf(aes(fill = DEMOCRAT)) +
  scale_fill_gradient(low="red",
                      high="blue",
                      aes(name="Change in Democrat votes")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(
    data = states,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE
  )
ggsave(dmap, file="dmap.eps", device="eps")

plot_grid(rmap, dmap)
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# PART3:
load("CENSUS.3.RData")
CENSUS.3 <- CENSUS.3 %>%
  mutate(County = toTitleCase(tolower(County)))

census <- census %>%
  rename("State"="state") %>%
  arrange(State, County) %>%
  mutate(County = toTitleCase(tolower(County)))

CENSUS.3 <- CENSUS.3 %>%
  arrange(State, County) 

core <- core %>%
  arrange(State, County)

all(core$County == CENSUS.3$County)
nrow(core) == nrow(CENSUS.3)
# merge CENSUS.3 and census
geometrty.CENSUS.3 <- CENSUS.3$geometry
geometry.core <- core$geometry
CENSUS.3$geometry = NULL
# core$geometry = NULL
core.2 <- merge(CENSUS.3, core, 
                by = c("State", "County"), 
                all=TRUE)

head(core.2)
dim(core.2)

mod1 <- lm(REPUBLICAN ~ perMale + perWhite, data=core.2)
mod2 <- lm(DEMOCRAT ~ perMale + perWhite, data=core.2)
mod3 <- lm(REPUBLICAN ~ perMaleChange + perWhiteChange, data=core.2)
mod4 <- lm(DEMOCRAT ~ perMaleChange + perWhiteChange, data=core.2)
mod5 <- lm(REPUBLICAN ~ perMaleChange + perWhiteChange + factor(State), data=core.2)
mod6 <- lm(DEMOCRAT ~ perMaleChange + perWhiteChange + factor(State), data=core.2)

core.2<-core.2 %>%
  select(-c(area))
core.2$geometry<-NULL
stargazer(core.2, type="html", out="SumStat.html")
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type="html", out="regress.html")

