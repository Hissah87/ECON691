# This script for HW2
#Created by Hissah Altamimi.

rm(list=ls())

# PART1:
library(rvest)
library(tidyverse)
library(stringr)
library(cowplot)

states<-c("colorado","wyoming","utah","new-mexico","nebraska")
for(state in states){
  nyt <- "https://www.nytimes.com/elections/2016/results/"
  url <- paste0(nyt, state)
  webpage <- read_html(url)
  tables <- webpage %>%
    html_nodes("table")
  results <- tables[2] %>%
    html_table(header = TRUE) %>%
    as.data.frame() %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",", "", Clinton)),
           "Trump" = as.numeric(gsub(",", "", Trump)),
           "pctClinton" = (Clinton)/(Clinton + Trump),
           "pctTrump" = (Trump)/(Clinton + Trump),
           "State" = state)
  assign(state, results)
}
head(colorado)
head(wyoming)
head(utah)
head(`new-mexico`)
head(nebraska)
colorado$State <- "CO"
wyoming$State <- "WY"
utah$State <- "UT"
`new-mexico`$State <- "NM"
nebraska$State <- "NE"
VOTES <- rbind(colorado, wyoming, utah, `new-mexico`, nebraska)
save(VOTES, file="VOTES.RData")
write.csv(VOTES, file="VOTES.csv")

# PART2:
# CENSUS.1
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
acs <- get_acs(geography = "county", 
               geometry = TRUE, 
               variables = vars, 
               state = my_states, 
               year = 2016)

head(acs)

library(magrittr)
library(dplyr)

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
                               TRUE ~ "other"))
acs <- acs %>%
  select(!c(moe, variable))

acs <- acs %>%
  spread(key = variable2, value = estimate)

acs <- acs %>%
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         PerCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID", "NAME", starts_with("per"), "geometry")

head(acs)

states <- c("Colorado", "Wyoming", "Utah", "New Mexico", "Nebraska")
for (state in states) {
  acs$County <- trimws(gsub(" County, .+", "", acs$NAME))
  acs$State <- trimws(gsub(".+ County, ", "", acs$NAME))
}
names(acs)
CENSUS.1 <- acs %>%
  select(!c("GEOID", "NAME"))

save(acs, file = "CENSUS.1.RData")
write.csv(acs, file = "CENSUS.1.csv")


# CENSUS.2
acs <- get_acs(geography = "county", 
               geometry = TRUE, 
               variables = vars, 
               state = my_states, 
               year = 2019)

head(acs)

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
                               TRUE ~ "other"))
acs <- acs %>%
  select(!c(moe, variable))

acs <- acs %>%
  spread(key = variable2, value = estimate)

acs <- acs %>%
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         PerCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID", "NAME", starts_with("per"), "geometry")

head(acs)

states <- c("Colorado", "Wyoming", "Utah", "New Mexico", "Nebraska")
for (state in states) {
  acs$County <- trimws(gsub(" County, .+", "", acs$NAME))
  acs$State <- trimws(gsub(".+ County, ", "", acs$NAME))
}
names(acs)
CENSUS.2 <- acs %>%
  select(!c("GEOID", "NAME"))

save(CENSUS.2, file = "CENSUS.2.RData")

# CENSUS.3
dim(CENSUS.1) == dim(CENSUS.2)
all(CENSUS.1$County == CENSUS.2$County)
CENSUS.1.2016 <- CENSUS.1 %>%
  mutate(year = 2016)
CENSUS.2.2019 <- CENSUS.2 %>%
  mutate(year = 2019)
head(CENSUS.1.2016)
head(CENSUS.2.2019)

CENSUS.1.2016$geometry = NULL
CENSUS.2.2019$geometry = NULL
CENSUS.1.2016 <- CENSUS.1.2016 %>%
  select(starts_with("per"))
CENSUS.2.2019 <- CENSUS.2.2019 %>%
  select(starts_with("per"))

all(names(CENSUS.1.2016) == names(CENSUS.2.2019))

CENSUS.3 <- CENSUS.1 %>%
  select("County", "State")

CENSUS.3 <- cbind(CENSUS.3, CENSUS.2.2019 - CENSUS.1.2016)
head(CENSUS.3)
tail(CENSUS.3)
dim(CENSUS.3)
class(CENSUS.3)

# PART3:
table(CENSUS.3$State)
CENSUS.3 <- CENSUS.3 %>% 
  mutate(State = case_when(State == "Colorado" ~ "CO",
                           State == "Nebraska" ~ "NE",
                           State == "New Mexico" ~ "NM",
                           State == "Utah" ~ "UT",
                           State == "Wyoming" ~ "WY")
  )

CENSUS.3 <- CENSUS.3 %>%
  rename_with(~ str_c(., 'Change'), starts_with('per'))

head(CENSUS.3)
tail(CENSUS.3)

VOTES.State.County <- arrange(VOTES, State, County) %>%  select(County, State)
CENSUS3.State.County <- arrange(CENSUS.3, State, County) %>% select(County, State)

CENSUS3.State.County[which(CENSUS3.State.County$County != VOTES.State.County$County),]
#       County State                       geometry
# 164  De Baca    NM MULTIPOLYGON (((-104.8924 3...
# 165 Doña Ana    NM MULTIPOLYGON (((-107.2996 3...
VOTES.State.County[which(CENSUS3.State.County$County != VOTES.State.County$County),]
#       County State
# 164   DeBaca    NM
# 165 Dona Ana    NM

CENSUS.3 <- CENSUS.3 %>%
  arrange(State, County) %>%
  mutate(County = case_when(County == "De Baca" ~ "De Baca",
                            County == "Doña Ana" ~ "Doña Ana",
                            County == "Dona Ana" ~ "Doña Ana",
                            TRUE ~ County)
  )

CENSUS.3 %>% 
  filter(State=="NM") %>%
  select(County) %>%
  as.data.frame()

save(CENSUS.3, file="CENSUS.3.RData")


VOTES <- VOTES %>%
  arrange(State, County)  %>%
  mutate(County = case_when(County == "De Baca" ~ "De Baca",
                            County == "Doña Ana" ~ "Doña Ana",
                            County == "Dona Ana" ~ "Doña Ana",
                            TRUE ~ County)
  )

VOTES %>% 
  filter(State=="NM") %>%
  select(County) %>%
  as.data.frame()

acs <- merge(CENSUS.3, VOTES, by=c("State", "County"), all=TRUE)
class(acs)

map1 <- ggplot(acs) +
  geom_sf(aes(fill = pctClinton)) +
  scale_fill_gradient(low="red",
                      high="blue",
                      aes(name="Vote percentage in Clinton")) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

map2 <- ggplot(acs) +
  geom_sf(aes(fill = perWhite)) + 
  scale_fill_gradient(low="yellow",
                      high="blue",
                      aes(name="% change in white population")) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


plot_grid(map1, map2)

