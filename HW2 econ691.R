
#This is a script is HW2.
#created by Hissah Altamimi on Sep, 13, 2021.


rm(list = ls())
library(rvest)
library(tidyverse)
url<-"https://www.nytimes.com/elections/2016/results/colorado"
webpage<-read_html(url)
tables<-webpage %>%
  html_nodes("table")
results<-as.data.frame(html_table(tables[2],fill=TRUE,header=TRUE))
results2<-tables[2] %>%
  html_table(fill=TRUE,header=TRUE)%>%
  as.data.frame()
Colorado<-results2 %>%
  rename("County"="Vote.by.county") %>%
  mutate("Clinton" = as.numeric(gsub(",", "", Clinton)),
         "Trump" = as.numeric(gsub(",", "", Trump)),
         "pctClinton" = (Clinton)/(Clinton + Trump),
         "pctTrump" = (Trump)/(Clinton + Trump))
head(Colorado)

rm(list = ls())
library(rvest)
library(tidyverse)
states<-c("colorado","wyoming","utah","new-mexico","nebraska")
for(i in states){
  url.1<-"https://www.nytimes.com/elections/2016/results/"
  url<-paste0(url.1,i)
  webpage<-read_html(url)
  tables<-webpage %>%
    html_nodes("table")
  results<-as.data.frame(html_table(tables[2], fill = TRUE, header = TRUE))
  results2<-tables[2] %>%
    html_table(fill = TRUE, header = TRUE) %>%
    as.data.frame() %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",", "", Clinton)),
           "Trump" = as.numeric(gsub(",", "", Trump)),
           "pctClinton" = (Clinton)/(Clinton + Trump),
           "pctTrump" = (Trump)/(Clinton + Trump))
  assign(i, results2)
  head(results2)
  save(results2,file="VOTES.RData")
  write.csv(results2,file="VOTES.csv")
  
}

library(tidycensus)
census_api_key("2f358eb210d5db4d9b725eafda61e2f76d75e99c", overwrite = TRUE, install = TRUE)
options(tigris_use_cache =TRUE)
readRenviron("~/.Renviron")
Sys.getenv("2f358eb210d5db4d9b725eafda61e2f76d75e99c")
vars<-c("B01001_001", "B01001_002","B02001_001","B02001_002", "B02001_003", "B05001_001", "B05001_006", "B07001_001",
          "B07001_017","B07001_033","B07001_049", "B07001_065", "B07001_081")
my_states <- c("CO", "WY", "UT", "NM", "NE")
acs <- get_acs(geography = "county", geometry = TRUE, variables = vars, state = my_states, year = 2019)
library(magrittr)
library(dplyr)
library(tidyverse)
il1.acs <- acs %>%
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
  spread(key = variable2, value = estimate)
il3.acs <- il1.acs %>%
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         PerCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID", starts_with("per"), "geometry")
nrow(il3.acs)
head(il3.acs)
save(il3.acs, file = "CENSUS.1.RData")
write.csv(il3.acs, file = "CENSUS.1.csv")

library(tidyverse)
library(sf)
library(ggplot2)
ggplot(il3.acs) +
  geom_sf(aes(fill = perMale))


library(tidycensus)
census_api_key("2f358eb210d5db4d9b725eafda61e2f76d75e99c", overwrite = TRUE, install = TRUE)
options(tigris_use_cache = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("2f358eb210d5db4d9b725eafda61e2f76d75e99c")
vars <- c("B01001_001", "B01001_002", "B02001_001", "B02001_002", "B02001_003", "B05001_001", "B05001_006", "B07001_001",
          "B07001_017", "B07001_033", "B07001_049", "B07001_065", "B07001_081")
my_states <- c("CO", "WY", "UT", "NM", "NE")
acs <- get_acs(geography = "county", geometry = TRUE, variables = vars, state = my_states, year = 2019, survey = "acs1")
library(magrittr)
library(dplyr)
library(tidyverse)
il1.acs <- acs %>%
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
  spread(key = variable2, value = estimate)
il2.acs <- il1.acs %>%
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         PerCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID", starts_with("per"), "geometry")
nrow(il2.acs)
head(il2.acs)
save(il2.acs, file = "CENSUS.2.RData")
write.csv(il2.acs, file = "CENSUS.2.csv")

library(cowplot)
p1<-ggplot(results2)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,1), aes(name = "Percent
  Clinton")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

p2<-ggplot(il6.acs)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low = "black", high = "white", limits = c(0,1), aes(name = "Percent
  White")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1,p2)

