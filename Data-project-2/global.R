
library(tidyverse)
library(shinythemes)
library(sf)
library(here)
library(leaflet)
library(stringr)
library(lubridate)
library(shinyWidgets)

ai_ga_data_all <- read_csv("acme_synth_data.csv")

ai_ga_data_all <- ai_ga_data_all %>% 
  mutate(yearMonth = ymd(yearMonth, truncated = 1))


ai_ga_data_scotland <- ai_ga_data_all %>% 
  filter(catchment != "not Scotland")



colnames(ai_ga_data_all)
                              

# Chris' code!   

# Load in spatial data
scotland <- st_read("raw_data/Scotland_laulevel1_2011/scotland_laulevel1_2011.shp")

# Assign city to catchment
scotland_catchment <- scotland %>% 
  mutate(catchment = recode(name,
                            "Dumfries and Galloway" = "Glasgow",
                            "Lochaber" = "Inverness",
                            "Ross and Cromarty" = "Inverness",
                            "Eilean Siar (Western Isles)" = "Inverness",
                            "Scottish Borders" = "Edinburgh",
                            "West Lothian" = "Edinburgh",
                            "Fife" = "Edinburgh",
                            "Argyll and Islands" = "Glasgow",
                            "Argyll and Bute Islands" = "Glasgow",
                            "East Lothian" = "Edinburgh",
                            "Skye and Lochalsh" = "Inverness",
                            "Midlothian" = "Edinburgh",
                            "West Dunbartonshire" = "Glasgow",
                            "Glasgow City" = "Glasgow",
                            "Perth and Kinross" = "Edinburgh",
                            "North East Moray" = "Inverness",
                            "South Lanarkshire" = "Glasgow",
                            "Badenoch and Strathspey" = "Inverness",
                            "Aberdeen City" = "Inverness",
                            "Orkney Islands" = "Inverness",
                            "Aberdeenshire" = "Inverness",
                            "Stirling" = "Glasgow",
                            "East Ayrshire" = "Glasgow",
                            "North Ayrshire mainland" = "Glasgow",
                            "North Lanarkshire" = "Glasgow",
                            "West Moray" = "Inverness",
                            "Dundee City" = "Edinburgh",
                            "Inverclyde" = "Glasgow",
                            "Inverness and Nairn" = "Inverness",
                            "Angus" = "Edinburgh",
                            "Shetland Islands" = "Inverness",
                            "Caithness and Sutherland" = "Inverness",
                            "South Ayrshire" = "Glasgow",
                            "Renfrewshire" = "Glasgow",
                            "Arran and Cumbrae" = "Glasgow",
                            "Clackmannanshire" = "Edinburgh",
                            "East Renfrewshire" = "Glasgow",
                            "Falkirk" = "Glasgow",
                            "Helensburgh and Lomond" = "Glasgow",
                            "Edinburgh, City of" = "Edinburgh",
                            "East Dunbartonshire" = "Glasgow"))

# Group catchments and simplify geometry.
scotland_simple <- scotland_catchment %>%
  st_simplify(dTolerance = 2000) %>%
  group_by(catchment) %>%
  summarise(geometry = st_union(geometry))

scotland_gcs <- st_transform(scotland_simple, "+proj=longlat +datum=WGS84")
st_crs(scotland_gcs)

unique(ai_ga_data_scotland$sourceMedium)

ai_source_regrouped  <- ai_ga_data_scotland %>% 
  mutate(ai_source = case_when(
    str_detect(sourceMedium, "facebook|linkedin|twitter|t.co|instagram|youtube|lnkd.in|Tiktok") == TRUE ~ "Social Media",
                              str_detect(sourceMedium, "cpc|Admedo|programmatic" ) == TRUE  ~ "Paid Digital Marketing",
                              str_detect(sourceMedium, "organic|search|duckduck|ecosia|bing") == TRUE  ~ "Organic Search",
                              str_detect(sourceMedium, "newsletter|partners|subscribe|Blog|Website Subscriber|cme") == TRUE  ~ "Acme Comms",
                              str_detect(sourceMedium, "cotsman|thedailyrecord|thetimes|dailymail") == TRUE  ~ "Newspapers",
                              str_detect(sourceMedium, "Highland|Government|widgets|supplier|Retail News") ~ "Industry Links",
                              str_detect(sourceMedium, "direct") == TRUE ~ "Direct",
                              TRUE ~ "Other Source")) %>% 
  mutate(specific_source = case_when(
    str_detect(sourceMedium, "facebook") == TRUE ~ "Facebook",
    str_detect(sourceMedium, "twitter|t.co" ) == TRUE  ~ "Twitter",
    str_detect(sourceMedium, "linkedin|lnkd.in") == TRUE  ~ "Linked In",
    str_detect(sourceMedium, "instagram") == TRUE  ~ "Instagram",
    str_detect(sourceMedium, "youtube.com") == TRUE  ~ "You Tube",
    TRUE ~ sourceMedium)) %>% 
    mutate(catchment = ifelse(catchment == "Scotland unknown", "Scotland Unclassified", catchment))



ai_source_regroup <-  ai_source_regrouped %>% 
  group_by(catchment, yearMonth, ai_source) %>% 
  summarise(count = sum(sessions))


 

ai_source_completions <- ai_source_regrouped %>% 
    mutate(completions_all = goal2Completions + goal9Completions + goal11Completions)


# Tab3 Code - Kerr -  Finish -------------------------------------------------------------------------

# Tab4 Code - JO -------------------------------------------------------------------------------------

goals <- ai_ga_data_all %>%
    select(yearMonth, goal2Completions, goal9Completions, goal11Completions, catchment) %>%
    rename(Catchment = catchment) %>%
    filter(Catchment != "not Scotland") %>%
    mutate(yearMonth = ymd(yearMonth, truncated = 1)) %>%
    pivot_longer(cols = starts_with("goal"), names_to = "Goal", names_patter = "(\\d+)", values_to = "Completions") %>%
    mutate(Goal = factor(Goal, levels = c("2", "9", "11"))) %>%
    group_by(yearMonth, Catchment, Goal) %>%
    summarise(Completions = sum(Completions))
  
# Tab4 Code - Jo - Finish ----------------------------------------------------------------------------

