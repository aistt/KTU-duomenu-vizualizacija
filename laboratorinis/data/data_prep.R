library(tidyverse)
library(jsonlite)

download.file("https://atvira.sodra.lt/imones/downloads/2022/monthly-2022.csv.zip", "temp")
unzip("temp")
raw <- read.delim("monthly-2022.csv", sep = ";")
names(raw) <- c("code", "jarCode", "name", "municipality", "ecoActCode", "ecoActName",
                "month", "avgWage", "numInsured", "avgWage2", "numInsured2", "tax")

codes <- raw %>%
  group_by(ecoActCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  head(22)

raw %>%
  filter(ecoActCode %in% codes$ecoActCode) %>%
  write_csv("lab_sodra.csv")

raw_json <- head(raw) %>%
  toJSON()

write(raw_json, "lab_sodra.json")


