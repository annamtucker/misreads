# clean up bandedbirds data

library(tidyverse)
library(lubridate)

resights = read_csv("data/resights_DE.csv")
banding = read_csv("data/banding_DE.csv")

rdat <- resights %>% 
  select(`Resight Date`, Location, Species, `Flag Color`, `Flag Code`,
         `Observer First`, `Observer Last`) %>% 
  rename(date = `Resight Date`, loc = `Location`, species = `Species`,
         col = `Flag Color`, code = `Flag Code`) %>% 
  mutate(code = substr(code, 3, nchar(code)-1),
         date = parse_date(date, format = "%m/%d/%Y"),
         year = year(date),
         day = ifelse(month(date) == 5, day(date), day(date)+31),
         flag = paste(col, code),
         obs = ifelse(!str_detect(`Observer First`, "\\s") & 
                            !str_detect(`Observer Last`, "\\s"), 
                      paste(`Observer First`, `Observer Last`), NA)) %>%
  filter(!is.na(col)) %>% 
  mutate(type = "resight")

bdat <- banding %>% 
  select(`BirdID`, `Capture Date`, Location, Species, `Flag Color In`, `Flag Code In`,
         `Metal In`, `Metal Out`, `Weight`, Disp) %>% 
  rename(date = `Capture Date`, loc = `Location`, species = `Species`,
         col = `Flag Color In`, code = `Flag Code In`) %>% 
  mutate(code = substr(code, 3, nchar(code)-1),
         date = parse_date_time(date, orders = "%m/%d/%Y %I:%M:%S",
                                truncated =3),
         year = year(date),
         day = ifelse(month(date) == 5, day(date), day(date)+31),
         flag = paste(col, code)) %>% 
  mutate(type = "capture")

write.csv("rdat", file = "output/resighting_clean.csv", row.names = F)
write.csv("bdat", file = "output/banding_clean.csv", row.names = F)



