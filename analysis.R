#load necessary library
library(dplyr)

#read data
ncaa <- readxl::read_xlsx("data_clean.xlsx")

#investigate structure 
str(ncaa)

#convert spread and result columns to number format
ncaa$Spread <- as.numeric(trimws(gsub(" \\(.*\\)", "", ncaa$Spread)))
str(ncaa)
any(is.na(ncaa$Spread))
ncaa$Result <- as.numeric(ncaa$Result)
str(ncaa)

#convert spreads to expected score differentials, eliminating need for spreads
ncaa$Expected_Result <- ncaa$Spread * -1
str(ncaa)
ncaa <- ncaa %>%
  select(-Spread)
str(ncaa)

#calculate if team covered spread
ncaa <- ncaa %>%
  mutate(Cover = case_when(
    Result > Expected_Result ~ 1,
    Result < Expected_Result ~ 0,
    TRUE ~ 0
  ))
head(ncaa)
str(ncaa)

#find seed with highest percentage of spreads covered
summary_table <- ncaa %>%
  group_by(Seed) %>%
  summarize(cover_rate = mean(Cover), avg_exp_diff = mean(Expected_Result), avg_result = mean(Result))
summary_table
