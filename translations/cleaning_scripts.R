library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)
library(udpipe)

to_clean <- read_excel("output/cleaning_log/cleaning_log_2021-11-29.xlsx") %>% 
              mutate(unique_id = paste(question.name,X_uuid, sep = "_"))

cleaned <-read_excel("output/cleaning_log/cleaning_log_2021-11-29_1.xlsx") %>% 
            mutate(unique_id = paste(question.name, X_uuid, sep = "_"))


clean <- to_clean %>% 
            mutate(cleaned = case_when(unique_id %in% cleaned$unique_id ~ "yes", TRUE ~ "no"))

clean <- clean %>% 
                filter(cleaned == "no")

write.xlsx(clean, (file = sprintf("translations/cleaning_log_%s.xlsx", today())))
