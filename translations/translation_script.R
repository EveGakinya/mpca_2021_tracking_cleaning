library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)
library(udpipe)
df_trans <- read_excel("translations/translation_7.xlsx")%>%
  mutate(unique_id = paste(question, uuid, sep = "_"))
to_df <- read_excel("translations/translation_8.xlsx")%>%
  mutate(unique_id = paste(question, uuid, sep = "_"))
translation <- to_df %>%
  mutate(translated = case_when (unique_id %in% df_trans$unique_id ~ "yes", TRUE ~ "no"))
table(translation$translated)
translation_df <- translation %>%
  filter(translated == "no") %>%
  dplyr::select(question, orginal_value, translated_value, uuid)
write.xlsx(translation_df, (file = sprintf("translations/translation_df_%s.xlsx", today())))


