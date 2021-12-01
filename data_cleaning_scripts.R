setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(magrittr)
library(lubridate)
library(readxl)
library(kableExtra)
library(knitr)
library(readr)
library(openxlsx)
library(Hmisc)
library(htmlwidgets)
library(stringr)
library(naniar)
library(tidyverse)
library(data.table)

source("functions/audit_function_full.R")
source("functions/function_handler.R")

assessment_start_date <- as.Date("2021-11-17")

# set min time and max # interviews per day

time_limit <- 10
flag_time_limit <- 20
max_interv <- 10
# Reading data from excel file#########################################################################################################

# df <- read_excel("input/raw_data/OPT2101_MSNA_June2021_2021-08-16.xlsx", sheet = "OPT2101_MSNA_June2021", col_types = "text")
df <- read_excel("input/raw_data/Evaluation_Gaza_MPCA_Nov_translated.xlsx")
# Checking duplicates #################################################################################################################
df_duplicates <- df %>% 
  group_by(`_uuid`) %>% 
  dplyr::filter(n()>1)

# Exporting the duplicates for checking ##############################################################################################
write.xlsx(df_duplicates, (file = sprintf("output/duplicates/df_duplicates_%s.xlsx", today())))


# Keeping distinct records after confriming the surveys were true duplicates.#########################################################
df <- df %>% distinct(`_uuid`, .keep_all = TRUE)

#Outlier checking ###################################################################################################################
cleaning_log <- cleaninginspectoR::find_outliers(df)


# Time check from audit files########################################################################################################

df$today <- as.Date(df$start, "%Y-%m-%d")

df <- time_check_audit(audit_dir_path = "audit/", df,  "_uuid", time_limit = time_limit)


df <- df %>% rename(audit_duration = interview_duration)

df <- df %>% 
  filter(date_assessment >= as.Date("2021-11-17") & consent == "yes")



# When survey does not continue to hh member calculation, these surveys are not valid #################################################

df <- df %>% 
  mutate(not_eligible = case_when(is.na(hh_size) ~ "yes",TRUE ~ "no"),
         X_uuid = `_uuid`)


### Deleted interviews column###########################################################################################################
df <- df %>% 
  mutate(
    deleted = case_when(
      time_validity == "Deleted" | not_eligible == "yes" ~ "yes",
      TRUE ~ "no"))


# Number of NAs check##################################################################################################################
df$NAs <- apply(df,1,FUN=function(x){length(which(is.na(x)))})

# EXPORT FOR DATA CHECKING ##########################################################################################################
write.csv(df, sprintf("output/data_checking/mpca_all_data_%s.csv",today()), row.names = F)


# DO CLEANING ######################################################################################################################
# read cleanimg conditions csv list ################################################################################################
conditionDf <- read.xlsx("input/conditions/conditions_log.xlsx")

# return logs
logs <- read_conditions_from_excel_limited_row(df, conditionDf);

# create new columns "log_number"
logs$log_number = seq.int(nrow(logs))
# order data frame by log_number
ordered_df <- logs[order(logs$log_number),]
###ordered_df$contact_number <- as.factor(ordered_df$contact_number)

readr::write_excel_csv(ordered_df, sprintf("output/cleaning_log/cleaning_log_%s.csv",today()))
write.xlsx(ordered_df, (file = sprintf("output/cleaning_log/cleaning_log_%s.xlsx", today())))

# export data with check columns
logs_as_columns <- read_conditions_from_excel_column(df, conditionDf);
write.csv(logs_as_columns, sprintf("output/cleaning_log/data_w_checks/data_checks_%s.csv",today()), row.names = FALSE)

######################################################################################################
# rename columns - change "/" to "."
rename_vars <- function(df){
  names(df) <- c(gsub("/",".",names(df)))
  
  return(df)
  
}

df <- rename_vars(df)



########################################################################################################################################
# Read log csv file after decision on flagged data #####################################################################################

log_df <- read.csv(sprintf("input/filled_cleaning_log/cleaning_log_%s.csv",today()), as.is = TRUE)
replaced_df <- read_logs(df, log_df, conditionDf) 

#######################################################################################################################################

# take uuids of deleted surveys and remove from cleaned dataset
deleted_surveys <- replaced_df %>% 
  filter(deleted == "yes")

# remove deleted surveys from cleaned dataset
replaced_df %<>% filter(deleted == "no")


#DELETE COLUMNS
replaced_df[, c(
  "consent",
  "deviceid",
  "end",
  "start",
  "date",
  "audit",
  "audit_URL",
  "date_assessment",
  "enumerator_num",
  "_id",
  "_submission_time",
  "_validation_status",
  "_status",
  "_submitted_by",
  "_tags",
  "_index",
  "_notes",
  "today",
  "start_end",
  "duration_minutes",
  "audit_duration",
  "time_validity",
  "not_eligible",
  "will_to_response",
  "NAs",
  "mpca_assistance_gpc_no",
  "mpca_assistance_wfp_no",
  "household_size_2",
  "calc_note",
  "ability_to_meet_needs",
  "behaviors_mpca_period",
  "exp_note",
  "lcsi_note",
  "change_behaviors_mpca_note",
  "thanks",
  "end_submit",
  "contact_name",
  "contact_number",
  "_uuid"
  
)] <- list(NULL)

#Deleting columns those questions were in the pilot tool but were removed in the final tool
replaced_df[, c(
  "education_exp",
  "durable_goods",
  "furniture_exp",
  "vehicles_exp",
  "asset_exp",
  "outside_exp",
  "decision_making",
  "decision_making_other",
  "barriers_meeting_needs",
  "barriers_meeting_needs.insufficient_money",
  "barriers_meeting_needs.distance",
  "barriers_meeting_needs.security_situation",
  "barriers_meeting_needs.social_discrimination",
  "barriers_meeting_needs.insufficient_goods",
  "barriers_meeting_needs.poor_quality",
  "barriers_meeting_needs.other",
  "barriers_meeting_needs.don_t_know",
  "barriers_meeting_needs.prefer_not_to_answer",
  "barriers_meeting_needs_other",
  "behaviors_result_mpca",
  "card_challenges",
  "card_challenges_which",
  "card_challenges_which.did_not_receive_notifications",
  "card_challenges_which.card_did_not_work",
  "card_challenges_which.atms_were_broken",
  "card_challenges_which.atms_were_far",
  "card_challenges_which.atm_points_unsafe",
  "card_challenges_which.do_not_know_atm_use",
  "card_challenges_which.experience_harassment",
  "card_challenges_which.other",
  "card_challenges_which.don_t_know",
  "card_challenges_which.prefer_not_to_answer",
  "card_challenges_other",
  "ability_to_access_services",
  "access_education",
  "access_health",
  "access_water",
  "access_electricity",
  "access_legal",
  "violence_within_household",
  "mental_health",
  "relatives_less_assistance"
  
)] <- list(NULL)


replaced_df$deleted <- NULL

###########################################################################################################
#Replacing 999 with NAs ##########################################################################
replaced_df <- replaced_df %>% replace_with_na_all(condition = ~.x == 999)

###########Relocating variable X_uuid

replaced_df <- replaced_df %>% relocate( X_uuid, .before = refugee_status) %>% 
               arrange(desc(refugee_status))

###########################################################################################################
# export clean data
write.csv(replaced_df, sprintf("output/cleaned_data/mpca_data_clean_parent_%s.csv",today()), row.names = FALSE)

# export deletion log
mpca_deleted <- deleted_surveys

write.xlsx(mpca_deleted, (file = sprintf("output/deletion_log/mpca_data_deleted_%s.xlsx", today())))

# select columns for deletion log
deleted_redacted <- deleted_surveys %>%
  dplyr::select(
    X_uuid,
    date_assessment,
    enumerator_num,
    duration_minutes,
    time_validity,
    deleted
  )



# export to one spreadsheet
mpca_datasets <-
  list(
    "MCPA_2021_OPT" = replaced_df,
    "cleaning_log_hh" = log_df,
    "deletion_log" = deleted_redacted
  )

write.xlsx(mpca_datasets, (file = sprintf("output/cleaned_data/mpca_data_clean_all_%s.xlsx", today())))
