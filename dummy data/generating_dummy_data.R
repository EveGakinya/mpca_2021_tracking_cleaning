library(magrittr)
library(dplyr)
library(kableExtra)
library(readxl)
library(sjmisc)
library(purrr)
library(tidyr)

questions <- read_excel("input/questionnaire/MPCA_tool_V4_161121_Translated.xlsx",sheet = "survey")
choices <- read_excel("input/questionnaire/MPCA_tool_V4_161121_Translated.xlsx",sheet = "choices")

dummy_dataset <- xlsformfill::xlsform_fill(questions = questions, choices = choices, n = 300)

library(lubridate)
library(openxlsx)
