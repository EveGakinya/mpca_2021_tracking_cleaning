
compare_columns_valuesLimit_col <- function(
  df, cn, condition, question_label, issue_label,  issue_arabic, columns, old_value, action_label, index) {

  varname <- paste("", cn , sep="")
  data <- df %>%  
    mutate(issue = issue_label,
           issue.arabic = issue_arabic,
           flag_index = index,
           log_number = 0,
           question.name = question_label,
           changed = NA,
           old.value =case_when(
             eval(
               parse(text = condition)
             ) ~ as.character(eval(parse(text = old_value))),
             TRUE ~ "Okay"),
           new.value = NA,
           !!varname := case_when(
             eval(
               parse(text = condition)
             ) ~ action_label,
             TRUE ~ "Okay"
           ))
  
  filterDate <- data %>%
    dplyr::select(columns)  %>% 
    filter(
      eval(
        parse(text =  paste("data$", cn , sep="")
        )) == 'Flagged' | 
        eval(
          parse(text =  paste("data$", cn , sep="")
          )) == 'Deleted')
  
  return(filterDate)
}

compare_columns_values <- function(df, cn, condition) {
  varname <- paste("", cn , sep="")
  
  data <- df %>%  
    mutate(!!varname := case_when(
      eval(
        parse(text = condition)
      ) ~ "Flagged",
      TRUE ~ "Okay"
    ))

  return(data)
}

# this function is handling datetime diffirence
interview_time_handler <- function(df){
  new_df <- df %>% 
    mutate(
      time_spend = difftime(
        as.POSIXct(df$end, format="%Y-%m-%dT%H:%M:%OS"), 
        as.POSIXct(df$start, format="%Y-%m-%dT%H:%M:%OS"), units='mins')
    )
  return(new_df)
}

# this function is handling date diffirence
interview_date_handler <- function(df){
  new_df <- df %>% 
    mutate(
      arrival_displace_date_diff = difftime(
        as.POSIXct(df$arrival_date_idp, format="%Y-%m-%d"), 
        as.POSIXct(df$displace_date_idp, format="%Y-%m-%d"), units='days')
    )
  
  return(new_df)
}

read_conditions_from_excel_column <- function(df, conditionDf) {
  counter <- 0
  for (row in 1:nrow(conditionDf)) {
    result_col <- conditionDf[row, "new_column_name"]
    conditions  <- conditionDf[row, "conditions"]
    type  <- conditionDf[row, "type"]
    
    if (type == "date-time") {
      df_result <- interview_time_handler(df_result)
      df_result <- compare_columns_values(df_result, result_col, paste(conditions));
    } else if (type == "normal") {
      if (counter == 0) {
        df_result <- compare_columns_values(df, result_col, paste(conditions));
        counter <- counter + 1;
      } else {
        df_result <- compare_columns_values(df_result, result_col, paste(conditions));
      }
    } else if (type == "date") {
      df_result <- interview_date_handler(df_result)
      df_result <- compare_columns_values(df_result, result_col, paste(conditions));
    }
  }
  
  return(df_result)
}

read_conditions_from_excel_limited_row <- function(df, conditionDf, idf) {
  df_total = data.frame()
  
  columns <- c("log_number", "_uuid", "date_assessment", "enumerator_num" , "question.name", "issue","issue.arabic", "action", "changed",	
               "old.value",	"new.value", "flag_index", "contact_name","contact_number")

  for (row in 1:nrow(conditionDf)) {
    result_col <- conditionDf[row, "result_column_name"]
    conditions  <- conditionDf[row, "conditions"]
    type  <- conditionDf[row, "type"];
    question_label  <- conditionDf[row, "question.name"];
    issue_label  <- conditionDf[row, "issue_label"];
    issue_arabic  <- conditionDf[row, "issue_arabic"];
    old_value  <- conditionDf[row, "old_value"];
    action_label  <- conditionDf[row, "action_label"];
    
    if (type == "date-time") {
      df <- interview_time_handler(df)
      flagged_rows <- compare_columns_valuesLimit_col(df, result_col, paste(conditions), 
                                                      question_label, issue_label,issue_arabic, columns,
                                                      old_value, action_label, row);
      new_df <- data.frame(flagged_rows)
      df_total <- rbind(df_total, new_df)
    } else if (type == "normal") {
        flagged_rows <- compare_columns_valuesLimit_col(df, result_col, paste(conditions), 
                                                        question_label, issue_label,issue_arabic, columns, 
                                                        old_value, action_label, row);
        new_df <- data.frame(flagged_rows)
        df_total <- rbind(df_total, new_df)
    } else if (type == "date") {
      df <- interview_date_handler(df)
      flagged_rows <- compare_columns_valuesLimit_col(df, result_col, paste(conditions), 
                                                      question_label, issue_label,issue_arabic, columns, 
                                                      old_value, action_label, row);
      new_df <- data.frame(flagged_rows)
      df_total <- rbind(df_total, new_df)
    }
  }
  
  return(df_total)
}

read_logs <- function(df, logDF, conditionDf) {
  df_total = data.frame()
  for (row in 1:nrow(logDF)) {
    flag_index <- logDF[row, "flag_index"]
    action  <- logDF[row, "action"]
    changed  <- logDF[row, "changed"]
    new_value  <- logDF[row, "new.value"]
    question_name <- logDF[row, "question.name"]

    current_condtion <- conditionDf[as.numeric(flag_index), "conditions"]
    current_type <- conditionDf[as.numeric(flag_index), "type"]
    old_value <- conditionDf[as.numeric(flag_index), "old_value"]
    X_uuid <-  logDF[row, "X_uuid"]
    
    print(row)
    
    
    varname <- paste("df$", question_name , sep="")
    
    
    df[, question_name] <-
      ifelse(df$`_uuid` == X_uuid, new_value, eval(parse(text = varname)))
  }
  
  return(df)
}

