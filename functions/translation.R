#Check for each variable, if there is anything to translate, collects and extracts it for translation
get_translations <- function(rawDF, textFieldsDF, uuid_ = "_uuid"){
  
  # Create empty vectors
  question <- vector()
  orginal_value <- vector()
  translated_value <- NA
  uuid <- vector()
  #Iterates for each given columns
  for (j in 1:nrow(textFieldsDF)) {
    #Extracts values
    for (rowi in 1:nrow(rawDF)){
      value_raw <- rawDF[rowi, paste(textFieldsDF[j,])]
      if(!is.na(value_raw)){
        # append values to vectors
        question <- c(question, paste(textFieldsDF[j,]))
        orginal_value <- c(orginal_value, as.character(value_raw))
        uuid <- c(uuid, as.character(rawDF[rowi,uuid_]))
      }
    }
    # Feedback
    cat("\014")
    print (paste("Getting un-translated column", j, "of", nrow(textFieldsDF)))
  }
  
  forTranslation <- data.frame(question, orginal_value, translated_value, uuid)
  return(forTranslation)
  
}

#Receives the translation and raw data set then replace all translated values with orginal values
set_translation <- function(rawDF, translations, uuid_ = "_uuid"){
  for (i in 1:length(rawDF)) {
    for (j in 1:length(translations$question)) {
      var_name <- unique(translations$question[j])
      
      for (r in 1:as.numeric(count(rawDF[i]))) {
        #Compares the values and uuid
        if (paste(var_name) == paste(translations[j, ][1]) & paste(rawDF[r, uuid_]) == paste(translations[j, ][4])) {
          rawDF[r,var_name] <- translations[j, ][3]
        }
      }
      # Feedback
      cat("\014")
      print (paste("Replacing translation", j, "of", length(translations$question)))
    }
    break()
  }
  return(rawDF)
}

#Iterates on all data set and looks for any non-ascii value
check <- function(rawDF, uuid_){
  # Create empty vectors
  question <- NA
  orginal_value <- NA
  translated_value <- NA
  uuid <- NA
  
  for (j in 1:length(rawDF)) {
    #Gets the name of each variable
    #if current column has any non-ascii value, then iterate on it
    value_count <- count(rawDF[which(grepl("[^\u0001-\u007F]+", rawDF[[j]])),])
    
    if(value_count > 0){
      #iterates for each value 
      for (rowi in 1:nrow(rawDF)){
        value_raw <- rawDF[rowi, j]
        if(!is.na(value_raw)){
          # append values to vectors
          question <- c(question, names(rawDF[j]))
          orginal_value <- c(orginal_value, as.character(value_raw))
          uuid <- c(uuid, as.character(rawDF[rowi,uuid_]))
        }
      }
    }
    # Feedback
    cat("\014")
    print (paste("Checking column", j, "of", length(rawDF)))
  }
  
  forTranslation <- data.frame(question, orginal_value, translated_value, uuid)
  return(forTranslation)
  
}