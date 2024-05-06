
#' Functions file associated with the script "resinductscript_sw.R"

## Functions included:
#' addYears() - Function to assign Years and check for errors, when the core has no missing rings
#' addYears.MR() - Function to assign Years and check for errors, when the core has missing rings
#' completeYears() - Functions to add Years where resin ducts were not measured
#' ductProduction() - Function to count number of years where resin ducts were produced




# Load libraries ----
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tibble)){install.packages("tibble")}
if(!require(data.table)){install.packages("data.table")}


## Function to assign Years and check for errors, when the core has no missing rings
addYears = function(data, saveTemp = TRUE, connectionTemp){
  
  if(!dir.exists(connectionTemp)){
    stop(paste("Connection", connectionTemp, "does not exist"))
  }
  
  year = year_start
  year_end = year_end
  
  ## Add EW/LW borders ----
  for(i in 1:nrow(data)){
    
    if (i == 1 & isTRUE(data$Area[i] == 0)) {
      data$Year[i] = 'EW'
    } 
    # When 1x Area = 0 and previous and next Areas are ducts
    if(i != 1 & isTRUE(data$Area[i] == 0) & isTRUE(data$Area[i+1] != 0) & isTRUE(data$Area[i-1] != 0)) {
      data$Year[i] = 'EW'
    }
    
    # When 2x Area = 0 and previous and next Areas are ducts
    if(i != 1 & isTRUE(data$Area[i] == 0) & isTRUE(data$Area[i-1] != 0) & isTRUE(data$Area[i+1] == 0) & isTRUE(data$Area[i+2] != 0)) {
      data$Year[i] = 'LW'
    }
    if (i != 1 & isTRUE(data$Area[i] == 0) & isTRUE(data$Year[i-1] == "LW") & isTRUE(is.na(data$Year[i-2]) | data$Year[i-2] == "EW" | grepl("\\d", data$Year[i-2]))) {
      data$Year[i] = 'LW'
    }
    
    # When 3x Area = 0
    ## Loop through previous Area = 0 to check if it was EW or LW
    if(i != 1 & isTRUE(data$Area[i] == 0) & is.na(data$Year[i])==TRUE) {
      j = i - 1
      while (j > 1 & is.na(data$Year[j])) {
        j = j - 1
      }
      ### If previous was LW, assign EW
      if (!is.na(data$Year[j]) && data$Year[j] == "LW") {
        data$Year[i] = "EW"
      }
      ### If previous was EW, assign LW
      if (!is.na(data$Year[j]) && data$Year[j] == "EW") {
        data$Year[i] = "LW"
      }
      ### If there is no previous LW or EW (j reached row 1), assign EW
      if (is.na(data$Year[j]) & j == 1) {
        data$Year[i] = "EW"
      }
    }
  }#END for EW/LW borders
  
  ## Add years ----
  for(i in 1:nrow(data)){
    
    if (i == 1 & is.na(data$Year[i]) == TRUE) {
      data$Year[i] = year
    } 
    if(is.na(data$Year[i]) == TRUE & isTRUE(data$Year[i-1] == "EW")){
      data$Year[i] = year
    }
    if(i != 1 & is.na(data$Year[i] == TRUE) & isTRUE(data$Year[i-1] == "LW")){
      year = year+1
      data$Year[i] = year
    }
    if(isTRUE(data$Year[i] == "LW") & isTRUE(data$Year[i+1] == "EW")){
      year = year+1
    }
    if(i != 1 & is.na(data$Year[i])==TRUE) {
      data$Year[i] = data$Year[i-1]
    }
    
  }#END for Years
  
  
  ## Save Temp file with EW/LW sequence ----
  if(saveTemp == TRUE){
    write.table(data, paste0(connectionTemp,sampleID,"_temp.txt"), row.names = FALSE)
  }
  
  
  ## Check errors ----
  
  ## Check if EW/LW were correctly placed
  vec <- data$Year[data$Year %in% c("EW","LW")]
  
  if(2*length(vec[vec == "EW"]) != length(vec[vec == "LW"])){
    
    # Find vector index of the error (consecutive EW)
    indices <- c()
    for (i in 1:(length(vec)-2)) {
      if (vec[i] == "EW" && vec[i+1] == "EW") {
        indices <- c(indices, i)
      }
      if (vec[i] == "LW" && vec[i+1] == "LW" && vec[i+2] == "LW") {
        indices <- c(indices, i)
      }
    }
    ## Attention: sometimes index is not printed, look for error anyway
    print(paste(sampleID, "Error in ImageJ EW/LW assignment: vector index", indices, "out of", length(vec),
                "- aprox year", round(year_start+indices/3, 1)))
  }
  
  
  years_vec <- data$Year[!data$Year %in% c("EW","LW")]
  years_vec <- as.numeric(years_vec)
  
  ## Check if last year is correctly assigned
  if(years_vec[length(years_vec)] != year_end){
    
    # Find row index that matches years_vec[length(years_vec)]
    lastYear_index <- which(data$Year == years_vec[length(years_vec)])[1]
    
    ## If last year is year_end-1 followed by a final sequence LW, LW, EW, LW, LW, do nothing
    if(as.numeric(as.character(data$Year[lastYear_index])) == year_end - 1 &
       isTRUE(data$Year[length(data$Year)-4] == "LW") & isTRUE(data$Year[length(data$Year)-3] == "LW") &
       isTRUE(data$Year[length(data$Year)-2] == "EW") &
       isTRUE(data$Year[length(data$Year)-1] == "LW") & isTRUE(data$Year[length(data$Year)] == "LW")){
      # Do nothing
    } else {
      # Print all rows after the matching row index
      print(paste(sampleID, "Last dated year:",(years_vec[length(years_vec)])))
      print(data$Year[(lastYear_index + 1):nrow(data)])
    }
  }
  
  
  ## Add EW/LW column ----
  data$SW = NA
  
  for(i in 1:nrow(data)){
    
    if (grepl("\\d+", data$Year[i])) {
      following_char_index <- match(TRUE, grepl("[A-Za-z]+", data$Year[(i + 1):nrow(data)])) + i
      data$SW[i] <- data$Year[following_char_index]
    }
  }
  
  data = data[!is.na(data$SW), ]
  data$Year = as.numeric(data$Year)
  
  return(data)
}




## Function to assign Years and check for errors, when the core has missing rings
addYears.MR = function(data, saveTemp = TRUE, connectionTemp){
  
  if(!dir.exists(connectionTemp)){
    stop(paste("Connection", connectionTemp, "does not exist"))
  }
  
  year = year_start
  year_end = year_end
  yearMR = filesMR$Year[filesMR$File %in% file]

  ## Add missing ring label ----
  ### If only one MR
  if(length(yearMR) == 1){
    position = (yearMR - year)*3 + 1 # position of MR in the sequence EW LW LW
  }
  ### If two MR
  if(length(yearMR) == 2){
    position = (yearMR - year)*3 + 1 # position of first MR in the sequence EW LW LW
    position[2] = position[2] -2 # position of the second year in the sequence EW LW LW
  }
  ### If > 2 MR
  if(length(yearMR) > 2){
    stop(paste(sampleID, "has more than 2 missing rings - Adjust function addYears.MR"))
  }
  
  aux = data
  aux = rownames_to_column(data, "Row")
  aux = aux[aux$Area == 0, ]
  aux[position, "Year"] <- "MR"
  mr_index = as.numeric(aux$Row[aux$Year %in% "MR"])
  data$Year[mr_index] <- "MR"

  ## Add EW/LW borders ----
  for(i in 1:nrow(data)){
    
    if(i == 1 & isTRUE(data$Area[i] == 0)) {
      data$Year[i] = 'EW'
    } 
    # When 1x Area = 0 and previous and next Areas are ducts
    if(i != 1 & isTRUE(data$Area[i] == 0) & isTRUE(data$Area[i+1] != 0) & isTRUE(data$Area[i-1] != 0)) {
      data$Year[i] = 'EW'
    }
    # When 1x Area = 0 and previous Year is MR
    if(i != 1 & isTRUE(data$Area[i] == 0) & isTRUE(data$Year[i-1] == "MR")) {
      data$Year[i] = 'EW'
    }
    
    # When 2x Area = 0 and previous and next Areas are ducts
    if(i != 1 & isTRUE(data$Area[i] == 0) & isTRUE(data$Area[i-1] != 0) & isTRUE(data$Area[i+1] == 0) & isTRUE(data$Area[i+2] != 0)) {
      data$Year[i] = 'LW'
    }
    if (i != 1 & isTRUE(data$Area[i] == 0) & isTRUE(data$Year[i-1] == "LW") & isTRUE(is.na(data$Year[i-2]) | data$Year[i-2] == "EW" | grepl("\\d", data$Year[i-2]))) {
      data$Year[i] = 'LW'
    }
    
    # When 3x Area = 0
    ## Loop through previous Area = 0 to check if it was EW or LW
    if(i != 1 & isTRUE(data$Area[i] == 0) & is.na(data$Year[i])==TRUE) {
      j = i - 1
      while (j > 1 & is.na(data$Year[j])) {
        j = j - 1
      }
      ### If previous was LW, assign EW
      if (!is.na(data$Year[j]) && data$Year[j] == "LW") {
        data$Year[i] = "EW"
      }
      ### If previous was EW, assign LW
      if (!is.na(data$Year[j]) && data$Year[j] == "EW") {
        data$Year[i] = "LW"
      }
      ### If previous was MR, assign EW
      if (!is.na(data$Year[j]) && data$Year[j] == "MR") {
        data$Year[i] = "EW"
      }
      ### If there is no previous LW or EW (j reached row 1), assign EW
      if (is.na(data$Year[j]) & j == 1) {
        data$Year[i] = "EW"
      }
    }
  }#END for EW/LW borders
  
  ## Add years ----
  for(i in 1:nrow(data)){
    
    if (i == 1 & is.na(data$Year[i]) == TRUE) {
      data$Year[i] = year
    } 
    if(is.na(data$Year[i]) == TRUE & isTRUE(data$Year[i-1] == "EW")){
      data$Year[i] = year
    }
    if(i %in% mr_index & isTRUE(data$Year[i] == "MR")){
      year = year + 1 # 1 year as MR
      # stop()
    }
    if(i != 1 & isTRUE(data$Year[i] == "EW") & isTRUE(data$Year[i-1] == "MR")){
      year = year + 1 # sum up  1 year after MR
    }
    if(i != 1 & is.na(data$Year[i] == TRUE) & isTRUE(data$Year[i-1] == "MR")){
      year = year + 1 # sum up  1 year after MR
      data$Year[i] = year
    }
    if(i != 1 & is.na(data$Year[i] == TRUE) & isTRUE(data$Year[i-1] == "LW")){
      year = year + 1 # sum up  1 year after LW 
      data$Year[i] = year # and print it (year with ducts)
    }
    if(isTRUE(data$Year[i] == "LW") & isTRUE(data$Year[i+1] == "EW")){
      year = year + 1 # sum up 1 year after LW 
      # and don't print it (year without ducts: NA)
    }
    if(i != 1 & is.na(data$Year[i])==TRUE) {
      data$Year[i] = data$Year[i-1]
    }
    
  }#END for Years
  
  
  ## Save Temp file with EW/LW sequence ----
  if(saveTemp == TRUE){
    write.table(data, paste0(connectionTemp,sampleID,"_temp.txt"), row.names = FALSE)
  }
  
  
  ## Check errors ----
  
  ## Check if EW/LW were correctly placed
  vec <- data$Year[data$Year %in% c("EW","LW")]
  
  if(2*length(vec[vec == "EW"]) != length(vec[vec == "LW"])){
    
    # Find vector index of the error (consecutive EW)
    indices <- c()
    for (i in 1:(length(vec)-2)) {
      if (vec[i] == "EW" && vec[i+1] == "EW") {
        indices <- c(indices, i)
      }
      if (vec[i] == "LW" && vec[i+1] == "LW" && vec[i+2] == "LW") {
        indices <- c(indices, i)
      }
    }
    ## Attention: sometimes index is not printed, look for error anyway
    print(paste(sampleID, "Error in ImageJ EW/LW assignment: vector index", indices, "out of", length(vec),
                "- aprox year", round(year_start+indices/3, 1)))
  }
  
  
  years_vec <- data$Year[!data$Year %in% c("EW","LW","MR")]
  years_vec <- as.numeric(years_vec)
  
  ## Check if last year is correctly assigned
  if(years_vec[length(years_vec)] != year_end){
    
    # Find row index that matches years_vec[length(years_vec)]
    lastYear_index <- which(data$Year == years_vec[length(years_vec)])[1]
    
    ## If last year is year_end-1 followed by a final sequence LW, LW, EW, LW, LW, do nothing
    if(as.numeric(as.character(data$Year[lastYear_index])) == year_end - 1 &
       isTRUE(data$Year[length(data$Year)-4] == "LW") & isTRUE(data$Year[length(data$Year)-3] == "LW") &
       isTRUE(data$Year[length(data$Year)-2] == "EW") &
       isTRUE(data$Year[length(data$Year)-1] == "LW") & isTRUE(data$Year[length(data$Year)] == "LW")){
      # Do nothing
    } else {
      # Print all rows after the matching row index
      print(paste(sampleID, "Last dated year:",(years_vec[length(years_vec)])))
      print(data$Year[(lastYear_index + 1):nrow(data)])
    }
  }
  

  ## Add EW/LW column ----
  data$SW = NA
  # Add MR years
  data$SW[mr_index] <- "MR"
  data$Year[mr_index] <- yearMR
  
  for(i in 1:nrow(data)){
    # If Year is a numeric character and SW is NA (no MR label)
    if(grepl("\\d+", data$Year[i]) & is.na(data$SW)[i]) {
      following_char_index <- match(TRUE, grepl("[A-Za-z]+", data$Year[(i + 1):nrow(data)])) + i
      data$SW[i] <- data$Year[following_char_index]
    }
  }
  
  data = data[!is.na(data$SW), ]
  data$Year = as.numeric(data$Year)
  
  return(data)
}




## Functions to add Years where resin ducts were not measured
completeYears <- function(data) {

    sampleID = unique(data$SampleID)
    years_vec <- data$Year
    new_data <- data.frame()
    
    for (i in 2:length(years_vec)) {
      year_before <- years_vec[(i - 1)]
      year_i <- years_vec[i]
      
      if (year_i - year_before > 1) {
        missing_years <- seq(year_before + 1, year_i - 1)
        missing_rows <- data.frame(SampleID = sampleID,
                                   Year = missing_years,
                                   Area = NA,
                                   SW = NA,
                                   MR = "N")
        new_data <- rbind(new_data, missing_rows)
      }
    }
    
    complete_data <- rbind(data, new_data)
  
  complete_data <- complete_data[order(complete_data$SampleID, complete_data$Year), ]
  rownames(complete_data) <- NULL
  return(complete_data)
}




## Function to count number of years where resin ducts were produced (if MR, return 0)
ductProduction <- function(x) {
  if (any(!is.na(x) & x != 0)) {
    length(x)
  } else {
    0
  }
}

