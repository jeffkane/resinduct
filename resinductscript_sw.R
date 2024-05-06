#### Resin Duct Seasonwood Data Processing ####

#' The resinductscript_sw.R script is a variation of resinductscript.R that allows quantification of axial resin ducts by seasonwood (SW) in tree rings.
#' It builds files of resin duct metrics corresponding to earlywood and latewood calendar years of the season and year the ducts were formed. 
#'
#' This code will take TXT files built from the measurement of resin ducts in ImageJ (https://imagej.nih.gov/ij/)
#' and process them so that the final output will be a TXT file containing 5 resin duct metrics
#' corresponding to calendar years in which the ducts were formed. 
#' It is assumed that a CSV ring width file with earlywood (EW), latewood (LW) and whole ring width exists
#' and that sample IDs match those of TXT file names.
#' For further details, refer to the associated paper (Hood et al.(2020))
#' 
#' 
#' 
#' There are FIVE individual sections that follow. Each can be run separately but may require a file type
#' output from a previous section. Each section should be run in its entirety for best results.
#' Section 0 must be run in any case
#' 
#' The sections are as follows:
#' 0. RUNNING SCRIPT WITH FUNCTIONS - functions needed for the following sections
#' 1. ADDING YEARS TO RESIN DUCT METRICS - add years to resin ducts measured in ImageJ following protocol in Hood et al. (2020)
#' 2. COMBINING INDIVIDUAL FILES INTO ONE FILE - combines all .txt files created in previous section into one file with all trees of interest
#' 3. CALCULATING UNSTANDARDIZED DUCT METRICS - calculates resin duct metrics that have not been standardized by ring area
#' 4. CALCULATING STANDARDIZED DUCT METRICS - uses data from unstandardized metrics created in previous section combined with existing ring width measurements to calculate resin duct metrics standardized by ring area
#' 
#' 
#' NOTE: Only User defined variables should be changed!
#' 
#' 
#' IMPORTANT NOTE: 
#' This script assumes that the following steps are applied when processing the cores in ImageJ:
#' - After measuring EW ducts, insert 1 false measurement - this will return 1 Area == 0 on the txt file
#' - After measuring one year (EW and LW), insert 2 false measurements - this will return 2 Area == 0 on the txt file
#' - If there is a missing ring, add a 3rd false measurement - this will result in 3 Area == 0 on the txt file
#' A detailed explanation of how to further process the cores in Image J is described in Hood et al. (2020)
#' The function addYears() considers that 3 false measurements (i.e., three Area == 0) can be found in different conditions,
#' and corrects accordingly to the specific situation:
#' - A true missing ring
#' - A ring where EW produced ducts but LW did not produce any
#' - LW of previous ring produced ducts but not the EW of the next ring
#' For details of the function, see addYears() on the associated functions.R script
#'
#'
#'
#' Reference:
#' Hood, S. M., Reed, C. C., & Kane, J. M. (2020). Axial resin duct quantification in tree rings: A functional defense trait. MethodsX, 7, 101035.




#'####################################################################################
#### 0. RUNNING SCRIPT WITH FUNCTIONS ####

#'** User defined variables - CHANGE THESE: **

# Location of the script containing functions
function_location <- "R/"


###

### Main script - should not need to change:

# Run script with functions
source(paste0(function_location, "functions.R"))




#'####################################################################################
#### 1. ADDING YEARS TO RESIN DUCT MEASUREMENTS ####


#'** User defined variables - CHANGE THESE: **
#'
# Data frame with core file name and years with missing rings
filesMR <- data.frame(File = c("CT163.txt", "CT143.txt", "CT195.txt", "CT195.txt", "SW184.txt", "SW020.txt", "SW159.txt", "SW160.txt", "SW140.txt", "SW140.txt"),
                      Year = c(1994, 1973, 2001, 2010, 1991, 1994, 1989, 1989, 2001, 2013))

# Input and output directories and files
files_input  <- "resin_ducts/imagej/"      # set the location of imagej .txt files
outputTemp   <- "resin_ducts/dated/temp/"  # set the location of temporal files (useful to find errors)
files_output <- "resin_ducts/dated/"       # set the location of output files

# First and last year measured
year_start <- 1969     # first year for which resin ducts were measured 
year_end   <- 2015     # last year


###

### Main script - should not need to change:

Files <- list.files(files_input)  # list of files to loop through

for(file in Files){
  
  data_imagej <- read.table(paste0(files_input,file), header = TRUE)     # read in .txt file
  sampleID    <- substr(file,1,nchar(file)-4)
  new         <- data.frame("SampleID" = sampleID, "Area" = data_imagej$Area, "Year" = NA, "MR" = NA)     # create new data frame with sample ID and duct areas as well as year as an empty column

  # Function that assigns years to earlywood and latewood measurements and finds errors
  if(file %in% filesMR$File){
    new = addYears.MR(new, saveTemp = TRUE, connectionTemp = outputTemp)
    new$MR[new$SW == "MR"] = "Y"   # "Yes" when the ring has width = 0
    new$MR[new$SW != "MR"] = "N"
  } else {
    new = addYears(new, saveTemp = TRUE, connectionTemp = outputTemp)
    new$MR = "N"
  }
  
  new <- completeYears(new) ## add Years where resin ducts were not measured
  
  new$Area[new$SW == "MR"] <- NA    # change MR Area to NA
  #' When there ring width = 0, Duct_Size should be coded as NA
  
  write.table(new, paste0(files_output,sampleID,".txt"), row.names = FALSE)
}

###



#'####################################################################################
#### 2. COMBINING INDIVIDUAL FILES INTO ONE FILE ####

#' This section takes the files produced from the previous section and combines them into one .txt
#' that will be used for calculating resin duct metrics


#'** User defined variables - CHANGE THESE: ** 
#'
# Input and output directories and files
files_input  <- "resin_ducts/dated/"   # set the location of .txt files
newfile      <- "all_dated.txt"        # name of new file
files_output <- "resin_ducts/"         # set the location of output file


###

### Main script - should not need to change:

# Combine all individual files into one file
Files <- list.files(files_input, pattern = "*.txt", full.names = TRUE)
all_data <- rbindlist(lapply(Files, fread))

write.table(all_data, file = paste0(files_output, newfile), row.names = FALSE)




#'####################################################################################
#### 3. CALCULATING UNSTANDARDIZED DUCT METRICS ####

#' This section takes the file produced in the previous section and calculates three resin duct metrics
#' that have not been standardized by duct area:
#' Total duct area: Sum of duct area per annual ring (mm^2 / year)
#' Duct size: Mean size of all ducts per annual ring (mm^2)
#' Duct production: Total number of ducts per annual ring


#'** User defined variables - CHANGE THESE: ** 
#'
# Input and output directories and files
files_input  <- "resin_ducts/"                 # set the location of file created in previous section
file2        <- "all_dated.txt"                # name of file created in previous section
newfile2     <- "unstandardized_metrics.txt"   # name of file that will be created in this section with unstandardized duct metrics
files_output <- "resin_ducts/"                 # set the location of output file


###

### Main script - should not need to change:

data <- read.table(paste0(files_input, file2), header = TRUE)

# Calculate total duct area (TDA), mean duct size (size) and duct production (prod)
## By earlywood/latewood
metrics <- data %>% 
  group_by(SampleID, Year, SW, MR) %>% 
  summarise(Duct_Size = mean(Area),
            Total_Duct_Area = sum(Area),
            Duct_Production = ductProduction(Area),
            .groups = "drop") %>% 
  mutate(Duct_Size = Duct_Size * 645.16,
         Total_Duct_Area = Total_Duct_Area * 645.16) # in^2 to mm^2
          

metrics.ew <- metrics %>% 
  filter(SW %in% c("EW","MR") | is.na(SW)) %>% 
  rename(Duct_Size.ew = Duct_Size, Total_Duct_Area.ew = Total_Duct_Area, Duct_Production.ew = Duct_Production) %>% 
  select(-SW)

metrics.lw <- metrics %>% 
  filter(SW %in% c("LW","MR") | is.na(SW)) %>% 
  rename(Duct_Size.lw = Duct_Size, Total_Duct_Area.lw = Total_Duct_Area, Duct_Production.lw = Duct_Production) %>% 
  select(-SW)

## By whole ring
metrics.rw <- data %>% group_by(SampleID, Year, MR) %>% 
  summarise(Duct_Size.rw = mean(Area),
            Total_Duct_Area.rw = sum(Area),
            Duct_Production.rw = ductProduction(Area),
            .groups = "drop") %>% 
  mutate(Duct_Size.rw = Duct_Size.rw * 645.16,
         Total_Duct_Area.rw = Total_Duct_Area.rw * 645.16) # in^2 to mm^2

# Merge all in one file
metrics_all <- full_join(metrics.ew, metrics.lw, by = c("SampleID","Year", "MR"))
metrics_all <- full_join(metrics_all, metrics.rw, by = c("SampleID","Year", "MR"))
metrics_all <- metrics_all[order(metrics_all$SampleID, metrics_all$Year), ]


#' When Duct_Production = NA means that there were no ducts in either earlywood or latewood
#' and should be changed to Duct_Production = 0
metrics_all <- within(metrics_all, {
  Duct_Production.ew <- ifelse(is.na(Duct_Production.ew) & MR == "N", 0, Duct_Production.ew)
  Duct_Production.lw <- ifelse(is.na(Duct_Production.lw) & MR == "N", 0, Duct_Production.lw)
  Duct_Production.rw <- ifelse(is.na(Duct_Production.rw) & MR == "N", 0, Duct_Production.rw)
})

#' If Duct_Production = 0 (no ducts in the ring OR ring width = 0), 
#' Duct_Size should be NA and Total_Duct_Area should be 0
metrics_all[metrics_all[, "Duct_Production.ew"] == 0, "Duct_Size.ew"] <- NA
metrics_all[metrics_all[, "Duct_Production.ew"] == 0, "Total_Duct_Area.ew"] <- 0
metrics_all[metrics_all[, "Duct_Production.lw"] == 0, "Duct_Size.lw"] <- NA
metrics_all[metrics_all[, "Duct_Production.lw"] == 0, "Total_Duct_Area.lw"] <- 0
metrics_all[metrics_all[, "Duct_Production.rw"] == 0, "Duct_Size.rw"] <- NA
metrics_all[metrics_all[, "Duct_Production.rw"] == 0, "Total_Duct_Area.rw"] <- 0

write.table(metrics_all, paste0(files_output, newfile2), row.names = FALSE)


#' NOTE: The metrics calculated above are UNSTANDARDIZED, i.e. they have not been standardized by
#' ring area.
#' The following section includes the code for calculating standardized metrics given that the
#' width of the core is known and a ring width file (.rwl) containing the same sample IDs exists.




#'####################################################################################
#### 4. CALCULATING STANDARDIZED DUCT METRICS ####

#' This section uses data from unstandardized duct metrics calculated in previous section combined with
#' existing .rwl files for the same trees to calculate two standardized resin duct metrics. The output file
#' includes all 5 resin duct metrics as well as corresponding ring widths (mm) and ring areas (mm^2). It is
#' assumed that the sample ID ("Sample") for each tree is the same in both .rwl files and .csv file with
#' unstandardized metrics.
#' 
#' 
#' NOTE: If ring width is in .rwl file, continue Section 4 of original script

#'** User defined variables - CHANGE THESE: **
#'
# Input and output directories and files
files_input     <- "resin_ducts/"                 # set the location of file containing unstandardized metrics
file3           <- "unstandardized_metrics.txt"   # name of file created in previous section
newfile3        <- "all_duct_metrics.txt"         # name of file that will be created in this section with both unstandardized and standardized resin duct metrics
files_output    <- "resin_ducts/"                 # set the location of output file
files_ringwidth <- "LickCreek_tree_ring_data_LV.csv"    # set .csv file containing ring width measurements

# First and last year of measurements and core width
year_start <- 1969    # first year for which resin ducts were measured 
year_end   <- 2015    # last year for which resin ducts were measured
core_width <- 5.15    # width in mm of cores

# Name of columns of ring width file
yearColumn <- "year"    # name of the column in files_ringwidth that contains Years
sampleID   <- "coreNum" # name of the column in files_ringwidth that contains core ID
selectCols <- c("coreNum", "year", "rw.b", "ew.b", "lw.b")   # name of the columns of interest (core ID, Year, ring width measurement, earlywood width, latewood width)
#' NOTE: selectCols object should be same length and same order, if not, change it


###

### Main script - should not need to change:

rw             <- read.csv(files_ringwidth)
unstandardized <- read.table(paste0(files_input,file3), header = TRUE)     # read in file with unstandardized metrics

newCols <- c("SampleID", "Year", "Width.rw", "Width.ew", "Width.lw")  # new names of the columns of interest  

# Clean rw dataframe
rw[[yearColumn]] <- as.numeric(as.character(rw[[yearColumn]]))    # make sure year is numeric
rw <- rw[rw[[yearColumn]] >= year_start & rw[[yearColumn]] <= year_end, ]     # limits years to only those for which resin ducts were sampled
rw <- rw[rw[[sampleID]] %in% unstandardized$SampleID, ]   # filter out unmeasured cores
rw <- rw[ , names(rw) %in% selectCols]   # select columns of interest (core ID, Year, ring width measurement, earlywood width, latewood width)
names(rw) <- newCols    # change columns name so it can be merged to "unstandardized" data frame

# Merge data
ducts_all <- full_join(unstandardized, rw, by = c("SampleID","Year"))     # merge the ring width data (RW_mm) with unstandardized duct metrics data

ducts_all$Area.ew <- ducts_all$Width.ew * core_width    # calculates ring area in mm^2 using core width and ring width
ducts_all$Area.lw <- ducts_all$Width.lw * core_width 
ducts_all$Area.rw <- ducts_all$Width.rw * core_width

ducts_all$Rel_Duct_Area.ew <- (ducts_all$Total_Duct_Area.ew/ducts_all$Area.ew)*100    # calculates relative duct area
ducts_all$Rel_Duct_Area.lw <- (ducts_all$Total_Duct_Area.lw/ducts_all$Area.lw)*100 
ducts_all$Rel_Duct_Area.rw <- (ducts_all$Total_Duct_Area.rw/ducts_all$Area.rw)*100 

ducts_all$Duct_Density.ew <- ducts_all$Duct_Production.ew/ducts_all$Area.ew     # calculates duct density
ducts_all$Duct_Density.lw <- ducts_all$Duct_Production.lw/ducts_all$Area.lw 
ducts_all$Duct_Density.rw <- ducts_all$Duct_Production.rw/ducts_all$Area.rw


#' When ring width = 0, all duct metrics should be coded as 0 (except for Duct_Size)
ducts_all$Rel_Duct_Area.ew[ducts_all$MR == "Y"] <- 0
ducts_all$Rel_Duct_Area.lw[ducts_all$MR == "Y"] <- 0
ducts_all$Rel_Duct_Area.rw[ducts_all$MR == "Y"] <- 0

ducts_all$Duct_Density.ew[ducts_all$MR == "Y"] <- 0
ducts_all$Duct_Density.lw[ducts_all$MR == "Y"] <- 0
ducts_all$Duct_Density.rw[ducts_all$MR == "Y"] <- 0



write.table(ducts_all, paste0(files_output, newfile3), row.names = FALSE)   

#'#########################################################################################################################################################################
