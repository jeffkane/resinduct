#R Script: “ResinDucts_datareduction”

#### Resin Duct data processing ####

# This code will take .txt files built from the measurement of resin ducts in ImageJ (https://imagej.nih.gov/ij/)
# and process them so that the final output will be a .csv file containing 5 resin duct metrics
# corresponding to calendar years in which the ducts were formed. It is assumed that RWL files of
# ring widths have previously been created using standard dendrochronological techniques and that
# sample IDs match those of .txt file names.
# For further detail, refer to the associated paper (Hood et al., in prep).

# There are FOUR individual sections that follow. Each can be run separately but may require a file type
# output from a previous section. Each section should be run in its entirety for best results. 

# The sections are as follows:
# 1. ADDING YEARS TO RESIN DUCT METRICS - add years to resin ducts measured in ImageJ following protocol in Hood et al., in prep
# 2. COMBINING INDIVIDUAL .CSV FILES INTO ONE FILE - combines all .csv files created in previous section into one file with all trees of interest
# 3. CALCULATING UNSTANDARDIZED DUCT METRICS - calculates resin duct metrics that have not been standardized by ring area
# 4. CALCULATING STANDARDIZED DUCT METRICS - uses data from unstandardized metrics created in previous section combined with existing .rwl files to calculate resin duct metrics standardized by ring area

#####################################################################################
#### 1. ADDING YEARS TO RESIN DUCT MEASUREMENTS ####

# This section takes a .txt file of resin duct measurements exported from ImageJ and assigns years based
# on the initial year of measurement. Protocol for measuring resin ducts in ImageJ is as follows:

# 1. Start with inner most ring of interest (closest to pith) and note year of ring formation
# 2. Measure all ducts within one annual ring before inserting a "zero" to denote ring boundary; continue this system for each ring
# 3. If a ring HAS resin ducts that have been measured, ALWAYS insert a "zero" after measuring ducts within that ring
# 4. If a ring DOES NOT HAVE resin ducts, simply insert a "zero" and move on to the next ring.
# Further detail on measuring protocol can be found in Hood et al., in prep


# User defined variables - CHANGE THESE:
file_location <- "C:/FILE/LOCATION"     # set the location of .txt files

# script should be re-run from here for each .txt file
file <- "SAMPLEID.txt"     # name of .txt file
year <- 1900     # start year (inner-most ring year)

###

# Main script - should not need to change:
setwd(file_location)     # set directory
data <- read.table(file)     # read in .txt file
sample <- substr(file,1,nchar(file)-4)
newdata <- data.frame("Sample"=sample, "Area"=data$Area, "Year"=NA)     # create new data frame with sample ID and duct areas as well as year as an empty column

# loops through rows in the dataframe and assigns years to duct area measurements
for(i in 1:nrow(newdata)){
  if (i == 1) {
    newdata$Year[i] <- year
  }
  if(i != 1 & newdata$Area[i] == 0) {
    newdata$Year[i] <- '.'
  }
  if(i != 1 & isTRUE(newdata$Area[i-1] == 0)){
    year=year+1
    newdata$Year[i] <- year
  }
  if(i != 1 & is.na(newdata$Year[i])==TRUE) {
    newdata$Year[i] <- newdata$Year[i-1]
  }
}

newfile <- paste(sample,"dated.csv", sep="_")     # create new file name

write.csv(newdata, newfile, row.names=FALSE)     # write out new (.csv) file

#####################################################################################
#### 2. COMBINING INDIVIDUAL .CSV FILES INTO ONE FILE ####

# This section takes the .csv files produced from the previous section and combines them into one .csv
# that will be used for calculating resin duct metrics

# User defined variables - CHANGE THESE:
file_location <- "C:/FILE/LOCATION"     # set the location of .CSV files
newfile <- "test.csv"     # enter name of new file
# install.packages("data.table")     # install required packages (IF NECESSARY)

###

# Main script - should not need to change:
setwd(file_location)     # set directory
library(data.table)     # load required packages

#combines all individual .csv files into one file with all of the trees
files <- list.files(pattern="*.csv")
all_data <- rbindlist(lapply(files, fread))

write.csv(all_data, file = newfile, row.names=FALSE)     # write out new file in same location

#####################################################################################
#### 3. CALCULATING UNSTANDARDIZED DUCT METRICS ####

# This section takes the .csv file produced in the previous section and calculates three resin duct metrics
# that have not been standardized by duct area:
# Total duct area: Sum of duct area per annual ring (mm^2 / year)
# Duct size: Mean size of all ducts per annual ring (mm^2)
# Duct production: Total number of ducts per annual ring

# User defined variables - CHANGE THESE:
file_location <- "C:/FILE/LOCATION"     # set the location of CSV file created in previous section
file2 <- "test.csv"     # name of .csv file created in previous section
newfile2 <- "unstandardized_metrics.csv"     # name of .csv file that will be created in this section with unstandardized duct metrics

###

# Main script - should not need to change:
setwd(file_location)     # set directory
data <- read.csv(file2)     # read in .csv file created in previous section
data <- subset(data, data[,3] != ".")     # remove breaks between years

# calculates: total duct area (TDA), mean duct size (size), duct production (prod)
temp_tda <- aggregate(data$Area, by = list("Sample" = data$Sample, "Year" = data$Year), FUN = sum)
temp_size <- aggregate(data$Area, by = list("Sample" = data$Sample, "Year" = data$Year), FUN = mean)

fun1 <- function(x) {     # counts number of rows, unless a 0 value, which returns 0
  if (x != 0) {
    length(x)
  } else {
    0
  }
}

temp_prod <- aggregate(data$Area, by = list("Sample" = data$Sample, "Year" = data$Year), FUN = fun1)

# put all metrics into one data frame; convert TDA and size to mm since ImageJ records in inches
duct_metrics <- data.frame("Sample" = temp_tda$Sample, "Year" = temp_tda$Year,
                           "Total_Duct_Area" =  temp_tda$x*645.16, "Duct_Size" = temp_size$x*645.16, "Duct_Production" = temp_prod$x)

write.csv(duct_metrics, newfile2, row.names=FALSE)

### NOTE: The metrics calculated above are UNSTANDARDIZED, i.e. they have not been standardized by
# ring area. The following section includes the code for calculating standardized metrics given that the
# width of the core is known and a ring width file (.rwl) containing the same sample IDs exists.

#####################################################################################
#### 4. CALCULATING STANDARDIZED DUCT METRICS ####

# This section uses data from unstandardized duct metrics calculated in previous section combined with
# existing .rwl files for the same trees to calculate two standardized resin duct metrics. The output file
# includes all 5 resin duct metrics as well as corresponding ring widths (mm) and ring areas (mm^2). It is
# assumed that the sample ID ("Sample") for each tree is the same in both .rwl files and .csv file with
# unstandardized metrics.

# User defined variables - CHANGE THESE:
file_location <- "C:/FILE/LOCATION"     # set the location of CSV file containing unstandardized metrics AND corresponding .rwl files
file3 <- "unstandardized_metrics.csv"     # name of .csv file created in previous section
newfile3 <- "all_duct_metrics.csv"     # name of .csv file that will be created in this section with both unstandardized and standardized resin duct metrics
year_start <- 1900     # first year for which resin ducts were measured
year_end <- 1950     # last year for which resin ducts were measured
core_width <- 5     # width in mm of cores (e.g. many increment borers product 5 mm cores)

# install.packages("dplR")     # if necessary
# install.packages("read.table")     # if necessary
# install.packages("tidyr")     # if necessary

# Main script - should not need to change:
library(dplR)     # load required packages
library(data.table)
library(tidyr)
setwd(file_location)     # set directory
files <- list.files(pattern="*.rwl")     # pulls file names for all .rwl files within the working directory

# reads in .rwl files, adds years as a column, and pivots the dataframe to long format (sample and ring width as columns):
fun2 <- function(x) {
  temp <- read.rwl(x)
  temp <- cbind("Year"=rownames(temp), data.frame(temp, row.names=NULL))
  new <- pivot_longer(temp, 2:ncol(temp), names_to="Sample", values_to="RW_mm")     # assumes ring widths are in mm
  return(new)
}

temp2 <- rbindlist(lapply(files, fun2), use.names=FALSE)     # reads in all .rwl files in working directory and binds them all together in one dataframe
temp2$Year <- as.numeric(as.character(temp2$Year))     # make sure year is numeric
temp3 <- temp2[temp2$Year > (year_start-1) & temp2$Year < (year_end+1),]     # limits years to only those for which resin ducts were sampled

unstandardized <- read.csv(file3)     # read in .csv with unstandardized metrics
ducts_all <- merge(unstandardized, temp3)     # merge the ring width data (RW_mm) with unstandardized duct metrics data
ducts_all$Ring_Area_mm2 <- ducts_all$RW_mm * core_width     # calculates ring area in mm^2 using core width and ring width
ducts_all$Rel_Duct_Area <- (ducts_all$Total_Duct_Area/ducts_all$Ring_Area_mm2)*100     # calculates relative duct area
ducts_all$Duct_Density <- ducts_all$Duct_Production/ducts_all$Ring_Area_mm2     # calculates duct density

write.csv(ducts_all, newfile3, row.names=FALSE)     # write out final file containing 5 resin duct metrics as well as corresponding ring widths (mm) and ring areas (mm^2)

##########################################################################################################################################################################
