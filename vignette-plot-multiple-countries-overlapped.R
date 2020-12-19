#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)    }
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen) }
if (require("RColorBrewer")==F) { install.packages("RColorBrewer")          ; library(RColorBrewer)}
#
# Choose a dataset to analyze & get filename of that dataset
#
Folder    = "data"                                      # Set Folder where datasets stored
Files     = dir(Folder, "*.csv")                        # Grab all files in that folder
DataType  = "confirmed"                                 # Set type of dataset (confirmed|deaths)
Region    = "global"                                    # Set region to global (vs US)
FileIndex = grep(sprintf("%s_%s", DataType, Region),    # Get file index
                 Files, ignore.case=T)
FileName  = Files[FileIndex]                            # Get filename 
#
# Read file into data frame
#
dfOriginal = read.csv(sprintf("%s/%s", Folder, FileName)) # Get Original JHU-CSSE dataset
dfClean    = cleanData  (dfOriginal, Region)              # Collapse country-counties into single row
dfDaily    = createDaily(dfClean)                         # Calculate daily values
dfRange    = getRange(dfDaily, StartDate="2020-1-1",      # Limit dates 
                      EndDate="2020-12-31") 
#
# Do some annotations
#
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
AnnotateLabel=c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
dfAnnotation    = data.frame(AnnotateDate, AnnotateLabel)
#
# Create a vector of multiple countries
#
MultipleCountries=c("United Kingdom", "Germany", "Italy", "Greece") # Input: List of countries
#
# Plot multiple countries overlapped
#
PerMillion=T
plotState(dfRange, MultipleCountries, Region, DataType, dfAnnotation, PerMillion)
#
# Write pic to folder (pics/multiple-countries.png)
#
dev.copy(png, sprintf("pics/multiple-countries.png"), width=1280, height=720)
dev.off()

