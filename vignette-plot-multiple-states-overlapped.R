#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)    }
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen) }
if (require("RColorBrewer")==F) { install.packages("RColorBrewer")          ; library(RColorBrewer)}
#
# Choose a dataset to analyze & get filename
#
Folder    = "data"                                   # Replace "data' if datasets in diff folder
Files     = dir(Folder, "*.csv")                     # Grab all files in that folder
DataType  = "confirmed"                              # Set data type: confirmed | deaths
Region    = "US"                                     # Set region USA (vs "global")
FileIndex = grep(sprintf("%s_%s", DataType, Region), # Find file index based on pattern
                 Files, ignore.case=T)
FileName  = Files[FileIndex]                         # Get filename

#
# Read file into data frame
#
dfOriginal = read.csv(sprintf("%s/%s", Folder, FileName)) # Grab original JHU-CSSE data
dfClean    = cleanData  (dfOriginal, Region)              # Collapse states/counties into single row 
dfDaily    = createDaily(dfClean)                         # Create daily values
dfRange    = getRange(dfDaily, StartDate="2020-1-1",      # Limit date range
                      EndDate="2020-12-31") 
#
# Do some annotations
#
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
AnnotateLabel=c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
dfAnnotation    = data.frame(AnnotateDate, AnnotateLabel)
#
# Create a vector of multiple states
#
MultipleStates=c("New York", "Texas", "Florida", "California", "New Mexico")
#
# Plot state
#
plotState(dfRange, MultipleStates, Region, DataType, dfAnnotation)
#
# Save pic to folder (statepics/multiple-states.png)
#
dev.copy(png, sprintf("statepics/multiple-states.png",DataType,State), width=1280, height=720)
dev.off()
