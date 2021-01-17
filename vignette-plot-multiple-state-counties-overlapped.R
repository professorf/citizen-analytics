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
DataType  = "deaths"                                 # Set data type: confirmed | deaths
Region    = "US"                                     # Set region USA (vs "global")
State     = "New Mexico"
FileIndex = grep(sprintf("%s_%s", DataType, Region), # Find file index based on pattern
                 Files, ignore.case=T)
FileName  = Files[FileIndex]                         # Get filename

#
# Read file into data frame
#
dfOriginal = read.csv(sprintf("%s/%s", Folder, FileName)) # Grab original JHU-CSSE data
dfClean    = cleanData  (dfOriginal, Region, State)       # Collapse states/counties into single row 
dfDaily    = createDaily(dfClean)                         # Create daily values
dfRange    = getRange(dfDaily, StartDate="2020-1-1",      # Limit date range
                      EndDate="2021-1-16") 
#
# Do some annotations
#
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22", "2020-12-22")
AnnotateLabel=c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall", "Winter")
dfAnnotation    = data.frame(AnnotateDate, AnnotateLabel)
#
# Create a vector of multiple states
#
MultipleStates=c("Bernalillo", "Santa Fe", "Sandoval", "McKinley", "Taos")
#
# Plot state
#
PerMillion=F
plotState(dfRange, MultipleStates, Region, DataType, dfAnnotation, PerMillion)
#
# Save pic to folder (statepics/multiple-states.png)
#
#dev.copy(png, sprintf("statepics/multiple-states.png"), width=1280, height=720)
#dev.off()
