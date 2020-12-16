#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
#
# Choose a dataset to analyze & get filename
#
Folder    = "data"                                    # Replace "data' if datasets in diff folder
Files     = dir(Folder, "*.csv")                      # Grab all files in that folder
DataType  = "deaths"                                  # Set data type: confirmed | deaths
Region    = "US"                                      # Set region USA (vs "global")
FileIndex = grep(sprintf("%s_%s", DataType, Region),  # Find file index based on pattern 
                               Files, ignore.case=T)
FileName  = Files[FileIndex]                          # Get filename
#
# Read file into data frame 
#
dfOriginal = read.csv(sprintf("%s/%s", Folder, FileName)) # Grab original JHU-CSSE data
dfClean    = cleanData  (dfOriginal, Region)              # Collapse all states/counties into single row  
dfDaily    = createDaily(dfClean)                         # Create state/countries & values
dfRange    = getRange(dfDaily, StartDate = "2020-1-1",    # Limit date range
                               EndDate   = "2020-12-31")  

#
# Set a state, then plot it
#
State = "New Mexico"                                 # Replace this with other state
RetVal = plotState(dfRange, State, Region, DataType) # Plot it

# Save to folder (statepics)
dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
dev.off()
