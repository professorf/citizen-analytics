#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
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
dfClean     = cleanData  (dfOriginal, Region)                  # Collapse states/counties into single row 
dfDaily    = createDaily(dfClean)                              # Create daily values
dfRange    = getRange(dfDaily, StartDate="2020-1-1",          # Limit date range
                       EndDate="2020-12-31") 

#
# Create a list of just the 50 states, excluding territories and cruise ships!
#
States=dfRange$State
FiftyStateRows=which( States!="Grand Princess" 
                    & States!="Diamond Princess" 
                    & States!="District of Columbia"
                    & States!="American Samoa"
                    & States!="Guam"
                    & States!="Northern Mariana Islands"
                    & States!="Virgin Islands"
                    & States!="Puerto Rico")
StatesFifty=States[FiftyStateRows]
# There is also a dataframe in OpenCitizen called States50

#
# Do some annotations
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
dfAnnotation  = data.frame(AnnotateDate, AnnotateLabel)

#
# Now plot all states and save them to a folder (statepics)
#
for (State in StatesFifty) {
  RetVal = plotState(dfRange, State, Region, DataType, dfAnnotation)
  dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
  dev.off()
}
