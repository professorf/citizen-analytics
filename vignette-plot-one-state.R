#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
#
# Choose a dataset to analyze & get filename
#
Folder   = "data"
Files    = dir(Folder, "*.csv")
DataType = "deaths"                             # Options: confirmed | deaths
Region   = "US"                                 # USA (in world scripts, this is "global")
FilePatt=grep(sprintf("%s_%s", DataType, Region), Files, ignore.case=T)
FileName=Files[FilePatt]                        # Full filename

#
# Read fileName into data frame & read other important data sets
#

dfOrig = read.csv(sprintf("%s/%s"                   , Folder, FileName)) # Original JHU data
df     = cleanData  (dfOrig, Region)
dfd    = createDaily(df)
dft    = getRange(dfd, StartDate = "2020-10-1", EndDate = "") # Go to the last date 

State = "New Mexico" # Replace this with another state
RetVal = plotState(dft, State, Region, DataType)
dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
dev.off()
