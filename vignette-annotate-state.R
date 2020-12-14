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
df     = cleanUSData  (dfOrig)
dfd    = createUSDiffs(df)
dft    = getRange(dfd, StartDate = "2020-8-1", EndDate = "") # Go to the last date 

State = "New York" # Replace this with another state
# Annotation Dates
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-8-26")
# Annotation Labels
AnnotateLabel=c("Thanksgiving", "Halloween", "Colleges Start")
# Create a dataframe of annotationis
dfa    = data.frame(AnnotateDate, AnnotateLabel)

RetVal = plotState(dft, State, DataType)
dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
dev.off()
