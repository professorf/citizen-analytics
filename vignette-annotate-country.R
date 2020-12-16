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
DataType = "confirmed"                             # Options: confirmed | deaths
Region   = "global"                                 # USA (in world scripts, this is "global")
FilePatt=grep(sprintf("%s_%s", DataType, Region), Files, ignore.case=T)
FileName=Files[FilePatt]                        # Full filename

#
# Read fileName into data frame & read other important data sets
#

dfOrig = read.csv(sprintf("%s/%s"                   , Folder, FileName)) # Original JHU data
df     = cleanData  (dfOrig, Region)
dfd    = createDaily(df)
dft    = getRange(dfd, StartDate = "2020-1-1", EndDate = "2020-12-31") # Go to the last date 

Country = "US" # Replace this with another state
# Annotation Dates
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
AnnotateLabel=c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
dfa    = data.frame(AnnotateDate, AnnotateLabel)
RetVal = plotState(dft, Country, Region, DataType, dfa)
dev.copy(png, sprintf("pics/%s-%s.png",DataType,State), width=1280, height=720)
dev.off()
