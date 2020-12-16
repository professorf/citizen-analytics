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
DataType = "confirmed"                          # Options: confirmed | deaths
Region   = "global"                                 # USA (in world scripts, this is "global")
FilePatt=grep(sprintf("%s_%s", DataType, Region), Files, ignore.case=T)
FileName=Files[FilePatt]                        # Full filename

#
# Read fileName into data frame & read other important data sets
#

dfOrig = read.csv(sprintf("%s/%s"                   , Folder, FileName)) # Original JHU data
df     = cleanData  (dfOrig, Region)
dfd    = createDaily(df)
dft    = getRange(dfd, StartDate="2020-1-1", EndDate="2020-12-31") 

#
# Create a list of just the G20 Countries
#
Countries=G20$Country

#
# Do some annotations
#
# Annotation Dates
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
# Annotation Labels
AnnotateLabel=c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
# Create a dataframe of annotationis
dfa    = data.frame(AnnotateDate, AnnotateLabel)

#
# Now plot all G20
#
for (Country in Countries) {
  plotState(dft, Country, Region, DataType, dfa)
  dev.copy(png, sprintf("pics/%s-%s.png",DataType,Country), width=1280, height=720)
  dev.off()
}