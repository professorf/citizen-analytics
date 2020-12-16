#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)    }
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen) }
if (require("RColorBrewer")==F) { install.packages("RColorBrewer")          ; library(RColorBrewer)}
#
#
# PLOT MULTIPLE
#
#
#
# Choose a dataset to analyze & get filename
#
Folder   = "data"
Files    = dir(Folder, "*.csv")
DataType = "confirmed"                          # Options: confirmed | deaths
Region   = "global"                                 # USA (in world scripts, this is "global")
FileIndex=grep(sprintf("%s_%s", DataType, Region), Files, ignore.case=T)
FileName=Files[FileIndex]                        # Full filename

#
# Read fileName into data frame & read other important data sets
#

dfOriginal = read.csv(sprintf("%s/%s"                   , Folder, FileName)) # Original JHU data
dfClean     = cleanData  (dfOriginal, Region)
dfDaily    = createDaily(dfClean)
dfRange    = getRange(dfDaily, StartDate="2020-3-1", EndDate="2020-12-31") 

MultipleCountries=c("United Kingdom", "Germany", "Italy", "Greece") # Input: List of countries

#
# Do some annotations
#
# Annotation Dates
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
# Annotation Labels
AnnotateLabel=c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
# Create a dataframe of annotationis
dfAnnotation    = data.frame(AnnotateDate, AnnotateLabel)

plotState(dfRange, MultipleCountries, Region, DataType, dfAnnotation)
