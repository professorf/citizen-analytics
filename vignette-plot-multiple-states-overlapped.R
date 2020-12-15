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
Region   = "US"                                 # USA (in world scripts, this is "global")
FilePatt=grep(sprintf("%s_%s", DataType, Region), Files, ignore.case=T)
FileName=Files[FilePatt]                        # Full filename

#
# Read fileName into data frame & read other important data sets
#

dfOrig = read.csv(sprintf("%s/%s"                   , Folder, FileName)) # Original JHU data
df     = cleanUSData  (dfOrig)
dfd    = createUSDiffs(df)
dft    = getRange(dfd, StartDate="2020-8-1", EndDate="2020-12-31") 

# Extract the State Total Data & Calculate Daily Changes
#cs=c("New York", "Texas", "Florida", "Arizona")            # Input: List of states
MultipleStates=c("New York", "Texas", "Florida", "California", "New Mexico") # Input: List of states

#
# Do some annotations
#
# Annotation Dates
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
#AnnotateDate=c("2020-3-19", "2020-6-20", "2020-9-22")
# Annotation Labels
AnnotateLabel=c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
#AnnotateLabel=c("Spring", "Summer", "Fall")
# Create a dataframe of annotationis
dfa    = data.frame(AnnotateDate, AnnotateLabel)

plotState(dft, MultipleStates, DataType, dfa)
