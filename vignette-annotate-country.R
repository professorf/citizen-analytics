#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
#
# Choose a dataset to analyze & get filename
#
Folder   = "data"                                     # Set folder containing datasets
Files    = dir(Folder, "*.csv")                       # Grab all files in that folder
DataType = "confirmed"                                # Set datatype: confirmed | deaths
Region   = "global"                                   # Set region: US | global
FileIndex=grep(sprintf("%s_%s", DataType, Region),    # Get index to file 
                                Files, ignore.case=T)
FileName=Files[FileIndex]                             # Get file name
#
# Read fileName into data frame 
#
dfOriginal = read.csv(sprintf("%s/%s", Folder, FileName)) # Grab original JHU-CSSE dataset
dfClean     = cleanData  (dfOriginal, Region)                  # Collapse state-counties into a single row
dfDaily    = createDaily(dfClean)                              # Create daily values
dfRange    = getRange(dfDaily, StartDate = "2020-1-1",        # Limit data range 
                       EndDate = "2020-12-31")  
#
# Annotate Country
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
dfAnnotation  = data.frame(AnnotateDate, AnnotateLabel)
#
# Set country and plot
#
Country = "US" # Replace this with another state
RetVal = plotState(dfRange, Country, Region, DataType, dfAnnotation)
#
# Save country to folder (pics)
#
dev.copy(png, sprintf("pics/%s-%s.png",DataType,State), width=1280, height=720)
dev.off()
