#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
#
# Choose a dataset to analyze & get filename
#
Folder    = "data"                                   # Replace "data' if datasets in diff folder
Files     = dir(Folder, "*.csv")                     # Grab all files in the folder
DataType  = "deaths"                              # Set data type: confirmed | deaths
Region    = "global"                                 # Set region USA (vs "global")
FileIndex = grep(sprintf("%s_%s", DataType, Region), # Create file pattern to look for 
                               Files, ignore.case=T)
FileName  = Files[FileIndex]                         # Get filename
#
# Read file into data frame
#
dfOriginal=read.csv(sprintf("%s/%s",Folder, FileName)) # Grab original JHU-CSSE dataset
dfClean=cleanData(dfOriginal, Region)                  # Collapse all country county's into single row
dfDaily=createDaily(dfClean)                           # Create daily values
dfRange=getRange(dfDaily, StartDate="2020-1-1",        # Limit date range
                          EndDate="2020-12-31")
#
# Set a country, then plot it
#
Country = "US" # Note: Countries are just nation states, replace this with other countries
CountryInfo=plotState(dfRange, Country, Region, DataType)
#
# Save to a folder (pics)
#
dev.copy(png, sprintf("pics/%s-%s.png",DataType,State), width=1280, height=720)
dev.off()
