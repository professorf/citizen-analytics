#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
#
# Get a data file 
#
folder="data"
files=dir(folder, "*.csv")
dtype="confirmed"                          # INPUT: confirmed | deaths | recovered
percapita=F
rtype="global"                             # INPUT: global | US
filen=grep(sprintf("%s_%s", dtype, rtype), 
           files, ignore.case=T)
file=files[filen]
#
# Read file into data frame
#
dfo=read.csv(sprintf("%s/%s",folder, file))
dfc=cleanGlobalData(dfo)
dfd=createGlobalDiffs(dfc)
dft=getRange(dfd, StartDate="2020-6-1", EndDate="2020-12-31", DFType="Country")

# Do Annotations
# Annotation Dates
AnnotateDate=c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
# Annotation Labels
AnnotateLabel=c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
# Create a dataframe of annotationis
dfa    = data.frame(AnnotateDate, AnnotateLabel)

OneCountry="US"

CountryInfo=plotCountry(dft, OneCountry, DataType, dfa)
