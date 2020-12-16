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
dfc=cleanData(dfo, rtype)
dfd=createDaily(dfc)
dft=getRange(dfd, StartDate="2020-6-1", EndDate="2020-12-31")

OneCountry="US"

CountryInfo=plotState(dft, OneCountry, rtype, dtype)
