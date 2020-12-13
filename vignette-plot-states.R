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
dft    = getRange(dfd, "2020-10-1","2020-12-7") 

#
# Create a list of just the 50 states, excluding territories and cruise ships!
#
States=dft$State
FiftyStateRows=which( States!="Grand Princess" 
                    & States!="Diamond Princess" 
                    & States!="District of Columbia"
                    & States!="American Samoa"
                    & States!="Guam"
                    & States!="Northern Mariana Islands"
                    & States!="Virgin Islands"
                    & States!="Puerto Rico")
StatesFifty=States[FiftyStateRows]
# There is also a dataframe in OpenCitizen called States50

#
# Now plot all states
#
for (State in StatesFifty) {
  RetVal = plotState(dft, State, DataType)
  dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
  dev.off()
}
