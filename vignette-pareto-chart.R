#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
#
# Choose a dataset to analyze & get filename
#
Folder    = "data"                                      # Set folder containings datasets
Files     = dir(Folder, "*.csv")                        # Grab all files in that folder
DataType  = "deaths"                                 # Set type of dataset: confirmed | deaths
Region    = "US"                                        # Set region: US (vs "global")
FileIndex = grep(sprintf("%s_%s", DataType, Region),    # Get the index to the dataset 
                                  Files, ignore.case=T)
FileName  = Files[FileIndex]                            # Get filename

#
# Read fileName into data frame
#
dfOriginal = read.csv(sprintf("%s/%s", Folder, FileName)) # Get original JHU-CSSE dataset
dfClean    = cleanData  (dfOriginal, Region)              # Collapse state-counties into single row
dfDaily    = createDaily(dfClean)                         # Create daily values
dfRange    = getRange(dfDaily, StartDate = "2020-1-22",    # Limit data range
                               EndDate = "2020-12-22")  
#
# Create annotations
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22", "2020-12-21")
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall", "Winter")
dfAnnotation  = data.frame(AnnotateDate, AnnotateLabel)
#
# Set state and plot it
#
State = "New Mexico" # Replace this with another state
PerMillion=T
RetVal = plotState(dfRange, State, Region, DataType, dfAnnotation, PerMil=PerMillion)
#
# Save plot to folder (statepics)
#
dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
dev.off()
#
# Pareto chart of major contributors to a range
#
StateSums=rowSums(dfRange[2:length(dfRange)])
StartDate=names(dfRange)[2]
EndDate=names(dfRange)[length(dfRange)]
StartDate=gsub("X", "", gsub("[.]", "/", StartDate))
EndDate=gsub("X", "", gsub("[.]", "/", EndDate))
names(StateSums)=dfRange$State  
SortedStateSums=sort(StateSums,decreasing=T)
TotalSum=sum(StateSums)
MaxY=max(SortedStateSums)
XVals=barplot(SortedStateSums, las=2, cex.names=0.5, ylim=c(0,MaxY+.1*MaxY), 
              main=sprintf("Pareto Plot: %s (%s-%s)", "United States+Territories", 
                           StartDate, EndDate))
text(XVals,SortedStateSums, pos=3, format(SortedStateSums, big.mark=","), cex=0.5)
ParetoXVals=XVals
ParetoYVals=c()
Level80 = -1
iLevel80 = 0
RunningTotal=0
for (i in 1:length(SortedStateSums)) {
  RunningTotal=RunningTotal+SortedStateSums[i]
  PercentTotal=RunningTotal/TotalSum
  ScaledTotal = RunningTotal/TotalSum * MaxY
  if (PercentTotal>.80 && Level80<0) {
    Level80=ScaledTotal
    iLevel80=i
  }
  ParetoYVals=c(ParetoYVals, ScaledTotal)
}
lines(ParetoXVals, ParetoYVals)
lines(c(ParetoXVals[1], ParetoXVals[length(ParetoXVals)]), c(Level80, Level80), lty=2)
lines(c(ParetoXVals[iLevel80], ParetoXVals[iLevel80]), c(0, MaxY), lty=2)
#lines(XVals, SortedStateSums/TotalSum*MaxY)

