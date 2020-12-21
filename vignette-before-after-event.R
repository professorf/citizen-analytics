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
DataType  = "confirmed"                                 # Set type of dataset: confirmed | deaths
Region    = "global"                                    # Set region: US (vs "global")
FileIndex = grep(sprintf("%s_%s", DataType, Region),    # Get the index to the dataset 
                 Files, ignore.case=T)
FileName  = Files[FileIndex]                            # Get filename

#
# Read fileName into data frame
#
dfOriginal = read.csv(sprintf("%s/%s", Folder, FileName)) # Get original JHU-CSSE dataset
dfClean    = cleanData  (dfOriginal, Region)              # Collapse state-counties into single row
dfDaily    = createDaily(dfClean)                         # Create daily values
dfRange    = getRange(dfDaily, StartDate = "2020-1-1",    # Limit data range
                      EndDate = "2020-5-31")  
#
# Create annotations
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
dfAnnotation  = data.frame(AnnotateDate, AnnotateLabel)

#
# Set state and get info
#
State     = "US" # Replace this with another state
StateInfo = plotState(dfRange, State, Region)

#
# ANCOVA: Check if two slopes are equal
#

# Get event date & window
EventDate       = "3/25/20" #"4/9/20"
DaysBeforeAfter = 10 #30

# Convert to date, add DayBeforeAfter, Format back to string
DateStart = format((as.Date(EventDate, format="%m/%d/%y")-DaysBeforeAfter ), "%m/%d/%y")
DateEnd   = format((as.Date(EventDate, format="%m/%d/%y")+DaysBeforeAfter ), "%m/%d/%y")
DateStart = gsub("0([0-9])", "\\1", DateStart)
DateEnd   = gsub("0([0-9])", "\\1", DateEnd)

# Calculate the index to the DateStart & DateEnd
iDateStart = which(names(StateInfo$StateVals)==DateStart)
iDateEnd   = which(names(StateInfo$StateVals)==DateEnd)
iDateMid   = which(names(StateInfo$StateVals)==EventDate)
if (length(iDateStart)==0) { print("Start date not found in data range"); return }
if (length(iDateEnd  )==0) { print("End date not found in data range"  ); return }

# Create the list of values before and after the event, and include the event (@iDateMid)
BeforeVals = StateInfo$StateVals[iDateStart:iDateMid]
AfterVals  = StateInfo$StateVals[iDateMid  :iDateEnd]
AllVals    = c(BeforeVals, AfterVals[2:length(AfterVals)]) # "2" because EventDate value duplicated

# Plot all values in sequence, as a check
plot(AllVals, type="l", col="lightgray", xaxt="n", xlab="Dates", ylab="Values", main="Date Ranges")
axis(1, at=1:length(AllVals), labels=names(AllVals), las=2, cex.axis=.75)
lines(BeforeVals, type="b", pch=15, col="lightblue")
XVals= length(BeforeVals):(length(BeforeVals)+length(AfterVals)-1)
lines(XVals, AfterVals, type="b", pch=17, col="pink")
YMax = max(AllVals)
XMid = length(BeforeVals)
lines(c(XMid, XMid), c(0, YMax), lty=2, col="lightgray")

# Create two series of points that begin at 1
SeriesBeforeX = 1:length(BeforeVals)
SeriesAfterX  = 1:length(AfterVals)
bSequence     = T
if (bSequence) SeriesAfterX  = length(BeforeVals):(length(BeforeVals)+length(AfterVals)-1)

SeriesBeforeY = BeforeVals
SeriesAfterY  = AfterVals
YMax=max(c(SeriesBeforeY, SeriesAfterY))
plot(SeriesBeforeX, SeriesBeforeY, col="blue", pch=15, xlab="Dates", ylab="Cases", 
     ylim=c(0,YMax), xlim=c(0, ifelse(bSequence, max(SeriesAfterX), max(SeriesBeforeX))))
points(SeriesAfterX, SeriesAfterY, col="red", pch=17)
BeforeModel=lm(SeriesBeforeY~SeriesBeforeX)
summary(BeforeModel)
AfterModel=lm(SeriesAfterY~SeriesAfterX)
summary(AfterModel)
lines(SeriesBeforeX,BeforeModel$fitted.values, col="blue")
lines(SeriesAfterX,AfterModel$fitted.values, col="red")
title(sprintf("%s - COVID-19: %s Cases, %s-%s vs %s-%s", State, toupper(DataType), DateStart, EventDate, EventDate, DateEnd))
legend("topright",legend=c("Aug 1-Sep 6", "Sep 7-Sep 30"), col=c("blue", "red"), lty=1)
# Now create on big model
Condition=c(rep("Before",length(BeforeVals)), rep("After",length(AfterVals)))
dfAll=data.frame(x=c(SeriesBeforeX, SeriesAfterX),y=c(SeriesBeforeY, SeriesAfterY),condition=Condition)
ANCOVAModel=lm(y~x+condition+x:condition, dfAll)
summary(ANCOVAModel)
