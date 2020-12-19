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
dfRange    = getRange(dfDaily, StartDate = "2020-9-1",    # Limit data range
                      EndDate = "2020-12-31")  
#
# Create annotations
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
dfAnnotation  = data.frame(AnnotateDate, AnnotateLabel)
#
# Set state and plot it
#
State = "New Mexico" # Replace this with another state
#PerMillion=F
#RetVal = plotState(dfRange, State, Region, DataType, dfAnnotation, PerMil=PerMillion)
StateInfo=summarizeState(dfRange, State, Region)
##########
#
# Finally predict the curve that fits the points
#
##########
#
# First plot the points of the original curve
#
YVals=StateInfo$StateVals
Date1=names(YVals[1])
NVals=length(YVals)
DateN=names(YVals[NVals])
plot(YVals, ylab="# New Cases",xlab=sprintf("Days (0=%s, Last: %d=%s)",Date1,NVals-1,DateN)) # Actual curve from start to end
#
# Change the variables below for predicting forward
#
DaysForward = 30
CutoffDate  = DateN      # Cutoff day:           This is the day you want to end the data
PredictDate = format(as.Date(CutoffDate, format="%m/%d/%y")+DaysForward, "%m/%d/%y")
#
# Draw a vertical line for peak day & label it
#
iMaxDay=which(names(YVals)==StateInfo$MaxDate)
lines(c(iMaxDay, iMaxDay), c(0,StateInfo$MaxDayVal), lty=2)
text(iMaxDay, StateInfo$MaxDayVal, pos=1, sprintf("Peak: %s\n%s", formatC(StateInfo$MaxDayVal, big.mark=","), StateInfo$MaxDate))
#
# Draw a vertical line for last day & label it
#
iLastDay=which(names(YVals)==StateInfo$LastDate)
lines(c(NVals  , NVals  ), c(0,StateInfo$LastVal  ), lty=2)
text(iLastDay, StateInfo$LastVal, pos=2, sprintf("Last: %s\n%s", formatC(StateInfo$LastVal, big.mark=","), StateInfo$LastDate))
#
# Title the graph
title(sprintf("%s - COVID-19 New Cases (%s)\n%s-%s", State, DataType, names(StateInfo$StateVals[1]), StateInfo$LastDate))
#
# Guess initial parameters: xoff, peak, shift, fat
#
xoff=1         # generally when the curve starts to rise from zero; 55 for US, 40 for US Deaths
xend=length(YVals) # The last point before prediction
xend=which(names(YVals)==CutoffDate)
#
dccurve=data.frame(x=xoff:xend,y=YVals[xoff:xend])  # Create data frame xoff to end
dccurve$x=dccurve$x-xoff                                     # Shift xvals so that xoff=0
bmodel = nls(y~peak/10^((log(x/shift)/log(fat))^2), dccurve,  
             start=list(peak=StateInfo$MaxDayVal, shift=iMaxDay,fat=2),         # Confirmed: 30000, 15, 15; Deaths: 2500,15,5 for US
             trace=T) 

rez=resid(bmodel)
#
# Plot predicted curve
#
pred1=predict(bmodel)                 # predicted values for the first part of the curve
lines(dccurve$x+xoff,pred1)           #
#summary(bmodel)

#
# Now predict to a certain date
#
daycposix=as.POSIXct(CutoffDate, tz="", "%m/%d/%y")    # Convert cutoff date to posix
daypposix=as.POSIXct(PredictDate, tz="", "%m/%d/%y")    # Convert predict date to posix
ddays=as.integer(daypposix-daycposix)-1          # Number of days forward

t=max(dccurve$x)                                 # Start with the last value + 1
lastt=t+ddays                                    # Remember this is w/out xoff

plot(YVals, ylim=c(0,StateInfo$MaxDayVal), xlim=c(0, lastt+xoff), # Plot original
     ylab="# New Cases",
     xlab=sprintf("Days (0=%s, Last: %d=%s)",
                  Date1,NVals-1,DateN),
     xaxt="n"
)

DateLabels=names(dc)
# Add predicted labels
for (i in 1:DaysForward) {
  DateLabels=c(DateLabels,format(as.Date(CutoffDate, format="%m/%d/%y")+i, "%m/%d/%y"))
}

axis(1, at=1:length(DateLabels), las=2, labels=DateLabels, cex.axis=0.5)

lines(dccurve$x+xoff,pred1)                      # Plot lines up to cutoff 
lines(c(iMaxDay,iMaxDay),c(0,StateInfo$MaxDayVal), col="lightblue" , lty=2)
text(iMaxDay,StateInfo$MaxDayVal, pos=1, col="lightblue" , sprintf("Peak: %s\n%s", formatC(StateInfo$MaxDayVal, big.mark=","), StateInfo$MaxDate))
lines(c(NVals  , NVals  ), c(0,StateInfo$LastVal), col="gray", lty=2)
iLastDay=which(names(YVals)==StateInfo$LastDate)
text(iLastDay, StateInfo$LastVal, pos=4, col="gray", sprintf("Last: %s\n%s", formatC(StateInfo$LastVal, big.mark=","), StateInfo$LastDate))

xpredict=(max(dccurve$x)):lastt                  # from prev last t-value to last t-value
pred2=predict(bmodel, data.frame(x=xpredict)) #
# now label
#
# do new predicted values
lines(xpredict+xoff,pred2, col="pink")
title(sprintf("%s - COVID-19 New Cases (%s)\n%s-%s", State, DataType, names(StateInfo$StateVals[1]), StateInfo$LastDate))

#
# Print the date when you reach the cutofflevel
#
cutoffday=lastt+xoff                             # remember lastt is offset
day0posix=as.POSIXct(Date1, tz="", "%m/%d/%y")    # Need to convert to posix first
cutoffdate=as.Date(day0posix)+cutoffday          # Finally, the date you reach cutoff
lastval=round(pred2[length(pred2)])
#
# now label
#
#text(cutoffday, 0, col="red", sprintf("Predicted value %s on: %s", formatC(lastval,big.mark=","), as.character(cutoffdate)), pos=2)
totaldeaths=round(sum(pred1)+sum(pred2))
#text(iMaxDay,StateInfo$MaxDayVal, pos=1, col="gray", sprintf("Peak: %s\n%s", formatC(StateInfo$MaxDayVal, big.mark=","), StateInfo$MaxDate))
lines(c(cutoffday+1, cutoffday+1), c(0,lastval), col="red", lty=2)
text(cutoffday+1, lastval, pos=2, col="red", sprintf("Predicted: %s\n%s", formatC(lastval, big.mark=","), PredictDate))
