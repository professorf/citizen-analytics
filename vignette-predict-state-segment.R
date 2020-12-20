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
dfRange    = getRange(dfDaily, StartDate = "2020-3-1",    # Limit data range
                      EndDate = "2020-9-1")  
#
# Create annotations
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
dfAnnotation  = data.frame(AnnotateDate, AnnotateLabel)

#
# Set state and get info
#
State     = "New Mexico" # Replace this with another state
StateInfo = summarizeState(dfRange, State, Region)

##########
#
# Finally predict the curve that fits the points
#
##########
#
# First plot the points of the original curve
#
YVals = StateInfo$StateVals # All the values in the range
Date1 = names(YVals[1])     # First date
NVals = length(YVals)       # Number of values between first & last date inclusive
DateN = names(YVals[NVals]) # Last Date
plot(YVals, xaxt="n", ylab="# New Cases",xlab=sprintf("Days (0=%s, Last: %d=%s)",Date1,NVals-1,DateN)) # Actual curve from start to end

# Plot the running 7-day average
lines(7:length(StateInfo$StateVals), StateInfo$Avg7, col="lightgreen")

# Add date labels to the axis, otherwise it's the index numbers
DateLabels=names(YVals)
axis(1, at=1:length(DateLabels), las=2, labels=DateLabels, cex.axis=0.5)

# Title the graph
title(sprintf("%s - COVID-19 New Cases (%s)\n%s-%s", State, DataType, names(StateInfo$StateVals[1]), StateInfo$LastDate))

# Annotate peak date
iMaxDay=which(names(YVals)==StateInfo$MaxDate)
lines(c(iMaxDay, iMaxDay), c(0,StateInfo$MaxDayVal), lty=2)
text(iMaxDay, StateInfo$MaxDayVal, pos=1, sprintf("Peak: %s\n%s", formatC(StateInfo$MaxDayVal, big.mark=","), StateInfo$MaxDate))

# Annotate last date
iLastDate=which(names(YVals)==StateInfo$LastDate)
lines(c(NVals  , NVals  ), c(0,StateInfo$LastVal  ), lty=2)
text(iLastDate, StateInfo$LastVal, pos=2, sprintf("Last: %s\n%s", formatC(StateInfo$LastVal, big.mark=","), StateInfo$LastDate))

#
# Start of segment prediction code. 
# Similar to A-B looping, you have an A-Date and a B-Date
# DaysForward variable is relative to the B-Date
#
ADate       = "3/10/20" # Date1          # The start of the segment to curve fit
BDate       = "6/1/20"  # DateN          # The end date of the segment to curve fit
iADate      = which(names(YVals)==ADate) #
iBDate      = which(names(YVals)==BDate) #

DaysForward = 30                         # Number of days to predict after B date
PredictDate = format(as.Date(BDate,      # The date you want to predict
              format="%m/%d/%y")+DaysForward, "%m/%d/%y")

#
# Model after guessing initial parameters: peak=MaxDayVal, shift=iMaxDay, fat=2, lift=0
#
dfCurveToFit=data.frame(x=iADate:iBDate,y=YVals[iADate:iBDate])  # Create data frame iModelStartDate to end
MaxABVal=max(dfCurveToFit$y)
iMaxABVal=which(dfCurveToFit$y==MaxABVal)
dfCurveToFit$x=dfCurveToFit$x-iADate # Make first value 0
CurveModel = nls(y~peak/10^((log(x/shift)/log(fat))^2), dfCurveToFit,  
             start=list(peak=MaxABVal, shift=iMaxABVal,fat=2),         # Confirmed: 30000, 15, 15; Deaths: 2500,15,5 for US
             trace=T) 
dfCurveToFit$x=iADate:iBDate         # Restore values so things plot properly
# Residuals=resid(CurveModel)
# hist(Residuals) # Want a normal distribution

#
# Plot fitted curve
#
FittedVals=predict(CurveModel)                 # predicted values for the first part of the curve
lines(dfCurveToFit$x,FittedVals)
summary(CurveModel)

#
# Now predict to a certain date
#
iPredictDate=dfCurveToFit$x[length(dfCurveToFit$x)]+DaysForward 
iMaxX=ifelse(iLastDate>iPredictDate, iLastDate, iPredictDate)
# Plot the original graph BUT with an EXPANDED xlim
plot(YVals, 
     ylim=c(0,StateInfo$MaxDayVal), xlim=c(0, iMaxX),
     ylab="# New Cases", xlab=sprintf("Days (0=%s, Last: %d=%s)", Date1,NVals-1,DateN),
     xaxt="n"
)

# Plot the running 7-day average
lines(7:length(StateInfo$StateVals), StateInfo$Avg7, col="lightgreen")

# Create labels for the x-axis
DateLabels=names(YVals)    # Start with existing labels
if (iPredictDate>iLastDate) {
  DaysToAdd=iPredictDate-iLastDate
  for (i in 1:DaysToAdd) { # Add predicted labels
    DateLabels=c(DateLabels,format(as.Date(DateN, format="%m/%d/%y")+i, "%m/%d/%y"))
  }
}

# Label the x-axis
axis(1, at=1:length(DateLabels), las=2, labels=DateLabels, cex.axis=0.5)

# Draw the fitted graph
lines(dfCurveToFit$x,FittedVals)                      # Plot lines up to cutoff 

# Annotate graph with Peak, Last, & Predicted values
lines(c(iMaxDay,iMaxDay),c(0,StateInfo$MaxDayVal), col="lightblue" , lty=2)
text(iMaxDay,StateInfo$MaxDayVal, pos=1, col="lightblue" , sprintf("Peak: %s\n%s", formatC(StateInfo$MaxDayVal, big.mark=","), StateInfo$MaxDate))
lines(c(NVals  , NVals  ), c(0,StateInfo$LastVal), col="gray", lty=2)
iLastDate=which(names(YVals)==StateInfo$LastDate)
text(iLastDate, StateInfo$LastVal, pos=2, col="gray", sprintf("Last: %s\n%s", formatC(StateInfo$LastVal, big.mark=","), StateInfo$LastDate))

# Predict beyond the cutoff date to the predict date
iPredictRange=iBDate:iPredictDate           # Need last point to draw properly   
iPredictRange=iPredictRange-iADate          # Normalize to zero
PredictVals=predict(CurveModel, data.frame(x=iPredictRange))
iPredictRange=iBDate:iPredictDate           # Restore original x range so plots properly   

# Draw the predicted graph
lines(iPredictRange, PredictVals, col="pink")
title(sprintf("%s - COVID-19 New Cases (%s)\n%s-%s", State, DataType, names(StateInfo$StateVals[1]), StateInfo$LastDate))

#
# Label the predicted graph
#
totaldeaths=round(sum(FittedVals)+sum(PredictVals))
LastPredictVal=round(PredictVals[length(PredictVals)])
lines(c(iPredictDate, iPredictDate), c(0,LastPredictVal), col="red", lty=2)
text(iPredictDate, LastPredictVal, pos=2, col="red", sprintf("Predicted: %s\n%s", formatC(lastval, big.mark=","), PredictDate))

#
# Finally, do any last minute annotations
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22")
adDateLabels   = as.Date(DateLabels, format="%m/%d/%y")
adAnnotateDate = as.Date(AnnotateDate)
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall")
XValList=sapply(adAnnotateDate, function (date) {which (adDateLabels==date)})
for (i in 1:length(XValList)) {
  XVal=XValList[[i]]
  if (length(XVal)>0) {
    lines(c(XVal, XVal), c(0,StateInfo$MaxDayVal), lty=2, col="lightgray")
    text(XVal, StateInfo$MaxDayVal, pos=1, col="lightgray", sprintf("%s\n%s", AnnotateLabel[i], AnnotateDate[i]))
  }
}
