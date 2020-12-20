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
iLastDay=which(names(YVals)==StateInfo$LastDate)
lines(c(NVals  , NVals  ), c(0,StateInfo$LastVal  ), lty=2)
text(iLastDay, StateInfo$LastVal, pos=2, sprintf("Last: %s\n%s", formatC(StateInfo$LastVal, big.mark=","), StateInfo$LastDate))

#
# Start of prediction code. Change the DaysForward variable as needed
#
DaysForward = 60                         # Number of days to predict after cutoff date
PredictDate = format(as.Date(DateN, # The date you want to predict
              format="%m/%d/%y")+DaysForward, "%m/%d/%y")

#
# Model after guessing initial parameters: peak=MaxDayVal, shift=iMaxDay, fat=2, lift=0
#
dfCurveToFit = data.frame(x=1:length(YVals),y=YVals)
CurveModel   = nls(y~peak/10^((log(x/shift)/log(fat))^2)+lift, dfCurveToFit,  
               start=list(peak=StateInfo$MaxDayVal, shift=iMaxDay,fat=2,lift=0), trace=T) 

# Plot residuals to check quality of model
#Residuals=resid(CurveModel)
#hist(Residuals) # Want a normal distribution

#
# Plot fitted curve (w/out prediction just as a check)
#
FittedVals=predict(CurveModel)                 # predicted values for the first part of the curve
lines(dfCurveToFit$x,FittedVals)
summary(CurveModel)

#
#
# Now predict forward 
#
#
iPredictDate=dfCurveToFit$x[length(dfCurveToFit$x)]+DaysForward # INDEX of the predicted date

# Plot the original graph BUT with an EXPANDED xlim
plot(YVals, ylim=c(0,StateInfo$MaxDayVal), xlim=c(0, iPredictDate), 
     ylab="# New Cases", xlab="", xaxt="n")
# Plot the running 7-day average
lines(7:length(StateInfo$StateVals), StateInfo$Avg7, col="lightgreen")

# Create labels for the x-axis
DateLabels=names(YVals)    # Start with existing labels
for (i in 1:DaysForward) { # Add predicted labels
  DateLabels=c(DateLabels,format(as.Date(DateN, format="%m/%d/%y")+i, "%m/%d/%y"))
}

# Label the x-axis
axis(1, at=1:length(DateLabels), las=2, labels=DateLabels, cex.axis=0.5)

#
# Draw the fitted graph first
#
lines(dfCurveToFit$x,FittedVals)                       

# Annotate graph with Peak, Last, & Predicted values
lines(c(iMaxDay,iMaxDay),c(0,StateInfo$MaxDayVal), col="lightblue" , lty=2)
text(iMaxDay,StateInfo$MaxDayVal, pos=1, col="lightblue" , sprintf("Peak: %s\n%s", formatC(StateInfo$MaxDayVal, big.mark=","), StateInfo$MaxDate))
lines(c(NVals  , NVals  ), c(0,StateInfo$LastVal), col="gray", lty=2)
iLastDay=which(names(YVals)==StateInfo$LastDate)
text(iLastDay, StateInfo$LastVal, pos=4, col="gray", sprintf("Last: %s\n%s", formatC(StateInfo$LastVal, big.mark=","), StateInfo$LastDate))

# Predict from the last date to the predict date
iPredictRange=(max(dfCurveToFit$x)):iPredictDate           # Need last point to draw properly         
PredictVals=predict(CurveModel, data.frame(x=iPredictRange))

# Draw the predicted graph
lines(iPredictRange, PredictVals, col="pink")
title(sprintf("%s - COVID-19 New Cases (%s)\n%s-%s", State, DataType, names(StateInfo$StateVals[1]), StateInfo$LastDate))

#
# Label the predicted graph
#
LastPredictedVal=round(PredictVals[length(PredictVals)])
totaldeaths=round(sum(FittedVals)+sum(PredictVals[2:length(PredictVals)])) # 2 since PredictVals[1]-is FittedVals[N]
lines(c(iPredictDate, iPredictDate), c(0,LastPredictedVal), col="red", lty=2)
text(iPredictDate, LastPredictedVal, pos=2, col="red", sprintf("Predicted: %s\n%s", formatC(LastPredictedVal, big.mark=","), PredictDate))

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
