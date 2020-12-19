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
PerMillion=F
#RetVal = plotState(dfRange, State, Region, DataType, dfAnnotation, PerMil=PerMillion)
StateInfo=summarizeState(dfRange, State, Region)
##########
#
# Finally predict the curve that fits the points
#
##########
#
# First plot the points of the curve
#
dc=StateInfo$StateVals
day0=names(dc[1])
N=length(dc)
dayN=names(dc[N])
#
# Change the variables below for predicting forward
#
dayc=day0      # Cutoff day:           This is the day you want to end the data
dayp=dayN      # Specific Predict day: This is the day you want to actually predict
dayc="12/17/20"  # Force cutoff day to be 
dayp="12/31/20"  # Force predict day to be 

plot(dc, ylab="# New Cases",xlab=sprintf("Days (0=%s, Last: %d=%s)",day0,N-1,dayN)) # Actual curve from start to end
iMaxDay=which(names(dc)==StateInfo$MaxDate)
lines(c(iMaxDay,iMaxDay),c(0,StateInfo$MaxDayVal), lty=2)
title(sprintf("%s - COVID-19 New Cases (%s): %s on %s\nPeak: %s on %s (Day #%d)", State, DataType, StateInfo$LastVal, StateInfo$LastDate, StateInfo$MaxDayVal, StateInfo$MaxDate, iMaxDay))
#
# Guess initial parameters: xoff, peak, shift, fat
#
xoff=1         # generally when the curve starts to rise from zero; 55 for US, 40 for US Deaths
xend=length(dc) # The last point before prediction
xend=which(names(dc)==dayc)
#
dccurve=data.frame(x=xoff:xend,y=dc[xoff:xend])  # Create data frame xoff to end
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
daycposix=as.POSIXct(dayc, tz="", "%m/%d/%y")    # Convert cutoff date to posix
daypposix=as.POSIXct(dayp, tz="", "%m/%d/%y")    # Convert predict date to posix
ddays=as.integer(daypposix-daycposix)-1          # Number of days forward

t=max(dccurve$x)                                 # Start with the last value + 1
lastt=t+ddays                                    # Remember this is w/out xoff

plot(dc, ylim=c(0,StateInfo$MaxDayVal), xlim=c(0, lastt+xoff), # Plot original
     ylab="# New Cases",
     xlab=sprintf("Days (0=%s, Last: %d=%s)",
                  day0,N-1,dayN)
)

lines(dccurve$x+xoff,pred1)                      # Plot lines up to cutoff 

xpredict=(max(dccurve$x)):lastt                  # from prev last t-value to last t-value
pred2=predict(bmodel, data.frame(x=xpredict)) #
# now label
#
# do new predicted values
lines(xpredict+xoff,pred2)
title(sprintf("%s - COVID-19 New Cases (%s): %s on %s\nPeak: %s on %s (Day #%d)", State, DataType, StateInfo$LastVal, StateInfo$LastDate, StateInfo$MaxDayVal, StateInfo$MaxDate, iMaxDay))

#
# Print the date when you reach the cutofflevel
#
cutoffday=lastt+xoff                             # remember lastt is offset
day0posix=as.POSIXct(day0, tz="", "%m/%d/%y")    # Need to convert to posix first
cutoffdate=as.Date(day0posix)+cutoffday          # Finally, the date you reach cutoff
lastval=round(pred2[length(pred2)])
#
# now label
#
text(cutoffday, 0, sprintf("Predicted value %d on: %s", lastval, as.character(cutoffdate)), pos=2)
totaldeaths=round(sum(pred1)+sum(pred2))
