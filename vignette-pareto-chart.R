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
DataType  = "deaths"                                    # Set type of dataset: confirmed | deaths
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
dfRange    = getRange(dfDaily, StartDate = "2020-12-1",    # Limit data range
                               EndDate = "2020-12-31")  

# Limit data frame to just the Fifty states
States50Rows = which (dfRange$State %in% States50$State)
dfStates50  = dfRange[States50Rows,]
#
# Pareto chart of major contributors to a range
#

# Reduce all states daily values to a single sum
StateSums = rowSums(dfStates50[2:length(dfStates50)])

# Get starting and ending dates of range; format as m/d/y
StartDate   = names(dfStates50)[2]
EndDate     = names(dfStates50)[length(dfStates50)]
StartDate   = gsub("X", "", gsub("[.]", "/", StartDate))
EndDate     = gsub("X", "", gsub("[.]", "/", EndDate))
# Do the Pareto bar plot of the sorted values, after setting various parameters
names(StateSums) = States50$StateCode           # StateSums lack labels, so add state labels
SortedStateSums  = sort(StateSums,decreasing=T) 
StateColors = sapply(names(SortedStateSums), function (StateCode) {
  Row = which (States50$StateCode==StateCode)
  ifelse (States50$GovParty[Row]=="R", "pink", "lightblue")
})
TotalSum         = sum(StateSums)               # Save total of all states
MaxY             = max(SortedStateSums)         # Save MaxY so we can plot properly
XVals            = barplot(SortedStateSums, las=2, cex.axis=.5, cex.names=0.5, ylim=c(0,MaxY+.1*MaxY), 
                           ylab=toupper(DataType), xlab="", col=StateColors, border=StateColors)

# Add values on top of bars
text(XVals,SortedStateSums, pos=3, format(SortedStateSums, big.mark=","), cex=0.5)

# Calculate the y-values of the  Pareto curve, which simply a vector the (stepwise) running total
ParetoXVals  = XVals # The x-values of the pare
ParetoYVals  = c()
Level80      = -1
iLevel80     = 0
RunningTotal = 0
for (i in 1:length(SortedStateSums)) {
  RunningTotal = RunningTotal+SortedStateSums[i] # Calc stepwise sum
  PercentTotal = RunningTotal/TotalSum           # Calc percentage of total sum
  ScaledTotal  = RunningTotal/TotalSum * MaxY    # Scale total for plotting y-axis properly
  if (PercentTotal>.80 && Level80<0) {           # Remember the 80% level and step (i)
    Level80=ScaledTotal
    iLevel80=i
  }
  ParetoYVals=c(ParetoYVals, ScaledTotal)        # Save the Scaled Total for later plotting
}

# Now plot the Pareto curve
box()
title(sprintf("COVID-19 Pareto Chart: %s (%s-%s)\n%d States account for 80%% of the total %s (%s)", 
              "United States", StartDate, EndDate, iLevel80, ifelse (DataType=="confirmed", "cases", "deaths"), format(TotalSum, big.mark=",")))
# Draw the curve
lines(ParetoXVals, ParetoYVals, type="l", lty="dotted", xlab="", ylab="", col="gray")
# Draw the horizontal 80% indicator line
lines(c(ParetoXVals[iLevel80], ParetoXVals[length(ParetoXVals)]), c(Level80, Level80), lty=2, col="gray")
# Draw the vertical 80% indicator line
lines(c(ParetoXVals[iLevel80], ParetoXVals[iLevel80]), c(0, Level80), lty=2, col="gray")
# Add a label indicating 80%
axis(side=4, at=Level80, line=0, las=2, label="80%", cex.axis=.5)
# lines(XVals, SortedStateSums/TotalSum*MaxY) # The non-running total pareto

# Credit JHU-CSSE dataset
legend("topright", "Source: JHU CSSE Dataset", cex=0.50)


