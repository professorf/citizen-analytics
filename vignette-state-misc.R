#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
if (require("RColorBrewer")==F) { install.packages("RColorBrewer")          ; library(RColorBrewer)}
#
# Choose a dataset to analyze & get filename
#
DataType = "deaths"                             # Options: confirmed | deaths
Region   = "US"                                 # USA (in world scripts, this is "global")
Files    = dir(Folder, "*.csv")
Folder   = "data"
FilePatt=grep(sprintf("%s_%s", DataType, Region), Files, ignore.case=T)
FileName=Files[FilePatt]                        # Full filename

#
# Read fileName into data frame & read other important data sets
#

dfOrig=read.csv(sprintf("%s/%s"                   , Folder, FileName)) # Original JHU data
df  = cleanUSData  (dfOrig)
dfd = createUSDiffs(df)

#
# Set up accumulators for population density and total per million and totals
#
cPopuDens=c()   # Cumulative population density
cOverallPerMillion=c()   # Cumulative per million
cTotal=c()   # Cumulative total deaths (or confirmed)
cLastVal=c()   # Cumulative last daily death (or confirmed)
cLastValPerMillion=c() # Cumulative last daily death (or confirmed) per million
cStateArea=c()    # Cumulative area
cStatePopulation=c()    # Cumulative population
#
# Now plot all states
#
for (State in States50$State) {
  RetVal = summarizeState(dfd, State)

  # Update Accmulators
  cPopuDens=c(cPopuDens, RetVal$PopulationDensity)       # Accumulated population densities
  cTotal=c(cTotal, RetVal$Total)       # Accumulated total (deaths or cases)
  cLastVal=c(cLastVal, RetVal$LastVal) # Accumulated last daily totals
  cLastValPerMillion=c(cLastValPerMillion, RetVal$LastValPerMillion)
  cOverallPerMillion=c(cOverallPerMillion, RetVal$OverallPerMillion)       # Accumulated per million (deaths or case)
  cStateArea =c(cStateArea, RetVal$StateArea)      # Accumulated areas
  cStatePopulation =c(cStatePopulation, RetVal$StatePopulation)      # Accumulated population
}
#
# Plot state current deaths barplot
#

#
# Select colors
#
parcol=sapply(States50$GovParty, function(x) {if (x=="R") "pink" else "lightblue"})

# daily deaths
xoff=barplot(cLastVal, names=States50$StateCode, ylim=c(0,max(cLastVal)+100), col=parcol, cex.names=0.75)
text(xoff,cLastVal,cLastVal,cex=0.75,pos=3)
title(sprintf("U.S.: State daily %s on %s\nTotal on %s: %d", DataType, RetVal$LastDate, RetVal$LastDate, sum(cLastVal)))

# daily deaths per million
xoff=barplot(cLastValPerMillion, names=States50$StateCode, ylim=c(0,max(cLastValPerMillion)+10), col=parcol, cex.names=0.75)
text(xoff,cLastValPerMillion,round(cLastValPerMillion, 1),cex=0.75,pos=3)
title(sprintf("U.S.: State daily %s on %s (per million)", DataType, RetVal$LastDate))

xoff=barplot(cTotal, names=States50$StateCode, ylim=c(0,max(cTotal)+5000), col=parcol, cex.names=0.75, border=parcol)
text(xoff,cTotal,cTotal,cex=0.75,pos=3)
title(sprintf("U.S.: Overall %s as of %s\nTotal on %s: %d", DataType, RetVal$LastDate, RetVal$LastDate, sum(cTotal)))

# deaths per million
xoff=barplot(cOverallPerMillion, names=States50$StateCode, ylim=c(0,max(cOverallPerMillion)+500), col=parcol, cex.names=0.75, border=parcol)
text(xoff,cOverallPerMillion,round(cOverallPerMillion),cex=0.75,pos=3)
title(sprintf("U.S.: Overall %s (per million) as of %s\nTotal on %s: %d", DataType, RetVal$LastDate, RetVal$LastDate, sum(cTotal)))

# Calculate percentages R vs D
Rrows=which(States50$GovParty=="R")
Drows=which(States50$GovParty=="D")
Rdths=sum(cLastVal[Rrows])
Ddths=sum(cLastVal[Drows])
Tdths=sum(cLastVal)
print(sprintf("R deaths: %d (%.2f)%%; D deaths: %d (%.2f)%%", Rdths, Rdths/Tdths*100, Ddths, Ddths/Tdths*100))
