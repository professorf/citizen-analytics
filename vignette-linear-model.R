#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("RColorBrewer")==F) { install.packages("RColorBrewer")          ; library(RColorBrewer)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
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
  #
  # Update Accmulators
  #
  # (Change to lists)
  cPopuDens=c(cPopuDens, RetVal$PopulationDensity)       # Accumulated population densities
  cTotal=c(cTotal, RetVal$Total)       # Accumulated total (deaths or cases)
  cLastVal=c(cLastVal, RetVal$LastVal) # Accumulated last daily totals
  cLastValPerMillion=c(cLastValPerMillion, RetVal$LastValPerMillion)
  cOverallPerMillion=c(cOverallPerMillion, RetVal$OverallPerMillion)       # Accumulated per million (deaths or case)
  cStateArea =c(cStateArea, RetVal$StateArea)      # Accumulated areas
  cStatePopulation =c(cStatePopulation, RetVal$StatePopulation)      # Accumulated population
}
#
# Linear Model 
#
Population.Density=cPopuDens
Overall.Per.Million=cOverallPerMillion
Total.Area=cStateArea
Total.DC=cTotal

plot(Population.Density, Overall.Per.Million, ylim=c(0,max(Overall.Per.Million)+250)) 
title(sprintf("Covid-19, U.S. States: Population Density vs %s Per Million", toupper(DataType)))
model=lm(Overall.Per.Million~Population.Density)
summary(model)
lines(Population.Density, predict(model))
text(cPopuDens, cOverallPerMillion, OpenCitizen::States50$StateCode, pos=3, cex=0.50)
dev.copy(png, sprintf("statepics/z-%s-linear-model.png",DataType), width=1280, height=720)
dev.off()
hist(residuals(model))
dev.copy(png, sprintf("statepics/z-%s-residuals.png", DataType), width=1280, height=720)
dev.off()
