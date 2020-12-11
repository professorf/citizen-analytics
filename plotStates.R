#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("RColorBrewer")==F) { install.packages("RColorBrewer")          ; library(RColorBrewer)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
#
# Source required functions
#
source("cleanUSData.R")
source("createUSDiffs.R")
source("plotState.R")
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

dfOrig=read.csv(sprintf("%s/%s"                   , Folder, FileName)) # Original JHU data
dfPopu=read.csv(sprintf("%s/jhu-statepop.csv"     , Folder          )) # Population data
dfArea=read.csv(sprintf("%s/census-state-area.csv", Folder          )) # Land area data

df  = cleanUSData  (dfOrig)
dfd = createUSDiffs(df)

States=dfd$State

#
# Create a list of just the 50 states, excluding territories and cruise ships!
#
FiftyStateRows=which( States!="Grand Princess" 
                    & States!="Diamond Princess" 
                    & States!="District of Columbia"
                    & States!="American Samoa"
                    & States!="Guam"
                    & States!="Northern Mariana Islands"
                    & States!="Virgin Islands"
                    & States!="Puerto Rico")
States50=States[FiftyStateRows]

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
# Now plot a specific State
#
for (State in States50) {
  RetVal = plotState(dfd, State, DataType)
  dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
  dev.off()
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
Deaths.Per.Million=cOverallPerMillion
Total.Area=cStateArea
Total.DC=cTotal

plot(Population.Density, Deaths.Per.Million, ylim=c(0,max(Deaths.Per.Million)+250)) 
title("Covid-19, U.S. States: Population Density vs Deaths Per Million")
model=lm(Deaths.Per.Million~Population.Density)
summary(model)
lines(Population.Density, predict(model))
text(cPopuDens, cOverallPerMillion, st2, pos=3, cex=0.50)
dev.copy(png, sprintf("statepics/z-%s-linear-model.png",DataType), width=1280, height=720)
dev.off()
hist(residuals(model))
dev.copy(png, sprintf("statepics/z-%s-residuals.png", DataType), width=1280, height=720)
dev.off()
#plot(model)

#
# Plot state current deaths barplot
#

#
# First create list of state governor political parties 
# Src: wikipedia 9/15/2020, https://en.wikipedia.org/wiki/List_of_United_States_governors)
#
party=c("R", "R", "R", "R", "D", 
        "D", "D", "D", "R", "R",
        "D", "R", "D", "R", "R",
        "D", "D", "D", "D", "R",
        "R", "D", "D", "R", "R",
        "D", "R", "D", "R", "D",
        "D", "D", "D", "R", "R",
        "R", "D", "D", "D", "R",
        "R", "R", "R", "R", "R",
        "D", "D", "R", "D", "R")
st2=c  ("AL","AK","AZ","AR","CA",
        "CO","CT","DE","FL","GA",
        "HI","ID","IL","IN","IA",
        "KS","KY","LA","ME","MD",
        "MA","MI","MN","MS","MO",
        "MT","NE","NV","NH","NJ",
        "NM","NY","NC","ND","OH",
        "OK","OR","PA","RI","SC",
        "SD","TN","TX","UT","VT",
        "VA","WA","WV","WI","WY")
#
# Select colors
#
parcol=sapply(party, function(x) {if (x=="R") "pink" else "lightblue"})

# daily deaths
xoff=barplot(cLastVal, names=st2, ylim=c(0,max(cLastVal)+100), col=parcol, cex.names=0.75)
text(xoff,cLastVal,cLastVal,cex=0.75,pos=3)
title(sprintf("U.S.: State daily %s on %s\nTotal on %s: %d", DataType, RetVal$LastDate, RetVal$LastDate, sum(cLastVal)))

# daily deaths per million
xoff=barplot(cLastValPerMillion, names=st2, ylim=c(0,max(cLastValPerMillion)+10), col=parcol, cex.names=0.75)
text(xoff,cLastValPerMillion,round(cLastValPerMillion, 1),cex=0.75,pos=3)
title(sprintf("U.S.: State daily %s on %s (per million)", DataType, RetVal$LastDate))

xoff=barplot(cTotal, names=st2, ylim=c(0,max(cTotal)+5000), col=parcol, cex.names=0.75, border=parcol)
text(xoff,cTotal,cTotal,cex=0.75,pos=3)
title(sprintf("U.S.: Overall %s as of %s\nTotal on %s: %d", DataType, RetVal$LastDate, RetVal$LastDate, sum(cTotal)))

# deaths per million
xoff=barplot(cOverallPerMillion, names=st2, ylim=c(0,max(cOverallPerMillion)+500), col=parcol, cex.names=0.75, border=parcol)
text(xoff,cOverallPerMillion,round(cOverallPerMillion),cex=0.75,pos=3)
title(sprintf("U.S.: Overall %s (per million) as of %s\nTotal on %s: %d", DataType, RetVal$LastDate, RetVal$LastDate, sum(cTotal)))

# Calculate percentages R vs D
Rrows=which(party=="R")
Drows=which(party=="D")
Rdths=sum(cLastVal[Rrows])
Ddths=sum(cLastVal[Drows])
Tdths=sum(cLastVal)
print(sprintf("R deaths: %d (%.2f)%%; D deaths: %d (%.2f)%%", Rdths, Rdths/Tdths*100, Ddths, Ddths/Tdths*100))
