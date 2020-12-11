#
# Add required libraries
#
if (require("RColorBrewer")==F) {  install.packages("RColorBrewer");  library(RColorBrewer) }
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

#NumCols=length(colnames(dfd))
#TestState="New Mexico"
#row=which(dfd$state==TestState) # Test
#x=unlist(dfd[row,2:(NumCols-1)])
#barplot(x)
#title(TestState)

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
cpd=c()   # Cumulative population density
cpm=c()   # Cumulative per million
ctt=c()   # Cumulative total deaths (or confirmed)
clt=c()   # Cumulative last daily death (or confirmed)
cltpm=c() # Cumulative last daily death (or confirmed) per million
ca=c()    # Cumulative area
cp=c()    # Cumulative population
#
# Now plot a specific State
#
for (State in States50) {
  row=which (df$State==State)
  #
  # Determine date columns
  #
  cols=grep("^X", colnames(df)) # dates have "X" as a column-label prefix)
  #
  # Extract the time series data and clean-up labels 
  #
  ts=df[row,cols]                       # a row (1xN matrix)
  tv=unlist(ts)                         # convert to vector
  names(tv)=gsub("X","", names(tv))     # remove "X" in column names
  names(tv)=gsub("[.]", "/", names(tv)) # change "." to "/"
  #
  # Plot
  #
  ymax =max(tv)+max(tv)/10
  options(scipen=9)
  #barplot(tv, ylim=c(0, ymax)) # 2020-08-11 Don't do cumulative plot
  #
  # Title graph
  #
  lastval=tv[length(tv)]
  lastnam=names(tv)[length(tv)]
  yestval=tv[length(tv)-1]
  dval=lastval-yestval
  slastval=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(lastval), perl=T)
  sdval=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(dval), perl=T)
  #title(sprintf("%s - COVID-19 CUMULATIVE %s: %s as of %s (%s%s)", State, toupper(DataType), slastval, lastnam, ifelse((dval>0),"+", "-"), sdval)) # 2020-08-11: Don't do cumulative
  #
  # Create a vector of daily changes
  #
  dc=sapply(1:(length(tv)-1), function(x) {(tv[x+1]-tv[x])})
  #
  # Plot daily changes
  #
  ymaxc=max(dc)+max(dc)/5
  #barplot(dc,ylim=c(0, ymaxc))
  pal=brewer.pal(8, "Dark2")
  bp=barplot(dc,ylim=c(0, ymaxc), col="#CCCCCC", bor="white", xlab="date", ylab=sprintf("# %s",DataType))
  #
  # 7-day moving average
  #
  avg=sapply(7:length(dc), function(i) { mean(dc[(i-6):i])})
  lines(bp[7:length(dc)], avg, col=pal[1])
  #avg=sapply(4:(length(dc)-3), function(i) { mean(dc[(i-3):(i+3)])})
  #lines(bp[4:(length(dc)-3)], avg, col=pal[1])
  #
  # Title graph
  #
  lastvalc=dc[length(dc)]
  lastnamc=names(dc)[length(dc)]
  imaxday=which(dc==max(dc))[1]
  maxdayval=dc[imaxday]
  maxdaynam=names(dc)[imaxday]
  slastvalc=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(lastvalc), perl=T)
  smaxdayval=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(maxdayval), perl=T)
  popu=dfPopu$Population[which(dfPopu$State==State)]
  pops=formatC(popu, format="f", big.mark = ",", digits=0)
  iastate=which(dfArea$State==State)
  if (identical(iastate, integer(0))==T) {
    areas="unk" 
    area=1
  } else {
    area = dfArea$landsqm[iastate]
    areas=formatC(area, format="f", big.mark=",", digits=0)
  }
  pd=popu/area
  pas=formatC(pd, format="f", big.mark=",", digits=2)
  tt=sum(dc)
  tts=formatC(tt, format="f", big.mark=",", digits=0)
  pm=tt/popu*1000000
  pms=formatC(pm, format="f", big.mark=",", digits=2)
  title(sprintf("%s - COVID-19 DAILY %s (Total): %s on %s (%s)\nPop: %s; Area: %s sq-miles; Peak: %s on %s\nPopulation Density: %s; Total per Million: %s", State, toupper(DataType), slastvalc, lastnamc, tts, pops, areas, smaxdayval, maxdaynam, pas, pms))
  dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
  dev.off()
  #
  # Update Accmulators
  #
  cpd=c(cpd, pd)       # Accumulated population densities
  ctt=c(ctt, tt)       # Accumulated total (deaths or cases)
  clt=c(clt, lastvalc) # Accumulated last daily totals
  cltpm=c(cltpm, lastvalc/popu*1000000)
  cpm=c(cpm, pm)       # Accumulated per million (deaths or case)
  ca =c(ca, area)      # Accumulated areas
  cp =c(cp, popu)      # Accumulated population
}
#
# Linear Model 
#
Population.Density=cpd
Deaths.Per.Million=cpm
Total.Area=ca
Total.DC=ctt

plot(Population.Density, Deaths.Per.Million, ylim=c(0,max(Deaths.Per.Million)+250)) 
title("Covid-19, U.S. States: Population Density vs Deaths Per Million")
model=lm(Deaths.Per.Million~Population.Density)
summary(model)
lines(Population.Density, predict(model))
text(cpd, cpm, st2, pos=3, cex=0.50)
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
xoff=barplot(clt, names=st2, ylim=c(0,max(clt)+100), col=parcol, cex.names=0.75)
text(xoff,clt,clt,cex=0.75,pos=3)
title(sprintf("U.S.: State daily %s on %s\nTotal on %s: %d", DataType, lastnamc, lastnamc, sum(clt)))

# daily deaths per million
xoff=barplot(cltpm, names=st2, ylim=c(0,max(cltpm)+10), col=parcol, cex.names=0.75)
text(xoff,cltpm,round(cltpm, 1),cex=0.75,pos=3)
title(sprintf("U.S.: State daily %s on %s (per million)", DataType, lastnamc))

xoff=barplot(ctt, names=st2, ylim=c(0,max(ctt)+5000), col=parcol, cex.names=0.75, border=parcol)
text(xoff,ctt,ctt,cex=0.75,pos=3)
title(sprintf("U.S.: Overall %s as of %s\nTotal on %s: %d", DataType, lastnamc, lastnamc, sum(ctt)))

# deaths per million
xoff=barplot(cpm, names=st2, ylim=c(0,max(cpm)+500), col=parcol, cex.names=0.75, border=parcol)
text(xoff,cpm,round(cpm),cex=0.75,pos=3)
title(sprintf("U.S.: Overall %s (per million) as of %s\nTotal on %s: %d", DataType, lastnamc, lastnamc, sum(ctt)))

# Calculate percentages R vs D
Rrows=which(party=="R")
Drows=which(party=="D")
Rdths=sum(clt[Rrows])
Ddths=sum(clt[Drows])
Tdths=sum(clt)
print(sprintf("R deaths: %d (%.2f)%%; D deaths: %d (%.2f)%%", Rdths, Rdths/Tdths*100, Ddths, Ddths/Tdths*100))
