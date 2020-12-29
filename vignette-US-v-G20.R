#
# Vignette: The United States vs G20  
#
#
# Add required libraries
#
if (require("devtools")    ==F) { install.packages("devtools")              ; library(devtools)}
if (require("OpenCitizen" )==F) { install_github  ("professorf/OpenCitizen"); library(OpenCitizen)}
#
# Choose a dataset to analyze & get filename
#
Folder   = "data"                                     # Set folder containing datasets
Files    = dir(Folder, "*.csv")                       # Grab all files in that folder
DataType = "deaths"                                # Set datatype: confirmed | deaths
Region   = "global"                                   # Set region: US | global
FileIndex=grep(sprintf("%s_%s", DataType, Region),    # Get index to file 
               Files, ignore.case=T)
FileName=Files[FileIndex]                             # Get file name
#
# Read fileName into data frame 
#
dfOriginal = read.csv(sprintf("%s/%s", Folder, FileName)) # Grab original JHU-CSSE dataset
dfClean     = cleanData  (dfOriginal, Region)                  # Collapse state-counties into a single row
dfDaily    = createDaily(dfClean)                              # Create daily values
dfRange    = getRange(dfDaily, StartDate = "2020-1-1",        # Limit data range 
                      EndDate = "2020-12-31")  
#
# Annotate Country
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22", "2020-12-21")
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall", "Winter")
dfAnnotation  = data.frame(AnnotateDate, AnnotateLabel)#
# Now plot a specific country
#
bPer100K=F
iplist=c() # Index of the peak
plist=c()  # Peak value
#
# Set up accumulators for population density and total per million
#
cpd=c()
cpm=c()
cpp=c()   # cumulative population
cdd=c()   # cumulative deaths
cpeu=c()  # Cumulative Population of the EU sans US + UK
cpg20=c() # Cumulative population of g20 sans US
pus=0     # Population US
#
dfg20mus=NULL # G20 minus the us
dfus=NULL     # Dataframe for just the US
dfeu=NULL
EU=c("Austria" ,"Belgium","Bulgaria" ,"Croatia"   ,"Cyprus","Czechia"    ,"Denmark",
     "Estonia" ,"Finland","France"   ,"Germany"   ,"Greece","Hungary"    ,"Ireland",
     "Italy"   ,"Latvia" ,"Lithuania","Luxembourg","Malta" ,"Netherlands","Poland" ,
     "Portugal","Romania","Slovakia" ,"Slovenia"  ,"Spain" ,"Sweden"     , "UK")	

G20RowsNoUS = which(dfRange$State %in% setdiff(G20$Country, "US"))
EURows     = which(dfRange$State %in% EU)
USRow       = which(dfRange$State=="US")

dfEU      = dfRange[EURows,]
dfG20NoUS = dfRange[G20RowsNoUS, ]
dfUS      = dfRange[USRow, ]

EUValues     = colSums(dfEU[2:length(dfEU)])
G20NoUSValues = colSums(dfG20NoUS[2:length(dfG20NoUS)])
USValues      = unlist(dfUS[1,2:length(dfUS)])

plot(G20NoUSValues, type="p", col="gray")
lines(EUValues, type="p", col="lightblue")
lines(USValues, type="p", col="pink")

EUAvg7      = calcMovingAverage(EUValues)
G20NoUSAvg7 = calcMovingAverage(G20NoUSValues)
USAvg7      = calcMovingAverage(USValues)

lines(7:length(G20NoUSValues), G20NoUSAvg7, col="black")
lines(7:length(EUValues), EUAvg7, col="blue")
lines(7:length(USValues), USAvg7, col="red")
for (Country in dfg$G20) { 
  row=which (df$Country==Country)
  population=dfp$Population[which(dfp$Country==Country)]
  area=dfa$areasqm[which(dfa$Country==Country)]
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
  #barplot(tv, ylim=c(0, ymax)) # For G20 don't plot cumulative
  #
  # Title graph
  #
  lastval=tv[length(tv)]
  lastnam=names(tv)[length(tv)]
  
  yestval=tv[length(tv)-1]
  dval=lastval-yestval
  slastval=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(lastval), perl=T)
  sdval=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(dval), perl=T)
  #title(sprintf("%s - COVID-19 TOTAL Cases: %s as of %s (%s%s)", Country, slastval, lastnam, ifelse((dval>0),"+", "-"), sdval)) # For G20 don't plot cumulative
  #
  # Create a vector of daily changes
  #
  dc=sapply(1:(length(tv)-1), function(x) {(tv[x+1]-tv[x])})
  if (bPer100K==T) dc=dc/population*100000
  #
  # Plot daily changes
  #
  ymaxc=max(dc)+max(dc)/5
  pal=brewer.pal(8, "Dark2")
  if (bPer100K==F) {
    bp=barplot(dc,ylim=c(0, ymaxc), col="#CCCCCC", bor="white", xlab="date", ylab=sprintf("# %s",dtype))
  } else {
    bp=barplot(dc,ylim=c(0, ymaxc), col="#CCCCCC", bor="white", xlab="date", ylab=sprintf("# %s per 100K",dtype))
  }
  #
  # 7-day moving average
  #
  #avg=sapply(7:length(dc), function(i) { mean(dc[(i-6):i])})
  #lines(bp[7:length(dc)], avg, col=pal[1])
  avg=sapply(4:(length(dc)-3), function(i) { mean(dc[(i-3):(i+3)])})
  lines(bp[4:(length(dc)-3)], avg, col=pal[1])
  #
  # Title graph
  #
  
  # Last day values and strings
  lastvalc=dc[length(dc)]
  lastnamc=names(dc)[length(dc)]
  imaxday=which(dc==max(dc))[1]
  maxdayval=dc[imaxday]
  maxdaynam=names(dc)[imaxday]
  
  iplist=c(iplist,imaxday)
  plist=c(plist, maxdayval)
  slastvalc=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(lastvalc), perl=T)
  # Max day value and strings
  smaxdayval=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(maxdayval), perl=T)
  smaxdayval=formatC(maxdayval, format="f", big.mark=",", digits=2)
  # Population values and strings
  spop=formatC(population, format="f", big.mark = ",", digits=0)
  tot=sum(dc)
  stot=formatC(tot, format="f", big.mark=",", digits=0)
  # Per million calculations and strings
  perm=tot/population*1000000
  sperm=formatC(perm, format="f", big.mark=",", digits=0)
  # Area calculations and strings
  sarea=formatC(area, format="f", big.mark = ",", digits=0)
  pd=population/area
  spd=formatC(pd, format="f", big.mark=",", digits=2)
  # Update accumulators
  cpd=c(cpd,pd)
  cpm=c(cpm,perm)
  cpp=c(cpp,population)
  cdd=c(cdd,tot)
  title(sprintf("%s - COVID-19 DAILY %s (OVERALL): %s on %s (%s)\nPop: %s; Area: %s; Peak: %s on %s;\nPopulation Density: %s; Total Per Mil: %s", Country, toupper(dtype),slastvalc, lastnamc, stot, spop, sarea, smaxdayval, maxdaynam, spd, sperm))
  dev.copy(png, sprintf("pics/%s-%s.png",dtype,Country), width=1280, height=720)
  dev.off()
  dftemp = data.frame(country=Country, t(dc))
  if (Country!="US") {
    dfg20mus=rbind(dfg20mus,dftemp)
    cpg20=c(cpg20, population)
    if (Country %in% eu) {
      dfeu=rbind(dfeu,dftemp)
      cpeu=c(cpeu,population)
    }
  } else {
    dfus=dftemp
    pus=population
  }
}
names(iplist)=dfg$G20
colwid=max(iplist)/9
colbar=brewer.pal(9, "YlOrRd")[round(iplist/colwid)]
bp=barplot(iplist, horiz=T, las=2, cex.names=0.45, col=colbar, xlim=c(0, max(iplist)+10), xaxt="n")
text(iplist+2, bp, iplist, cex=0.45)
title("G20 Nations: Reaction Estimate Based on Peak Day\nLower is Better")
dev.copy(png, sprintf("pics/reaction-barplot.png"), width=1280, height=720)
dev.off()
#
# Linear Model for G20
#
Population.Density=cpd
Deaths.Per.Million=cpm
plot(Population.Density, Deaths.Per.Million) 
title("Covid-19, G-20: Population Density vs Deaths Per Million")
text(cpd, cpm, dfg$G20, pos=3, cex=0.50)
model=lm(Deaths.Per.Million~Population.Density)
summary(model)
lines(Population.Density, predict(model))
dev.copy(png, sprintf("spics/z-%s-linear-model.png",dtype), width=1280, height=720)
dev.off()
hist(residuals(model))
#plot(model)

#
# All three compared (US, G20-US, EU+UK)
# 
if (dtype=="confirmed") dt="Daily New Cases" else dt="Daily Deaths"
# g20
g20v=as.integer(colSums(dfg20mus[,2:length(dfg20mus)]))
if (percapita) g20v=g20v/sum(cpg20)*1000000
# eu
euv=as.integer(colSums(dfeu[,2:length(dfeu)]))
if (percapita) euv=euv/sum(cpeu)*1000000
# us
usv=t(dfus[,2:length(dfus)])
if (percapita) usv=usv/pus*1000000
# Now plot overlay
ymaxuge=ifelse((max(g20v)>=max(euv)), max(g20v), max(euv))
ymaxuge=ifelse((max(usv) >=ymaxuge) , max(usv) , ymaxuge)
plot(g20v, col="gray", ylab=dt, ylim=c(0,ymaxuge), xlab=sprintf("Days: 0=%s, %d=%s",names(dc[1]), (length(dc)-1), names(dc[length(dc)])))
avg20=sapply(7:length(g20v), function(i) { mean(g20v[(i-6):i])})
lines(7:length(g20v), avg20, col="black")

lines(euv, type="p", col="lightblue")
avgeu=sapply(7:length(euv), function(i) { mean(euv[(i-6):i])})
lines(7:length(euv), avgeu, col="blue")

lines(usv, type="p", col="pink")
avgus=sapply(7:length(usv), function(i) { mean(usv[(i-6):i])})
lines(7:length(usv),avgus, col="red")

if (percapita) title(sprintf("COVID-19 %s US vs. G20 vs European Union [per million]", dt)) else title(sprintf("COVID-19 %s US vs. G20 vs European Union", dt))
legend("topleft", legend=c("G20 (w/out US)", "EU (w/UK)", "US"), col=c("black", "blue", "red"), lty=1)
legend("bottomright", "Source: JHU CSSE Dataset", cex=0.50)
