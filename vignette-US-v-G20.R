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
                      EndDate = "2021-12-31")  
#
# Annotate Country
#
AnnotateDate  = c("2020-11-26", "2020-10-31", "2020-9-1", "2020-3-19", "2020-6-20", "2020-9-22", "2020-12-21")
AnnotateLabel = c("Thanksgiving", "Halloween", "Labor Day", "Spring", "Summer", "Fall", "Winter")
dfAnnotation  = data.frame(AnnotateDate, AnnotateLabel)#

#
# Now plot G20, EU, & US overlayed
#

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

plot(G20NoUSValues, type="p", col="gray", xaxt="n", ylab=toupper(DataType))
lines(EUValues, type="p", col="lightblue")
lines(USValues, type="p", col="pink")

EUAvg7      = calcMovingAverage(EUValues)
G20NoUSAvg7 = calcMovingAverage(G20NoUSValues)
USAvg7      = calcMovingAverage(USValues)
MaxValue    = max(G20NoUSValues) # Probably the Max

lines(7:length(G20NoUSValues), G20NoUSAvg7, col="black")
lines(7:length(EUValues), EUAvg7, col="blue")
lines(7:length(USValues), USAvg7, col="red")

DateLabels=names(USValues) # All three values have the same dates, just use US's values
DateLabels=gsub("\\.", "\\/", gsub("X", "", DateLabels))

title(sprintf("COVID-19 %s, US vs. G20 vs European Union (as of %s)", toupper(DataType), DateLabels[length(DateLabels)]))
legend("topleft", legend=c("G20 (w/out US)", "EU (w/UK)", "US"), col=c("black", "blue", "red"), lty=1)
legend("bottomright", "Source: JHU CSSE Dataset", cex=0.75)

axis(side=1, at=1:length(USValues), labels=DateLabels, las=2, cex.axis=0.5)
#
# Now add annotations
#
Dates=as.Date(names(dfUS[2:length(names(dfUS))]), format="X%m.%d.%y")
YVal=MaxValue
overlayAnnotations(dfAnnotation, Dates, MaxValue, "red", "black")
