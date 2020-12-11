plotState = function (dfd, State, DataType) {
  Row=which (dfd$State==State)
  #
  # Determine date columns
  #
  Cols=grep("^X", colnames(dfd)) # dates have "X" as a column-label prefix)
  #
  # Extract the time series data and clean-up labels 
  #
  StateDailyVals=dfd[Row,Cols]                       # a row (1xN matrix)
  StateVals=unlist(StateDailyVals)                         # convert to vector
  names(StateVals)=gsub("X","", names(StateVals))     # remove "X" in column names
  names(StateVals)=gsub("[.]", "/", names(StateVals)) # change "." to "/"
  #
  # Plot daily changes
  #
  MaxVal=max(StateVals)# +max(StateVals)/5
  #barplot(StateVals,ylim=c(0, MaxVal))
  Colors=brewer.pal(8, "Dark2")
  XBarPlot=barplot(StateVals,ylim=c(0, MaxVal), col="#CCCCCC", bor="white", xlab="Date", ylab=sprintf("# %s",DataType))
  #
  # 7-day moving average
  #
  Avg7=sapply(7:length(StateVals), function(i) { mean(StateVals[(i-6):i])})
  lines(XBarPlot[7:length(StateVals)], Avg7, col=Colors[1])
  #Avg4=sapply(4:(length(StateVals)-3), function(i) { mean(StateVals[(i-3):(i+3)])})
  #lines(XBarPlot[4:(length(StateVals)-3)], Avg4, col=Colors[1])
  #
  # Title graph
  #
  LastVal=StateVals[length(StateVals)]
  LastDate=names(StateVals)[length(StateVals)]
  iMaxDay=which(StateVals==max(StateVals))[1]
  MaxDayVal=StateVals[iMaxDay]
  MaxDate=names(StateVals)[iMaxDay]
  LastValFmt=formatC(LastVal, format="f", digits=0, big.mark=",")
  MaxDayValFmt=formatC(MaxDayVal, format="f", digits=0, big.mark=",")
  StatePopulation=USPopulation$Population[which(USPopulation$State==State)]
  StatePopulationFmt=formatC(StatePopulation, format="f", big.mark = ",", digits=0)
  StateRow=which(USArea$State==State)
  if (identical(StateRow, integer(0))==T) {
    StateAreaFmt="unk" 
    StateArea=1
  } else {
    StateArea = USArea$landsqm[StateRow]
    StateAreaFmt=formatC(StateArea, format="f", big.mark=",", digits=0)
  }
  PopulationDensity=StatePopulation/StateArea
  PopulationDensityFmt=formatC(PopulationDensity, format="f", big.mark=",", digits=2)
  Total=sum(StateVals)
  TotalFmt=formatC(Total, format="f", big.mark=",", digits=0)
  OverallPerMillion=Total/StatePopulation*1000000
  OverallPerMillionFmt=formatC(OverallPerMillion, format="f", big.mark=",", digits=2)
  LastValPerMillion=LastVal/StatePopulation*1000000
  title(sprintf("%s - COVID-19 DAILY %s (Total): %s on %s (%s)\nPop: %s; Area: %s sq-miles; Peak: %s on %s\nPopulation Density: %s; Total per Million: %s", State, toupper(DataType), LastValFmt, LastDate, TotalFmt, StatePopulationFmt, StateAreaFmt, MaxDayValFmt, MaxDate, PopulationDensityFmt, OverallPerMillionFmt))

  ret=list(PopulationDensity=PopulationDensity, Total=Total, LastVal=LastVal, LastDate=LastDate, LastValPerMillion=LastValPerMillion, OverallPerMillion=OverallPerMillion,StateArea=StateArea, StatePopulation=StatePopulation)
  ret
}
