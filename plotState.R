plotState = function (dfd, State, DataType) {
  Row=which (dfd$state==State)
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
  # Plot
  #
  YMax =max(StateVals)
  options(scipen=9) # Prevents scientific notation. 9=# of digits before sci. not.
  barplot(StateVals, ylim=c(0, YMax)) # 2020-08-11 Don't do cumulative plot
  #
  # Title graph
  #
  Total      = sum(StateVals)  
  LastVal    = StateVals[length(StateVals)]
  LastDate   = names(StateVals)[length(StateVals)]
  TotalFmt   = formatC(Total, format="f", digits=0, big.mark=",") 
  LastValFmt = formatC(LastVal, format="f", digits=0, big.mark=",")
  
  title(sprintf("%s - COVID-19 CUMULATIVE %s: %s as of %s (%s%s)", State, toupper(DataType), TotalFmt, LastDate, ifelse((LastVal>=0),"+", "-"), LastValFmt)) # 2020-08-11: Don't do cumulative
  #
  # Plot daily changes
  #
  ymaxc=max(StateVals)+max(StateVals)/5
  #barplot(StateVals,ylim=c(0, ymaxc))
  pal=brewer.pal(8, "Dark2")
  bp=barplot(StateVals,ylim=c(0, ymaxc), col="#CCCCCC", bor="white", xlab="date", ylab=sprintf("# %s",DataType))
  #
  # 7-day moving average
  #
  Avg7=sapply(7:length(StateVals), function(i) { mean(StateVals[(i-6):i])})
  lines(bp[7:length(StateVals)], Avg7, col=pal[1])
  #Avg4=sapply(4:(length(StateVals)-3), function(i) { mean(StateVals[(i-3):(i+3)])})
  #lines(bp[4:(length(StateVals)-3)], Avg4, col=pal[1])
  #
  # Title graph
  #
  lastvalc=StateVals[length(StateVals)]
  lastnamc=names(StateVals)[length(StateVals)]
  imaxday=which(StateVals==max(StateVals))[1]
  maxdayval=StateVals[imaxday]
  maxdaynam=names(StateVals)[imaxday]
  slastvalc=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(lastvalc), perl=T)
  smaxdayval=gsub("(?!^)(?=(?:\\d{3})+$)", ",", as.character(maxdayval), perl=T)
  popu=USPopulation$Population[which(USPopulation$State==State)]
  pops=formatC(popu, format="f", big.mark = ",", digits=0)
  iastate=which(USArea$State==State)
  if (identical(iastate, integer(0))==T) {
    areas="unk" 
    area=1
  } else {
    area = USArea$landsqm[iastate]
    areas=formatC(area, format="f", big.mark=",", digits=0)
  }
  pd=popu/area
  pas=formatC(pd, format="f", big.mark=",", digits=2)
  tt=sum(StateVals)
  tts=formatC(tt, format="f", big.mark=",", digits=0)
  pm=tt/popu*1000000
  pms=formatC(pm, format="f", big.mark=",", digits=2)
  title(sprintf("%s - COVID-19 DAILY %s (Total): %s on %s (%s)\nPop: %s; Area: %s sq-miles; Peak: %s on %s\nPopulation Density: %s; Total per Million: %s", State, toupper(DataType), slastvalc, lastnamc, tts, pops, areas, smaxdayval, maxdaynam, pas, pms))
  dev.copy(png, sprintf("statepics/%s-%s.png",DataType,State), width=1280, height=720)
  dev.off()
  #
  # Update Accmulators [update outside of function]
  #
  
  #cpd=c(cpd, pd)       # Accumulated population densities
  #ctt=c(ctt, tt)       # Accumulated total (deaths or cases)
  #clt=c(clt, lastvalc) # Accumulated last daily totals
  #cltpm=c(cltpm, lastvalc/popu*1000000)
  #cpm=c(cpm, pm)       # Accumulated per million (deaths or case)
  #ca =c(ca, area)      # Accumulated areas
  #cp =c(cp, popu)      # Accumulated population
  # NEED TO RETURN A LIST  
  ret=list(pd=pd, tt=tt, lastvalc=lastvalc, lastvalcpm=lastvalc/popu*1000000, pm=pm,area=area, popu=popu)
  ret
}
