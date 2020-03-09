# This can't be run without having run consonantsPerSyllable.R first
#source("./consonantsPerSyllable.R")

# Importing and aggregating name data - KEEPING YEAR DATA
# https://www.ssa.gov/OACT/babynames/names.zip
babyNamesLong <- data.frame()
files <- list.files(path="./names", pattern="*.txt", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
  year <- as.numeric(str_sub(x, start = 12, end = 15))
  tempDF <- read.csv(x, header = FALSE)
  tempDF <- cbind(tempDF, year)
  babyNamesLong <<- rbind(babyNamesLong, tempDF)
})
colnames(babyNamesLong) <- c("name", "sex", "count", "year")

# Treat consonant:syllable ratio as categorical data
dfF <- babyNames[babyNames$sex == "F",]
dfM <- babyNames[babyNames$sex == "M",]

# Creating a table to aggregate counts at each consonant to syllable ratio
weightedDF <- expand.grid(unique(babyNames[,"cRatio"]), 1880:2018, 0, 0)
colnames(weightedDF) <- c("ratio", "year", "M", "F")

for (row in 1:nrow(weightedDF)) {
  print((row/nrow(weightedDF))*100)
  ratio <- as.numeric(weightedDF[row, "ratio"])
  year <- as.numeric(weightedDF[row, "year"])
  
  # Unique names for each ratio
  fNames <- unique(dfF[dfF$cRatio == ratio, "name"])
  mNames <- unique(dfM[dfM$cRatio == ratio, "name"])
  
  dfYear <- babyNamesLong[babyNamesLong$year == year,]
  
  # Counts for each ratio by year
  weightedDF[row, "F"] <- sum(dfYear[dfYear$name %in% fNames, "count"])
  weightedDF[row, "M"] <- sum(dfYear[dfYear$name %in% mNames, "count"])
}
write.csv(weightedDF, "./weightedDF.csv")
weightedDF <- read.csv("./weightedDF.csv")

for (year in 1880:2018) {
  dfYear <- weightedDF[weightedDF$year == year,]
  
  # Remove 0's and making smaller by half the 1st quartile
  dfYear <- dfYear[dfYear$M + dfYear$F != 0,]
  dfYear$F <- floor(dfYear$F / (quantile(dfYear$F)[2] / 2))
  dfYear$M <- floor(dfYear$M / (quantile(dfYear$M)[2] / 2))
  
  # Using vector of ratios to weight by name frequency
  weightF <- c()
  weightM <- c()
  for (row in 1:nrow(dfYear)) {
    weightF <- c(weightF, rep.int(dfYear[row,"ratio"], dfYear[row,"F"]))
    weightM <- c(weightM, rep.int(dfYear[row,"ratio"], dfYear[row,"M"]))
  }
  ifelse(length(weightF) > length(weightM),
         weightF <- sample(weightF, length(weightM)),
         weightM <- sample(weightM, length(weightF)))
  
  dfPlot <- data.frame(Female = weightF, Male = weightM)
  dfPlot <- melt(dfPlot)
  
  ggplot(dfPlot, aes(x = variable, y = value, fill = variable)) +
    geom_boxplot() +
    theme(legend.position="none") +
    ylab("Consonants per Syllable (weighted by name frequency)") +
    xlab("") +
    ylim(c(0,6)) +
    coord_flip() +
    ggtitle(paste("Consonants per Syllable for Baby Names in", year))
  
  ggsave(paste("./boxAnim/", year, ".png", sep = ""))
}

