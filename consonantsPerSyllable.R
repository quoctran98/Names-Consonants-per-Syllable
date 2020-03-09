library(quanteda)
library(stringr)
library(ggplot2)
library(reshape2)

# Importing and aggregating name data
# https://www.ssa.gov/OACT/babynames/names.zip
babyNamesLong <- data.frame()
files <- list.files(path="./names", pattern="*.txt", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
  babyNamesLong <<- rbind(babyNamesLong, read.csv(x, header = FALSE))
})
colnames(babyNamesLong) <- c("name", "sex", "count")
babyNames <- aggregate(data = babyNamesLong, count ~ name + sex, FUN = sum)

# Function to find number of consonants/vowels
nconsonant <- function (word) {
  word <- tolower(word)
  consonants <- letters[!letters %in% c("a", "e", "i", "o", "u")]
  return(sum(str_count(word, consonants)))
}
# Adding syllable and consonant count to each name
# Takes a bit to run
for (row in 1:nrow(babyNames)) {
  print((row / nrow(babyNames)) * 100)
  name <- as.character(babyNames[row, "name"])
  babyNames[row, "syllables"] <- nsyllable(name)
  babyNames[row, "length"] <- str_length(name)
  babyNames[row, "consonants"] <- nconsonant(name)
  babyNames[row, "vowels"] <- babyNames[row, "length"] - babyNames[row, "consonants"]
}
babyNames$cRatio <- as.numeric(babyNames$consonants / babyNames$syllables)
babyNames$vRatio <- as.numeric(babyNames$vowels / babyNames$syllables)
babyNames <- na.omit(babyNames)
write.csv(babyNames, "./babyNames.csv") # Save the dataframe

babyNames <- read.csv("./babyNames.csv", header = TRUE) # Read the csv again

weightRatio <- function (df, ratioCol) {
  # Treat consonant:syllable ratio as categorical data
  dfF <- df[df$sex == "F",]
  dfM <- df[df$sex == "M",]
  
  # Creating a table to aggregate counts at each consonant to syllable ratio
  weightedDF <- data.frame("ratio" = unique(df[,ratioCol]), 0, 0)
  colnames(weightedDF) <- c("ratio", "M", "F")
  for (row in 1:nrow(weightedDF)) {
    ratio <- as.numeric(weightedDF[row, "ratio"])
    weightedDF[row, "F"] <- sum(dfF[dfF[,ratioCol] == ratio, "count"])
    weightedDF[row, "M"] <- sum(dfM[dfM[,ratioCol] == ratio, "count"])
  }
  
  # Reducing the total counts to make the next part faster
  # Cuts off any ratios below 1st quartile
  weightedDF$F <- floor(weightedDF$F / quantile(weightedDF$F)[2])
  weightedDF$M <- floor(weightedDF$M / quantile(weightedDF$M)[2])
  
  # Using vector of ratios to weight by name frequency
  weightF <- c()
  weightM <- c()
  for (row in 1:nrow(weightedDF)) {
    weightF <- c(weightF, rep.int(weightedDF[row,"ratio"], weightedDF[row,"F"]))
    weightM <- c(weightM, rep.int(weightedDF[row,"ratio"], weightedDF[row,"M"]))
  }
  ifelse(length(weightF) > length(weightM), 
         weightF <- sample(weightF, length(weightM)),
         weightM <- sample(weightM, length(weightF)))
  
  # Melting and returning
  df <- data.frame(Female = weightF, Male = weightM)
  df <- melt(df)
  return(df)
}

df <- weightRatio(babyNames,"cRatio")

ggplot(df, aes(x = variable, y = value, fill = variable)) +
  geom_violin() +
  theme(legend.position="none") +
  ylab("Consonants per Syllable (weighted by name frequncy)") +
  xlab("") +
  coord_flip() +
  ggtitle("Male Names Tend to Have More Consonants per Syllable") +
  labs(subtitle = "From SSA Data (Babies Born 1880-2018)")

t.test(df[df$variable == "Female", "value"], df[df$variable == "Male", "value"])

