library(quanteda)
library(stringr)
library(ggplot2)
library(reshape2)

# Importing and aggregating name data
# https://www.ssa.gov/OACT/babynames/names.zip
babyNames <- data.frame()
files <- list.files(path="./names", pattern="*.txt", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
  babyNames <<- rbind(babyNames, read.csv(x, header = FALSE))
})
colnames(babyNames) <- c("name", "sex", "count")
babyNames <- aggregate(data = babyNames, count ~ name + sex, FUN = sum)

# Function to find number of consonants
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
  babyNames[row, "consonants"] <- nconsonant(name)
}
babyNames$ratio <- as.numeric(babyNames$consonants / babyNames$syllables)
babyNames <- na.omit(babyNames)
write.csv(babyNames, "./babyNames.csv") # Save the dataframe

# Treat consonant:syllable ratio as categorical data
babyNamesF <- babyNames[babyNames$sex == "F",]
babyNamesM <- babyNames[babyNames$sex == "M",]
uniqueRatios <- unique(babyNames$ratio)

# Creating a table to aggregate counts at each consonant to syllable ratio
weightRatio <- data.frame("ratio" = uniqueRatios, 0, 0)
colnames(weightRatio) <- c("ratio", "M", "F")
for (row in 1:nrow(weightRatio)) {
  ratio <- as.numeric(weightRatio[row, "ratio"])
  weightRatio[row, "F"] <- sum(babyNamesF[babyNamesF$ratio == ratio, "count"])
  weightRatio[row, "M"] <- sum(babyNamesM[babyNamesM$ratio == ratio, "count"])
}

# Reducing the total counts to make the next part faster
# Cuts off any ratios below 1st quartile
weightRatio$F <- floor(weightRatio$F / quantile(weightRatio$M)[2])
weightRatio$M <- floor(weightRatio$M / quantile(weightRatio$M)[2])

# Using vector of ratios to weight by name frequency
weightF <- c()
weightM <- c()
for (row in 1:nrow(weightRatio)) {
  weightF <<- c(weightF, rep.int(weightRatio[row,"ratio"], weightRatio[row,"F"]))
  weightM <<- c(weightM, rep.int(weightRatio[row,"ratio"], weightRatio[row,"M"]))
}
ifelse(length(weightF) > length(weightM), 
       weightF <- sample(weightF, length(weightM)),
       weightM <- sample(weightM, length(weightF)))

# Melting and plotting
df <- data.frame(Female = weightF, Male = weightM)
df <- melt(df)

ggplot(df, aes(x = variable, y = value, fill = variable)) +
  geom_violin() +
  theme(legend.position="none") +
  ylab("Consonants per Syllable (weighted by name frequncy)") +
  xlab("") +
  coord_flip() +
  ggtitle("Male Names Tend to Have More Consonants per Syllable") +
  labs(subtitle = "From SSA Data (Babies Born 1880-2018)")

t.test(weightF, weightM)
