# Clean the environment
rm(list=ls())

# 1. Load the candidates data.
assets <- read.csv("Candidates_Assets.csv", header=T, stringsAsFactors=FALSE)
str(assets)

# Convert to numeric the %age of votes captured by various candidates/parties per constituency
assets$Net_Assets <- as.numeric(assets$Total.Assets)
# 12 NAs introduced by coercion, check the data with NAs in "Net.Assets" column.
assets[is.na(assets$Net_Assets),"Total.Assets"]
# There are characters instead of numeric data in these 12 rows, in the Total.Assets column
# Lets delete these 12 rows
assets_clean <- assets[complete.cases(assets),]
# Check if 12 deleted?
nrow(assets) - nrow(assets_clean) # 12 gone

# Let's focus on Top 10 parties by total asset size
bigBucks <- aggregate(Net_Assets~Party, data=assets_clean, sum)
top10Party <- bigBucks$Party[order(bigBucks$Net_Assets,decreasing = TRUE)[1:10]]
top10 <- subset(assets_clean, Party %in% top10Party)

# ANNOVA - Is the av. asset size of candidates accross equal(Level of Significance = 99%)
aov.model <- aov(top10$Net_Assets~factor(top10$Party))
summary(aov.model)
# As the p-value is less that our level of significance, we reject the Null Hypothesis that the vote %age is equal. 

#Where are the differences? 
TukeyHSD(aov.model)
plot(TukeyHSD(aov.model))

