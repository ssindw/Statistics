# Clean the environment
rm(list=ls())

# 1. Load the data for UttarPradesh.
uttarPradesh <- read.csv("uttarPradesh.csv", header=T, stringsAsFactors=FALSE)
str(uttarPradesh)

# Convert to numeric the %age of votes captured by various candidates/parties per constituency
uttarPradesh$candidate_vote_share <-as.numeric(substr(uttarPradesh$Cand.Votes.as...of.Total.Const.Votes,1,nchar(uttarPradesh$Cand.Votes.as...of.Total.Const.Votes)-1))

# Top 5 parties in UP?
partyByTotalVotes_UP <- aggregate(LOD..Cand.Votes.Polled~Partyabbre, data=uttarPradesh, sum)
top5Party <- partyByTotalVotes_UP$Partyabbre[order(partyByTotalVotes_UP$LOD..Cand.Votes.Polled,decreasing = TRUE)[1:5]]
uttarPradeshTop5 <- subset(uttarPradesh, Partyabbre %in% top5Party)

# ANNOVA - Is the vote %age of vote of various parties accross various consituiencies equal?(Level of Significance = 99%)
aov.model <- aov(uttarPradeshTop5$candidate_vote_share~factor(uttarPradeshTop5$Partyabbre))
summary(aov.model)
# As the p-value is less that our level of significance, we reject the Null Hypothesis that the vote %age is equal. 

#Where are the differences? 
TukeyHSD(aov.model)
plot(TukeyHSD(aov.model))



#######################################################################################################################
# 2. Load the data for Uttrakhand.
Uttrakhand <- read.csv("Uttrakhand.csv", header=T, stringsAsFactors=FALSE)

# Convert to numeric the %age of votes captured by various candidates/parties per constituency
Uttrakhand$candidate_vote_share <-as.numeric(substr(Uttrakhand$Cand.Votes.as...of.Total.Const.Votes,1,nchar(Uttrakhand$Cand.Votes.as...of.Total.Const.Votes)-1))

# Top 5 parties in UK?
partyByTotalVotes_UK <- aggregate(LOD..Cand.Votes.Polled~Partyabbre, data=Uttrakhand, sum)
top5Party_UK <- partyByTotalVotes_UK$Partyabbre[order(partyByTotalVotes_UK$LOD..Cand.Votes.Polled,decreasing = TRUE)[1:5]]
UttrakhandTop5 <- subset(Uttrakhand, Partyabbre %in% top5Party_UK)

# ANNOVA - Is the vote %age of vote of various parties accross various consituiencies equal?(Level of Significance = 99%)
aov.model_uk <- aov(UttrakhandTop5$candidate_vote_share~factor(UttrakhandTop5$Partyabbre))
summary(aov.model_uk)
# As the p-value is less that our level of significance, we reject the Null Hypothesis that the vote %age is equal. 

#Where are the differences? 
TukeyHSD(aov.model_uk)
plot(TukeyHSD(aov.model_uk))

#######################################################################################################################
# 3. Load the data for Punjab.
Punjab <- read.csv("Punjab.csv", header=T, stringsAsFactors=FALSE)
str(Punjab)

# Convert to numeric the %age of votes captured by various candidates/parties per constituency
Punjab$candidate_vote_share <-as.numeric(substr(Punjab$Cand.Votes.as...of.Total.Const.Votes,1,nchar(Punjab$Cand.Votes.as...of.Total.Const.Votes)-1))

# Top 5 parties in Punjab?
partyByTotalVotes_Pu <- aggregate(LOD..Cand.Votes.Polled~Partyabbre, data=Punjab, sum)
top5Party_Pu <- partyByTotalVotes_Pu$Partyabbre[order(partyByTotalVotes_Pu$LOD..Cand.Votes.Polled,decreasing = TRUE)[1:5]]
PunjabTop5 <- subset(Punjab, Partyabbre %in% top5Party_Pu)

# ANNOVA - Is the vote %age of vote of various parties accross various consituiencies equal?(Level of Significance = 99%)
aov.model_Pu <- aov(PunjabTop5$candidate_vote_share~factor(PunjabTop5$Partyabbre))
summary(aov.model_Pu)
# As the p-value is less that our level of significance, we reject the Null Hypothesis that the vote %age is equal. 

#Where are the differences? 
TukeyHSD(aov.model_Pu)
plot(TukeyHSD(aov.model_Pu))


#######################################################################################################################
# Load the data for Goa.
Goa <- read.csv("Goa.csv", header=T, stringsAsFactors=FALSE)
str(Goa)

# Convert to numeric the %age of votes captured by various candidates/parties per constituency
Goa$candidate_vote_share <-as.numeric(substr(Goa$Cand.Votes.as...of.Total.Const.Votes,1,nchar(Goa$Cand.Votes.as...of.Total.Const.Votes)-1))

# Top 5 parties in Goa?
partyByTotalVotes_Goa <- aggregate(LOD..Cand.Votes.Polled~Partyabbre, data=Goa, sum)
top5Party_Goa <- partyByTotalVotes_Goa$Partyabbre[order(partyByTotalVotes_Goa$LOD..Cand.Votes.Polled,decreasing = TRUE)[1:5]]
GoaTop5 <- subset(Goa, Partyabbre %in% top5Party_Goa)

# ANNOVA - Is the vote %age of vote of various parties accross various consituiencies equal?(Level of Significance = 99%)
aov.model_Goa <- aov(GoaTop5$candidate_vote_share~factor(GoaTop5$Partyabbre))
summary(aov.model_Goa)
# As the p-value is less that our level of significance, we reject the Null Hypothesis that the vote %age is equal. 

#Where are the differences? 
TukeyHSD(aov.model_Goa)
plot(TukeyHSD(aov.model_Goa))


#######################################################################################################################
# Load the data for Manipur.
Manipur <- read.csv("Manipur.csv", header=T, stringsAsFactors=FALSE)
str(Manipur)

# Convert to numeric the %age of votes captured by various candidates/parties per constituency
Manipur$candidate_vote_share <-as.numeric(substr(Manipur$Cand.Votes.as...of.Total.Const.Votes,1,nchar(Manipur$Cand.Votes.as...of.Total.Const.Votes)-1))

# Top 5 parties in Manipur?
partyByTotalVotes_Ma <- aggregate(LOD..Cand.Votes.Polled~Partyabbre, data=Manipur, sum)
top5Party_Ma <- partyByTotalVotes_Ma$Partyabbre[order(partyByTotalVotes_Ma$LOD..Cand.Votes.Polled,decreasing = TRUE)[1:5]]
ManipurTop5 <- subset(Manipur, Partyabbre %in% top5Party_Ma)

# ANNOVA - Is the vote %age of vote of various parties accross various consituiencies equal?(Level of Significance = 99%)
aov.model_Ma <- aov(ManipurTop5$candidate_vote_share~factor(ManipurTop5$Partyabbre))
summary(aov.model_Ma)
# As the p-value is less that our level of significance, we reject the Null Hypothesis that the vote %age is equal. 

#Where are the differences? 
TukeyHSD(aov.model_Ma)
plot(TukeyHSD(aov.model_Ma))
