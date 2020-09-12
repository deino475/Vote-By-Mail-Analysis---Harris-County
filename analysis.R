#LOAD IMPORTANT LIBRARIES FOR ANALYSIS
library(leaflet)
library(descr)

#READ VOTER FILE DATA
ballots <- read.csv(file.choose())

#DERIVE IMPORTANT VARIABLES
ballots$Age <- 2020 - ballots$BirthDate
ballots$VoteByMail <- ""
ballots$VoteByMail[which(ballots$VoteType == "Ballot By Mail")] <- "Yes"
ballots$VoteByMail[which(ballots$VoteType != "Ballot By Mail")] <- "No"

#FILTER OUT DIRTY DATA
ballots <- ballots[which(ballots$Sex != "1"),]
ballots <- ballots[which(ballots$Sex != "0"),]
ballots <- ballots[which(ballots$Sex != ""),]

#VISUALIZE THE MEAN AGE BASED ON IF VOTE BY MAIL
boxplot(ballots$Age ~ ballots$VoteByMail, 
        horizontal = TRUE, 
        outline = FALSE,
        xlab = "Age of Voter",
        ylab = "Voted By Mail in 2019?",
        main = "Age By Voting Type")

crosstab(ballots$VoteByMail, ballots$Sex,
         xlab = "Sex",
         ylab = "Voted By Mail in 2019?",
         sub = "Intersection of Sex and Voting By Mail",
         prop.r = TRUE,
         prop.c = TRUE)

#PREPROCESSING DATA FOR MAP
houston.zip.codes <- rgdal::readOGR(file.choose())
houston.zip.codes$Voters <- 0
houston.zip.codes$Mail.In.Voters <- 0
houston.zip.codes$Color <- "#ECF0F1"

## ASSIGN
for (i in 1:nrow(houston.zip.codes)) {
  zip.code <- houston.zip.codes$ZIP_CODE[i]
  houston.zip.codes$Voters[i] <- length(ballots$VoteByMail[which(ballots$VoterZIP == zip.code)])
  sub.ballots <- ballots[which(ballots$VoterZIP == zip.code),]
  houston.zip.codes$Mail.In.Voters[i] <- length(sub.ballots$VoteByMail[which(sub.ballots$VoteByMail == "Yes")])
}
## REMOVE ZIP CODE DATA WITH NO VOTERS
houston.zip.codes <- houston.zip.codes[which(houston.zip.codes$Voters == 0)]
houston.zip.codes$Percent.Mail.In <- round(houston.zip.codes$Mail.In.Voters / houston.zip.codes$Voters * 100)

#CREATING MAP
pal <- colorNumeric(palette = "Blues", domain = houston.zip.codes$Percent.Mail.In)
m <- leaflet(houston.zip.codes)
m <- addTiles(m)
m <- addPolygons(m, color = ~pal(Percent.Mail.In), fillOpacity = 0.8, popup = ~paste0(ZIP_CODE,": ","<br>Number of Voters in 2019: ", Voters, "<br>Number of Mail-in Voters in 2019: ", Mail.In.Voters, "<br>Percent Mail-in: ", Percent.Mail.In, "%"))
m <- addLegend(m, pal = pal, values = ~Percent.Mail.In, opacity = 0.7, title = "% Vote By Mail", position = "bottomright")
m

#DISTRIBUTION OF DATES BALLOT RECEIVED
ballots$DateReceived <- as.Date(ballots$ActivityDate,"%m/%d/%Y %H:%M:%S")
ballots$DayDiff <- as.Date("2019-11-05") - ballots$DateReceived
ballots$DayDiff <- as.integer(ballots$DayDiff)
vbm.ballots <- ballots[which(ballots$VoteByMail == 1),]
hist(vbm.ballots$DayDiff, 
     main = "Days Before Election Harris County Clerk Received Ballot",
     xlab = "Number of Days Before",
     ylab = "Number of Ballots")

vbm.ballots$ReceivedCategory = "Days Before"
vbm.ballots$ReceivedCategory[which(vbm.ballots$DayDiff < 0)] <- "After Election"
vbm.ballots$ReceivedCategory[which(vbm.ballots$DayDiff == 0)] <- "Day Of Election"
freq(vbm.ballots$ReceivedCategory, main = "When Ballots Are Received", xlab = "Category")

boxplot(vbm.ballots$DayDiff, horizontal = TRUE)

#PROJECTED TWO DAY DELAY FOR BALLOTS
vbm.ballots$AdjustedTime <- vbm.ballots$DayDiff - 2
vbm.ballots$AdjustedReceived <- "Days Before"
vbm.ballots$AdjustedReceived[which(vbm.ballots$AdjustedTime < 0)] <- "After Election"
vbm.ballots$AdjustedReceived[which(vbm.ballots$AdjustedTime == 0)] <- "Day Of Election"
freq(vbm.ballots$AdjustedReceived, main = "When Ballots Are Received", xlab = "Category")

boxplot(vbm.ballots$AdjustedTime, horizontal = TRUE)


