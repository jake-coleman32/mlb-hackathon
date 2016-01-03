setwd("/Users/Jake/Documents/Trumedia_hackathon/mlb-hackathon-coleman")
library(dplyr)

#Reading in the data
p_13 <- read.csv("pitches_2013.csv",header=TRUE)
p_14 <- read.csv("pitches_2014.csv",header=TRUE)
p_15 <- read.csv("pitches_2015.csv",header=TRUE)
p_all <- rbind(p_13,p_14,p_15)

p_all$FB <- as.factor(p_all$FB)
p_all$stadium <- as.factor(p_all$stadium)
p_all$sameHand <- as.factor(p_all$sameHand)
p_all$man_on_base <- as.factor(p_all$man_on_base)
p_all$inside <- as.factor(p_all$inside)
p_all$pitchType <- as.factor(p_all$pitchType)
p_all$top_of_zone <- as.factor(p_all$top_of_zone)
p_all$x_mov <- abs(p_all$x_mov)
p_all$z_mov <- abs(p_all$z_mov)

########
#EDA
########

#Plotting HRs
p_15 <- read.csv("pitches_2015.csv",header=TRUE)
p_15_fb <- p_15 %>%
  filter(FB==1)
p_15_hr <- p_15_fb %>%
  filter(HR==1)

types <- unique(as.character(p_15_hr$pitchType))
p_15_hr$pitchCol = numeric(dim(p_15_hr)[1])
for(i in 1:dim(p_15_hr)[1]){
  p_15_hr$pitchCol[i] <- which(str_detect(types,as.character(p_15_hr$pitchType[i])))
}
cols <- rainbow(length(types),start = 0,end=1)
cols[4] <- "black"

(bzone <- mean(p_15$szb,na.rm=TRUE))
(tzone <- mean(p_15$szt,na.rm=TRUE))

with(p_15_hr%>%filter(batterHand=="L"),
     plot(px,pz,xlim = c(-2,2),ylim = c(1,4),pch=19,cex = .4,
          col = cols[pitchCol], main = "Home Runs by Left-Handed Hitters, 2015",
          xlab = "x-plane", ylab = "z-plane"))
legend('topright',types,col = cols,pch = 19,cex = .8)

segments(x0 = c(-.83,-.83,-.83,.83),
         y0 = c(bzone,tzone,bzone,tzone),
         x1 = c(.83,.83,-.83,.83),
         y1 = c(bzone,tzone,tzone,bzone),
         lwd=2)

with(p_15_hr%>%filter(batterHand=="R"),
     plot(px,pz,xlim = c(-2,2),ylim = c(1,4),pch=19,cex = .4,
          col = cols[pitchCol], main = "Home Runs by Right-Handed Hitters, 2015",
          xlab = "x-plane", ylab = "z-plane"))
legend('topright',types,col = cols,pch = 19,cex = .8)

segments(x0 = c(-.83,-.83,-.83,.83),
         y0 = c(bzone,tzone,bzone,tzone),
         x1 = c(.83,.83,-.83,.83),
         y1 = c(bzone,tzone,tzone,bzone),
         lwd=2)

#Correlation investigation
with(p_all,cor(x_mov,z_mov))
with(p_all,cor(x_mov,tot_mov))
with(p_all,cor(z_mov,tot_mov))

#HR by Stadium plot
HR_stadium <- p_all %>%
  group_by(stadium) %>%
  summarise(HRs = sum(HR))

HR_years <- cbind(HR_stadium_13$HRs,HR_stadium_14$HRs,HR_stadium_15$HRs)
colnames(HR_years) <- c("13","14","15")
rownames(HR_years) <- HR_stadium$stadium

HRs <- rbind(HR_stadium_13,HR_stadium_14,HR_stadium_15)
HRs$Year <- c(rep("13",30),rep("14",30),rep("15",30))

HRs <- data.frame(HRs)

library(ggplot2)
ggplot(HRs, aes(factor(stadium), HRs, fill = Year)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Stadium") +
  ggtitle("Home Runs by Stadium,Years")

#Getting percentiles for NYM in 2014, 2015, and total
which(names(sort(HR_years[,2]))=="NYM")/30
which(names(sort(HR_years[,3]))=="NYM")/30
which(names(sort(apply(HR_years,1,sum)))=="NYM")/30


##########
#Modeling
##########

#Dataset for models
p_all_fb <- p_all %>% filter(FB==1) 
p_all_fb$HR <- factor(p_all_fb$HR)

p_all_mod_dset <- p_all_fb %>%
  select(-c(pitcher,FB,battedBallDistance, battedBallAngle, LD, pitchID,
            vx0,vy0,vz0,x_mov,z_mov,ax,ay,az, spinRate,SpinDir))


p_mod_full <- glm(HR~.,data = p_all_mod_dset, family = "binomial" )
summary(p_mod_full)

#Get coefficients
p <- length(coefficients(p_mod_full))
coefs_mat <- matrix(numeric(p*3),ncol = 3)
colnames(coefs_mat) <- c("Estimate","Std. Error","Significance")
rownames(coefs_mat) <- names(coefficients(p_mod_full))
coefs_mat[,1] <- round(coefficients(p_mod_full),3)
coefs_mat[,2] <- round(sqrt(diag(summary(p_mod_full)$cov.unscaled)),3)
coefs_mat[,3] <- round(summary(p_mod_full)$coefficients[,4],3)

#Print coefficients
coef_mat1 <- coefs_mat[1:20,]
xtable(coef_mat1,digits = 3)
coef_mat2 <- coefs_mat[21:49,]
xtable(coef_mat2,digits = 3)

#Predictive Histogram
hist(predict(p_mod_full,type = "response"), main = "Histogram of Predicted HR/FB",
     xlab = "Predicted Probabilities",prob=TRUE)
