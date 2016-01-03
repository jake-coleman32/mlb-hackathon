##############
#Starter - Poisson regression

pitcher_2013 <- read.csv("pitcher_dset_2013.csv",header=TRUE,as.is=TRUE)
pitcher_2014 <- read.csv("pitcher_dset_2014.csv",header=TRUE)
pitcher_2015 <- read.csv("pitcher_dset_2015.csv",header=TRUE)

#Cleaning up pitchers, changing "NULLs" to NA
pitchers_13_c <- as.data.frame(lapply(pitcher_2013, function(x){
  null_x <- which(x=="NULL")
  y = x
  y[null_x] <- NA
  return(y)
}))

#Cleaning up pitchers, changing "NULLs" to NA
pitchers_14_c <- as.data.frame(lapply(pitcher_2014, function(x){
  null_x <- which(x=="NULL")
  y = x
  y[null_x] <- NA
  return(y)
}))

#Cleaning up pitchers, changing "NULLs" to NA
pitchers_15_c <- as.data.frame(lapply(pitcher_2015, function(x){
  null_x <- which(x=="NULL")
  y = x
  y[null_x] <- NA
  return(y)
}))

starters_13 <- pitchers_13_c %>% filter(IP > 100) 
starters_13$year <- 2013
starters_14 <- pitchers_14_c %>% filter(IP > 100)
starters_14$year <- 2014
starters_15 <- pitchers_15_c %>% filter(IP > 100)
starters_15$year <- 2015

st_stand_13 <- starters_13 %>%
  filter(ff_count>100,ch_count > 100, cu_count>100) %>%
  select(pitcherHand, pitcher, tot_pitches,tot_HR,tot_FB,tot_LD,tot_in_play,top_perc,inside_perc,
         IP, starts_with("ff_"),starts_with("cu_"),starts_with("ch_"))

st_stand_14 <- starters_14 %>%
  filter(ff_count>100,ch_count > 100, cu_count>100) %>%
  select(pitcherHand, pitcher, tot_pitches,tot_HR,tot_FB,tot_LD,tot_in_play,top_perc,inside_perc,
         IP, starts_with("ff_"),starts_with("cu_"),starts_with("ch_"))


st_stand_15 <- starters_15 %>%
  filter(ff_count>100,ch_count > 100, cu_count>100) %>%
  select(pitcherHand, pitcher, tot_pitches,tot_HR,tot_FB,tot_LD,tot_in_play,top_perc,inside_perc,
         IP, starts_with("ff_"),starts_with("cu_"),starts_with("ch_"))

#home run densities
#Not completely different, ok to use only 2015
#Not included in paper
plot(density(st_stand_13$tot_HR),col = 'blue',lwd=2,
     main = "Density of Home Runs, Starters With FF/CU/CH",
     xlab = "Home Runs")
lines(density(st_stand_14$tot_HR),col = 'red',lwd=2)
lines(density(st_stand_15$tot_HR),col = 'green',lwd=2)
legend("topright",c("2013","2014","2015"),lwd=2,col = c("blue","red","green"))

#Getting data ready for modeling
X_15 = st_stand_15 %>%
  select(starts_with("ff"),starts_with("cu"),starts_with("ch"),inside_perc,top_perc) %>%
  select(-ends_with("acc"),-ends_with("count"),-ends_with("tot_mov"),-ends_with("spin"))%>%
  mutate(ff_perc = logit(ff_perc), 
         cu_perc = logit(cu_perc),
         ch_perc = logit(ch_perc),
         top_perc = logit(top_perc),
         inside_perc = logit(inside_perc))

#Making it numeric
X_15n <- apply(X_15,2,function(x){
  return(as.numeric(as.character(x)))
})
X_15n <- cbind(rep(1,dim(X_15n)[1]),as.data.frame(X_15n))
names(X_15n) <- c("Int",names(X_15)) #intercept

#Model fine but coefs don't differ from zero
pois_mod <- glm(as.numeric(as.character(tot_HR))~ 
                  as.numeric(as.character(ff_velo)) + 
                  as.numeric(as.character(ff_xmov)) +
                  as.numeric(as.character(ff_perc)) + 
                  as.numeric(as.character(ff_zmov)) +
                  as.numeric(as.character(ch_velo)) + 
                  as.numeric(as.character(ch_xmov)) +
                  as.numeric(as.character(ch_perc)) +
                  as.numeric(as.character(ch_zmov))+
                  as.numeric(as.character(cu_velo)) +
                  as.numeric(as.character(cu_xmov)) +
                  as.numeric(as.character(cu_perc)) +
                  as.numeric(as.character(cu_zmov)),
                family = poisson(link = "log"),
                data = st_stand_15, offset = log(tot_pitches), 
                subset = (tot_pitches>0))
summary(pois_mod)
1-pchisq(pois_mod$deviance,pois_mod$df.residual)

####JAGS CODE
library(R2jags)
library(R2WinBUGS)
jags_l <- list(Y = as.numeric(as.character(st_stand_15$tot_HR)),
               X = X_15n,
               t = as.numeric(as.character(st_stand_15$IP)),
               n = dim(st_stand_15)[1],
               p = dim(X_15n)[2]
               )

hr_mod <- function(){
  #Likelihood
  for(i in 1:n){
    Y[i]~dpois(mu[i]*eta[i])
    eta[i]~dgamma(lambda[i],lambda[i])
    log(mu[i]) <- log(t[i]) + X[i,]%*%beta # + alp[hand[i]]
    lambda[i]~dgamma(2,2)
  }
  
  for(j in 1:p){
    beta[j] ~ dnorm(0,.01)
  }
}

params = c("beta","eta","lambda")

# Create a function that provides intial values for WinBUGS
hr.inits = function() {
  beta <- rep(0,jags_l$p)
 # alp <- rep(0,2)
  lambda <- rep(1,dim(X_15n)[1])
  return(list(beta=beta,lambda=lambda))
}


hr.model.file = paste(getwd(),"hr-model.txt", sep="/")
write.model(hr_mod, hr.model.file)


hr.sim = jags(jags_l, inits=hr.inits, params, model.file=hr.model.file,
                n.iter=5000)

#Runs chain until convergence, returns samples post-burn-in
hr.auto = autojags(hr.sim, n.iter = 5000)
hr.bugs = as.mcmc(hr.sim$BUGSoutput$sims.matrix)  # create an MCMC object

#Make output more manageable
beta_list <- vector("list",jags_l$p)
for(i in 1:jags_l$p){
  beta_list[[i]] <- as.numeric(hr.bugs[,paste0("beta[",i,"]")])
  hist(beta_list[[i]],main = names(X_15n)[i])
}
lapply(beta_list,quantile,probs = c(.05,.95))#See 95% credible intervals

beta_mat <- as.matrix(as.data.frame(beta_list))
colnames(beta_mat) <- names(X_15n)
boxplot(beta_mat,las = 2,main = "Coefficient Distributions",
        ylab = "Values")
abline(h=0)
(beta_means <- apply(beta_mat,2,mean))

#Model Validation: See if we can recreate data
#Pick a sample, do XiBeta + alpha + log(ti) to get mean, then draw from poisson with that mean

eta_list <- vector("list",dim(X_15n)[1])
for(i in 1:dim(X_15n)[1]){
  eta_list[[i]] <- as.numeric(hr.bugs[,paste0("eta[",i,"]")])
}
lapply(eta_list,quantile,probs = c(.025,.975))

eta_mat <- as.matrix(as.data.frame(eta_list))
colnames(eta_mat) <- st_stand_15$pitcher
boxplot(eta_mat,las=2,pch = 20,cex = .4,xaxt = 'n',xlab = "Pitchers")
abline(h=1)

eta_means <- apply(eta_mat,2,mean)

(samp <- sample(dim(hr.bugs)[1],1)) 
y <- numeric(dim(X_15n)[1])
mu <- numeric(dim(X_15n)[1])
for(i in 1:dim(X_15n)[1]){
  mu[i] <- exp(as.numeric(X_15n[i,])%*%beta_mat[samp,] + sum(alp_mat[samp,]) + log(jags_l$t[i]))
  y[i] <- rpois(1,mu[i]*eta_mat[samp,i])
}
hist(y,main = "Home Run Distribution",
     xlab = "HR Counts",breaks = 10)

#True data
hist(jags_l$Y, main = "Home Run Distribution", xlab = "HR Counts", breaks = 5)
