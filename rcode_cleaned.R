library(haven)
library(foreign)
library(MASS)
library(lmtest)
library(sandwich)
library(readr)
library(tidyverse)
library(stargazer)
library(haven)
setwd("Desktop/sem. 3/advanced comparative politics/term paper/data")

gwf_original <- read_dta("gwf-original.dta")

gwf_original$
df <- read_dta("Dataset_Dec2008.dta")

perso <- read_csv("GWF+personalism-scores.csv")


personalism_data <- data.frame(polity_code = perso$cowcode, year= perso$year,personalism= perso$latent_personalism,
                               pers_2pl = perso$pers_2pl,  perse_se_2pl= perso$pers_se_2pl, 
                               leader_duration=log(perso$gwf_leader_duration),first_leader=perso$gwf_firstldr ,
                               case_fail =perso$gwf_case_fail,milmerit=perso$milmerit_persB)


#merge data for data preparation
df <- merge(x = df, y = personalism_data , by = c("polity_code","year"), all.x = TRUE)

df_new <- df[which(df$year >= 1993),]


#replication study for Egorov's paper

#oil reserves
m1 <- lm(mf ~ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2 +factor(id)+factor(year),
         data=df_new )

summary(m1)

#with robust SE
coeftest(m1, vcov = vcovHC(m1, type="HC1"))


#oil price* oil reserves
df_new$logpriceres <- log(1+(df_new$oilprice*df_new$oilres))

m2 <- lm(mf~ polity2+ logpriceres +loggdppcppp+ logpoptotal+loggovgdp + logpriceres*polity2 +factor(id)+factor(year),
         data=df_new )

#with robust SE
coeftest(m2, vcov = vcovHC(m2, type="HC1"))


# oil production
m3 <-lm(mf ~ polity2+ logoilprod +loggdppcppp+ logpoptotal+loggovgdp + logoilprod*polity2 +factor(id)+factor(year),
        data=df_new )

#with robust SE
coeftest(m3, vcov = vcovHC(m3, type="HC1"))


# oil price* oil production
df_new$logpricepro <- log(1+(df_new$oilprice*df_new$oilprod))
  
m4 <- lm(mf ~ polity2+ logpricepro +loggdppcppp+ logpoptotal+loggovgdp + logpricepro*polity2 +factor(id)+factor(year),
        data=df_new )

#with robust SE
coeftest(m4, vcov = vcovHC(m4, type="HC1"))


#with personalism variable




m1_perso <- lm(mf ~ personalism+ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(id)+factor(year),
               data=df_new )

summary(m1_perso)

#with robust SE
coeftest(m1_perso, vcov = vcovHC(m1_perso, type="HC1"))

##### for r_square comparison  

### with same dataset  firstly without personalism 
df_new_new <- df_new[!is.na(df_new$personalism),]

r_square_test <- lm(mf ~ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(id)+factor(year),
               data=df_new_new )
#### with same dataset with personalism
summary(r_square_test)

r_square_test_perso <- lm(mf ~personalism+ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(id)+factor(year),
                data=df_new_new )

summary(r_square_test_perso)

#### with same dataset with personalism



#m2
m2_perso <- lm(mf ~ personalism+polity2 + logpriceres +loggdppcppp+ logpoptotal+loggovgdp+logpriceres*polity2+factor(id)+factor(year),
               data=df_new )

#with robust SE
coeftest(m2_perso, vcov = vcovHC(m2_perso, type="HC1"))

#m3
m3_perso <- lm(mf ~ personalism+polity2+ logoilprod +loggdppcppp+ logpoptotal+loggovgdp +logoilprod*polity2+factor(id)+factor(year),
               data=df_new )

#with robust SE
coeftest(m3_perso, vcov = vcovHC(m3_perso, type="HC1"))

#m4
m4_perso <- lm(mf ~ personalism+ polity2+logpricepro +loggdppcppp+ logpoptotal+loggovgdp +logpricepro*polity2+factor(id)+factor(year),
         data=df_new )

#with robust SE
coeftest(m4_perso, vcov = vcovHC(m4, type="HC1"))


#personalism

m1_perso_oil <- lm(personalism ~leader_duration+first_leader+leader_duration*first_leader+logoilres +loggdppcppp+ logpoptotal+loggovgdp+factor(id)+factor(year), data=df_new)

summary(m1_perso_oil)

m2_perso_oil <- lm(personalism ~leader_duration+first_leader+leader_duration*first_leader+logpriceres +loggdppcppp+ logpoptotal+loggovgdp+factor(id)+factor(year), data=df_new)
summary(m2_perso_oil)


#substantive effects for m1_perso
#simulation

nsim <- 1000

gamma_hat1 <- c(m1_perso$coefficients)

V_hat1 <- vcov(m1_perso)

S1 <- mvrnorm(nsim, gamma_hat1, V_hat1)

perso_scenario <- seq(min(df_new$personalism,na.rm = TRUE,0.1),max(df_new$personalism,na.rm = TRUE,0.9),length.out = 100)

EV_perso <- S1[,2]%*%t(perso_scenario)

quants_mean_fun <-  function(x) {
  c(quants = quantile(x, probs = c(0.025, 0.975)), mean = mean(x))
}

quants_mean <- apply(EV_perso, 2, quants_mean_fun)

#plot
plot(perso_scenario, quants_mean[1,], 
     pch = 19, 
     cex = 0.3,
     bty = "n", 
     las = 1,
     ylim = c(-25, 5), 
     ylab="Effect of Personalism on Media Freedom",
     main = "Expected Effect of Personalism on Media Freedom", 
     xlab = "Range of Personalism",
     type = "n")

# Now we add the lines.
lines(perso_scenario, quants_mean[3,])

lines(perso_scenario, quants_mean[1,], lty = "dashed", col = "grey")
lines(perso_scenario, quants_mean[2,], lty = "dashed", col = "grey")
#check if this plot is correct


#personalism countries
df_new2 <- df_new[!is.na(df_new$personalism), ]
df_new3 <- df_new2[!is.na(df_new2$mf), ]

X <- matrix(NA,length(unique(df_new3$abbr)),5)


X[,1] <- c("CUB", "HTI", "MEX", "GTM", "SLV", "VEN", "PER", "PRY", "RUS", "BLR", "ARM", "GEO", "AZE", "GNB", "GMB", "SEN","MRT",
           "NER", "CIV", "GIN", "BFA", "LBR", "SLE" ,"GHA", "TGO", "CMR", "NGA", "GAB" ,"CAF", "TCD", "COG", "ZAR",
           "UGA", "KEN", "TZA", "BDI" ,"RWA", "ERI", "AGO" ,"MOZ", "ZMB", "ZWE", "MWI", "ZAF", "NAM", "LSO" ,"BWA", "SWZ",
           "MDG", "MAR", "DZA", "TUN", "LBY" ,"SDN" ,"IRN", "IRQ", "EGY", "SYR" ,"JOR" ,"SAU", "KWT", "ARE", "OMN", "AFG",
           "TKM", "TJK", "KGZ", "UZB", "KAZ", "CHN", "MNG", "PRK", "PAK", "MMR", "LKA", "NPL", "THA", "KHM", "LAO", "MYS",
           "SGP", "IDN")

A <- c("CUB", "HTI", "MEX", "GTM", "SLV", "VEN", "PER", "PRY", "RUS", "BLR", "ARM", "GEO", "AZE", "GNB", "GMB", "SEN","MRT",
       "NER", "CIV", "GIN", "BFA", "LBR", "SLE" ,"GHA", "TGO", "CMR", "NGA", "GAB" ,"CAF", "TCD", "COG", "ZAR",
       "UGA", "KEN", "TZA", "BDI" ,"RWA", "ERI", "AGO" ,"MOZ", "ZMB", "ZWE", "MWI", "ZAF", "NAM", "LSO" ,"BWA", "SWZ",
       "MDG", "MAR", "DZA", "TUN", "LBY" ,"SDN" ,"IRN", "IRQ", "EGY", "SYR" ,"JOR" ,"SAU", "KWT", "ARE", "OMN", "AFG",
       "TKM", "TJK", "KGZ", "UZB", "KAZ", "CHN", "MNG", "PRK", "PAK", "MMR", "LKA", "NPL", "THA", "KHM", "LAO", "MYS",
       "SGP", "IDN")

for ( i in 1:length(A)) {
  
  X[i,2] <-  mean(df_new3$mf[which(df_new3$abbr == A[i])])
  X[i,3] <-  mean(df_new3$personalism[which(df_new3$abbr == A[i])])
}

#plot (media freedom and personalism with mean of each country)
pdf(file = "naiveregression.pdf")
m1_perso_only <- lm(mf ~ personalism, data=df_new )
summary(m1_perso_only)
par(mar=c(4,4,2,2))
plot (
  x = X[,3],
  y = X[,2],
  pch = 16,
  xlab = "Personalism",
  ylab = "Media Freedom",
  type = "p",
  las = 1,
  main = "",
  bty= "n",
  cex.lab=1.5,
  cex.axis=1.2
  
)
text(as.numeric(X[,3]),as.numeric(X[,2]), labels=X[,1], cex= 0.7 , pos = 3)
points(
  jitter(as.numeric(X[,3]), .50),
  jitter( as.numeric(X[,2]), .50),
  col = "blue",
  pch = 1
)

abline(
  a = (coef(m1_perso_only)[1]),    ### for intercept
  b = (coef(m1_perso_only)[2]),     ####for slope
  lwd = 1,
  col = "red"
)
dev.off()

#residuals
residuals_mf_model <- lm(mf ~ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(id)+factor(year),
               data=df_new3 )

residuals_perso_model <- lm(personalism ~ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(id)+factor(year),
                            data=df_new3 )

res_model <- lm(residuals(residuals_mf_model)~residuals(residuals_perso_model))
coeftest(res_model, vcov = vcovHC(res_model, type="HC1"))

#residual plot
pdf(file = "residuals.pdf")
plot (
  x = residuals(residuals_perso_model),
  y = residuals(residuals_mf_model),
  pch = 16,
  xlab = "Personalism",
  ylab = "Media Freedom",
  type = "p",
  las = 1,
  main = "Media Freedom and Personalism, Residuals",
  bty="n",
  cex.lab=1.5,
  cex.axis=1.2,
  cex.lab=1.2,
  cex.axis=1.2
)

abline(
  a = (coef(res_model)[1]),    ### for intercept
  b = (coef(res_model)[2]),     ####for slope
  lwd = 1,
  col = "blue"
)
dev.off()







#### cross-validation

## Cross Validation 
summary(m1_perso)

length(coef(m1_perso))

set.seed(17)
m <- 10
beta_res <- matrix(NA, nrow = m, ncol =length(coef(m1_perso)))
se_res <- matrix(NA, nrow = m, ncol =length(coef(m1_perso)))

for (i in 1:m) {
  
  
  sel <- sample(1:nrow(df_new), floor(2 / 3 * nrow(df_new)))
  
  
  m1_perso_cv <- lm(mf ~ personalism+ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(id)+factor(year),
                 data=df_new )
  
  
  
  #with robust SE
  coeftest(m1_perso_cv, vcov = vcovHC(m1_perso_cv, type="HC1"))
  
  
  beta_res[i,] <- c(coef(m1_perso_cv))
  se_res[i, ] <- sqrt(diag(vcov(m1_perso_cv)))
}


q <- apply(beta_res, 2, mean, na.rm = T)
seq <- apply(se_res ^ 2, 2, mean, na.rm = T)
sq <-
  apply(sweep(beta_res, 2, 
              apply(beta_res, 2, mean, 
                    na.rm = T)) ^ 2 / (m - 1), 2, 
        sum, na.rm = T) * (1 + 1 / m)

q_se <- sqrt(seq + sq)

m1_perso_cv_coef <- coef(m1_perso_cv)
m1_perso_cv_se <- sqrt(diag(vcov(m1_perso_cv)))

results <- cbind(m1_perso_cv_coef,m1_perso_cv_se,q, q_se)


##### for r_square score comparison with same data sets 

### with same dataset  firstly without personalism 
df_new_new <- df_new[!is.na(df_new$personalism),]

r_square_test <- lm(mf ~ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(id)+factor(year),
                    data=df_new_new )
#### with same dataset with personalism
summary(r_square_test)

r_square_test_perso <- lm(mf ~personalism+ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(id)+factor(year),
                          data=df_new_new )

summary(r_square_test_perso)



#### box plot 


summary(df_new_new$logoilres)
summary(df_new_new$personalism)  #median 0.49
library(tidyverse)
meanoilbiggerzero <- df_new_new[which(df_new_new$logoilres > 0),]
mean(meanoilbiggerzero$mf,na.rm=TRUE)
meanoilzero <- df_new_new[which(df_new_new$logoilres == 0),]
mean(meanoilzero$mf,na.rm=TRUE)

medianbiggerpersonalism <- df_new_new[which(df_new_new$personalism >= median(df_new_new$personalism)),]
mean(medianbiggerpersonalism$mf,na.rm=TRUE)
mediansmallerpersonalism <- df_new_new[which(df_new_new$personalism < median(df_new_new$personalism)),]
mean(mediansmallerpersonalism$mf,na.rm=TRUE)


pdf(file = "boxplot1.pdf")
par(mar=c(4,13,3,1))
boxplot(meanoilbiggerzero$mf, meanoilzero$mf, medianbiggerpersonalism$mf,mediansmallerpersonalism$mf,
        main = "Media Fredoom across Authoritarian regimes",
        frame.plot = FALSE,
        at = c(1,2,4,5),
        col = c("#009999", "#0000FF","#009999", "#0000FF"),
        names = c("oilreservers", "nonoilreservers", "median above personalism", "median below personalism"),
        las = 2,
        border=c("#009999", "#0000FF","#009999", "#0000FF"),
        horizontal  = TRUE,
        xlab="Media Freedom",
        notch  = TRUE,
        cex.axis = 1.1,
        pch=16,
        cex.lab=1.3
        
          )
axis(2, labels = FALSE, col = "blue")
axis(1, labels = FALSE, col = "blue")
dev.off()

#### cross-validation

## Cross Validation 
summary(m1_perso)




df_new11 <- data.frame(personalism = df_new$personalism,
                      polity2     = df_new$polity2 ,
                      logoilres   = df_new$logoilres,  
                      loggdppcppp = df_new$loggdppcppp,
                      logpoptotal  =df_new$logpoptotal, 
                      loggovgdp    =df_new$loggovgdp, 
                        id=     df_new$id,    
                        year=   df_new$year,
                       mf  = df_new$mf)

df_new11 <-  df_new11[complete.cases(df_new11), ]
m1_perso <- lm(mf ~ personalism+ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2,
               data=df_new11)
coeftest(m1_perso, vcov = vcovHC(m1_perso, type="HC1"))
summary(m1_perso)
set.seed(17)
m <- 10
beta_res <- matrix(NA, nrow = m, ncol =length(coef(m1_perso_cv)))
se_res <- matrix(NA, nrow = m, ncol =length(coef(m1_perso_cv)))

for (i in 1:m) {
  
  
  sel <- sample(1:nrow(df_new1), floor(2 / 3 * nrow(df_new1)))
  
  
  m1_perso_cv <- lm(mf ~ personalism+ polity2+logoilres +loggdppcppp+ logpoptotal+loggovgdp + logoilres*polity2+factor(year)+factor(id),
                    data=df_new1[sel,]) 
  
  coeftest(m1_perso_cv, vcov = vcovHC(m1_perso_cv, type="HC1"))
  length(coef(m1_perso_cv))
   beta_res[i,] <- c(coef(m1_perso_cv))
  se_res[i, ] <- sqrt(diag(vcov(m1_perso_cv)))
}


q <- apply(beta_res, 2, mean, na.rm = T)
seq <- apply(se_res ^ 2, 2, mean, na.rm = T)
sq <-
  apply(sweep(beta_res, 2, 
              apply(beta_res, 2, mean, 
                    na.rm = T)) ^ 2 / (m - 1), 2, 
        sum, na.rm = T) * (1 + 1 / m)

q_se <- sqrt(seq + sq)

m1_perso_cv_coef <- coef(m1_perso_cv)
m1_perso_cv_se <- sqrt(diag(vcov(m1_perso_cv)))

results <- cbind(m1_perso_cv_coef,m1_perso_cv_se,q, q_se)
colnames(results) <- c("Coefs Main Model", "SEs Maim Model", "Coefs Cross-Val", "SE Cross-Val" )
results
stargazer(results)