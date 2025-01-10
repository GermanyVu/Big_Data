#library(knitr) # library for nice R markdown output

#to import function 

#source("fdr.R")  # this command tells you what R script to look for the function 
                  #works similar to import in python



#functions
prob_calc <- function(odds){
  prob = odds/(1+odds)
  return( prob)
}
## deviance calculations

## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}


fdr_cut <- function(pvals, q, plotit=FALSE, ...){
  pvals <- pvals[!is.na(pvals)]
  N <- length(pvals)
  
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/N) ])
  
  if(plotit){
    sig <- factor(pvals<=alpha)
    o <- order(pvals)
    plot(pvals[o], col=c("grey60","red")[sig[o]], pch=20, ..., 
         ylab="p-values", xlab="tests ordered by p-value", main = paste('FDR =',q))
    lines(1:N, q*(1:N)/N)
  }
  
  return(alpha)
}

#------------------------------------------------------------------------------------


## Read in the data
homes <- read.csv("homes2004.csv")
# conditional vs marginal value
par(mfrow=c(1,2)) # 1 row, 2 columns of plots
hist(homes$VALUE, col="grey", xlab="home value", main="")
plot(VALUE ~ factor(BATHS),
     col=rainbow(8), data=homes[homes$BATHS<8,],
     xlab="number of bathrooms", ylab="home value")


# par(mfrow=c(1,2))
# plot(VALUE ~ STATE, data=homes,
#   col=rainbow(nlevels(homes$STATE)),
#   ylim=c(0,10ˆ6), cex.axis=.65)



# linear regression on log(PRICE) on everything except AMMORT
pricey <- glm(log(LPRICE) ~ .-AMMORT, data=homes)

#what is R2 

#how many coefficients are there?
num_coef <- length(summary(pricey)$coef[-1,1])

# extract pvalues
pvals <- summary(pricey)$coef[-1,4]
# example: those variable insignificant at alpha=0.1
sig_names <- names(pvals)[pvals <= .1]
print(length(sig_names))

q=0.1
alpha_star <- fdr_cut(pvals, q)
pvals_ordered <- pvals[order(pvals,decreasing=F)] 
print(alpha_star)
pvals_sig <- pvals_ordered[ which(pvals_ordered <= alpha_star)]
pvals_insig <- pvals_ordered[ which(pvals_ordered > alpha_star)]
print(paste0("number of significant p values: ", length(pvals_sig) ))

# you'll want to do .1 with your FDR cutoff
# you can use the `-AMMORT' type syntax to drop variables
plot(pricey,pch=19)
r_sq = R2(log(homes$LPRICE),(pricey$fitted.values), family="gaussian")
print(r_sq)
print(1- 7187/13000)
insig_names = names(pvals_insig)
#remove the insignificant variables
pricey_sig <- glm(log(LPRICE) ~ .-AMMORT - NUNITS -  BEDRMS - ETRANS  , data=homes)
r_sq_subtract = R2(log(homes$LPRICE),(pricey_sig$fitted.values), family="gaussian")
pricey_sig
print(1- 7188/13000)
print(r_sq_subtract)


#question 2
homes$gt20dwn <-
  factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)
print(homes$gt20dwn)

pricey_dwn <- glm((gt20dwn) ~ .-AMMORT -LPRICE   , data=homes, family = "binomial")
sp <- summary(pricey_dwn)
# length(sp$coefficients[,1])
# beta_pa = sp$coefficients[34,1]
# beta_bath = sp$coefficients[37,1]

beta_pa = pricey_dwn$coefficients[which(names(pricey_dwn$coefficients) == "STATEPA")]
beta_baths = pricey_dwn$coefficients[which(names(pricey_dwn$coefficients) == "BATHS")]
beta_firsthome = pricey_dwn$coefficients[which(names(pricey_dwn$coefficients) == "FRSTHOY")]

odds_pa = exp(beta_pa)
odds_baths = exp(beta_baths)
odds_firsthome = exp(beta_firsthome)
prob_baths = prob_calc(odds_baths)
prob_firsthome = prob_calc(odds_firsthome)
prob_pa = prob_calc(odds_pa)
pricey_dwn_interaxn <- glm((gt20dwn) ~ .-AMMORT -LPRICE + FRSTHO*BATHS , data=homes, family = "binomial")
beta_interaxn = pricey_dwn_interaxn$coefficients[which(names(pricey_dwn_interaxn$coefficients) == "BATHS:FRSTHOY")]
odds_interaxn = exp(beta_interaxn)

plot(gt20dwn ~ FRSTHO, data=homes,
     col=c(1,3), xlab="Buyer's First Home?",
     #
     ylab="Greater than 20% down")

plot(  homes$gt20dwn[ which( homes$STATE == "PA")] ~  homes$MATBUY[ which( homes$STATE == "PA")]  ,
     xlab = "Buyer's First Home?",
     ylab = "Greater than 20% down" ,
     main = "first home?",
     pch  = 5,
     cex  = .1,
     col  = c(1,3))

plot(  homes$BATHS[ which( homes$STATE == "PA")] ~  homes$MATBUY[ which( homes$STATE == "PA")]  ,
       xlab = "Buyer's First Home?",
       ylab = "number of bathrooms" ,
       main = "first home?",
       pch  = 5,
       cex  = .1,
       col  = c(4,5))
#homes$FIRST_BATHS <- (ifelse(homes$MATBUY=="Y",1,0)*homes$BATHS)
# model with interaction 
pricey_dwn_interaxn <- glm((gt20dwn) ~ .-AMMORT -LPRICE + FRSTHO*BATHS   , data=homes, family = "binomial")

#question3


# Use the code ``deviance.R" to compute OOS deviance
source("deviance.R")
subset <- which(homes$VALUE>100000)

homes_subset <- subset(homes, VALUE>100000)
homes_test_set <- subset(homes, VALUE < 100000)
# Null model has just one mean parameter
ybar <- mean(log(homes_subset$LPRICE))
D0 <- deviance(y=log(homes_subset$LPRICE), pred=ybar)

r_sq_subset = 1- (ybar/D0)
print(r_sq_subset)
pricey_subset_model <- glm(log(homes_subset$LPRICE) ~ .-AMMORT, data=homes_subset, family = "gaussian")
pred_y = predict(pricey_subset_model, data =homes_test_set )
length(pred_y)
r_sq_test_set = R2(log(homes_test_set$LPRICE),pred_y, family="gaussian")

print(r_sq)
# extract pvalues
pvals_subset <- summary(pricey_subset)$coef[-1,4]

# q=0.1
# alpha_star <- fdr_cut(pvals, q)
# pvals_ordered <- pvals[order(pvals,decreasing=F)] 
# print(alpha_star)
# pvals_sig <- pvals_ordered[ which(pvals_ordered <= alpha_star)]
# pvals_insig <- pvals_ordered[ which(pvals_ordered > alpha_star)]
# print(paste0("number of significant p values: ", length(pvals_sig) ))
# 
# # you'll want to do .1 with your FDR cutoff
# # you can use the `-AMMORT' type syntax to drop variables
# plot(pricey,pch=19)
# r_sq = R2(log(homes$LPRICE),(pricey$fitted.values), family="gaussian")
