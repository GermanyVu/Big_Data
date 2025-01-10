library(textir) # to get the data

library(maptpx) # for the topics function

data(congress109) # load the data

print(congress109Counts)
colnames(congress109Counts)
rownames(congress109Counts)
fs <- scale(as.matrix(congress109Counts/rowSums(congress109Counts) ))

#question 1 Fit K-means to speech text for K in 5,10,15,20,25. Use an IC to choose 
# the K and interpret the selected model.
for (i in c(5,10,15,20,25)) {
kmfs <- kmeans(fs,i)  
print(i)
print(kmfs)

}
#kfit <- lapply(1:200, function(k) kmeans(fs,k))
source("kIC.R") ## utility script

# you give it kmeans fit, 
# then "A" for AICc (default) or "B" for BIC

kaicc <- sapply(kfit,kIC)

kbic <- sapply(kfit,kIC,"B")


## plot 'em

plot(kaicc, xlab="K", ylab="IC", 
     ylim=range(c(kaicc,kbic)), # get them on same page
     bty="n", type="l", lwd=2)

abline(v=which.min(kaicc))

lines(kbic, col=4, lwd=2)



abline(v=which.min(kbic),col=4)
#the index with the minimum BIC is 2
# this means that the we need to split it up into two groups 

#question2 Fit a topic model for the speech counts. Use Bayes factors to choose 
#the number of topics, and interpret your chosen model.

x <- as.simple_triplet_matrix(congress109Counts)


tpc <- topics(x,K=10) 


dim(tpc$theta)
colSums(tpc$theta)

dim(tpc$omega)
rowSums(tpc$omega)

## choosing the number of topics
## If you supply a vector of topic sizes, it uses a Bayes factor to choose
## (BF is like exp(-BIC), so you choose the bigggest BF)
## the algo stops if BF drops twice in a row

tpcs <- topics(x,K=5*(1:5), verb=10) # it chooses 10 topics 

# summary prints the top `n' words for each topic,
# under ordering by `topic over aggregate' lift:
#    the topic word prob over marginal word prob.

summary(tpcs, n=10) 


#question 3  Connect the unsupervised clusters to partisanship.
# tabulate party membership by K-means cluster. Are there any non-partisan topics?
#  I fit topic regressions for each of party and repshare. Compare to regression onto phrase percentages:
#  x<-100*congress109Counts/rowSums(congress109Counts)

library(gamlr)


xclust <- sparse.model.matrix(~factor(kfit[[k]]$cluster)+wine$color) # cluster membership matrix

wineregclust <- cv.gamlr(xclust,wine$quality,lambda.min.ratio=1e-5) # 

plot(wineregclust)

max(1-wineregclust$cvm/wineregclust$cvm[1]) # OOS R2 around 0.22

