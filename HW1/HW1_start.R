# ***** AMAZON REVIEWS 

# READ REVIEWS

data<-read.table("Review_subset.csv",header=TRUE)
dim(daGETWDta)
print(data)
# 13319 reviews
# ProductID: Amazon ASIN product code
# UserID:  id of the reviewer
# Score: numeric from 1 to 5
# Time: date of the review
# Summary: text review
# nrev: number of reviews by this user
# Length: length of the review (number of words)

# READ WORDS

words<-read.table("words.csv")

words<-words[0:1066,1] #sliced it down a lot 
print(words)
words_len<-length(words)
print(words_len)
#1125 unique words

# READ text-word pairings file

doc_word<-read.table("word_freq.csv")
print(doc_word)
#replaces the names of the cols with specified vector
names(doc_word)<-c("Review ID","Word ID","Times Word" ) #combines list of elements 

doc_word <- doc_word[0:20,]
print(doc_word)
# Review ID: row of the file  Review_subset
# Word ID: index of the word
# Times Word: number of times this word occurred in the text


# We'll do 1125 univariate regressions of 
# star rating on word presence, one for each word.
# Each regression will return a p-value, and we can
# use this as an initial screen for useful words.

# Don't worry if you do not understand the code now.
# We will go over similar code in  the class in a few weeks.

# Create a sparse matrix of word presence


library(gamlr)
# 
# spm<-sparseMatrix(i=doc_word[,1], #  rows
#                   j=doc_word[,2], # cols
#                   x=doc_word[,3], # data
#                   dimnames=list(id=1:nrow(data),words=words)) # assigns row names and col names
spm<-sparseMatrix(i=doc_word[,1], #  rows
                  j=doc_word[,2], # cols
                  x=doc_word[,3]) # data

sp<-sparseMatrix(i=doc_word[,1], #  rows
                  j=doc_word[,2], # cols
                  x=doc_word[,3],
                 dimnames=list(id=1:6,words=words)) # data
                
print(sp)
dim(spm)
class(spm)
# 13319 reviews using 1125 words

# Create a dense matrix of word presence

P <- as.data.frame(as.matrix(sp>0))
P
m <- as.matrix(sp )
m
library(parallel)

# to make a function in r
# function_name <- function(parameters){
#   function body 
# }
margreg <- function(p){
	fit <- lm(stars~p)
	sf <- summary(fit)
	return(sf$coef[2,4]) 
}

# The code below is an example of parallel computing
# No need to understand details now, we will discuss more later

cl <- makeCluster(detectCores())

# Pull out stars and export to cores

stars <- data$Score #list of reviews
clusterExport(cl,"stars") 
 
# Run the regressions in parallel

mrgpvals <- unlist(parLapply(cl,P,margreg))
mrgpvals_ordered<-mrgpvals[order(mrgpvals,decreasing=FALSE)]
plot(mrgpvals_ordered,pch=9)

# If parallel stuff is not working, 
# you can also just do (in serial):
# mrgpvals <- c()
# for(j in 1:1125){
# 	print(j)
# 	mrgpvals <- c(mrgpvals,margreg(P[,j]))
# }
# make sure we have names

names(mrgpvals) <- colnames(P)

# The p-values are stored in mrgpvals 


library(Matrix)
i <- c(1, 4, 5, 6)
j <- c(2, 7, 3, 1)
x <- c(2,2, 2, 2)
sm <- sparseMatrix(i, j, x = x, dimnames=list(id=1:6,words=1:7))
print(sm)
dm <- as.data.frame(as.matrix(sm>0))

dm
for(i in 1:6){
for(j in 1:1066){
  if (P[i,j]){
  print(c(i,j) ) 
  
    }
  
   }
}
#-------------------------------------------------------
#pseudo data sep = ','
data<-read.table("Review_subset_pseudo.csv",header=TRUE, sep = ',')

print(data)

words<-read.table("words_pseudo.csv" , sep = ',')
print(words)

doc_word<-read.table("word_freq_pseudo.csv", sep = ',')
print(doc_word)
names(doc_word)<-c("Review ID","Word ID","Times Word" )

sp<-sparseMatrix(i=doc_word[,1], #  rows
                 j=doc_word[,2], # cols
                 x=doc_word[,3],
                 dimnames=list(id=1:20,words=words[1:26,1]))# dat
P <- as.data.frame(as.matrix(sp>0))
p <- P[,1]
stars <- data$Score #list of reviews
stars
for(j in 1:1){
	print(j)
	mrgpvals <- c(mrgpvals,margreg(P[,1]))
}
fit <- lm(stars~p)
sp
