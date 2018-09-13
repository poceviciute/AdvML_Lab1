# Advanced machine learning

#Lab 1

library(bnlearn)
library(gRain) 
library(RBGL)
#source("https://bioconductor.org/biocLite.R")
#biocLite("RBGL")
library(gRbase)

#Questio 1
data("asia")
summary(asia)

BN1<-hc(asia)
summary(BN1)
plot(BN1)
arcs(BN1)
vstructs(BN1)
cpdag(BN1)

BN2 <- hc(asia, start = NULL, restart = 10)
plot(BN2)
cpdag(BN2)

#The imaginary or equivalent sample size of the prior distribution can be specified using the iss parameter; 
#it specifies the weight of the prior compared to the sample and thus controls the smoothness of the posterior distribution.

BN3 <- hc(asia, start = NULL, score = "bde", restart = 2, iss=10)
plot(BN3)
cpdag(BN3)

BN4 <- hc(asia, start = NULL, score = "bde", restart = 2, iss=100)
plot(BN43)
cpdag(BN4)

BN5 <- hc(asia, start = NULL, score = "bde", restart = 5, iss=50)
plot(BN5)
cpdag(BN5)

BN6 <- hc(asia, start = NULL, score = "bde", restart = 10, iss=50)
plot(BN6)
cpdag(BN6)

cnames <- colnames(asia)
startg <- random.graph(nodes=cnames, num=1, method="melancon")
plot(startg)
BN7 <- hc(asia, start = startg)
plot(BN7)

BN8 <- hc(asia, start = startg, score = "bde", restart = 5, iss=10)
plot(BN8)

all.equal(BN8, BN2) 
all.equal(BN8, BN4)
all.equal(BN1,BN7) 
all.equal(BN1, BN6) 
all.equal(BN1,BN2)
all.equal(BN2,BN3)
all.equal(BN2, BN4)
all.equal(BN3,BN4)
all.equal(BN1, BN3)
all.equal(BN1,BN4)
all.equal(BN3, BN5)
all.equal(BN2, BN5)
all.equal(BN4,BN5)
all.equal(BN1, BN5)


# Question 2
N <- nrow(asia)
ind <- sample(1:N, size = 0.8*N)
train <- asia[ind,]
test <- asia[-ind,]

train_learn <- hc(train, start = NULL, restart = 10)
summary(train_learn)
plot(train_learn)

# Learn parameters using ML
ml_fit <- bn.fit(train_learn, train, method = "mle")
ml_fit
bn.fit.barchart(ml_fit$A)
bn.fit.barchart(ml_fit$S)
bn.fit.dotplot(ml_fit$S)



# Learn parameters using Bayes method
#bayes_fit = bn.fit(train_learn, train, method = "bayes")
#bayes_fit
#bn.fit.barchart(bayes_fit$A)
#bn.fit.barchart(bayes_fit$S)
#bn.fit.dotplot(bayes_fit$S)

# As S has no parents, the porbabilities are close to 0,5


# Using grain package"
ml_grain <- as.grain(ml_fit)

ml_grain2 <- compile(ml_grain) 
prop_ml_grain <- propagate.grain(ml_grain2)
querygrain(ml_grain, nodes = nodeNames(ml_grain)) # marginal prob
querygrain(ml_grain,nodes=c("L","B"), type="joint")# joint prob

tt <- querygrain(prop_ml_grain, type="joint")
sum(tt==0)/length(tt)


##################################
# Do we need to come up with our own evidence?


##################################
## Prediction based on evidence


cnames2 <- colnames(test[-2])
new_test <- test[,-2]

# create the list
create_list <- function(l) {
  list1 <- as.list(as.character(l)) #new_test[1,]
  yesno <- c("no", "yes")
  final_list <- list()
  for (i in 1:length(l)) {
    if (list1[i] == "1") {
      final_list[i] <- yesno[1]
    } else {
      final_list[i] <- yesno[2]
    }
  }
  names(final_list) <- cnames2
  return(final_list)
}

#evidence_lists <- apply(new_test,1,create_list)

# They are supposed to be the same, but they are not :((((
net12 <- setEvidence(ml_grain, evidence=list(A="no",T="yes",L="no",B="no",E="yes",X="yes" ,D="yes"))
pEvidence(net12)
Ev1 <- setEvidence(ml_grain,nodes=as.vector(cnames2),states=as.vector(test[1,-2])) # S is no
pEvidence(Ev1)
######
# Predict probabilities based on test data DOES NOT WORK ;(
final_prob <- apply(new_test,1,function(line){
  list_ev <- create_list(line)
  Ev <- setEvidence(ml_grain,evidence=list_ev)
  return(pEvidence(Ev))
})
####
#length(final_prob)
#dim(new_test)

## works
final_prob2 <- c()
Evid <- c()
list_ev<- list()
for (i in 1:nrow(new_test)){
  list_ev <- create_list(new_test[i,])
  Evid <- setEvidence(prop_ml_grain,evidence=list_ev)  
  final_prob2[i] <- pEvidence(Evid)
}
###
hist(final_prob2)

Ev <- setEvidence(ml_grain,evidence=create_list(new_test[1,])) # 
pEvidence(Ev)

final_prob[150]
final_prob2[1]



se1<-setEvidence(compile(ml_LS), evidence = list(S=c(1,0),new_test[1,]))
pEvidence(se1)
querygrain(se1)


plot(train_learn)

# Prediction Uniform
prob_no <- ml_fit$S$prob["no"]
s <- vector(length = nrow(test))
for(i in 1:nrow(test)){
  if(runif(1)<prob_no){
    s[i] <- "no"
  }else{
    s[i] <- "yes"
  }
}
# Confusion matrix
table(test$S, s)

dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #true graph
plot(dag)
arcs(dag)
arcs(hc1)
plot(hc1)
