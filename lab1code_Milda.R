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

##################################################
# Question 2
N <- nrow(asia)
ind <- sample(1:N, size = 0.8*N)
train <- asia[ind,]
test <- asia[-ind,]
cnames2 <- colnames(test[-2])
new_test <- test[,-2]

train_learn <- hc(train, start = NULL, restart = 10)
summary(train_learn)
plot(train_learn)

# Learn parameters using ML
ml_fit <- bn.fit(train_learn, train, method = "mle")
ml_fit
#bn.fit.barchart(ml_fit$A)
#bn.fit.barchart(ml_fit$S)
#bn.fit.dotplot(ml_fit$S)

#pred1 <- predict(ml_fit,node = "S", data=test)

# As S has no parents, the porbabilities are close to 0,5


# Using grain package"
ml_grain <- as.grain(ml_fit)
ml_grain <- compile(ml_grain) 

S_probs2 <- c()
S_probs2<- apply(new_test,1,function(a){
    Evid2 <- setEvidence(ml_grain,evidence=a)
    probs <- querygrain(Evid2)$S
    return(probs)
})

Predict2 <- c()
Predict2[which(S_probs2[1,]>0.5)] <- "no"
Predict2[which(S_probs2[2,]>=0.5)] <- "yes"

table(test$S, Predict2)


##################################
## Prediction based on evidence




# # create the list
# create_list <- function(l) {
#   list1 <- as.list(as.character(l)) #new_test[1,]
#   yesno <- c("no", "yes")
#   final_list <- list()
#   for (i in 1:length(l)) {
#     if (list1[i] == "1") {
#       final_list[i] <- yesno[1]
#     } else {
#       final_list[i] <- yesno[2]
#     }
#   }
#   names(final_list) <- cnames2
#   return(final_list)
# }
# 
# 
# ## works
# 
# Predictions <- c()
# Evid <- c()
# list_ev<- list()
# for (i in 1:nrow(new_test)){
#   list_ev <- create_list(new_test[i,])
#   Evid <- setEvidence(ml_grain2,evidence=list_ev)  
#   probs <- querygrain(Evid)
#   
#   if (probs$S[1]>0.5){
#       Predictions[i] <- "no"  
#   }else{
#       Predictions[i] <- "yes" 
#   }
#   
# }
###

# Confusion matrix with my graph
#table(test$S, Predictions)

# The real graph
dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #true graph
dag_fit <- bn.fit(dag, train, method = "mle")
dag_grain <- as.grain(dag_fit)
dag_compile <- compile(dag_grain)


S_probs <- c()
S_probs<- apply(new_test,1,function(a){
    Evid2 <- setEvidence(dag_compile,evidence=a)
    probs <- querygrain(Evid2)$S
    return(probs)
})

Predict_S <- c()
Predict_S[which(S_probs[1,]>0.5)] <- "no"
Predict_S[which(S_probs[2,]>=0.5)] <- "yes"

table(test$S, Predict_S)

################################################
# Question 3

plot(train_learn)
mb_done <- mb(ml_fit,"S")
#mb_done2 <- mb(ml_fit,"L")
new_test3 <- new_test[,mb_done]
S_probs3 <- c()
S_probs3<- apply(new_test3,1,function(a){
    Evid2 <- setEvidence(dag_compile,evidence=a)
    probs <- querygrain(Evid2)$S
    return(probs)
})

Predict3 <- c()
Predict3[which(S_probs3[1,]>0.5)] <- "no"
Predict3[which(S_probs3[2,]>=0.5)] <- "yes"

table(test$S, Predict3)

######################
# Question 4
# Naive Bayes
cnames <- colnames(asia)
naive_graph = empty.graph(cnames)
arc.set = matrix(c("S", "A", "S","D", "S", "X", "S", "E", "S", "B", "S", "L", "S", "T"),
                             ncol = 2, byrow = TRUE,
                             dimnames = list(NULL, c("from", "to")))
arcs(naive_graph) = arc.set
plot(naive_graph)

naive_fit <- bn.fit(naive_graph, train, method = "mle")
# Using grain package"
naive_grain <- as.grain(naive_fit)
naive_grain <- compile(naive_grain) 


S_probs4 <- c()
S_probs4<- apply(new_test,1,function(a){
    Evid2 <- setEvidence(naive_grain,evidence=a)
    probs <- querygrain(Evid2)$S
    return(probs)
})

Predict4 <- c()
Predict4[which(S_probs4[1,]>0.5)] <- "no"
Predict4[which(S_probs4[2,]>=0.5)] <- "yes"

table(test$S, Predict4)

S_probs4[,30:40]
S_probs3[,30:40]
S_probs2[,30:40]
S_probs[,30:40]