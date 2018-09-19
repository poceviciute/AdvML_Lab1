# Advanced machine learning

#Lab 1

library(bnlearn)
library(gRain) 
library(RBGL)
#source("https://bioconductor.org/biocLite.R")
#biocLite("RBGL")
library(gRbase)

#1
data("asia")
summary(asia)

hc1<-hc(asia)
summary(hc1)
plot(hc1)
arcs(hc1)
vstructs(hc1)
cpdag(hc1)

hc2 <- hc(asia, start = NULL, score = "bde", restart = 2, iss=100)
plot(hc2)
cpdag(hc2)

hc3 <- hc(asia, start = NULL, score = "bde", restart = 2, iss=1000)
plot(hc3)
cpdag(hc3)

hc4 <- hc(asia, start = NULL, score = "bde", restart = 10, iss=100)
plot(hc4)
cpdag(hc4)

hc5 <- hc(asia, start = NULL, score = "bde", restart = 10, iss=1000)
plot(hc5)
cpdag(hc5)

hc6 <- hc(asia, start = NULL, restart = 10)
plot(hc6)
cpdag(hc6)

startgraph <- random.graph(nodes=colnames(asia), num=1)
plot(startgraph)
hc7 <- hc(asia, start = startgraph)
plot(hc7)

hc8 <- hc(asia, start = startgraph, score = "bde", restart = 10, iss=100)
plot(hc8)

all.equal(hc1, hc2)
all.equal(hc1, hc3)
all.equal(hc1, hc4)
all.equal(hc1, hc5)
all.equal(hc1, hc6) 
all.equal(hc1, hc7) 
all.equal(hc1, hc8) 
all.equal(hc2, hc3)
all.equal(hc2, hc4)
all.equal(hc2, hc5)
all.equal(hc2, hc6)
all.equal(hc2, hc7)
all.equal(hc2, hc8)
all.equal(hc3, hc4)
all.equal(hc3, hc5)
all.equal(hc3, hc6)
all.equal(hc3, hc7)
all.equal(hc3, hc8)
all.equal(hc4, hc5)
all.equal(hc4, hc6)
all.equal(hc4, hc7)
all.equal(hc4, hc8)
all.equal(hc5, hc6)
all.equal(hc5, hc7)
all.equal(hc5, hc8)
all.equal(hc6, hc7)
all.equal(hc6, hc8)
all.equal(hc7, hc8)


#2
N <- nrow(asia)
ind <- sample(1:N, size = 0.8*N)
train <- asia[ind,]
test <- asia[-ind,]

train_learn <- hc(train)
summary(train_learn)
plot(train_learn)
ml_fit <- bn.fit(train_learn, train, method = "mle")
bn.fit.barchart(ml_fit$A)
bn.fit.barchart(ml_fit$S)
bn.fit.dotplot(ml_fit$S)


ml_grain <- as.grain(ml_fit)
ml_comp <- compile(ml_grain) #moralization, triangulation, now ready to predict

#apply setevidence to every row in test without S to get prob. for S
#then classify S for each row with prob.
new_test <- test[,-which(colnames(test)=="S")]


# Prediction
grain_pred <- apply(new_test, 1, function(x){
  setev <- setEvidence(ml_comp, evidence = x)
  querygrain(setev)$S})

gp_no<-grain_pred["no",]
gp_yes<-grain_pred["yes",]

pred_S <- vector()
pred_S[which(gp_no>0.5)] <- "no"
pred_S[which(gp_yes>=0.5)] <- "yes"

# Confusion matrix
table(test$S, pred_S)

# qg1<-querygrain(setEvidence(ml_comp, evidence = list(new_test[1,])))
# qg1$S
# querygrain(setEvidence(ml_comp, evidence = list(test[1,-4])))
# querygrain(setEvidence(ml_comp, evidence = list(test[1,-6])))
# querygrain(ml_grain, nodes = nodeNames(ml_grain)) #marginal
# querygrain(ml_grain, nodes = nodeNames(ml_grain), type = "conditional")


dag <- model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #true graph
dag_fit <- bn.fit(dag, train, method = "mle")
#all.equal(ml_fit, dag_fit)
dag_grain <- as.grain(dag_fit)
dag_comp <- compile(dag_grain)
dag_pred <- apply(new_test, 1, function(x){
  setev <- setEvidence(dag_comp, evidence = x)
  querygrain(setev)$S})
dag_no<-dag_pred["no",]
dag_yes<-dag_pred["yes",]

# Prediction
dagpred_S <- vector()
dagpred_S[which(dag_no>0.5)] <- "no"
dagpred_S[which(dag_yes>=0.5)] <- "yes"

# Confusion matrix
table(test$S, dagpred_S)

# plot(dag)
# arcs(dag)
# arcs(train_learn)
# plot(hc1)

## works well because we have a small graph so we can do exact
#(If they have the same markov blanket they will produce the same results. They will predict the same.)
#(Network is small, not that much data.)

# 3

markov_blanket <- mb(ml_fit, "S")
blanket_test <- test[,markov_blanket]

mb_pred <- apply(blanket_test, 1, function(x){
  setev <- setEvidence(ml_comp, evidence = x)
  querygrain(setev)$S})

mb_no<-mb_pred["no",]
mb_yes<-mb_pred["yes",]

mb_S <- vector()
mb_S[which(mb_no>0.5)] <- "no"
mb_S[which(mb_yes>=0.5)] <- "yes"

table(test$S, mb_S)


#### 4 Naive-Bayes

# Construct naive bayes network
n_names <- colnames(train)
n_names <- n_names[-which(n_names=="S")]
nb <- empty.graph(c(n_names, "S"))
arc_set <- matrix(c(rep("S",length(n_names)),n_names), ncol = 2,
                  byrow = FALSE, dimnames = list(NULL, c("from", "to")))
arcs(nb) <- arc_set
plot(nb)

# Fit network
nb_fit <- bn.fit(nb, train, method = "mle")
nb_grain <- as.grain(nb_fit)
nb_comp <- compile(nb_grain)

# Prediction
nb_pred <- apply(new_test, 1, function(x){
  setev <- setEvidence(nb_comp, evidence = x)
  querygrain(setev)$S})
nb_no<-nb_pred["no",]
nb_yes<-nb_pred["yes",]

nb_S <- vector()
nb_S[which(nb_no>0.5)] <- "no"
nb_S[which(nb_yes>=0.5)] <- "yes"

# Confusion matrix
table(test$S, nb_S)
#bn <- naive.bayes(train, "S")
#plot(bn)


#Why is S pointing to the other variables and not the other way around?
#S disease, the other are symptoms. Disease causes the symptoms. 
#They are distributed in a certain way given the class variable. 
#The other way around is computationally demanding, but can be done 
#(but it's not naive bayes because everything is made dependent).