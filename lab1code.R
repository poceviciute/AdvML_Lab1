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

hc2 <- hc(asia, start = NULL, score = "bde", restart = 2, iss=10)
plot(hc2)
cpdag(hc2)

hc3 <- hc(asia, start = NULL, score = "bde", restart = 2, iss=100)
plot(hc3)
cpdag(hc3)

hc4 <- hc(asia, start = NULL, score = "bde", restart = 10, iss=10)
plot(hc4)
cpdag(hc4)

hc5 <- hc(asia, start = NULL, score = "bde", restart = 10, iss=100)
plot(hc5)
cpdag(hc5)

hc6 <- hc(asia, start = NULL, restart = 10)
plot(hc6)
cpdag(hc6)

startgraph <- random.graph(nodes=colnames(asia), num=1)
plot(startgraph)
hc7 <- hc(asia, start = startgraph)
plot(hc7)

hc8 <- hc(asia, start = startgraph, score = "bde", restart = 10, iss=10)
plot(hc8)

all.equal(hc8, hc2) #the same
all.equal(hc8, hc4)
all.equal(hc1,hc7) #the same
all.equal(hc1, hc6) #the same
all.equal(hc1,hc2)
all.equal(hc2,hc3)
all.equal(hc2, hc4)
all.equal(hc3,hc4)
all.equal(hc1, hc3)
all.equal(hc1,hc4)
all.equal(hc3, hc5)
all.equal(hc2, hc5)
all.equal(hc4,hc5)
all.equal(hc1, hc5)


#2

ind <- sample(1:5000, size = 0.8*5000)
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
compile(ml_grain)
#setFinding(ml_grain)
querygrain(ml_grain, nodes = nodeNames(ml_grain))

prob_no <- ml_fit$S$prob[1]

s_pred <- sapply(rep.int(1, nrow(test)), function(a){
    s <- c()
    if (runif(1) < prob_no) {
        s <- "no"
    } else{
        s <- "yes"
    }
    return(s)
    
})


dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #true graph
plot(dag)
