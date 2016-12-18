library(tm)
library(topicmodels)

# reproducing Cao's example
doc = c("drug clinical patients","drug disease","HIV virus aids","aids HIV disease")
tt1 = Corpus(VectorSource(doc))
dtm1 = DocumentTermMatrix(tt1)

avg_dis <- function(lda, k){
  s =0
  for(i in 1:(k-1)){
    for(j in (i+1):k){
        s = s+ sum(lda[i,]*lda[j,])/(sqrt(sum(lda[i,]^2))*sqrt(sum(lda[j,]^2)))
    }
  }
  return(2*s/(k*(k-1)))
}

set.seed(30)
lda1 = LDA(dtm1, 3, method ="Gibbs")
lda1p = exp(attributes(lda1)$beta)

lda2 = LDA(dtm1, 2, method ="Gibbs")
lda2p = exp(attributes(lda2)$beta)

lda3 = LDA(dtm1, 4, method ="Gibbs")
lda3p = exp(attributes(lda3)$beta)
avg_dis(lda3p,4)
avg_dis(lda1p,3)
avg_dis(lda2p,2)

library(RColorBrewer)
par(mfrow = c(3,1),mar=c(4,8,3,2))
sequential <- brewer.pal(4, "BuGn")
barplot(lda3p, names.arg = attributes(lda3)$terms,cex.names = 1.3, width = 1,
        col = sequential, xlim = c(0,9), ylab = "Topic distributions over words",
        main = "K=4")
legend("bottomright", legend = 1:4,fill = sequential[4:1], title = "Topics")

sequential <- brewer.pal(3, "BuGn")
barplot(lda1p, names.arg = attributes(lda1)$terms,cex.names = 1.3, width = 1,
        col = sequential, xlim = c(0,9), ylab = "Topic distributions over words",
        main = "K=3")
legend("bottomright", legend = 1:3,fill = sequential[3:1], title = "Topics")

sequential <- brewer.pal(2, "BuGn")
barplot(lda2p, names.arg = attributes(lda2)$terms,cex.names = 1.3, width = 1,
        col = sequential, xlim = c(0,9), ylab = "Topic distributions over words",
        main = "K=2")
legend("bottomright", legend = 1:2,fill = sequential[2:1], title = "Topics")



# Metric plot

library(ldatuning)
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 10, to = 50, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "VEM",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
plot(result$Griffiths2004, pch = 10)
par(new = TRUE)
#plot(result$Deveaud2014, pch = 11)
#par(new = TRUE)
plot(result$CaoJuan2009, pch = 13)
par(new = TRUE)
plot(result$Arun2010, pch = 14)


# CTM VS LDA with held out data
# CTM runs very slow(around 1.5 hour each run)

library(tm)
library(topicmodels)
data <- read.csv('/home/grad/lx38/Downloads/session_data.csv', stringsAsFactors = FALSE)
tt = Corpus(VectorSource(data[,"AbstractPerSession"]))
dtm = DocumentTermMatrix(tt)

#lda = LDA(dtm, k= 44)

control_CTM_VEM <-list(estimate.beta = TRUE,
                       verbose = 0, prefix = tempfile(), save = 0, keep = 0,
                       seed = as.integer(Sys.time()), nstart = 1L, best = TRUE,
                       var = list(iter.max = 500, tol = 10^-6),
                       em = list(iter.max = 1000, tol = 10^-4),
                       initialize = "random",
                       cg = list(iter.max = 1000, tol = 10^-2))
#ctm = CTM(dtm, 10, control = control_CTM_VEM)

#terms(lda,15)


set.seed(0908)
SEED <- 20080809
#p1 = matrix(NA, ncol = 11, nrow = 5)
#p2 = matrix(NA, ncol = 11, nrow = 5)
#l1 = matrix(NA, ncol = 11, nrow = 5)
#l2 = matrix(NA, ncol = 11, nrow = 5)
#D <- nrow(data)
#folding <- sample(rep(seq_len(5), ceiling(D))[seq_len(D)])
i=1


for(n in c(15,20)){
training <- LDA(dtm[folding != i,], method ="Gibbs",k = n, control = list(seed = SEED))
testing <- LDA(dtm[folding == i,], method ="Gibbs",model = training, control = list(estimate.beta = FALSE, seed = SEED))
p1[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
l1[i,n/5-1] = logLik(testing)
}





training <- CTM(dtm[folding != i,], n, control = control_CTM_VEM)
testing <- CTM(dtm[folding == i,], model = training, control = control_CTM_VEM)
p2[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
l2[i,n/5-1] = logLik(testing)

n=35
#training <- LDA(dtm[folding != i,], method ="Gibbs", k = n, control = list(seed = SEED))
#testing <- LDA(dtm[folding == i,], method ="Gibbs", model = training, control = list(estimate.beta = FALSE, seed = SEED))
#p1[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
#l1[i,n/5-1] = logLik(testing)
training <- CTM(dtm[folding != i,], n, control = control_CTM_VEM)
testing <- CTM(dtm[folding == i,], model = training, control = control_CTM_VEM)
p2[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
l2[i,n/5-1] = logLik(testing)



n=45
#training <- LDA(dtm[folding != i,], method ="Gibbs", k = n, control = list(seed = SEED))
#testing <- LDA(dtm[folding == i,], method ="Gibbs", model = training, control = list(estimate.beta = FALSE, seed = SEED))
#p1[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
#l1[i,n/5-1] = logLik(testing)
training <- CTM(dtm[folding != i,], n, control = control_CTM_VEM)
testing <- CTM(dtm[folding == i,], model = training, control = control_CTM_VEM)
p2[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
l2[i,n/5-1] = logLik(testing)

n=50
#training <- LDA(dtm[folding != i,], method ="Gibbs", k = n, control = list(seed = SEED))
#testing <- LDA(dtm[folding == i,], method ="Gibbs", model = training, control = list(estimate.beta = FALSE, seed = SEED))
#p1[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
#l1[i,n/5-1] = logLik(testing)
training <- CTM(dtm[folding != i,], n, control = control_CTM_VEM)
testing <- CTM(dtm[folding == i,], model = training, control = control_CTM_VEM)
p2[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
l2[i,n/5-1] = logLik(testing)


n=55
training <- LDA(dtm[folding != i,], method ="Gibbs", k = n, control = list(seed = SEED))
testing <- LDA(dtm[folding == i,], method ="Gibbs", model = training, control = list(estimate.beta = FALSE, seed = SEED))
p1[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
l1[i,n/5-1] = logLik(testing)
training <- CTM(dtm[folding != i,], n, control = control_CTM_VEM)
testing <- CTM(dtm[folding == i,], model = training, control = control_CTM_VEM)
p2[i,n/5-1] = perplexity(testing, dtm[folding == i,], use_theta = FALSE)
l2[i,n/5-1] = logLik(testing)


par(mfrow = c(1,2))
plot(seq(25,55,by=5), p2[1,4:10], ylim = c(1,4000), ylab = "Perplexity", 
     xlab = "Number of topics", main = "LDA vs CTM", pch = 1, col = "blue")
points(seq(25,55,by=5), p1[1,4:10], pch = 2, col = "red")
legend("right", pch = c(1,2), legend = c("CTM", "LDA"), col = c("blue","red"))


plot(seq(25,55,by=5),l2[1,4:10], ylim = c(-280000,-200000), ylab = "Log likelihood", 
     xlab = "Number of topics", main = "LDA vs CTM", pch = 1, col = "blue")
points(seq(25,55,by=5),l1[1,4:10], pch =2, col = "red")
legend("right", pch = c(1,2), legend = c("CTM", "LDA"), col = c("blue","red"))


