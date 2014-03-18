library(foreign)
library(plyr)
library(poLCA)

# Change to your directory
setwd("/media/tom/New Volume/Dropbox/LSE/MY455/Week 5")

# Cluster Analysis(CLA)
eastwest <- read.dta("IIAG_eastwest_CLA_2012.dta")

d <- dist(eastwest[,c(9,10,11,12,13,14)], method="euclidean")
fit <- hclust(d, "average")
fit2 <- hclust(d, "ward")

# Creates 2 plots for Average and Ward's linkage
plot(fit, labels = eastwest$country, main = "Average Linkage", xlabel = "Country")
plot(fit2, labels = eastwest$country, main = "Ward's Linkage", xlabel = "Country")
groups <- cutree(fit2, k=3)
rect.hclust(fit2, k=3, border="red")

# Calculates summary statistics for 3 clusters
names(groups) <- eastwest$country
cluster1 <- eastwest[eastwest$country %in% 
                       names(groups[groups == 1]),c(9,10,11,12,13,14)]
stats1 <- sapply(cluster1, each(min, max, mean, sd, var))
print(stats1)

names(groups) <- eastwest$country
cluster2 <- eastwest[eastwest$country %in% 
                       names(groups[groups == 2]),c(9,10,11,12,13,14)]
stats2 <- sapply(cluster2, each(min, max, mean, sd, var))
print(stats2)

names(groups) <- eastwest$country
cluster3 <- eastwest[eastwest$country %in% 
                       names(groups[groups == 3]),c(9,10,11,12,13,14)]
stats3 <- sapply(cluster3, each(min, max, mean, sd, var))
print(stats3)

# Latent Class Modelling(LCM)
job <- read.csv("EVS_job_LCM.csv")
job <- job[,2:7]
for (i in 1:6) {
  job[,i] <- as.factor(job[,i])
  levels(job[,i]) <- c("not mentioned", "mentioned")
}
names(job) <- c('pay','people','security','achieve','interest','equal') 

f <- cbind(pay, people, security, achieve, interest, equal) ~ 1
lca2 <- poLCA(f, job, nclass=2)
lca3 <- poLCA(f, job, nclass=3)

# Prints predicted probabilities by class
m <- matrix(c("Pay not mentioned", "Pay mentioned", "People not mentioned", 
              "People mentioned", "Security not mentioned", "Security mentioned", 
              "Achieve not mentioned", "Achieve mentioned", "Interest not mentioned", 
              "Interest mentioned", "Equal not mentioned", "Equal mentioned"), nrow=12, ncol=1)
colnames(m) <- c("Variable")
print(cbind(m, round(rbind(t(lca3$probs$pay), t(lca3$probs$people), t(lca3$probs$security), t(lca3$probs$achieve), 
                           t(lca3$probs$interest), t(lca3$probs$equal)), digits=2)))