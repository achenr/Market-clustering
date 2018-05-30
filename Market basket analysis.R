# Reading data
library(readxl)
library(caret)

rm(list=ls())   # Clear the environment

setwd("E:/UIC/Study/IDS 572/Assignments/5")
getwd()

data <- read_excel("E:/UIC/Study/IDS 572/Assignments/5/BathSoap_Data.xls",sheet=2)
data$max_to_one <- apply(data[, 23:30], 1, max)

#********************************************************************************************************************************************************
                                                                      # Q1 - Part A

## Variables to be used:-
# # brands, 
# brand runs, 
# total volume, 
# # transactions, 
# value, 
# Avg. price, 
# shareto other brands, 
# max to one brand


# For applying k-means, you have to normalize the data
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}

# Extract the necessary variables stipulated above
data1 = data[,c(12, 13, 14, 15, 16, 19, 31, 47)]
data1_complete <- data1[complete.cases(data1), ]                # Remove rows with 'NAs' in them
data1_km <- as.data.frame(lapply(data1_complete, normalize))    # Normalize the data

# Use the elbow curve method to calculate the best 'K' value
mydata <- data1_km
wcss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))   # General formula for wcss

for (i in 1:15) 
{
  wcss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}

plot(1:15, wcss, type="b", 
     xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", 
     pch=20, cex=2)
### Conclusion - A value of k = 4 seems apropriate
k = 4   # From previous step

# Perform K-Means with values of K around the optimal 'K' found identified from the Elbow method
# to check for the betweenness ratio (bcss/tss)


bcss <- NULL
tss <- NULL
bcss_tss <- NULL

for(i in 1:15)
{
  set.seed(1000)
  km = kmeans(mydata, i, nstart=100)
  bcss[i] <- km$betweenss
  tss[i] <- km$totss
  bcss_tss[i] <- km$betweenss/km$totss
}

wcss_norm <- (lapply(as.data.frame(wcss), normalize))
bcss_norm <- (lapply(as.data.frame(bcss), normalize))

plot(1:15, wcss_norm[[1]], type="l", 
     xlab="Number of Clusters", ylab="Sum of squares - normalized",
     main="Assessing the Optimal Number of Clusters - wcss, bcss, bcss/tss", 
     pch=20, cex=2, lwd = 2, col = "dark green")
lines(bcss_norm[[1]], type="l", col = "red", lwd = 2, cex = 2)
lines(bcss_tss, type = "l", col = "dark blue", lwd = 2, cex = 2)
abline(v = 4, lwd = 2, lty = 5)
abline(h = bcss_tss[4], lwd = 2, lty = 5)

### Conclusion - The blue line represents the betweenness ratio (bcss/tss) which is max at k = 15. But this value of K is not useful as it would lead to 
### too many clusters and you cannot create a marketing strategy tailored to all 15 segments. From a business perspective, we choose an optimal no. of 
### clusters = 4that strikes a balance between the business needs keeping in mind the betweenness ratio (verticle dashed black line).

### BUT - the betweenness ratio at k = 4 is still very small. Steps need to be taken to improve the clustering process



##------------------------------------------------------------    Sidebar   ---------------------------------------------------------------------------
## One way of improving our results is by performing PCA on our variables and then using them in the cluster analysis

## Performing the PCA
pcomp <- prcomp(mydata)
summary(pcomp)

pcomp$x[,1:2]
head(pcomp$x[,1:2])

# plot(pcomp$x[,1:2], xlab="Z1", ylab="Z2", pch=16, cex = 1.5, main = "The first two principal components") 


# Re-run k-means analysis using 'j' number of principal components
for(j in 1:4)
{
  pca_data <- data.frame(pcomp$x[,1:j])
  
  bcss <- NULL
  tss <- NULL
  bcss_tss <- NULL
  
  for (i in 1:15) 
  {
    wcss[i] <- sum(kmeans(pca_data, centers=i)$withinss)
  }
  
  for(i in 1:15)
  {
    set.seed(1000)
    km = kmeans(pca_data, i, nstart=100)
    bcss[i] <- km$betweenss
    tss[i] <- km$totss
    bcss_tss[i] <- km$betweenss/km$totss
  }
  
  wcss_norm <- (lapply(as.data.frame(wcss), normalize))
  bcss_norm <- (lapply(as.data.frame(bcss), normalize))
  
  plot(1:15, wcss_norm[[1]], type="l", 
       xlab ="Number of Clusters", ylab ="Sum of squares - normalized",
       main = paste(j,"Principal component(s) - Elbow Curve - wcss, bcss, bcss/tss"), 
       pch = 20, cex = 2, lwd = 2, col = "dark green")
  lines(bcss_norm[[1]], type="l", col = "red", lwd = 2, cex = 2)
  lines(bcss_tss, type = "l", col = "dark blue", lwd = 2, cex = 2)
  abline(v = 4, lwd = 2, lty = 5)
  abline(h = bcss_tss[4], lwd = 2, lty = 5)
  
}


# Final k-means model with k = 4
km = kmeans(pca_data, 4, nstart=100)
ads_data <- cbind(data1_km, km$cluster)   # Get back the original data along with their corresponding clusters
colnames(ads_data)[ncol(ads_data)] <- "k"
ads_data$k <- factor(ads_data$k)

## Perform an ANOVA on the variables to check if there is difference in the distributions

# test <- aov(No..of.Brands ~ k, ads_data)
# fit <- lm(Value ~ k, ads_data)
# test <- anova(fit)
# test
# summary(fit)

test <- aov(Value ~ k, ads_data)
tuk <- TukeyHSD(test)
plot(tuk)




## Check distributions
# colnames(ads_data)
# 
# 
# # Barplots
# df <- NULL
# agg_data <- NULL
# 
# for(i in 1:(ncol(ads_data)-1))
# {
#   df <- data.frame(aggregate(ads_data[,2] ~ k, data = ads_data, sum), row.names=1)
#   if(i==1) {agg_data <- df} else {agg_data <- cbind(agg_data, df)}
#   # agg_data <- cbind(agg_data, df)
#   
# }
# colnames(agg_data) <- colnames(ads_data)[-ncol(ads_data)]


###########################

# Q1 (b)


data2 = data[,c(20,21,22,32:46)]
data2_complete <- data2[complete.cases(data2), ]                # Remove rows with 'NAs' in them
data2_km <- as.data.frame(lapply(data2_complete, normalize))    # Normalize the data

mydata <- data2_km

## Performing the PCA
pcomp <- prcomp(mydata)
summary(pcomp)

pcomp$x[,1:18]
head(pcomp$x[,1:18])

# plot(pcomp$x[,1:2], xlab="Z1", ylab="Z2", pch=16, cex = 1.5, main = "The first two principal components") 


# Re-run k-means analysis using 'j' number of principal components
for(j in 1:4)
{
  pca_data <- data.frame(pcomp$x[,1:j])
  
  bcss <- NULL
  tss <- NULL
  bcss_tss <- NULL
  
  for (i in 1:15) 
  {
    wcss[i] <- sum(kmeans(pca_data, centers=i)$withinss)
  }
  
  for(i in 1:15)
  {
    set.seed(1000)
    km = kmeans(pca_data, i, nstart=100)
    bcss[i] <- km$betweenss
    tss[i] <- km$totss
    bcss_tss[i] <- km$betweenss/km$totss
  }
  
  wcss_norm <- (lapply(as.data.frame(wcss), normalize))
  bcss_norm <- (lapply(as.data.frame(bcss), normalize))
  
  plot(1:15, wcss_norm[[1]], type="l", 
       xlab ="Number of Clusters", ylab ="Sum of squares - normalized",
       main = paste(j,"Principal component(s) - Elbow Curve - Q1 (b)"), 
       pch = 20, cex = 2, lwd = 2, col = "dark green")
  lines(bcss_norm[[1]], type="l", col = "red", lwd = 2, cex = 2)
  lines(bcss_tss, type = "l", col = "dark blue", lwd = 2, cex = 2)
  abline(v = 4, lwd = 2, lty = 5)
  abline(h = bcss_tss[4], lwd = 2, lty = 5)
  
}


# Final k-means model with k = 4
km = kmeans(pca_data, 4, nstart=100)
ads_data <- cbind(data2_km, km$cluster)   # Get back the original data along with their corresponding clusters
colnames(ads_data)[ncol(ads_data)] <- "k"
ads_data$k <- factor(ads_data$k)

## Perform an ANOVA on the variables to check if there is difference in the distributions

# test <- aov(No..of.Brands ~ k, ads_data)
# fit <- lm(Pr.Cat.5 ~ k, ads_data)
# test <- anova(fit)
# test
# summary(fit)

test <- aov(Pur.Vol.Other.Promo.. ~ k, ads_data)
tuk <- TukeyHSD(test)
plot(tuk)




#########################################################################
# Q1 (c)

data3 = cbind(data1, data2)
data3_complete <- data3[complete.cases(data3), ]                # Remove rows with 'NAs' in them
data3_km <- as.data.frame(lapply(data3_complete, normalize))    # Normalize the data

mydata <- data3_km

## Performing the PCA
pcomp <- prcomp(mydata)
summary(pcomp)

pcomp$x[,1:18]
head(pcomp$x[,1:18])

# plot(pcomp$x[,1:2], xlab="Z1", ylab="Z2", pch=16, cex = 1.5, main = "The first two principal components") 


# Re-run k-means analysis using 'j' number of principal components
for(j in 1:4)
{
  pca_data <- data.frame(pcomp$x[,1:j])
  
  bcss <- NULL
  tss <- NULL
  bcss_tss <- NULL
  
  for (i in 1:15) 
  {
    wcss[i] <- sum(kmeans(pca_data, centers=i)$withinss)
  }
  
  for(i in 1:15)
  {
    set.seed(1000)
    km = kmeans(pca_data, i, nstart=100)
    bcss[i] <- km$betweenss
    tss[i] <- km$totss
    bcss_tss[i] <- km$betweenss/km$totss
  }
  
  wcss_norm <- (lapply(as.data.frame(wcss), normalize))
  bcss_norm <- (lapply(as.data.frame(bcss), normalize))
  
  plot(1:15, wcss_norm[[1]], type="l", 
       xlab ="Number of Clusters", ylab ="Sum of squares - normalized",
       main = paste(j,"Principal component(s) - Elbow Curve - Q1 (b)"), 
       pch = 20, cex = 2, lwd = 2, col = "dark green")
  lines(bcss_norm[[1]], type="l", col = "red", lwd = 2, cex = 2)
  lines(bcss_tss, type = "l", col = "dark blue", lwd = 2, cex = 2)
  abline(v = 4, lwd = 2, lty = 5)
  abline(h = bcss_tss[4], lwd = 2, lty = 5)
  
}


# Final k-means model with k = 4
km = kmeans(pca_data, 4, nstart=100)
ads_data <- cbind(data3_km, km$cluster)   # Get back the original data along with their corresponding clusters
colnames(ads_data)[ncol(ads_data)] <- "k"
ads_data$k <- factor(ads_data$k)

## Perform an ANOVA on the variables to check if there is difference in the distributions

# test <- aov(No..of.Brands ~ k, ads_data)
# fit <- lm(Pr.Cat.5 ~ k, ads_data)
# test <- anova(fit)
# test
# summary(fit)

test <- aov(Pur.Vol.Other.Promo.. ~ k, ads_data)
tuk <- TukeyHSD(test)
plot(tuk)


#********************************************************************************************************************************************************
                                                                            # Q2 - Part A

data_q2 <- data[-1]
data_q2$SEC <- factor(data_q2$SEC)
data_q2$FEH <- factor(data_q2$FEH)
data_q2$MT <- factor(data_q2$MT)
data_q2$SEX <- factor(data_q2$SEX)
data_q2$AGE <- factor(data_q2$AGE)
data_q2$EDU <- factor(data_q2$EDU)
data_q2$HS <- factor(data_q2$HS)
data_q2$CHILD <- factor(data_q2$CHILD)
data_q2$CS <- factor(data_q2$CS)
data_q2$`Affluence Index` <- factor(data_q2$`Affluence Index`)

data_q2_complete <- data_q2[complete.cases(data_q2), ]              # Remove rows with 'NAs' in them
temp <- data_q2_complete[1:10]
data_q2_complete <- data_q2_complete[11:ncol(data_q2_complete)]
data_q2_km <- as.data.frame(lapply(data_q2_complete, normalize))    # Normalize the data

# data_q2_km <- cbind(temp, data_q2_km)

mydata <- data_q2_km

## Performing the PCA
pcomp <- prcomp(mydata)
summary(pcomp)

pcomp$x[,1:36]
head(pcomp$x[,1:36])

# plot(pcomp$x[,1:2], xlab="Z1", ylab="Z2", pch=16, cex = 1.5, main = "The first two principal components") 


# Re-run k-means analysis using 'j' number of principal components
for(j in 1:4)
{
  pca_data <- data.frame(cbind(pcomp$x[,1:j], temp))
  
  bcss <- NULL
  tss <- NULL
  bcss_tss <- NULL
  
  for (i in 1:15) 
  {
    wcss[i] <- sum(kmeans(pca_data, centers=i)$withinss)
  }
  
  for(i in 1:15)
  {
    set.seed(1000)
    km = kmeans(pca_data, i, nstart=100)
    bcss[i] <- km$betweenss
    tss[i] <- km$totss
    bcss_tss[i] <- km$betweenss/km$totss
  }
  
  wcss_norm <- (lapply(as.data.frame(wcss), normalize))
  bcss_norm <- (lapply(as.data.frame(bcss), normalize))
  
  plot(1:15, wcss_norm[[1]], type="l", 
       xlab ="Number of Clusters", ylab ="Sum of squares - normalized",
       main = paste(j,"Principal component(s) - Elbow Curve - Q1 (b)"), 
       pch = 20, cex = 2, lwd = 2, col = "dark green")
  lines(bcss_norm[[1]], type="l", col = "red", lwd = 2, cex = 2)
  lines(bcss_tss, type = "l", col = "dark blue", lwd = 2, cex = 2)
  abline(v = 4, lwd = 2, lty = 5)
  abline(h = bcss_tss[4], lwd = 2, lty = 5)
  
}


# Final k-means model with k = 4
km = kmeans(pca_data, 4, nstart=100)
ads_data <- cbind(data_q2_km, temp, km$cluster)   # Get back the original data along with their corresponding clusters
colnames(ads_data)[ncol(ads_data)] <- "k"
ads_data$k <- factor(ads_data$k)

## Perform an ANOVA on the variables to check if there is difference in the distributions

# test <- aov(No..of.Brands ~ k, ads_data)
# fit <- lm(Pr.Cat.5 ~ k, ads_data)
# test <- anova(fit)
# test
# summary(fit)

test <- aov(Total.Volume ~ k, ads_data)
tuk <- TukeyHSD(test)
plot(tuk)


## Check distributions
colnames(ads_data)


# Barplots
df <- NULL
agg_data <- NULL

library(gmodels)
library(lattice)
library(ggplot2)
library(cowplot)
library(gridExtra)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Across variable
myplots <- NULL

for (i in 37:(ncol(ads_data)-1))
  local({
    i <- i
    
    chk <- data.frame(CrossTable(unlist(ads_data[i]), ads_data$k))
    colnames(chk)[1] <- colnames(ads_data[i])
    colnames(chk)[2] <- "k"
    colnames(chk)[3] <- "Freq"
    
    p1 <- ggplot(chk, aes(unlist(chk[1]), Freq, fill = k)) +
      geom_bar(stat="identity", position = "dodge") +
      scale_fill_brewer(palette = "Set1") +
      labs(x = colnames(chk)[1])
    
    print(i-36)
    print(p1)
    
    myplots[[i-36]] <<- p1  # add each plot into plot list
  })


multiplot(plotlist = myplots[1:9], cols = 3)



table(ads_data[37], ads_data$k)
CrossTable(unlist(ads_data[37]), ads_data$k)
zzz <- data.frame(CrossTable(ads_data$SEC, ads_data$k))
colnames()


for(i in 37:(ncol(ads_data)-1))
{
  df <- data.frame(aggregate(ads_data[,i] ~ k, data = ads_data, sum), row.names=1)
  if(i==1) {agg_data <- df} else {agg_data <- cbind(agg_data, df)}
  # agg_data <- cbind(agg_data, df)

}


colnames(agg_data) <- colnames(ads_data)[-ncol(ads_data)]