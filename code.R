###Part 1 Code

##Set the efficient of Lambda
set.seed(100)
lambda <- 0.2
n <- 40
simulations <- data.frame(x=replicate(1000,rexp(n,lambda)))
simulations_mean <- data.frame(x=apply(simulations,2,mean))

library(ggplot2)

## get the histogram for the comparasion
h <- ggplot(simulations_mean,aes(x=x))+
        geom_histogram(aes(y=..count..,fill=..count..,color="red"))
h+labs(title = "Distribution of the Average Exponentials",y="Frequency",x="Mean")

## Comparasion between sample mean and theoretical mean
sample_mean <- mean(simulations_mean$x)
theory_mean <- 1/lambda
sample_mean
theory_mean

## Comparasion between sample variance and theoretical variance
sample_variance <- var(simulations_mean)
theory_variance <- ((1/lambda)^2)/n
sample_variance
theory_variance
Difference_variance <- sample_variance-theory_variance
Difference_variance

## Distribution
histogram_dist <- ggplot(simulations_mean,aes(x=x))+
        geom_histogram(aes(y=..density..,fill=..density..))+labs(title = "Distribution of the Average Exponentials",y="Density",x="Mean")
histogram_dist+geom_vline(xintercept = sample_mean,col="black")+ 
        geom_density(colour="green")+
        stat_function(fun=dnorm,args=list( mean=1/lambda, sd=sqrt(theory_variance)),color = "red") +
        geom_vline(xintercept=theory_mean, colour="red", linetype="dashed") 


###Part 2 Code
##Loading the data
library(datasets)
data("ToothGrowth")

##Pre-checking the data
str(ToothGrowth)
dim(ToothGrowth)
summary(ToothGrowth)

library(ggplot2)
boxplot_overview <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
        geom_boxplot(aes(fill=factor(dose)))+ geom_point()+ 
        facet_grid(.~supp)+labs(title = "Tooth Growth Influence")
boxplot_overview 

##Hypothesis Testing 
##H0:Orange juice will affect the tooth growth compared to ascorbic acid.
t.test(ToothGrowth$len[ToothGrowth$supp == "OJ"], ToothGrowth$len[ToothGrowth$supp == "VC"], paired = FALSE, var.equal = FALSE)



