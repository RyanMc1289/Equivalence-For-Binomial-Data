# Simulating Binomial Data for Equivalence - Ryan M. McManus #

set.seed(412724) #for reproducing exact results (if you do NOT set this, your results will be somewhat different!)
library(tidyverse) #for plotting eventual results (use "install.packages("tidyverse")" first if you're new to R)
library(psych) #for describing p-value and effect size distributions (use "install.packages("psych")" first if new to R)
options(scipen = 99) #turns off scientific notation
n_sims <- 100000 #number of simulated experiments (higher numbers take longer to simulate)

# NO EFFECT (Probability of Success = 50%) #

## N = 10 ##

p_noeffect_n10 <- numeric(n_sims) #set up empty container for all simulated p-values
est_noeffect_n10 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_noeffect_n10 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_noeffect_n10 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 10, p = 0.5) #produce 10 simulated responses for one stimulus
  z<-binom.test(x = x, n = 10, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_noeffect_n10[i]<-z$p.value #get the p-value and store it
  est_noeffect_n10[i]<-(z$estimate) #get probability estimate and store it
  lowci_noeffect_n10[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_noeffect_n10[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 50 ##

p_noeffect_n50 <- numeric(n_sims) #set up empty container for all simulated p-values
est_noeffect_n50 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_noeffect_n50 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_noeffect_n50 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 50, p = 0.5) #produce 50 simulated responses for one stimulus
  z<-binom.test(x = x, n = 50, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_noeffect_n50[i]<-z$p.value #get the p-value and store it
  est_noeffect_n50[i]<-(z$estimate) #get probability estimate and store it
  lowci_noeffect_n50[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_noeffect_n50[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 100 ##

p_noeffect_n100 <- numeric(n_sims) #set up empty container for all simulated p-values
est_noeffect_n100 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_noeffect_n100 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_noeffect_n100 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 100, p = 0.5) #produce 100 simulated responses for one stimulus
  z<-binom.test(x = x, n = 100, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_noeffect_n100[i]<-z$p.value #get the p-value and store it
  est_noeffect_n100[i]<-(z$estimate) #get probability estimate and store it
  lowci_noeffect_n100[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_noeffect_n100[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 200 ##

p_noeffect_n200 <- numeric(n_sims) #set up empty container for all simulated p-values
est_noeffect_n200 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_noeffect_n200 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_noeffect_n200 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 200, p = 0.5) #produce 200 simulated responses for one stimulus
  z<-binom.test(x = x, n = 200, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_noeffect_n200[i]<-z$p.value #get the p-value and store it
  est_noeffect_n200[i]<-(z$estimate) #get probability estimate and store it
  lowci_noeffect_n200[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_noeffect_n200[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 500 ##

p_noeffect_n500 <- numeric(n_sims) #set up empty container for all simulated p-values
est_noeffect_n500 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_noeffect_n500 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_noeffect_n500 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 500, p = 0.5) #produce 500 simulated responses for one stimulus
  z<-binom.test(x = x, n = 500, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_noeffect_n500[i]<-z$p.value #get the p-value and store it
  est_noeffect_n500[i]<-(z$estimate) #get probability estimate and store it
  lowci_noeffect_n500[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_noeffect_n500[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 1,000 ##

p_noeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated p-values
est_noeffect_n1000 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_noeffect_n1000 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_noeffect_n1000 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 1000, p = 0.5) #produce 1000 simulated responses for one stimulus
  z<-binom.test(x = x, n = 1000, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_noeffect_n1000[i]<-z$p.value #get the p-value and store it
  est_noeffect_n1000[i]<-(z$estimate) #get probability estimate and store it
  lowci_noeffect_n1000[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_noeffect_n1000[i]<-z$conf.int[2] #get high 95% CI and store it
}

## Create datasets for plotting ##

data_noeffect_n10 <- data.frame(p_noeffect_n10)
data_noeffect_n10$n <- rep("N = 10", nrow(data_noeffect_n10))
data_noeffect_n10$p <- data_noeffect_n10$p_noeffect_n10
data_noeffect_n10$est <- est_noeffect_n10
data_noeffect_n10$lowci <- lowci_noeffect_n10
data_noeffect_n10$highci <- highci_noeffect_n10
data_noeffect_n10 <- data_noeffect_n10[, -1]

data_noeffect_n50 <- data.frame(p_noeffect_n50)
data_noeffect_n50$n <- rep("N = 50", nrow(data_noeffect_n50))
data_noeffect_n50$p <- data_noeffect_n50$p_noeffect_n50
data_noeffect_n50$est <- est_noeffect_n50
data_noeffect_n50$lowci <- lowci_noeffect_n50
data_noeffect_n50$highci <- highci_noeffect_n50
data_noeffect_n50 <- data_noeffect_n50[, -1]

data_noeffect_n100 <- data.frame(p_noeffect_n100)
data_noeffect_n100$n <- rep("N = 100", nrow(data_noeffect_n100))
data_noeffect_n100$p <- data_noeffect_n100$p_noeffect_n100
data_noeffect_n100$est <- est_noeffect_n100
data_noeffect_n100$lowci <- lowci_noeffect_n100
data_noeffect_n100$highci <- highci_noeffect_n100
data_noeffect_n100 <- data_noeffect_n100[, -1]

data_noeffect_n200 <- data.frame(p_noeffect_n200)
data_noeffect_n200$n <- rep("N = 200", nrow(data_noeffect_n200))
data_noeffect_n200$p <- data_noeffect_n200$p_noeffect_n200
data_noeffect_n200$est <- est_noeffect_n200
data_noeffect_n200$lowci <- lowci_noeffect_n200
data_noeffect_n200$highci <- highci_noeffect_n200
data_noeffect_n200 <- data_noeffect_n200[, -1]

data_noeffect_n500 <- data.frame(p_noeffect_n500)
data_noeffect_n500$n <- rep("N = 500", nrow(data_noeffect_n500))
data_noeffect_n500$p <- data_noeffect_n500$p_noeffect_n500
data_noeffect_n500$est <- est_noeffect_n500
data_noeffect_n500$lowci <- lowci_noeffect_n500
data_noeffect_n500$highci <- highci_noeffect_n500
data_noeffect_n500 <- data_noeffect_n500[, -1]

data_noeffect_n1000 <- data.frame(p_noeffect_n1000)
data_noeffect_n1000$n <- rep("N = 1000", nrow(data_noeffect_n1000))
data_noeffect_n1000$p <- data_noeffect_n1000$p_noeffect_n1000
data_noeffect_n1000$est <- est_noeffect_n1000
data_noeffect_n1000$lowci <- lowci_noeffect_n1000
data_noeffect_n1000$highci <- highci_noeffect_n1000
data_noeffect_n1000 <- data_noeffect_n1000[, -1]

alldata_noeffect <- rbind(data_noeffect_n10, data_noeffect_n50, data_noeffect_n100, data_noeffect_n200, data_noeffect_n500, data_noeffect_n1000)

alldata_noeffect$n <- as.factor(alldata_noeffect$n)
alldata_noeffect$n <- ordered(alldata_noeffect$n, levels = c("N = 10", "N = 50", "N = 100",
                                                             "N = 200", "N = 500", "N = 1000"))

## Plotting P-values and Effect Sizes ##
print(pvalue_plot_noeffect <- ggplot(alldata_noeffect, aes(x=p))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0.05, color = "red") +
        xlab("Observed p-value") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(est_plot_noeffect <- ggplot(alldata_noeffect, aes(x=est))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0.5, color = "green") +
        xlab("Observed Estimate") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(lowci_plot_noeffect <- ggplot(alldata_noeffect, aes(x=lowci))+
        geom_histogram(bins = 100) +
        xlab("Observed Lower CI") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(highci_plot_noeffect <- ggplot(alldata_noeffect, aes(x=highci))+
        geom_histogram(bins = 100) +
        xlab("Observed Higher CI") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

# Count p-values <= 0.05 to determine statistical power / Check average observed estimate
power_noeffect_n10 <- alldata_noeffect %>%
  filter(n == "N = 10") %>%
  filter(p <= 0.05)
describe(power_noeffect_n10$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n10$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_noeffect_n10$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_noeffect_n10$highci)) # gives descriptive info about highci for p-values <= 0.05

power_noeffect_n50 <- alldata_noeffect %>%
  filter(n == "N = 50") %>%
  filter(p <= 0.05)
describe(power_noeffect_n50$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n50$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_noeffect_n50$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_noeffect_n50$highci)) # gives descriptive info about highci for p-values <= 0.05

power_noeffect_n100 <- alldata_noeffect %>%
  filter(n == "N = 100") %>%
  filter(p <= 0.05)
describe(power_noeffect_n100$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n100$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_noeffect_n100$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_noeffect_n100$highci)) # gives descriptive info about highci for p-values <= 0.05

power_noeffect_n200 <- alldata_noeffect %>%
  filter(n == "N = 200") %>%
  filter(p <= 0.05)
describe(power_noeffect_n200$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n200$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_noeffect_n200$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_noeffect_n200$highci)) # gives descriptive info about highci for p-values <= 0.05

power_noeffect_n500 <- alldata_noeffect %>%
  filter(n == "N = 500") %>%
  filter(p <= 0.05)
describe(power_noeffect_n500$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n500$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_noeffect_n500$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_noeffect_n500$highci)) # gives descriptive info about highci for p-values <= 0.05

power_noeffect_n1000 <- alldata_noeffect %>%
  filter(n == "N = 1000") %>%
  filter(p <= 0.05)
describe(power_noeffect_n1000$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n1000$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_noeffect_n1000$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_noeffect_n1000$highci)) # gives descriptive info about highci for p-values <= 0.05


# SMALL EFFECT (10% Difference from 50%) #

## N = 10 ##

p_smalleffect_n10 <- numeric(n_sims) #set up empty container for all simulated p-values
est_smalleffect_n10 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_smalleffect_n10 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_smalleffect_n10 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 10, p = 0.6) #produce 10 simulated responses for one stimulus
  z<-binom.test(x = x, n = 10, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_smalleffect_n10[i]<-z$p.value #get the p-value and store it
  est_smalleffect_n10[i]<-(z$estimate) #get probability estimate and store it
  lowci_smalleffect_n10[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_smalleffect_n10[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 50 ##

p_smalleffect_n50 <- numeric(n_sims) #set up empty container for all simulated p-values
est_smalleffect_n50 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_smalleffect_n50 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_smalleffect_n50 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 50, p = 0.6) #produce 50 simulated responses for one stimulus
  z<-binom.test(x = x, n = 50, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_smalleffect_n50[i]<-z$p.value #get the p-value and store it
  est_smalleffect_n50[i]<-(z$estimate) #get probability estimate and store it
  lowci_smalleffect_n50[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_smalleffect_n50[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 100 ##

p_smalleffect_n100 <- numeric(n_sims) #set up empty container for all simulated p-values
est_smalleffect_n100 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_smalleffect_n100 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_smalleffect_n100 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 100, p = 0.6) #produce 100 simulated responses for one stimulus
  z<-binom.test(x = x, n = 100, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_smalleffect_n100[i]<-z$p.value #get the p-value and store it
  est_smalleffect_n100[i]<-(z$estimate) #get probability estimate and store it
  lowci_smalleffect_n100[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_smalleffect_n100[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 200 ##

p_smalleffect_n200 <- numeric(n_sims) #set up empty container for all simulated p-values
est_smalleffect_n200 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_smalleffect_n200 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_smalleffect_n200 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 200, p = 0.6) #produce 200 simulated responses for one stimulus
  z<-binom.test(x = x, n = 200, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_smalleffect_n200[i]<-z$p.value #get the p-value and store it
  est_smalleffect_n200[i]<-(z$estimate) #get probability estimate and store it
  lowci_smalleffect_n200[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_smalleffect_n200[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 500 ##

p_smalleffect_n500 <- numeric(n_sims) #set up empty container for all simulated p-values
est_smalleffect_n500 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_smalleffect_n500 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_smalleffect_n500 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 500, p = 0.6) #produce 500 simulated responses for one stimulus
  z<-binom.test(x = x, n = 500, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_smalleffect_n500[i]<-z$p.value #get the p-value and store it
  est_smalleffect_n500[i]<-(z$estimate) #get probability estimate and store it
  lowci_smalleffect_n500[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_smalleffect_n500[i]<-z$conf.int[2] #get high 95% CI and store it
}

## N = 1,000 ##

p_smalleffect_n1000 <- numeric(n_sims) #set up empty container for all simulated p-values
est_smalleffect_n1000 <- numeric(n_sims) #set up empty container for all probability estimates
lowci_smalleffect_n1000 <- numeric(n_sims) #set up empty container for all low ci's of probability estimates
highci_smalleffect_n1000 <- numeric(n_sims) #set up empty container for all high ci's of probability estimates


for(i in 1:n_sims){ #for each simulated experiment
  x<-rbinom(n = 1, size = 1000, p = 0.6) #produce 1000 simulated responses for one stimulus
  z<-binom.test(x = x, n = 1000, p = 0.5, alternative = "two.sided", conf.level = 0.95) #perform binomial test
  p_smalleffect_n1000[i]<-z$p.value #get the p-value and store it
  est_smalleffect_n1000[i]<-(z$estimate) #get probability estimate and store it
  lowci_smalleffect_n1000[i]<-z$conf.int[1] #get low 95% CI and store it
  highci_smalleffect_n1000[i]<-z$conf.int[2] #get high 95% CI and store it
}

## Create datasets for plotting ##

data_smalleffect_n10 <- data.frame(p_smalleffect_n10)
data_smalleffect_n10$n <- rep("N = 10", nrow(data_smalleffect_n10))
data_smalleffect_n10$p <- data_smalleffect_n10$p_smalleffect_n10
data_smalleffect_n10$est <- est_smalleffect_n10
data_smalleffect_n10$lowci <- lowci_smalleffect_n10
data_smalleffect_n10$highci <- highci_smalleffect_n10
data_smalleffect_n10 <- data_smalleffect_n10[, -1]

data_smalleffect_n50 <- data.frame(p_smalleffect_n50)
data_smalleffect_n50$n <- rep("N = 50", nrow(data_smalleffect_n50))
data_smalleffect_n50$p <- data_smalleffect_n50$p_smalleffect_n50
data_smalleffect_n50$est <- est_smalleffect_n50
data_smalleffect_n50$lowci <- lowci_smalleffect_n50
data_smalleffect_n50$highci <- highci_smalleffect_n50
data_smalleffect_n50 <- data_smalleffect_n50[, -1]

data_smalleffect_n100 <- data.frame(p_smalleffect_n100)
data_smalleffect_n100$n <- rep("N = 100", nrow(data_smalleffect_n100))
data_smalleffect_n100$p <- data_smalleffect_n100$p_smalleffect_n100
data_smalleffect_n100$est <- est_smalleffect_n100
data_smalleffect_n100$lowci <- lowci_smalleffect_n100
data_smalleffect_n100$highci <- highci_smalleffect_n100
data_smalleffect_n100 <- data_smalleffect_n100[, -1]

data_smalleffect_n200 <- data.frame(p_smalleffect_n200)
data_smalleffect_n200$n <- rep("N = 200", nrow(data_smalleffect_n200))
data_smalleffect_n200$p <- data_smalleffect_n200$p_smalleffect_n200
data_smalleffect_n200$est <- est_smalleffect_n200
data_smalleffect_n200$lowci <- lowci_smalleffect_n200
data_smalleffect_n200$highci <- highci_smalleffect_n200
data_smalleffect_n200 <- data_smalleffect_n200[, -1]

data_smalleffect_n500 <- data.frame(p_smalleffect_n500)
data_smalleffect_n500$n <- rep("N = 500", nrow(data_smalleffect_n500))
data_smalleffect_n500$p <- data_smalleffect_n500$p_smalleffect_n500
data_smalleffect_n500$est <- est_smalleffect_n500
data_smalleffect_n500$lowci <- lowci_smalleffect_n500
data_smalleffect_n500$highci <- highci_smalleffect_n500
data_smalleffect_n500 <- data_smalleffect_n500[, -1]

data_smalleffect_n1000 <- data.frame(p_smalleffect_n1000)
data_smalleffect_n1000$n <- rep("N = 1000", nrow(data_smalleffect_n1000))
data_smalleffect_n1000$p <- data_smalleffect_n1000$p_smalleffect_n1000
data_smalleffect_n1000$est <- est_smalleffect_n1000
data_smalleffect_n1000$lowci <- lowci_smalleffect_n1000
data_smalleffect_n1000$highci <- highci_smalleffect_n1000
data_smalleffect_n1000 <- data_smalleffect_n1000[, -1]

alldata_smalleffect <- rbind(data_smalleffect_n10, data_smalleffect_n50, data_smalleffect_n100, data_smalleffect_n200, data_smalleffect_n500, data_smalleffect_n1000)

alldata_smalleffect$n <- as.factor(alldata_smalleffect$n)
alldata_smalleffect$n <- ordered(alldata_smalleffect$n, levels = c("N = 10", "N = 50", "N = 100",
                                                             "N = 200", "N = 500", "N = 1000"))

## Plotting P-values and Effect Sizes ##
print(pvalue_plot_smalleffect <- ggplot(alldata_smalleffect, aes(x=p))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0.05, color = "red") +
        xlab("Observed p-value") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(est_plot_smalleffect <- ggplot(alldata_smalleffect, aes(x=est))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0.6, color = "green") +
        xlab("Observed Estimate") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(lowci_plot_smalleffect <- ggplot(alldata_smalleffect, aes(x=lowci))+
        geom_histogram(bins = 100) +
        xlab("Observed Lower CI") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(highci_plot_smalleffect <- ggplot(alldata_smalleffect, aes(x=highci))+
        geom_histogram(bins = 100) +
        xlab("Observed Higher CI") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

# Count p-values <= 0.05 to determine statistical power / Check average observed estimate
power_smalleffect_n10 <- alldata_smalleffect %>%
  filter(n == "N = 10") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n10$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n10$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_smalleffect_n10$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_smalleffect_n10$highci)) # gives descriptive info about highci for p-values <= 0.05

power_smalleffect_n50 <- alldata_smalleffect %>%
  filter(n == "N = 50") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n50$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n50$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_smalleffect_n50$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_smalleffect_n50$highci)) # gives descriptive info about highci for p-values <= 0.05

power_smalleffect_n100 <- alldata_smalleffect %>%
  filter(n == "N = 100") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n100$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n100$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_smalleffect_n100$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_smalleffect_n100$highci)) # gives descriptive info about highci for p-values <= 0.05

power_smalleffect_n200 <- alldata_smalleffect %>%
  filter(n == "N = 200") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n200$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n200$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_smalleffect_n200$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_smalleffect_n200$highci)) # gives descriptive info about highci for p-values <= 0.05

power_smalleffect_n500 <- alldata_smalleffect %>%
  filter(n == "N = 500") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n500$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n500$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_smalleffect_n500$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_smalleffect_n500$highci)) # gives descriptive info about highci for p-values <= 0.05

power_smalleffect_n1000 <- alldata_smalleffect %>%
  filter(n == "N = 1000") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n1000$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n1000$est)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_smalleffect_n1000$lowci)) # gives descriptive info about lowci for p-values <= 0.05
describe(abs(power_smalleffect_n1000$highci)) # gives descriptive info about highci for p-values <= 0.05


# POWERING EQUIVALENCE TEST WITH BOUNDS 35 < 50 < 65 (assuming 10% deflection from 50%) #

## Create variable that specifies width of confidence intervals as within 35/65 bounds ##
equiv_smalleffect_n10 <- alldata_smalleffect %>%
  filter(n == "N = 10") %>%
  filter(lowci > 35 & highci < 65)
describe(equiv_smalleffect_n10$p) # gives descriptive info (using for count)

equiv_smalleffect_n50 <- alldata_smalleffect %>%
  filter(n == "N = 50") %>%
  filter(lowci > .35 & highci < .65)
describe(equiv_smalleffect_n50$p) # gives descriptive info (using for count)

equiv_smalleffect_n100 <- alldata_smalleffect %>%
  filter(n == "N = 100") %>%
  filter(lowci > .35 & highci < .65)
describe(equiv_smalleffect_n100$p) # gives descriptive info (using for count)

equiv_smalleffect_n200 <- alldata_smalleffect %>%
  filter(n == "N = 200") %>%
  filter(lowci > .35 & highci < .65)
describe(equiv_smalleffect_n200$p) # gives descriptive info (using for count)

equiv_smalleffect_n500 <- alldata_smalleffect %>%
  filter(n == "N = 500") %>%
  filter(lowci > .35 & highci < .65)
describe(equiv_smalleffect_n500$p) # gives descriptive info (using for count)

equiv_smalleffect_n1000 <- alldata_smalleffect %>%
  filter(n == "N = 1000") %>%
  filter(lowci > .35 & highci < .65)
describe(equiv_smalleffect_n1000$p) # gives descriptive info (using for count)


