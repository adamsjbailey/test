library(tidyverse)
library(dslabs)

a<-3
a

data("murders")
class(murders)
str(murders)
a<-2
b<--1
c<--4
-b+sqrt((b*b)-(4*a*c))/2*a

(-b + sqrt(b^2 - 4*a*c))/(2*a)
(-b - sqrt(b^2 - 4*a*c))/(2*a)

help(log)
log(1024,4)

library(dslabs)
data(movielens)
str(movielens)
nlevels(movielens$genres)


pop <- murders$population
pop

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time <- time/60
time
speed <- distance/time
speed
time[name="Olivia"]
time[4]
name
length(name)

library(dslabs)
data(heights)
options(digits = 3)

str(heights)
mean(heights$height)
indy <- which(heights$height > 68.3 & heights$sex == "Female")
indy

indy <- which(heights$sex == "Female")

length(indy)
ind <-match(heights$sex,"Female")
ind
sum(ind)
mean(heights$sex == "Female")
max(heights$height)
match(50,heights$height)
heights$sex[match(50,heights$height)]

x<- 50:8

sum(! x %in% heights$height)
heights2 <- mutate(heights, ht_cm = height*2.54)


ht_cm <- heights2$ht_cm
females <- filter(heights2, sex == "Female")
mean(females$ht_cm)

str(females)

library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region,data=olive)


x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}


options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

str(titanic)



titanic %>% filter(!is.na(Age))%>%ggplot(aes(Age, fill=Sex)) + geom_density(alpha=0.2)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age)) + geom_qq(dparams=params)+geom_abline()

titanic %>%ggplot(aes(Survived, fill=Sex)) + geom_bar(position = position_dodge())

titanic %>% filter(Fare>0) %>% ggplot(aes(,Fare, fill=Survived)) + geom_boxplot() + scale_y_continuous(trans="log2")

titanic %>%ggplot(aes(Pclass, fill=Survived)) + geom_bar()
titanic %>%ggplot(aes(Pclass, fill=Survived)) + geom_bar(position = position_fill())
titanic %>%ggplot(aes(Survived, fill=Pclass)) + geom_bar(position = position_fill())

titanic %>% filter(!is.na(Age))%>%ggplot(aes(Age, after_stat(count),fill=Survived)) + geom_density(alpha=0.2) + facet_grid(Sex~Pclass)

library(gtools)
a<-permutations(3,3)
str(a)
6/336

set.seed(1)
B<-10000
test <- replicate(B, {
  runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
  my_pick  <- sample(runners, 3)
  my_pick[1]=="Jamaica" & my_pick[2]=="Jamaica" & my_pick[3]=="Jamaica"
})
mean(test) 

a<-combinations(7,2)
nrow(a)


func<-function(x){a<-combinations(x,2)
nrow(a)
} 
entree<-seq(2:12)
sapply(entree, func)

head(esoph)
all_cases<-sum(esoph$ncases)
all_cases
all_controls<-sum(esoph$ncontrols)
all_controls
levels(esoph$tobgp)
sum(esoph$ncontrols[esoph$alcgp=="120+"])
sum(esoph$ncases[esoph$alcgp=="120+"])
45/(67+45)

esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)



esoph %>%
  filter(tobgp != "0-9g/day") %>%
  summarize(ncontrols = sum(ncontrols)) %>%
  mutate(p = ncontrols / all_controls) %>%
  pull(p)

esoph %>%
  filter(alcgp == "120+") %>%
  summarize(n = sum(ncases)) %>%
  mutate(p = n / all_cases) %>%
  pull(p)


esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(n = sum(ncases)) %>%
  mutate(p = n / all_cases) %>%
  pull(p)

esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncontrols = sum(ncontrols)) %>%
  mutate(p = ncontrols / all_controls) %>%
  pull(p)

esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(ncontrols = sum(ncontrols)) %>%
  mutate(p = ncontrols / all_controls) %>%
  pull(p)

set.seed(16, sample.kind = "Rounding")
act_scores<-rnorm(10000,20.9,5.7)
mean(act_scores)
sd(act_scores)
a<-act_scores>=36
mean(act_scores<=10)
x<-seq(1:36)
f_x<-dnorm(x,20.9,5.7)
plot(x,f_x)

m<-mean(act_scores)
s<-sd(act_scores)

z_scores<-(act_scores-m)/s
mean(z_scores>2)
a<-act_scores[z_scores>=2]
min(a)
qnorm(.975,m,s)

f<-function(x){pnorm(x,m,s)}
results<-sapply(x,f)
a<-x[results>=0.95]
min(a)
qnorm(.95,20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles<-quantile(act_scores,p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

theoretical_quantiles<-qnorm(p,20.9,5.7)
plot(theoretical_quantiles, sample_quantiles)



a <- 6
b <- -1
n<- 500
p<-5/38
mu <- n*(a*p + b*(1-p))
mu
sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
sigma

pnorm(0,mu,sigma)

p <- seq(0.25, 0.95, 0.05)

mu<- 44*p
sigma<- sqrt(p*(1-p))*sqrt(44)
t<-1-pnorm(35,mu,sigma)


p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

exp_val
min(p[which(t > 0.8)])

