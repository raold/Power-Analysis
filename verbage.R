# Begin build matrix to simulate data
# Initialize person vector
persons <- 1:20
persons_vector <- NULL
#append(persons, c(2,2), after=length(persons))
for(i in seq(along=persons)) {
  persons_vector <-  append(persons_vector, c(i,i), after=length(persons))
}
persons_vector <- sort(persons_vector)
persons_vector

# Initialize method vector
method        <- c('A', 'B')
method_vector <- rep(method, 20)
method_vector

# Initialize case vector
case        <- c(1,2, 2, 1)
case_vector <- rep(case, 10)
# If you wanted to randomize case assignment
# rep(case_vector <- sample(x=case_vector, replace=TRUE), 2000)
case_vector

## Varbable names
temp= expand.grid(as.factor(1:20), as.factor(1:2), c("A","B"))
names(temp) = c("Person","Case","Method")
tempOK = temp$Person + temp$Case + match(temp$Method, unique(temp$Method))
temp = temp[ (tempOK %% 2)==0 , ]
method_effect= c(A=17.45, B=14.83)
method_sd = c(A=5.13, B=4.35)
person_effect = rnorm(20, 0, 5)
case_effect = c(0,0)
unsystematic_sd = 2
temp$Time = case_effect[temp$Case] + person_effect[temp$Person] + method_effect[temp$Method] + rnorm(1, 0, method_sd[temp$Method]) + rnorm(nrow(temp), 0, unsystematic_sd)

# Initialize effect size vector for method A
# A = NULL HYPOTHESIS
n_hypoth      <- rnorm(n=20, mean=17.45, sd=5.13)
n_hypoth_avg  <- mean(n_hypoth)
n_hypoth_avg

# Initialize effect size vector for method B
# B = ALTERNATIVE HYPOTHESIS
a_hypoth      <- rnorm(n=20, mean=14.83, sd=4.35)
a_hypoth_avg  <- mean(a_hypoth)
a_hypoth_avg

# Create vector of zeros to add thrid column to study_matrix for times
zeros <- rep(0, 40)
study_matrix <- cbind(persons_vector,method_vector, case_vector, zeros)
colnames(study_matrix) <- c('Person', 'Method', 'Case', 'Time')
study_matrix
class(study_matrix)

# Match method A with corresponding n_hpoth time
n <- 1
for(i in seq(along=study_matrix[,2])) {
  if(study_matrix[i,2] == 'A') {
    study_matrix[i,4] <- n_hypoth[n]
    n <- n+1
  }
}


# Match method B to corresponding a_hpoth time
n <- 1
for(i in seq(along=study_matrix[,2])) {
  if(study_matrix[i,2] == 'B') {
    study_matrix[i,4] <- a_hypoth[n]
    n <- n+1
  }
}
study_matrix
# End build matrix

# Test statistic
group_means <- c(n_hypoth_avg, a_hypoth_avg))
group_means
effect <- n_hypoth_avg - a_hypoth_avg

# Convert matrix to data frame
study_matrix
head(study_matrix)
study_frame <- as.data.frame(study_matrix)
head(study_frame)
sapply(sf2, class)
sapply(study_frame[,4], as.numeric)
sapply(study_frame, class)
dput(head(study_frame))
sf2 <- transform(study_frame, Time = as.numeric(Time))
sf2
options(digits=6)
study_frame
round(study_frame[,4], 3)

# Begin power analysis
library(pwr)
#pwr.anova.test()
pwr.anova.test(k=2, n=NULL, f=3, sig.level=0.05, power=0.80)
?glm
attach(study_frame)
str(study_frame)
model <- glm(Time ~ Method + Case + Person, data=temp)
model
summary(model)
anova(model)
summary(anova(model))
an <- anova(model)
study_aov <- aov(as.numeric(Time) ~ Method + Case + Person, study_frame)
summary(study_aov)