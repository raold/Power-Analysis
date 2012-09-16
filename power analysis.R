## Richard Oldham 
## Masters thesis power analysis

# Begin model parameters
Nsubjects = 18
# Mean task completion time for H0 and HA
method_effect= c(A=18, B=15.5)

# Begin sources of variance
method_sd = c(A=5, B=4.5)
person_effect = rnorm(Nsubjects, 0, 5)
# KEY ASSUMPTION: no variance of time due to case
case_effect = c(0,0)
# All other sources of variance
unsystematic_sd = 2
# End model parameters

## Construct data frame(person, case, method); no explicit use of as.factor for subjects and case due to needing to select only a fraction of factorial (tempOK). I use as.factor() for the simulation below
temp= expand.grid(1:Nsubjects, 1:2, c("A","B"))
names(temp) = c("Person","Case","Method")
str(temp)
# Select fraction of total comb
tempOK = temp$Person + temp$Case + match(temp$Method, unique(temp$Method))
temp = temp[ (tempOK %% 2)==0 , ]

# Generate model task completion times
temp$Time = case_effect[temp$Case] + person_effect[temp$Person] + method_effect[temp$Method] + rnorm(1, 0, method_sd[temp$Method]) + rnorm(nrow(temp), 0, unsystematic_sd)

# Clean up and assign temp to dframe
dframe = temp[with(temp, order(Person, Method)), ]
row.names(dframe) = 1:nrow(temp)
dframe

# Simulate experiment, looping over dframe H0 and HA xNREPS
NREPS = 2000

pValues = sapply(1:NREPS, function(ignoreMe) {
  dframe = expand.grid(as.factor(1:2), c("A","B"), as.factor(1:Nsubjects))
  names(dframe) = c("Case","Method","Person")
  dframeOK = as.numeric(dframe$Person) + as.numeric(dframe$Case) + 
    as.numeric(match(dframe$Method, unique(dframe$Method)))
  dframe = dframe[ (dframeOK %% 2)==0 , ]
  dframe$Time = case_effect[dframe$Case] + person_effect[dframe$Person] + 
    method_effect[dframe$Method] + rnorm(1, 0, method_sd[dframe$Method]) + 
    rnorm(nrow(dframe), 0, unsystematic_sd)
  model <- glm(Time ~ Method + Case + as.factor(Person), data=dframe)
  summary(model)$coef["MethodB", "Pr(>|t|)"]
}
)
summary(pValues)
mean(pValues < 0.05)

#powerSim viz
require(NCStats)
powerSim(mu0=method_effect['A'], s.mua=method_effect['B'], s.sigma=5, s.n=20, s.alpha=0.05, lower.tail=TRUE)