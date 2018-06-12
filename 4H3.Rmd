```{r}
### a) Model the relationship between height in cm and log(weight) in log.kg
# Fit the model using map (quadratic approximation)
mod <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*log(weight),
    a ~ dnorm(178,100),
    b ~ dnorm(0,100),
    sigma ~ dunif(0,50)
  ),
  data = df
)
```

```{r}
# the reason that we don't center this data is because we take the log
# some of the values become negative
precis(mod, corr = TRUE)

```

```{r}
# interpretation
# Each row gives the quadratic approximation for each parameter.
# A person with log(weight) = 0 is expected to be -23.78cm tall. 
# With every 1 log.kg increase in weight, a person is expected to be 47.08 cm taller.
# The standard deviation gives the width of the distribution of heights around the mean

# The interval bounds indicate that 97% of the posterior probability lies between 
# 5.5% and 94.5% 

### b) Use samples from the quadratic approximate posterior of the model in a) to superimpose on the plot:
###    (1) the predicted mean height as a function of weight
###    (2) the 97% HDPI for the mean, and 
###    (3) the 97% HDPI for predicted heights

#######
# extract a sample from the posterior
post <- extract.samples(mod, n = 1e4)
```

```{r}
# function for calculating mu
mu.link <- function(weight){
  post$a + post$b*log(weight)
}
# select samples with weight between 0 and 70
weight.seq <-seq(from = 0, to = 70, by = 1)

# simulate mu and then compute mean and hpdi
# calculate the distribution of mu for each sample element
mu <- sapply(weight.seq, mu.link)
# calculate the mean of the distribution of mu for each sample element
mu.mean <- apply(mu, 2, mean)
# calculate the 97% HDPI for each distribution above
mu.HPDI<- apply(mu, 2, HPDI, prob = 0.97)
# simulate heights from the posterior
sim.height <- sim(mod, data = list(weight=weight.seq))
# summarize the simulated heights (mean and PI)
height.PI <-apply(sim.height, 2, PI, prob = 0.97)
```

```{r}
plot(height~weight, df,xlim = c(-1,70),ylim = c(-30
                                                ,180), col = col.alpha(rangi2,0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)

# (1) The solid line is the MAP estimate of the mean height at each weight
# (2) The narrow shaded interval around the line is the HPDI for mu
# (3) The 97% HPDI for predicted heights  
```
