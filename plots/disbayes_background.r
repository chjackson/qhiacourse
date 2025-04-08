library(tidyverse)

## Bayesian inference
## Prior distribution for a probability, e.g. prevalence of a disease
## Uniform prior and two posteriors

x <- seq(0, 1, by=0.001)
yprior <- dunif(x, 0, 1)
y10 <- dbeta(x, 2, 8)
y100 <- dbeta(x, 20, 80)
dat <- data.frame(x, y10, y100)

p <- ggplot(dat, aes(x=x,y=y10)) +
  geom_ribbon(aes(ymax=yprior, ymin=0), fill="gray60") +
  geom_ribbon(aes(ymax=y10, ymin=0), fill="blue", alpha=0.5) +
  geom_ribbon(aes(ymax=y100, ymin=0), fill="darkblue", alpha=0.5) +
  annotate(geom="text", x=0.6, y=1.5, label="Prior (Beta(1,1))", col="gray60") +
  annotate(geom="text", x=0.5, y=2.5, label="Posterior with 2 cases out of 10: Beta(1+2, 1+8)", col="blue") +
  annotate(geom="text", x=0.3, y=6.5, label="Posterior with 20 cases out of 100: Beta(1+20, 1+80)", col="darkblue") +
  xlim(0,1) +
  ylim(0,max(dat$y100)) +
  ylab("Probability density") +
  xlab("Disease prevalence") +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20))
ggsave("plots/prior_post.png")
p
dev.off()

p + geom_area(mapping = aes(y = ifelse(x>0.5, y, 0)), fill = "red")


## Counts and proportions equivalence

## Two examples

x <- seq(0, 1, by=0.001)
yprior <- dunif(x, 0, 1)
y10 <- dbeta(x, 4, 6)
y100 <- dbeta(x, 40, 60)
dat <- data.frame(x, y10, y100)

p <- ggplot(dat, aes(x=x,y=y10)) +
  geom_ribbon(aes(ymax=y10, ymin=0), fill="blue", alpha=0.5) +
  geom_ribbon(aes(ymax=y100, ymin=0), fill="darkblue", alpha=0.5) +
  annotate(geom="text", x=0.7, y=6.5, label="40 cases out of 100: Beta(40, 60)", col="darkblue") +
  annotate(geom="text", x=0.7, y=6, label="Estimate 0.4 (0.3 to 0.5)", col="darkblue") +
  geom_errorbarh(aes(xmin=qbeta(0.025,40,60),
                      xmax=qbeta(0.975,40,60), y=6), lwd=1.5, col="darkblue") +
  annotate(geom="text", x=0.7, y=2.5, label="4 cases out of 10: Beta(4, 6)", col="blue") +
  annotate(geom="text", x=0.7, y=2, label="Estimate 0.4 (0.14 to 0.70)", col="darkblue") +
  geom_errorbarh(aes(xmin=qbeta(0.025,4,6),
                      xmax=qbeta(0.975,4,6), y=1.5), lwd=1.5, col="blue") +
  xlim(0,1) +
  ylim(0,max(dat$y100)) +
  ylab("Probability density") +
  xlab("Disease prevalence") +
  theme(axis.text.x = element_text(size=20),
        axis.title = element_text(size=20),
        axis.text.y = element_blank())
ggsave("plots/count_prop.png")
p
dev.off()
