library(tidyverse)
library(plyr)

### Uncertainty about a belief as a probability density with tail

p <- 0.25
n <- 10
a <- n*p
b <- n*(1-p)
x <- seq(0, 1, by=0.001)
y <- dbeta(x, a, b)
dat <- data.frame(x, y)

p <- ggplot(dat, aes(x=x,y=y)) +
  geom_line(lwd=1.2) +
  xlim(0,1) +
  ylim(0,max(dat$y)) +
  ylab("Probability density") +
  #  xlab(expression(theta)) +
  xlab("Parameter value") +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size=16))

svg("plots/densarea.svg")
p + geom_area(mapping = aes(y = ifelse(x>0.5, y, 0)), fill = "red")
dev.off()



### Tornado plot

df <- 'Parameter,High value,Low value,UL_Difference
Parameter 1,8074,11181,3108
Parameter 2,8177,11007,2831
Parameter 3,8879,10188,1308
Parameter 4,4358,18697,14339
Parameter 5,9073,10087,1013
Parameter 6,12034,7572,4462
Parameter 7,11357,7933,3423
Parameter 8,9769,9202,567
Parameter 9,8833,10403,1570
Parameter 10,13450,4219,9231
Parameter 11,10691,7915,2776
Parameter 12,10036,8792,1244
' %>% read_csv(col_types="cdd")
df

# original value of output
base.value <- 9504

# get order of parameters according to size of intervals
# Defines the ordering of the factors, which defines the positions in the plot
order.parameters <- df %>% arrange(UL_Difference) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
width <- 0.95
# get data frame in shape for ggplot and geom_rect
df.2 <- df %>%
  gather(key='type', value='output.value', `High value`:`Low value`) %>%
  select(Parameter, type, output.value, UL_Difference) %>%
  # create the columns for geom_rect
  mutate(Parameter=factor(Parameter, levels=order.parameters),
         ymin=pmin(output.value, base.value),
         ymax=pmax(output.value, base.value),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2)

p <- ggplot() +
  geom_rect(data = df.2,
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size=20)) +
  geom_hline(yintercept = base.value) +
  scale_x_continuous(breaks = c(1:length(order.parameters)),
                     labels = order.parameters) +
  coord_flip()

svg("plots/tornado.svg", bg="transparent")
p
dev.off()



## Meta-analysis (fake data)

library(metafor)
madata <- data.frame(
  study = paste("Study",1:8),
  est = c(51.9, 70.4, 79.4, 70.5, 47.7, 28.0, 74.5, 73.6)
) |>
  mutate(lower = exp(log(est) - 0.4),
         upper = exp(log(est) + 0.4),
         se = (upper - lower)/4,
         pooled = FALSE)
mod <- rma(yi=madata$est, sei=madata$se)
pr <- predict(mod)
pooled <- data.frame(
  study = c("Pooled (average)","Pooled (predicted study)"),
  est = rep(pr$pred, 2),
  lower = c(pr$ci.lb, pr$pi.lb),
  upper = c(pr$ci.ub, pr$pi.ub),
  se = c(pr$se, NA), pooled=TRUE
) |> mutate(study = factor(study, levels=rev(study)))

svg("plots/ma.svg", height=4)
ggplot(madata, aes(x=study, y=est, ymin=lower, ymax=upper, col=pooled)) +
  coord_flip() +
  geom_point(size=3) + geom_linerange(linewidth=1.2) +
  geom_point(data=pooled, size=4) + geom_linerange(data=pooled, linewidth=1.5) +
  xlab("") + ylab("Estimate and credible interval") +
  theme(legend.position = "none",
        text = element_text(size=15)) +
  scale_colour_manual(values=c("FALSE"="black","TRUE"="red"))
dev.off()



## Uniform vs triangular vs normal

dtri <- function(x, est, lower, upper){
  ifelse((x < lower) | (x > upper), 0,
         ifelse(x < est, x - lower, upper - x)) / (2*(est-lower))
}

dat <- data.frame(x = seq(-3, 3, by=0.1)) |>
  mutate(yunif = dunif(x, -2, 2),
         ytri = dtri(x, 0, -2, 2),
         ynorm = dnorm(x, 0, 1))

ptheme <- theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                text = element_text(size=20))
svg("plots/bare_unif.svg")
ggplot(dat |> filter(x>=-2, x<=2), aes(x=x)) +
  geom_area(aes(y=yunif), fill="darkblue", alpha=0.8) +
  ylim(0,0.5) + xlim(-3, 3) + xlab("") + ylab("Probability density") + ptheme
dev.off()

svg("plots/bare_tri.svg")
ggplot(dat, aes(x=x)) +
  geom_area(aes(y=ytri), fill="darkblue", alpha=0.8) +
  ylim(0,0.5) + xlim(-3, 3) + ylab("Probability density") + ptheme
dev.off()

svg("plots/bare_norm.svg")
ggplot(dat, aes(x=x)) +
  geom_ribbon(aes(ymin=0, ymax=ynorm), fill="darkblue", alpha=0.8) +
  ylim(0,0.5) + xlim(-3, 3) + ylab("Probability density") + ptheme
dev.off()



### Normal distribution

svg("plots/norm.svg")
ggplot(dat, aes(x=x)) +
  geom_area(data=data.frame(x=c(-2,2), y=0.5), aes(x=x, y=y), alpha=0.1) +
  geom_vline(xintercept=0, col="gray50") +
  geom_ribbon(aes(ymin=0, ymax=ynorm), fill="darkblue", alpha=0.6) +
  ylim(0,0.5) + ylab("Probability density") + ptheme +
  scale_x_continuous(breaks=0, limits=c(-3, 3), labels=expression(mu), name="") +
  geom_line(data=data.frame(x=c(-2, 2), y=0.2), aes(y=y, x=x),
            arrow = arrow(length=unit(0.30,"cm"), ends="both", type = "closed")) +
  geom_line(data=data.frame(x=c(0, 1), y=0.3), aes(y=y, x=x),
            arrow = arrow(length=unit(0.30,"cm"), ends="both", type = "closed")) +
  annotate(geom="text", x=0, y=0.2, vjust=-1, label="95% credible interval", size=5) +
  annotate(geom="text", x=0.5, y=0.3, vjust=-1, size=5,
           label=expression(paste("Standard deviation ", sigma))) +
  theme(axis.text.x = element_text(size=20))
dev.off()



### Log-normal distribution

lmean <- log(2.5); lsd <- (log(4) - log(1))/4
dat <- data.frame(x = seq(0, 5, by=0.1)) |>
  mutate(ylnorm = dlnorm(x, lmean, lsd))

svg("plots/lnorm.svg")
ggplot(dat, aes(x=x)) +
  geom_area(aes(y=ylnorm), fill="darkblue", alpha=0.6) +
  scale_x_continuous(breaks = c(1, 2.5, 4)) +
  xlab("") + ylab("Probability density") +
  ggtitle(expression(paste("Log-normal: ",
                           mu, "=log(2.5), ", sigma, "=", (log(4)-log(1))/4))) +
theme(axis.text.x = element_text(size=20))
dev.off()


### Beta distribution

betaa <- function(m,s){(m*(1-m)/s^2 - 1)*m}
betab <- function(m,s){(m*(1-m)/s^2 - 1)*(1 - m)}
m <- 0.4; lower <- 0.2; upper <- 0.6; s <- (upper - lower)/(2*1.96)
m2 <- 0.04; lower <- 0.01; upper <- 0.4; s2 <- (upper - lower)/(2*1.96)
dat <- data.frame(x = seq(0, 1, by=0.01)) |>
  mutate(ybeta = dbeta(x, betaa(m,s), betab(m,s)),
         ybeta2 = dbeta(x, betaa(m2,s2), betab(m2,s2)))

svg("plots/beta.svg")
ggplot(dat, aes(x=x)) +
  geom_area(aes(y=ybeta), fill="darkblue", alpha=0.6) +
  geom_area(aes(y=ybeta2), fill="darkred", alpha=0.6) +
  scale_x_continuous(breaks = c(0, 0.1, 0.4, 1)) + ylim(0,5) +
  xlab("") + ylab("Probability density") +
  geom_line(data=data.frame(x=c(0.2, 0.6), y=3), aes(y=y, x=x),
            arrow = arrow(length=unit(0.30,"cm"), ends="both", type = "closed")) +
  geom_line(data=data.frame(x=c(0.01, 0.4), y=5), aes(y=y, x=x),
            arrow = arrow(length=unit(0.30,"cm"), ends="both", type = "closed")) +
  annotate(geom="text", x=0.1, y=5-0.02, hjust=0, vjust=1,
           label="(b) Estimate 0.1\nCI approximately 0.01 to 0.4", size=5) +
  annotate(geom="text", x=0.5, y=4-0.1, hjust=0, vjust=1,
           label="(a) Estimate 0.4\nCI 0.2 to 0.6", size=5) +
  ggtitle(expression(paste("Beta distributions: two examples"))) +
  theme(axis.text.x = element_text(size=20),
        plot.title = element_text(size=18))
dev.off()

## Actual CI width
diff(qbeta(c(0.025, 0.975), betaa(m2,s2), betab(m2,s2)))


## One-way sensitivity analysis doesn't convey nonlinear effects

x <- c(-1, 1)
y <- c(0, 0)
xfit <- seq(-1.5, 1.5, by=0.01)
yfit <- 1 - (xfit)^2
svg("plots/sa_nonlin.svg",height=4)
ggplot(data=NULL) +
  geom_line(aes(x=xfit, y=yfit)) +
  geom_point(aes(x=x, y=y)) +
  xlab("Model input") + ylab("Model output") +
  scale_x_continuous(breaks=c(-1, 1), labels=c("Low", "High")) +
  scale_y_continuous(breaks=NULL, labels=NULL) +
  theme(text=element_text(size=20))
dev.off()
