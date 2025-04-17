dr <- read.csv("plots/strokeDR.csv") ## TODO grasp this. Is it a MVN?
nSamples <- 10000
set.seed(1)
dr <- dr[sample(1:nrow(dr), size=nSamples, replace=TRUE),,drop=FALSE]
mu <- rlnorm(nSamples,2.665364, 0.3234522)    # background pollution level
ptransp <- rbeta(nSamples,5.795375, 8.947712) # proportion of pollution attributed t
inc_base <- 18530
scenario_travel_ratio <- 0.5

dose_response <- function(pm,alpha,beta,gamma,tau){
  1 + alpha * ( 1 - exp(- beta * ( pmax(pm - tau, 0) )^gamma ) )
}

model <- function(mu, ptransp, dr)  {
  scenario_pm <- mu*(ptransp*scenario_travel_ratio + 1 - ptransp)
  RR_base <- dose_response(mu, dr$alpha, dr$beta, dr$gamma, dr$tau)
  RR_scenario <- dose_response(scenario_pm, dr$alpha, dr$beta, dr$gamma, dr$tau)
  inc_diff <- inc_base - inc_base*RR_scenario/RR_base
  inc_diff
}

inc_diff <- model(mu, ptransp, dr)


## EVPPI scatterplot

md100 <- data.frame(ptransp, inc_diff)[1:500,]
library(mgcv)
par(mgp = c(2,0.7,0))
svg("plots/evppi_scatter.svg")
plot(md100$ptransp, md100$inc_diff, col="gray", pch=19,
     xlab = "X = Proportion of PM2.5 due to transport",
     ylab = "Y = Expected stroke cases averted",
     ylim=c(0,1250))
pgam <- gam(md100$inc_diff ~ s(md100$ptransp, bs="cr"))
lines(sort(md100$ptransp), fitted(pgam)[order(md100$ptransp)], lwd=2, col="blue")
arrows(0.7, 700, 0.7, 200, code=3, lwd=2, length=0.1, col="red")
ql <- quantile(md100$inc_diff, c(0.005, 0.995))
arrows(0.1, ql[1], 0.1, ql[2], code=3, lwd=2, length=0.1, col="red")
text(0.65, 1200, "\"Observed\" values", col="gray")
text(0.1, 500, "Uncertainty before\nlearning X", col="red", pos=4)
text(0.7, 380, "Residuals:\nUncertainty after\nlearning X", col="red", pos=2)
text(0.65, 1000, "Fitted values", col="blue")
dev.off()


## EVPPI results

inputs <- cbind(mu, ptransp, dr)
library(voi)
evppis <- c(
  "current" = 0,
  "mu" = evppivar(inc_diff, mu)$evppi,
  "ptransp" = evppivar(inc_diff, ptransp)$evppi,
  "dr" = evppivar(inc_diff, inputs, pars=c("alpha","beta","gamma","tau"), method="earth")$evppi
)

var(inc_diff)
evppivar(inc_diff, inputs,
         pars=c("mu","ptransp","alpha","beta","gamma","tau"),
         method="earth", degree=4) # approaching var(inc_diff).
## Computation error, acknowledge less reliable for large npar

names <- c(" ",
           "Background PM2.5", "%PM2.5 from transport", "Dose-response parameters")
evdf <- data.frame(
  name = factor(names, levels=rev(names)),
  evppi = evppis,
  prop = evppis / var(inc_diff),
  remvar = var(inc_diff) - evppis
) |>
  mutate(
    remsd = sqrt(remvar),
    est = mean(inc_diff),
    lower = mean(inc_diff) - 2*remsd,
    upper = mean(inc_diff) + 2*remsd
  )

ggplot(evdf, aes(x=name)) +
  geom_col(aes(y=evppi), position="dodge") +
  coord_flip(clip="off") +
  ylab("Standard deviation of expected stroke cases averted") + xlab("") +
  annotate(geom = "text", y=-2000, x=4, label="With current information", hjust=1) +
  annotate(geom = "text", y=-2000, x=3.5, label="With perfect information on:", hjust=1)

svg("plots/evppi_results.svg")
ggplot(evdf, aes(x=name)) +
  geom_point(aes(y=est), size=3) +
  geom_linerange(aes(ymin=lower, ymax=upper), linewidth=1) +
  coord_flip(clip="off") +
  ylab("Expected stroke cases averted") + xlab("") +
  annotate(geom = "text", y=120, x=4.2, label="With current information",
           hjust=1, size=8) +
  annotate(geom = "text", y=120, x=3.4, label="With perfect information on:",
           hjust=1, size=8) +
  theme(text = element_text(size=20) )
dev.off()
