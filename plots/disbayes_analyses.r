library(disbayes)
library(tidyverse)
library(gridExtra)

ihdbristol <- ihdengland %>%
  filter(area=="Bristol", gender=="Male") |>
  mutate(inc_prob = inc_num/inc_denom,
         inc_lower = qbeta(0.025, inc_num, inc_denom - inc_num),
         inc_upper = qbeta(0.975, inc_num, inc_denom - inc_num),
         prev_prob = prev_num/prev_denom,
         prev_lower = qbeta(0.025, prev_num, prev_denom - prev_num),
         prev_upper = qbeta(0.975, prev_num, prev_denom - prev_num),
         mort_prob = mort_num/mort_denom,
         mort_lower = qbeta(0.025, mort_num, mort_denom - mort_num),
         mort_upper = qbeta(0.975, mort_num, mort_denom - mort_num))

ptheme <- theme(axis.text = element_text(size=16),
                plot.title =  element_text(size=16))
p1 <- ggplot(ihdbristol, aes(x=age, y=inc_prob)) +
  geom_point() + geom_errorbar(aes(ymin=inc_lower, ymax=inc_upper)) +
  xlab("") + ggtitle("Incidence") + xlim(50, 100) + ylab("") + ptheme
p2 <- ggplot(ihdbristol, aes(x=age, y=prev_prob)) +
  geom_point() + geom_errorbar(aes(ymin=prev_lower, ymax=prev_upper)) +
  xlab("") + ggtitle("Prevalence")+ xlim(50, 100) + ylab("") + ptheme
p3 <- ggplot(ihdbristol, aes(x=age, y=mort_prob)) +
  geom_point() + geom_errorbar(aes(ymin=mort_lower, ymax=mort_upper)) +
  ggtitle("Mortality")+ xlim(50, 100) + xlab("Age") + ylab("") + ptheme
grid.arrange(p1, p2, p3, ncol=1)

svg("plots/ihdbristol_data.svg")
grid.arrange(p1, p2, p3, ncol=1)
dev.off()


## Naive independence model
dbres <- disbayes(data = ihdbristol, age = "age",
                  inc_num = "inc_num", inc_denom = "inc_denom",
                  prev_num = "prev_num", prev_denom = "prev_denom",
                  mort_num = "mort_num", mort_denom = "mort_denom",
                  cf_model = "indep")
svg("plots/ihdbristol_cfindep.svg")
plot(dbres) +  ylab("Case fatality") + xlab("Age") +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=18))
dev.off()


## Smooth model plus eqage (can't have eqage in independence model)
dbres <- disbayes(data = ihdbristol, age = "age",
                  inc_num = "inc_num", inc_denom = "inc_denom",
                  prev_num = "prev_num", prev_denom = "prev_denom",
                  mort_num = "mort_num", mort_denom = "mort_denom",
                  eqage = 40)
svg("plots/ihdbristol_cfsmooth.svg")
plot(dbres) +  ylab("Case fatality") + xlab("Age") +
theme(axis.text = element_text(size=18),
      axis.title = element_text(size=18))
dev.off()



## Some demo of weak data for a rare disease and/or small area breaking it
## Stomach cancer TODO make link downloadable
scengland <- readRDS(file.path("../../chronic/disbayes/metahit/gbddb.rds")) %>%
  dplyr::filter(disease == "Stomach cancer") %>%
  select(-disease, -areatype, -rem_num, -rem_denom)

scbristol <- scengland %>%
  filter(area=="Bristol", gender=="Male") |>
  mutate(inc_prob = inc_num/inc_denom,
         inc_lower = qbeta(0.025, inc_num, inc_denom - inc_num),
         inc_upper = qbeta(0.975, inc_num, inc_denom - inc_num),
         prev_prob = prev_num/prev_denom,
         prev_lower = qbeta(0.025, prev_num, prev_denom - prev_num),
         prev_upper = qbeta(0.975, prev_num, prev_denom - prev_num),
         mort_prob = mort_num/mort_denom,
         mort_lower = qbeta(0.025, mort_num, mort_denom - mort_num),
         mort_upper = qbeta(0.975, mort_num, mort_denom - mort_num))

## TODO bar plot with numbers for inc.   IHD vs SC
ptheme <- theme(axis.text = element_text(size=16),
                plot.title =  element_text(size=16))
bplot <- rbind(
  scbristol |> select(age, inc_num, inc_denom) |> mutate(disease="Stomach cancer"),
  ihdbristol |> select(age, inc_num, inc_denom) |> mutate(disease="Ischaemic heart disease")
) |>
  mutate(inc_num_std = round(1000*inc_num/inc_denom))

p1 <- ggplot(bplot, aes(x=age, y=inc_num_std, col=disease)) +
  geom_point() +
  xlab("Age") + ggtitle("Annual incidence (cases per 1000)") +
  xlim(50, 100) + ylim(0, 50) + ylab("") +
  theme(legend.position = "none",
        text = element_text(size=20)) +
  annotate(geom="text", x=80, y=50, label="Ischaemic heart disease",
           col="darkred",size=6) +
  annotate(geom="text", x=80, y=10, label="Stomach cancer",
           col="darkblue",size=6) +
  scale_color_manual(values = c("darkred","darkblue"))
svg("plots/scbristol_data.svg")
p1
dev.off()

## Fails with error referring to chol.default(-H)
dbres <- disbayes(data = scbristol, age = "age",
                  inc_num = "inc_num", inc_denom = "inc_denom",
                  prev_num = "prev_num", prev_denom = "prev_denom",
                  mort_num = "mort_num", mort_denom = "mort_denom",
                  eqage = 40)

## Succeeds
dbres <- disbayes(data = scbristol, age = "age",
                  inc_num = "inc_num", inc_denom = "inc_denom",
                  prev_num = "prev_num", prev_denom = "prev_denom",
                  mort_num = "mort_num", mort_denom = "mort_denom",
                  eqage = 70)
plot(dbres)

## This would also work, but takes several minutes to run
dbres <- disbayes(data = scbristol, age = "age",
                  inc_num = "inc_num", inc_denom = "inc_denom",
                  prev_num = "prev_num", prev_denom = "prev_denom",
                  mort_num = "mort_num", mort_denom = "mort_denom",
                  eqage=40, method="mcmc", chains=1, iter=2000)
plot(dbres)

## Using national data (not tried, would need to sum up scengland by area)
## exercise??

## Increasing case fatality
dbres <- disbayes(data = scbristol, age = "age",
                  inc_num = "inc_num", inc_denom = "inc_denom",
                  prev_num = "prev_num", prev_denom = "prev_denom",
                  mort_num = "mort_num", mort_denom = "mort_denom",
                  eqage = 60, cf_model="increasing")
plot(dbres)



### Data inconsistency demo

ihdleeds <- ihdengland %>%
  filter(area=="Leeds", gender=="Male")
dbres_inc <- disbayes(data = ihdleeds, age = "age",
                      inc_num = "inc_num", inc_denom = "inc_denom",
                      prev_num = "prev_num", prev_denom = "prev_denom",
                      mort_num = "mort_num", mort_denom = "mort_denom",
                      eqage = 30)
dbres_noinc <- disbayes(data = ihdleeds |> filter(gender=="Male"), age = "age",
                        prev_num = "prev_num", prev_denom = "prev_denom",
                        mort_num = "mort_num", mort_denom = "mort_denom",
                        eqage = 30)

res_inc <- tidy(dbres_inc) |> mutate(model="Estimated from prev. and inc.")
res_noinc <- tidy(dbres_noinc) |> mutate(model="Estimated from prev.")
obsdata <- ihdleeds |>
  mutate(estimate = inc_num/inc_denom, model="Observed (current) incidence", var="Incidence",
         lower=NA, upper=NA) |>
  select(age, estimate, lower, upper, var, model)
res <- rbind(res_inc, res_noinc) |>
  filter(var %in% c("inc","cf")) |>
  mutate(var = fct_recode(var, "Case fatality"="cf", "Incidence"="inc")) |>
  select(age, estimate=`50%`, lower=`2.5%`, upper=`97.5%`, var, model) |>
  mutate(estimate = 1 - exp(-estimate)) |> # rate to prob
  rbind(obsdata)

p <- ggplot(res, aes(x=age, y=estimate, col=model)) +
  facet_wrap(~var, scales="free_y") +
  geom_point() + geom_line() +
  #geom_ribbon(aes(ymin=lower, ymax=upper, fill=model), alpha=0.2) +
  ylab("Risk") + xlab("Age") + xlim(50, 100) +
  labs(col="", fill="") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme(legend.position = c(0.75, 0.9),
        legend.background = element_rect(fill='transparent'),
        text = element_text(size=20))

svg("plots/conflict.svg")
p
dev.off()
