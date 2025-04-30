victoria_lifetable <-
  readRDS("../../chronic/jibe/melbourne_mortality/data-r/victoria_lifetable.rds")
victoria_sa2_deaths <-
  readRDS("../../chronic/jibe/melbourne_mortality/data-r/victoria_sa2_deaths.rds")

svg("plots/melb_mort.svg")
ggplot(victoria_lifetable, aes(x=age, y=rate1000, col=sex)) +
  geom_hline(data=victoria_sa2_deaths, aes(yintercept = stdrate),
             col="blue", alpha=0.3, lwd=1.05) +
  geom_line(lwd=1.5) +
  labs(col="") +
  scale_y_continuous(trans="log",
                     breaks=c(0.0002, 0.0005, 0.001, 0.005,
                              0.01, 0.05, 0.1, 0.2)*1000) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  theme_bw() +
  theme(legend.position = c(0.5, 0.1),
        legend.justification = "bottom",
        legend.background = element_blank(),
        text = element_text(size=20)) +
  annotate(geom="text", x=0, y=20,
           label="Standardised average rates by small area",
           col="blue", alpha=0.6, hjust=0, size=6) +
  xlab("Age") + ylab("Mortality rate (per 1000 person-years)") +
  annotate(geom="text", x=10, y=500,
           label="Average over areas in state, by age and sex",
           col="blue", alpha=0.6, hjust=0, size=6)
dev.off()



melbourne_sa2_deaths <- victoria_sa2_deaths |>
  filter(sa2_code %in% sa2$SA2_MAINCODE_2016[sa2$GCCSA_NAME_2016=="Greater Melbourne"])

deaths_age_sa2 <- victoria_lifetable |>
  select(age, sex, rate1000) |>
  cross_join(melbourne_sa2_deaths |>
               select(sa2_code, sa2_name, RR)) |>
  mutate(rate1000 = rate1000*RR,
         area_sex = paste(sa2_code,sex))

svg("plots/melb_mort_disagg.svg", width=12)
deaths_age_sa2 |>
  filter(sa2_code %in% sample(unique(.data$sa2_code), 100)) |>
  droplevels() |>
  ggplot(aes(x=age, y=rate1000, col=sex, group=sa2_code)) +
  geom_line(lwd=1, alpha=0.2) +
  labs(col="") +
  facet_grid(cols=vars(sex)) +
  scale_y_continuous(trans="log",
                     breaks=c(0.0002, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.2)*1000) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=20)) +
  xlab("Age") + ylab("Mortality rate (per 1000 person-years)")
dev.off()

