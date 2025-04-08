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
  xlab(expression(theta)) +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20))

ggsave("plots/densarea.png")
p + geom_area(mapping = aes(y = ifelse(x>0.5, y, 0)), fill = "red")
dev.off()



### Tornado plot

df <- 'Parameter High_value Low_value UL_Difference
Parameter01 8074 11181 3108
Parameter02 8177 11007 2831
Parameter03 8879 10188 1308
Parameter04 4358 18697 14339
Parameter05 9073 10087 1013
Parameter06 12034 7572 4462
Parameter07 11357 7933 3423
Parameter08 9769 9202 567
Parameter09 8833 10403 1570
Parameter10 13450 4219 9231
Parameter11 10691 7915 2776
Parameter12 10036 8792 1244
' %>% read_table2()

# original value of output
base.value <- 9504

# get order of parameters according to size of intervals
# (I use this to define the ordering of the factors which I then use to define the positions in the plot)
order.parameters <- df %>% arrange(UL_Difference) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
width <- 0.95
# get data frame in shape for ggplot and geom_rect
df.2 <- df %>%
  gather(key='type', value='output.value', High_value:Low_value) %>%
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
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  geom_hline(yintercept = base.value) +
  scale_x_continuous(breaks = c(1:length(order.parameters)),
                     labels = order.parameters) +
  coord_flip()

ggsave("plots/tornado.png", bg="transparent"); p; dev.off()



### EVPPI scatterplot

