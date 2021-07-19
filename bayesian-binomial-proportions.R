library(tidyverse)
library(ggtext)

# Use the mode and 'concentration' parameterisation for the beta distribution.
# Concentration must be at least 2; this determines how concentrated the
# distribution is around the mode. A value of 2 gives an uninformative prior.
mode <- 0.065
concentration <- 80

# Transform the mode and concentration into the standard parameters for the Beta
# distribution prior.
alpha <- mode * (concentration - 2) + 1
beta  <- (1 - mode) * (concentration - 2) + 1

# Posterior for p ( 215:9, 500:21, 800:34)
n <- 215
x <- 9
credibility <- 0.95

a <- alpha + x
b <- beta + n - x
m <- (a - 1) / (a + b - 2)
cred_int <- qbeta(c((1 - credibility) / 2, credibility + (1 - credibility) / 2), a, b)
cred_int_dens <- dbeta(cred_int, a, b)

subtitle_info <- str_glue('
Prior:
  - mode = {round(mode, 4)}
  - concentration = {concentration}

Observed:
  - count = {n}
  - events = {x}
  - proportion = {round(x/n, 4)}

Posterior:
  - mode = {round(m, 4)}
  - {credibility * 100}% credibility interval = ({round(cred_int[1], 4)}, {round(cred_int[2], 4)})
')

ggplot() +
  xlim(0, 1) +
  stat_function(fun = ~ dbeta(.x, alpha, beta), colour = 'darkorange', n = 500) +
  stat_function(fun = ~ dbeta(.x, a, b), colour = 'steelblue', n = 500) +
  geom_segment(aes(x = cred_int, y = 0, xend = cred_int, yend = cred_int_dens), 
               data = tibble(cred_int, cred_int_dens), colour = 'steelblue', linetype = 'dashed') +
  labs(title = "<span style='color:#FF8C00'>Prior</span> and <span style='color:#4682B4'>posterior</span> distributions for the Binomial probability",
       subtitle = subtitle_info,
       x = 'Binomial Probability', y = 'Probability Density') +
  theme_bw(base_size = 12) +
  theme(plot.title = element_markdown(size = 16, face = 'bold'))

ggsave(str_glue('conservative-prior-{n}.png'), width = 1.41, height = 1, scale = 8)


