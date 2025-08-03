d <- read.csv(file.choose())
head(d)
library(tidyverse)
library(rethinking)
library(ggplot2)
library(brms)
library(bayesplot)
library(ggdist)
library(ggthemes)
distinct(d, prize_genre)
set.seed(13)

tibble(n = rnorm(1e6, mean = -1, sd = 1)) %>% 
  mutate(p = inv_logit_scaled(n)) %>% 
  
  ggplot(aes(x = p)) +
  geom_histogram(fill = "blue", binwidth = .02, boundary = 0) +
  scale_y_continuous(breaks = NULL)
d <- d %>% 
  str_trim(highest_degree)
d <- d %>% 
  mutate(person_role = ifelse(person_role == "shortlisted", 0, 1))
model <- brm(person_role | trials(1) ~ 1 + (1 | prize_genre) + (1 | gender) + (1 | ethnicity_macro) + (1 | highest_degree),
          data = d, 
          family = binomial,
          prior = c(prior(normal(-1, 1), class = Intercept),
                    prior(exponential(1), class = sd)),
          iter = 2000, 
          warmup = 1000, 
          chains = 4, 
          cores = 4)

as_draws_df(model) %>%
  select(starts_with("sd_")) %>%
  set_names(str_c("sigma[", c("ethnicity", "gender", "degree", "genre"), "]")) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  median_qi(.width = seq(from = .5, to = .9, by = .1)) %>%
  
  ggplot(aes(x = value, xmin = .lower, xmax = .upper, y = reorder(name, value))) +
  geom_interval(aes(alpha = .width), color = "orange3") +
  scale_alpha_continuous("CI width", range = c(.7, .15)) +
  scale_y_discrete(labels = ggplot2:::parse_safe) +
  xlim(0, NA) +
  theme(axis.text.y = element_text(hjust = 0),
        panel.grid.major.y = element_blank())

as_draws_df(model) %>%
  select(starts_with("sd_")) %>%
  set_names(str_c("sigma[", c("ethnicity", "gender", "degree", "genre"), "]")) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  ggplot(aes(x = value, y = name)) +
  stat_halfeye(.width = .95, size = 1, fill = "orange", adjust = 0.1) +
  scale_y_discrete(labels = ggplot2:::parse_safe, expand = expansion(add = 0.1)) +
  labs(subtitle = "The variation among treatment levels is small, but the\nvariation among the levels of block is still the smallest.") +
  theme(axis.text.y = element_text(hjust = 0))

levels <- c("raw_data", "multilevel")

p1 <- d %>%
  group_by(ethnicity_macro, person_role) %>%
  summarise(n = n()) %>%
  group_by(ethnicity_macro) %>%
  mutate(p = n / sum(n),
           type = factor("raw_data", levels = levels)) %>%
  filter(person_role == 1) %>%
  ggplot(aes(x = p, y = ethnicity_macro)) +
  geom_point() +
  scale_x_continuous("Probability of winning", limits = c(0, 1), breaks = c(0, .5, 1)) +
  facet_wrap(~ type) 

p1  

nd <- distinct(d, ethnicity_macro) %>% arrange(ethnicity_macro)

p2 <- fitted(
  model,
  newdata = nd,
  re_formula = ~ (1 | ethnicity_macro) ) %>%
  data.frame() %>%
  bind_cols(nd) %>% 
  mutate(prop = Estimate,
         type = factor("multilevel", levels = levels)) %>% 
  
  ggplot(aes(x = prop, xmin = Q2.5, xmax = Q97.5, y = ethnicity_macro)) + 
  geom_pointrange(color = "blue2", linewidth = 0.8, fatten = 2) +
  scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
  scale_y_discrete(labels = NULL) +
  facet_wrap(~ type)

d <- d %>%
  mutate(
    degree_new = recode(highest_degree,
                        "none"                    = "1",
                        "unknown"                 = "2",
                        "Certificate of Education"= "3",
                        "Diploma"                 = "4",
                        "Postgraduate"            = "5",
                        "Bachelors"               = "6",
                        "Masters"                 = "7",
                        "Juris Doctor"            = "8",
                        "MD"                      = "9",
                        "Doctorate"               = "10"
    ) %>% 
      as.integer())

library(gtools)
set.seed(1805)
delta <- rdirichlet(10, rep(2, 10))
str(delta)

delta %>% 
  data.frame() %>%
  set_names(1:10) %>% 
  mutate(row = 1:n()) %>% 
  pivot_longer(-row, names_to = "index") %>% 
  
  ggplot(aes(x = index, y = value, group = row,
             alpha = row == 3, color = row == 3)) +
  geom_line() +
  geom_point() +
  scale_alpha_manual(values = c(1/3, 1)) +
  scale_color_manual(values = canva_pal("Green fields")(4)[1:2]) +
  ylab("probability") +
  theme(legend.position = "none")

set.seed(12)

brms::rdirichlet(n = 1e4, alpha = rep(2, 7)) %>% 
  data.frame() %>% 
  set_names(1:7) %>% 
  pivot_longer(everything()) %>% 
  mutate(name  = name %>% as.double(),
         alpha = str_c("alpha[", name, "]")) %>% 
  
  ggplot(aes(x = value, color = name, group = name, fill= name)) + 
  geom_density(alpha = .8) + 
  scale_fill_gradient(low = canva_pal("Green fields")(4)[2],
                      high = canva_pal("Green fields")(4)[3]) +
  scale_color_gradient(low = canva_pal("Green fields")(4)[2],
                       high = canva_pal("Green fields")(4)[3]) +
  scale_x_continuous("probability", limits = c(0, 1),
                     breaks = c(0, .5, 1), labels = c("0", ".5", "1"), ) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = expression("Dirichlet"*(2*", "*2*", "*2*", "*2*", "*2*", "*2*", "*2))) +
  theme(legend.position = "none") +
  facet_wrap(~ alpha, labeller = label_parsed, nrow = 2)

mo <- 
  brm(data = d, 
      family = binomial,
      bf(person_role | trials(1) ~ a + b,
      a ~ 1 + (1 | prize_genre) + (1 | gender) + (1 | ethnicity_macro),
      b ~ 0 + mo(degree_new),
      nl = TRUE),
      prior = c(prior(normal(-1, 1), class = b, coef = Intercept, nlpar = a),
                prior(exponential(1), class = sd, group = prize_genre, nlpar = a),
                prior(exponential(1), class = sd, group = gender, nlpar = a),
                prior(exponential(1), class = sd, group = ethnicity_macro, nlpar = a),
                prior(normal(0, 0.111), class = b, coef = modegree_new, nlpar = b),
                prior(dirichlet(rep(2, 9)), class = simo, coef = modegree_new1, nlpar = b)),
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 12)
print(mo)

as_draws_df(mo) %>%
  transmute(bE = bsp_b_modegree_new * 9) %>%
  median_qi(.width = .89) %>%
  mutate_if(is.double, round, digit = 2)

my_lower <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr <- cor(x, y, method = "p", use = "pairwise")
  abs_corr <- abs(corr)
  
  ggally_text(
    label = formatC(corr, digits = 2, format= "f") %>% str_replace(., "0.", "."),
    mapping = aes(),
    size = 4,
    color = canva_pal("Green fields")(4)[2]) +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL)
}

my_daig <- function(data, mapping, ...) {
  ggplot(data, mapping = mapping) +
    geom_density(fill = canva_pal("Green fields")(4)[1], linewidth = 0) +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL) 
}

my_upper <- function(data, mapping, ...) {
 ggplot(data, data, mapping = mapping) +
    geom_hex(bins = 18) +
    scale_fill_gradient(low = canva_pal("Green fields")(4)[4],
                        high = canva_pal("Green fields")(4)[3]) +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme(panel.background = element_rect(fill = canva_pal("Green fields")(4)[2]))
  }

library(GGally)

delta_labels <- c("none", "unknown", "Certif", "Dipl", "Post",
                  "Bachs", "Mast", "Juris", "MD", "Doct")

as_draws_df(mo) %>%
  select(contains("simo_b_modegree_new1")) %>%
  set_names(str_c(delta_labels[2:10], "~(delta[", 1:9, "])")) %>%
  ggpairs(upper = list(continuous = my_upper),
          diag = list(continuous = my_daig),
          lower = list(continuous = my_lower),
          labeller = label_parsed) +
  theme(strip.text = element_text(size = 8))

d <- d %>%
  mutate(degree_norm = (degree_new - 1)/ 9)

d %>%
  distinct(highest_degree, degree_new, degree_norm) %>%
  arrange(degree_new)

mn <- 
  brm(data = d, 
      family = binomial,
      bf(person_role | trials(1) ~ a + b,
         a ~ 1 + (1 | prize_genre) + (1 | gender) + (1 | ethnicity_macro),
         b ~ 0 + degree_norm,
         nl = TRUE),
      prior = c(prior(normal(-1, 1), class = b, coef = Intercept, nlpar = a),
                prior(exponential(1), class = sd, group = prize_genre, nlpar = a),
                prior(exponential(1), class = sd, group = gender, nlpar = a),
                prior(exponential(1), class = sd, group = ethnicity_macro, nlpar = a),
                prior(normal(0, 1), class = b, nlpar = b)),
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 12)

nd1 <- distinct(d, degree_new) %>%
  arrange(degree_new)

f1 <- fitted(mo,
             newdata = nd1,
             re_formula = NULL)

as_draws_df(mo) %>% 
  select(contains("new1")) %>% 
  set_names(1:9) %>% 
  mutate(draw = 1:n(), 
         `0`  = 0) %>% 
  pivot_longer(-draw, names_to = "delta") %>% 
  arrange(delta) %>% 
  group_by(draw) %>% 
  mutate(cum_sum = cumsum(value)) %>% 
  
  ggplot(aes(x = delta, y = cum_sum)) +
  stat_pointinterval(.width = .95, size = 1,
                     color = canva_pal("Green fields")(4)[1]) +
  stat_pointinterval(.width = .5,
                     color = canva_pal("Green fields")(4)[4],
                     point_color = canva_pal("Green fields")(4)[2]) +
  scale_x_discrete(NULL, labels = parse(text = str_c("delta[", 0:9 , "]"))) +
  ylab("cumulative sum")

mo <- add_criterion(mo, "loo")
mn <- add_criterion(mn, "loo")

loo_compare(mo, mn, criterion = "loo") %>% print(simplify = F)
model_weights(mo, mn, weights = "loo") %>% round(digits = 2)
