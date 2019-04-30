
library(dplyr) 
library(tidyr) 
library(Lahman)


# load data ======
career <- Batting %>%
  dplyr::filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H),
            AB = sum(AB)) %>%
  mutate(average = H / AB)


career <- Master %>% 
  tbl_df() %>% 
  select(playerID, nameFirst, nameLast) %>% 
  unite(name, nameFirst, nameLast, sep = " ") %>% 
  inner_join(career, by = "playerID") %>% 
  select(-playerID)

career %>%
  arrange(average)




# side project: Maximum likelihood function in R ====

N <- 100
x <-  rnorm(N, mean = 3, sd = 2)

mean(x)
sd(x)


LL <- function(mu, sigma) {
  R = dnorm(x, mu, sigma)
  -sum(log(R))
}


mle(LL, start = list(mu = mean(x), sigma = sd(x)), method = "L-BFGS-B",
    lower = c(-Inf, 0), upper = c(Inf, Inf)) 


# filter data ======

career_filtered <- career %>%
  dplyr::filter(AB > 500)

# log-likelihood function


ll <- function(alpha, beta) {
  x <- career_filtered$H
  total <- career_filtered$AB
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE)) 
}

m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B",
         lower = c(0,0001, .1))
m
alpha0 <- coef(m)[1]
beta0 <- coef(m)[2]

career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

career_eb %>%
  ggplot(aes(average, eb_estimate)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = alpha0 / (alpha0 + beta0), linetype = "dashed", color = "red")

yankee_1998_career <- career_eb %>%
  mutate(alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H)

yankee_1998_career <- yankee_1998_career %>%
  mutate(low = qbeta(0.025, alpha1, beta1),
         high = qbeta(0.975, alpha1, beta1))


yankee_1998_career %>%
  arrange(desc(eb_estimate)) %>%
  head(7) %>%
  mutate(name = reorder(name, eb_estimate)) %>%
  ggplot(aes(eb_estimate, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  labs(x = "Estimated batting average (w/ 95% interval)",
       y = "Player")






