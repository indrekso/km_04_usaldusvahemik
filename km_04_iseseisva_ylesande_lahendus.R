library(haven)
library(tidyverse)
library(meantables)

r8 <- read_spss("data/ESS8e02_2.sav")

ee8_is <- r8 %>% 
  filter(cntry == "EE") %>% 
  select(slvpens, mnactic)

ee8_is <- ee8_is %>% 
  mutate(mnactic = na_if(mnactic, 7),
         mnactic = na_if(mnactic, 9),
         mnactic = recode(as.factor(mnactic),
                          "1" = "Tasustatud tööl",
                          "2" = "Õpib",
                          "3" = "Töötu",
                          "4" = "Töötu",
                          "5" = "Töövõimetu/puudega",
                          "6" = "Pensionil",
                          "8" = "Kodune"))

ee8_is %>% 
  drop_na() %>% 
  group_by(mnactic) %>% 
  mean_table(slvpens) %>% 
  ggplot(aes(group_cat, mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.1) +
    ylim(2.5, 5) +
    coord_flip()

ee8_is %>% 
  filter(mnactic == "Tasustatud tööl" | mnactic == "Pensionil") %>% 
  t.test(slvpens ~ mnactic, data = .)

ee8_is %>% 
  filter(mnactic == "Õpib" | mnactic == "Pensionil") %>% 
  t.test(slvpens ~ mnactic, data = .)

ee8_is %>% 
  filter(mnactic == "Töötu" | mnactic == "Pensionil") %>% 
  t.test(slvpens ~ mnactic, data = .)

ee8_is$mnactic %>% table()

is_w <- svydesign(id = ~1, data = ee8_is, weights = ~dweight)

slvpens_w <- svyby(~slvpens, ~mnactic, design = is_w, FUN = svymean, 
                 na.rm = TRUE, vartype = c("se", "ci"))

ggplot(slvpens_w, aes(x = mnactic, y = slvpens)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.1) +
  ylim(2.5, 5)





ee8_is <- ee8_is %>% 
  mutate(vanus = case_when(agea <= 30 ~ "15-30",
                           agea <= 45 ~ "31-45",
                           agea <= 60 ~ "46-60",
                           agea <= 75 ~ "61-75",
                           agea > 75 ~ "75+"))

ee8_is <- ee8_is %>% 
  mutate(vanus2 = case_when(agea <= 24 ~ "15-24",
                            agea <= 39 ~ "25-39",
                            agea <= 54 ~ "40-54",
                            agea <= 64 ~ "55-64",
                            agea <= 74 ~ "65-74",
                            agea >= 75 ~ "75+"))

ee8_is %>% 
  group_by(vanus) %>% 
  mean_table(slvpens)

ee8_is %>% 
  group_by(agea) %>% 
  summarise(mean = mean(slvpens, na.rm = TRUE),
            n = n()) %>% 
  ggplot(aes(agea, mean, size = n)) +
  geom_point()

ee8_is %>% 
  group_by(vanus2) %>% 
  summarise(mean = mean(slvpens, na.rm = TRUE)) %>% 
  ggplot(aes(vanus, mean, size = n)) +
  geom_point()