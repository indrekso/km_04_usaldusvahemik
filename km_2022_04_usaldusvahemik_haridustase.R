# Kvantitatiivsed meetodid
# Usaldusvahemik

# Harjutus 1

# Kaasame andmestikku lisaks vajalikud tunnused

ee8a <- r8 %>% 
  filter(cntry == "EE") %>% 
  select(gvsrdcc, edulvlb, pspwght)

# Kodeerime haridustaseme kategooriad ümber

ee8a <- ee8a %>% 
  mutate(har = case_when(edulvlb <= 213 ~ "Kuni põhiharidus",
                         edulvlb > 213 & edulvlb < 600 ~ "Kutseharidus",
                         edulvlb == 313 ~ "Keskharidus",
                         edulvlb >= 600 ~ "Kõrgharidus"))

# Kontrollime, kas sai õigesti

library(summarytools)
ctable(ee8a$edulvlb, ee8a$har, prop = "n")

# Edasi proovige kõigepealt ise :)