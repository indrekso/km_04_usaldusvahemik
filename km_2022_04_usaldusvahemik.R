# Kvantitatiivsed meetodid
# Usaldusvahemik

#### Usaldusvahemiku laius ja valimimaht, tunnuse hajuvus ja usaldusnivoo ####

library(haven)
library(tidyverse)
r8 <- read_spss("data/ESS8e02_2.sav")
ee8 <- r8 %>% 
  filter(cntry == "EE") %>% 
  select(lkredcc, region, pspwght)

library(summarytools)
descr(ee8$lkredcc)

ee8$region <- dplyr::recode(as.factor(ee8$region), 
                     "EE001" = "Põhja-Eesti", 
                     "EE004" = "Lääne-Eesti",
                     "EE006" = "Kesk-Eesti",
                     "EE007" = "Kirde-Eesti",
                     "EE008" = "Lõuna-Eesti")
table(ee8$region)

# Arvutame paketi rcompanion abiga hinnangute keskmised ja usalduspiirid regiooniti

# install.packages("rcompanion")
library(rcompanion)

groupwiseMean(lkredcc ~ region, ee8, na.rm = TRUE)

# Uurime usaldusvahemiku laiust ja vaatame, kuidas see on seotud teiste parameetritega

lkredcc_ci95 <- groupwiseMean(lkredcc ~ region, ee8, na.rm = TRUE)
lkredcc_ci95$laius <- lkredcc_ci95$Trad.upper - lkredcc_ci95$Trad.lower
lkredcc_ci95

# Arvutame ka standardhälbe, et uurida hajuvust gruppides

lkredcc_ci95$sd <- ee8 %>%
  group_by(region) %>%
  summarise(sd = sd(lkredcc, na.rm = TRUE)) %>%
  pull(sd)

lkredcc_ci95

# Mida suurem hajuvus, seda laiem usaldusvahemik. Vaatame ka, kui laiad on usaldusvahemikud sõltuvalt usaldusnivoost ja paneme nad joonisele

lkredcc_ci90 <- groupwiseMean(lkredcc ~ region, ee8, na.rm = TRUE, conf = 0.90)
lkredcc_ci99 <- groupwiseMean(lkredcc ~ region, ee8, na.rm = TRUE, conf = 0.99)

library(ggplot2)
ggplot(lkredcc_ci95, aes(x = region, y = Mean)) +
  geom_point(stat = "identity") +
  geom_errorbar(data = lkredcc_ci99, aes(ymin = Trad.lower, ymax = Trad.upper), width = 0.05, color = "blue") +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper), width = 0.1, color = "black") +
  geom_errorbar(data = lkredcc_ci90, aes(ymin = Trad.lower, ymax = Trad.upper), width = 0.15, color = "red")

# Nagu näha, erinevad usaldusnivood annavad mõnevõrra erinevad usaldusvahemikud.


#### Gruppide keskmiste võrdlemine usaldusvahemike abil ####

# Uurime lähemalt usaldusvahemikke usaldusnivool 95%

ggplot(lkredcc_ci95, aes(x = region, y = Mean)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper), width = 0.1, color = "black")

# Arvutame keskmiste vahede usalduspiirid

ee8 %>% 
  filter(region == "Kirde-Eesti" | region == "Kesk-Eesti") %>% 
  t.test(lkredcc ~ region, data = .)

ee8 %>% 
  filter(region == "Kirde-Eesti" | region == "Lääne-Eesti") %>% 
  t.test(lkredcc ~ region, data = .)


#### Usaldusvahemike arvutamine kaaludega ####

# Eelnevad näited olid lihtsuse mõttes tehtud ilma kaaludeta, täpsemate tulemuste saamiseks oleks tarvis ESS-i andmete puhul kasutada ka järelkihistamiskaale. Selleks on tarvis paketi survey abi.

# install.packages("survey")
library(survey)

ee8w <- svydesign(id = ~1, data = ee8, weights = ~pspwght)

# Arvutame tunnuse lkredcc kaalutud keskmise ja usalduspiirid

svymean(~lkredcc, design = ee8w, na.rm = TRUE) %>% 
  confint()

# Arvutame tunnuse lkredcc kaalutud keskmised ja usalduspiirid regioonides

svyby(~lkredcc, ~region, design = ee8w, FUN = svymean, na.rm = TRUE, vartype = c("se", "ci"))

# Teeme sõltumatute kogumite t-testi kaalutud andmetega

# install.packages("weights")
library(weights)

lk_ki <- subset(ee8, region == "Kirde-Eesti")
lk_ke <- subset(ee8, region == "Kesk-Eesti")
lk_le <- subset(ee8, region == "Lõuna-Eesti")

wtd.t.test(lk_ki$lkredcc, lk_ke$lkredcc, weight = lk_ki$pspwght, weighty = lk_ke$pspwght)
wtd.t.test(lk_ki$lkredcc, lk_le$lkredcc, weight = lk_ki$pspwght, weighty = lk_le$pspwght)

# wtd.t.test ei anna kaalutud keskmiste vahe usalduspiire, aga need saab ise arvutada standardvea põhjal

t_test_ki_le <- wtd.t.test(lk_ki$lkredcc, lk_le$lkredcc, weight = lk_ki$pspwght, weighty = lk_le$pspwght)
View(t_test_ki_le)

t_test_ki_le$additional[1] - 1.96 * t_test_ki_le$additional[4] # alumine usalduspiir usaldusnivool 95%
t_test_ki_le$additional[1] + 1.96 * t_test_ki_le$additional[4] # ülemine usalduspiir usaldusnivool 95%

#### Osakaalu usaldusvahemiku laius ####

# Kuidas sõltub osakaalu usaldusvahemik osakaalu (protsentnäitaja) suurusest?
# Näide valimi n = 1000 kohta

p <- seq(0, 1, 0.01)

dat <- data.frame(p)

dat$veapiir <- 1.96 * sqrt(p * (1 - p) / 1000)
View(dat)

joonis <- ggplot(dat, aes(p, veapiir)) +
  geom_path()

library(plotly)
ggplotly(joonis)

#### Osakaalude usaldusvahemike arvutamine ####

# Kasutame kahe erakondade reitingu uuringu andmeid (aastast 2020):
# Turu-uuringute AS, 12.-24. aug, n = 1003, (https://www.err.ee/1127504/turu-uuringud-reformierakonna-toetus-jai-augustis-keskerakonnale-alla)
# Turu-uuringute AS, 7.-17. sept, n = 1010, https://www.err.ee/1138657/reitingud-reformierakond-tousis-taas-populaarseimaks-parteiks). 
# Näide on selles mõttes meie jaoks kehv, et reitingud on ümardatud täisarvuni, aga käsitleme siis neid harjutuse korras kui uuringuga saadud tulemusi.

erakond <- rep(c("RE", "KE", "EKRE", "SDE", "Eesti 200", "Isamaa", "Rohelised", "Tulevik", "Muu"), 2)
reiting <- data.frame(erakond)
reiting$poolehoid <- c(0.23, 0.26, 0.20, 0.10, 0.09, 0.05, 0.02, 0.01, 0.04, 0.27, 0.24, 0.16, 0.11, 0.09, 0.05, 0.03, 0.03, 0.02)
reiting$kuu <- c(rep("August", 9), rep("September", 9))
reiting

# Vaatame tulemusi ka joonisel

ggplot(reiting, aes(x = erakond, y = poolehoid, fill = kuu)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_x_discrete(limits = reiting$erakond[1:9]) +
  labs(x = "Erakond",
       y = "Osakaal") +
  theme_light() 

# Arvutame usalduspiirid ja lisame joonisele

# Uudise pealkiri oli, et RE tõusis taas populaarseimaks parteiks. Kas saame siiski uuringu andmetel väita, et muutus RE ja KE poolehoiuprotsentides esineb ka populatsioonis (eeldades, et täisarvulised protsendid on täpsed)?

# Kasutame allpool usaldusvahemike arvutamiseks funktsiooni MultinomCI, mis eeldab indiviidide arve, mitte protsentjaotust, seega arvutame kõigepealt reitingute põhjal vastajate arvud (st kui palju vastajaid mingit erakonda eelistavad). 

reiting <- reiting %>% 
  mutate(poolehoid_n = ifelse(kuu == "August", poolehoid * 1003, poolehoid * 1010) %>% round())

# Arvutame usalduspiirid, kasutades funktsiooni MultinomCI paketist DescTools. Reitingute usalduspiirid on vaja arvutada augusti ja septembri kohta eraldi, sest eri kuude reitingud moodustavad omaette tervikud (ühe kuu reitingud moodustavad kokku 100%). 

library(DescTools)

poolehoid_aug <- reiting %>% 
  filter(kuu == "August") %>% 
  pull(poolehoid_n) %>% 
  MultinomCI(conf.level = 0.95, method = "wilson")

poolehoid_sept <- reiting %>% 
  filter(kuu == "September") %>% 
  pull(poolehoid_n) %>% 
  MultinomCI(conf.level = 0.95, method = "wilson")

# Paneme reitingute usalduspiirid kokku üheks tabeliks objekti poolehoid_ci.

poolehoid_ci <- rbind(poolehoid_aug, poolehoid_sept) %>% 
  as.data.frame()
poolehoid_ci

# Lisame algsest andmestikust ka erakondade nimed ja kuud.

poolehoid_ci$erakond <- reiting$erakond
poolehoid_ci$kuu <- reiting$kuu
poolehoid_ci

# Paneme reitingud koos usalduspiiridega joonisele.

library(scales)
ggplot(poolehoid_ci, aes(x = erakond, y = est, fill = kuu)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci), position = position_dodge2(width = 0.6, padding = 0.6), color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_x_discrete(limits = poolehoid_ci$erakond[1:9]) +
  labs(x = "Erakond",
       y = "Osakaal") +
  theme_light() 

#### Usaldusvahemike võrdlemine ####

# Usaldusvahemikud kattuvad omajagu. Pelgalt usaldusevahemike kattumise vaatlemisel võiks öelda, et RE reiting võib tõepoolest olla tõusnud, aga võib ka olla, et valimite juhuslikkuse tõttu on reitingud olnud kahel kuul samad või üsnagi sarnased. Sama saaks öelda ka KE kohta. Siiski, sarnaselt keskmiste võrldemisega tuleks siingi võtta arvesse erinevuste usaldusvahemikke (st RE reitingumuutuse usaldusvahemikku ja KE reitingumuutuse usaldusvahemikku).

# Kasutame selleks z-testi kahe osakaalu võrdlemiseks.

n1 <- reiting %>% 
  filter(kuu == "August") %>% 
  pull(poolehoid_n) %>% 
  sum()

n2 <- reiting %>% 
  filter(kuu == "September") %>% 
  pull(poolehoid_n) %>% 
  sum()

reiting %>% 
  filter(erakond == "RE") %>% 
  pull(poolehoid_n) %>% 
  prop.test(c(n1, n2), correct = F)

# Tulemus näitab, et usaldusnivool 95% saab RE reitingumuutust väita küll (usaldusvahemik ei sisalda nulli, kuigi, tõsi küll, ülemine ots ei ole nullist kuigi kaugel). prop.test annab meile küll hii-ruut-statistiku, mitte z. Kontrollime, milline oleks antud juhul z-statistik.

# Uudise pealkiri oli, et RE tõusis taas populaarseimaks parteiks. Kas saame siiski uuringu andmetel öelda, et RE ja KE reiting septembris on populatsioonis erinev (eeldades, et täisarvulised protsendid on täpsed)? Täpsemalt, kas usaldusnivool 95% on alust väita RE ja KE reitingute erinevust? 
# Arvutame reitingute vahe usalduspiirid. Selle jaoks on vaja RE ja KE septembrikuu reitinguid ja kõigi vastajate arvu septembris (viimane on eelnevalt arvutatud objektis n2).

p1 <- reiting %>% 
  filter(erakond == "RE" & kuu == "September") %>% 
  pull(poolehoid)

p2 <- reiting %>% 
  filter(erakond == "KE" & kuu == "September") %>% 
  pull(poolehoid)

# ülemine usalduspiir
p1 - p2 + 1.96 * sqrt((p1 + p2 - (p1 - p2)^2) / n2) 

# alumine usalduspiir
p1 - p2 - 1.96 * sqrt((p1 + p2 - (p1 - p2)^2) / n2) 

# Mida nendest usalduspiiridest järeldada saame? (NB! Andmed on 2020. aasta septembri kohta, paar aastat hiljem on reitingute pilt hoopis teistsugune, praeguste RE ja KE reitingute erinevuse kohta saab teha järelduse ilma usalduspiire arvutamatagi :))


# Harjutusülesanded

# Kas samal ajal kui RE tõusis, saame väita, et KE langes?

# KE ja RE reitingute erinevust 2020 septembris väita ei saanud. Kas 2020 augustis oli RE edumaa siiski statistilisest veast suurem, st sai väita RE kõrgemat reitingut KE-st?
