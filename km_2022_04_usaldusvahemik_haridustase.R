# Kvantitatiivsed meetodid
# Usaldusvahemik

# Harjutus 1
# Vastajatelt küsiti ka, kui tõenäoline on, et piisaval hulgal valitsusi rakendab meetmeid kliimamuutuse vähendamiseks (tunnus gvsrdcc)
#  Millised on selle arvamuse keskmise punkthinnangud ja vahemikhinnangud haridustasemeti?
#  Mida saab väita nende usaldusvahemike võrdlemise põhjal?
#  Milliste gruppide vahel saab populatsiooni tasandil väita erinevuste esinemist usaldusnivool 95%?
#  Tehke arvutused lihtsuse mõttes kaalumata andmetega

# Kaasame analüüsi aluseks olevasse andmestikku vajalikud tunnused.

ee8a <- r8 %>% 
  filter(cntry == "EE") %>% 
  select(gvsrdcc, edulvlb, pspwght)

# Kodeerime haridustaseme kategooriad ümber

ee8a <- ee8a %>% 
  mutate(har = case_when(edulvlb <= 213 ~ "Kuni põhiharidus",
                         edulvlb == 313 ~ "Keskharidus",
                         edulvlb > 213 & edulvlb < 600 ~ "Kutseharidus",
                         edulvlb >= 600 ~ "Kõrgharidus"))

# Kontrollime, kas sai õigesti

library(summarytools)
table(ee8a$edulvlb, ee8a$har)

# Edasi proovige kõigepealt ise :)

# Edasine lahendus:

# Muudame haridustasemete järjekorda, et haridustasemeid ei esitataks joonisel tähestikulises, vaid sisulises järjekorras. Kasutame selleks funktsiooni fct_relevel paketist forcats.

library(forcats)
ee8a$har <- fct_relevel(ee8a$har, "Kuni põhiharidus", "Kutseharidus", "Keskharidus", "Kõrgharidus")

# Eemaldame andmelünkadega indiviidid, et joonisel ei kuvataks eraldi kategooriana indiviide, kellel on haridustaseme tunnuses andmelünk (selliseid indiviide oli niikuinii ainult üks). Sellega kaasneb ka asjaolu, et funktsiooni groupwiseMean puhul pole vaja eraldi täpsustada na.rm = TRUE.

ee8a <- na.omit(ee8a)

# Arvutame keskmised ja nende usalduspiirid haridustasemeti.

usvah <- groupwiseMean(gvsrdcc ~ har, ee8a)

# Esitame keskmised ja usalduspiirid joonisel.

ggplot(usvah, aes(x = har, y = Mean)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper), width = 0.1, color = "black")

# Kontrollime, kas põhi- või madalama haridusega vastajate hinnangud keskmiselt erinevad kutseharidusega vastajate hinnangutest.

ee8a %>% 
  filter(har == "Kuni põhiharidus" | har == "Kutseharidus") %>% 
  t.test(gvsrdcc ~ har, data = .)

# Usaldusnivool 95% ei kata keskmiste vahe usaldusvahemik nulli, seega põhimõtteliselt saame järeldada keskmiste erinevust populatsioonis, samas on alumine usalduspiir väga lähedal nullile, nii et põhjapanevaid järeldusi ei maksaks teha.

# Kontrollime keskmiste hinnangute erinevust ka teiste haridustasemete puhul, kus usaldusvahemikud kattuvad pigem vähesel määral.

ee8a %>% 
  filter(har == "Kuni põhiharidus" | har == "Keskharidus") %>% 
  t.test(gvsrdcc ~ har, data = .)

ee8a %>% 
  filter(har == "Keskharidus" | har == "Kõrgharidus") %>% 
  t.test(gvsrdcc ~ har, data = .)

# Nende gruppide puhul saab samuti keskmiste erinevust väita, usaldusvahemik on siin ka nullist natuke kaugemal.







