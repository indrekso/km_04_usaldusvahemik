# Usaldusvahemike iseseisva ülesande lahendus

ee8w <- svydesign(id = ~1, data = ee8a, weights = ~pspwght)

# Arvutame tunnuse lkredcc kaalutud keskmised ja usalduspiirid regioonides

usvah <- svyby(~gvsrdcc, ~har, design = ee8w, FUN = svymean, na.rm = TRUE, vartype = c("se", "ci"))

ggplot(usvah, aes(x = har, y = gvsrdcc)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.1, color = "black")

pohi <- filter(ee8a, har == "Kuni põhiharidus")
kutse <- filter(ee8a, har == "Kutseharidus")
kesk <- filter(ee8a, har == "Keskharidus")
korg <- filter(ee8a, har == "Kõrgharidus")

wtd.t.test(pohi$gvsrdcc, kesk$gvsrdcc, weight = pohi$pspwght, weighty = kesk$pspwght)

# wtd.t.test ei anna kaalutud keskmiste vahe usalduspiire, aga need saab ise arvutada standardvea põhjal

t_test <- wtd.t.test(pohi$gvsrdcc, kutse$gvsrdcc, weight = pohi$pspwght, weighty = kutse$pspwght)
View(t_test)

t_test$additional[1] - 1.96 * t_test$additional[4] # alumine usalduspiir usaldusnivool 95%
t_test$additional[1] + 1.96 * t_test$additional[4] # ülemine usalduspiir usaldusnivool 95%