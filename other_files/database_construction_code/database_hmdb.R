
###0.0.2
tinyTools::setwd_project()
setwd("all_ms2_database/hmdb/")

load("hmdbDatabase0.0.2")

load("../mike_in_house/msDatabase_hilic0.0.2")
new = msDatabase_hilic0.0.2
new@database.info = hmdbDatabase0.0.2@database.info
new@spectra.info = hmdbDatabase0.0.2@spectra.info
new@spectra.data = hmdbDatabase0.0.2@spectra.data

hmdbDatabase0.0.2 = new

save(hmdbDatabase0.0.2, file = "hmdbDatabase0.0.2", compress = "xz")
