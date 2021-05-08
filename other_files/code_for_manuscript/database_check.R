##no
no_function()
rm(list=ls())

tinyTools::setwd_project()

####summary_database
summary_database =
  function(object){
    metabolite_number =
      length(object@spectra.info$Compound.name)
    spectra_number_pos =
      sum(unlist(lapply(object@spectra.data$Spectra.positive, length)))
    spectra_number_neg =
      sum(unlist(lapply(object@spectra.data$Spectra.negative, length)))
    spectra_number = spectra_number_pos + spectra_number_neg
    c(metabolite_number = metabolite_number,
      spectra_number = spectra_number)
  }

load("all_ms2_database/mike_in_house/msDatabase_rplc0.0.2")
load("all_ms2_database/mike_in_house/msDatabase_hilic0.0.2")

summary_database(msDatabase_rplc0.0.2)
summary_database(msDatabase_hilic0.0.2)

load("all_ms2_database/hmdb/hmdbDatabase0.0.2")
load("all_ms2_database/massbank/massbankDatabase0.0.2")
load("all_ms2_database/mona/monaDatabase0.0.2")
load("all_ms2_database/orbitrap/orbitrapDatabase0.0.1")
load("all_ms2_database/Fiehn/fiehn_hilic_database0.0.1")

summary_database(hmdbDatabase0.0.2)[1] +
summary_database(massbankDatabase0.0.2)[1] +
summary_database(monaDatabase0.0.2)[1] +
summary_database(orbitrapDatabase0.0.1)[1] +
summary_database(fiehn_hilic_database0.0.1)[1]

summary_database(hmdbDatabase0.0.2)[2] +
  summary_database(massbankDatabase0.0.2)[2] +
  summary_database(monaDatabase0.0.2)[2] +
  summary_database(orbitrapDatabase0.0.1)[2] +
  summary_database(fiehn_hilic_database0.0.1)[2]

load("all_ms2_database/hmdb/hmdbMS1Database0.0.1")
summary_database(hmdbMS1Database0.0.1)

load("all_ms2_database/kegg/keggMS1Database_1.0")
summary_database(keggMS1Database_1.0)

load("all_ms2_database/drugbank/drugbankMS1Database5.1.8")
summary_database(drugbankMS1Database5.1.8)

load("all_ms2_database/foodb/")
summary_database(drugbankMS1Database5.1.8)

load("all_ms2_database/t3db/t3dbMS1Database_1.0")
summary_database(t3dbMS1Database_1.0)

load("all_ms2_database/blood_exposome/bloodExposomeMS1Database_1.0")
summary_database(bloodExposomeMS1Database_1.0)
