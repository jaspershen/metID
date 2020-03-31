# sxtTools::setwd_project()
# setwd("test_data/ms2_identification_demo_data1/")
# 
# ##use MS1. RT and MS2 for data annotation
# result1 <- metIdentify2(ms1.data = "ms1.peak.table2.csv",
#                         ms2.data = "QC1_MSMS_NCE25.mgf",
#                         database = "msDatabase_rplc0.0.1",
#                         column = "rp")
# 
# ##use MS1 for metabolite annotation
# result2 <- metIdentify2(ms1.data = "ms1.peak.table2.csv",
#                         # ms2.data = "QC1_MSMS_NCE25.mgf",
#                         database = "hmdbMS1Database0.0.1",
#                         column = "rp",
#                         path = ".")
# 
# ##use ms1 and RT for metabolite annotation
# result3 <- metIdentify2(ms1.data = "ms1.peak.table2.csv",
#                         # ms2.data = "QC1_MSMS_NCE25.mgf",
#                         database = "msDatabase_rplc0.0.1",
#                         column = "rp",
#                         path = ".")
# 
# 
# getParams(object = result1)
# getParams(object = result2)
# getParams(object = result3)
# 
# table1 <- getIdentificationTable(result1)
# table1 <- getIdentificationTable(result1, type = "new")
# 
# table2 <- getIdentificationTable(result2)
# table2 <- getIdentificationTable(result2, type = "new")
# 
# table3 <- getIdentificationTable(result3)
# table3 <- getIdentificationTable(result3, type = "new")
# 
# 
# which_has_identification(object = result1)
# which_has_identification(object = result2)
# which_has_identification(object = result3)
# 
# get_iden_info(object = result1, which.peak = "pRPLC_1307", database = msDatabase_rplc0.0.2)
# get_iden_info(object = result2, which.peak = "pRPLC_1307", database = hmdbMS1Database0.0.1)
# get_iden_info(object = result3, which.peak = "pRPLC_1307", database = msDatabase_rplc0.0.2)
# 
# 
# plot <- ms2plot(object = result1, database = msDatabase_rplc0.0.2, which.peak = "pRPLC_1307", interaction.plot = FALSE)
# ms2plot(object = result2, database = hmdbMS1Database0.0.1, which.peak = "pRPLC_1307")
# ms2plot(object = result3, database = msDatabase_rplc0.0.2, which.peak = "pRPLC_1307")
# 
# 
# 
# result1_2 <- filterIden(object = result1, ms1.match.ppm = 5)
# 
# result2_2 <- filterIden(object = result2, ms1.match.ppm = 5)
# 
# get_ms2_spectrum_from_object(peak.name = "pRPLC_1307", object = result3)
# 
# 
