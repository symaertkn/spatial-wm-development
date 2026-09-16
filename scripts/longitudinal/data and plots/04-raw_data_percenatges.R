library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)

data_dir <- "data"

#logs data by year (2019-2025), loaded and bound together
logs_40 <- do.call(rbind, lapply(2019:2025, function(y) {
  load(file.path(data_dir, paste0("data", y, "_2026_06_02.Rdata")))
  one_year
}))

setDT(logs_40)
logs_40 <- logs_40[grade %between% c(3, 8)]        # group 3-8 only

logs_40[, `:=`(yr = as.integer(substr(created, 1, 4)),
               mo = as.integer(substr(created, 6, 7)))]
logs_40[, syear := fifelse(mo >= 8, yr, yr - 1L)]  # 2019 = school year 2019/20

num_B <- logs_40[syear %between% c(2019, 2024), uniqueN(user_id)]

duo <- fread("duo.csv")
base_B <- duo[TYPE_PO == "BO" & PEILJAAR == 2019 &
                LEERJAAR %between% c(3, 8), sum(AANTAL_LEERLINGEN)]
new_B  <- duo[TYPE_PO == "BO" & PEILJAAR %in% 2020:2024 &
                LEERJAAR == 3, sum(AANTAL_LEERLINGEN)]
den_B  <- base_B + new_B                          

100 * num_B / den_B    #16.00048                           
