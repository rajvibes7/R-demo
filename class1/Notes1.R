
library(lubridate)
library(tidyverse)
library(haven)

getwd()
setwd("D:/R training/UpdatedCDISCPilotData-main/UpdatedCDISCPilotData-main/UpdatedCDISCPilotData")

# 1.Multi domain Join 

adsl <- read_xpt("ADAM/ADSL.XPT") |>
  filter(SAFFL == "Y") |>
  select(USUBJID, SAFFL, TRT01A)

vs <- read_xpt("SDTM/VS.XPT") |>
  select(USUBJID, VSTESTCD, VSSTRESN, VISITNUM) 

before <- vs |>
  summarise(
    bfore_cnt = sum(!is.na(USUBJID)))


vs0 <- inner_join(adsl, vs, by = "USUBJID") |>
  filter(VSTESTCD %in% c("SYSBP", "DIABP"))

after <- vs0 |> 
  summarise(
    after_cnt = sum(!is.na(USUBJID))
  )



# 2. Transpose method

advs <- read_xpt("SDTM/VS.XPT") |>
  filter(VSTESTCD == "SYSBP") |>
  group_by(VSPOS, VISITNUM, VISIT) |>
  summarise(
    MEAN_AVAL = mean(VSSTRESN, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(VSPOS, VISITNUM) |>
  pivot_wider(
    names_from  = VISIT,
    values_from = MEAN_AVAL
  )

# 4. Loop over parameters

advs_summary_table <- read_xpt("SDTM/VS.XPT") |>
  filter(VSTESTCD %in% c("SYSBP", "DIABP", "PULSE", "TEMP")) |>
  group_by(VSTESTCD, VSPOS, VISIT) |>
  summarise(
    N    = sum(!is.na(VSSTRESN)),
    MEAN = mean(VSSTRESN, na.rm = TRUE),
    SD   = sd(VSSTRESN, na.rm = TRUE),
    .groups = "drop"
  ) 

# 5. Date derivation

ae <- data.frame(
  TRTSDT  = ymd("2024-01-15"),
  AESTDTC  = ymd("2024-01-20")
)

ae <- ae |>
  mutate(
    # Convert to Date
    TRTSDT  = ymd(TRTSDT),
    AESTDT  = ymd(AESTDTC),
    
    # Duration
    DURATION = as.numeric(AESTDT - TRTSDT),
    
    #  Derive ASTDY 
    ASTDY = case_when(
      !is.na(AESTDT) & !is.na(TRTSDT) & AESTDT >= TRTSDT ~ DURATION + 1,
      !is.na(AESTDT) & !is.na(TRTSDT) & AESTDT < TRTSDT  ~ DURATION,
      TRUE ~ NA_real_
    )
  )

