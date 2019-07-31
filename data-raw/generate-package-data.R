## Custom class types
model.class <- "model"
model.lst.class <- "model.list"

## iscam files with names that don't change depending on model
rep.file <- "iscam.rep"
par.file <- "iscam.par"
mcmc.file <- "iscam_mcmc.csv"
mcmc.biomass.file <- "iscam_sbt_mcmc.csv"
mcmc.recr.file <- "iscam_rt_mcmc.csv"
mcmc.recr.devs.file <- "iscam_rdev_mcmc.csv"
mcmc.fishing.mort.file <- "iscam_ft_mcmc.csv"
mcmc.natural.mort.file <- "iscam_m_mcmc.csv"
mcmc.fishing.mort.u.file <- "iscam_ut_mcmc.csv"
mcmc.vuln.biomass.file <- "iscam_vbt_mcmc.csv"
mcmc.proj.file <- "iscammcmc_proj_Gear1.csv"
mpd.proj.file <- "iscammpd_proj_Gear1.csv"

## iscam program filenames
iscam.exe.file <- "iscam.exe"
iscam.starter.file <- "iscam.dat"

use_data(model.class, overwrite = TRUE)
use_data(model.lst.class, overwrite = TRUE)
use_data(rep.file, overwrite = TRUE)
use_data(par.file, overwrite = TRUE)
use_data(mcmc.file, overwrite = TRUE)
use_data(mcmc.biomass.file, overwrite = TRUE)
use_data(mcmc.recr.file, overwrite = TRUE)
use_data(mcmc.recr.devs.file, overwrite = TRUE)
use_data(mcmc.fishing.mort.file, overwrite = TRUE)
use_data(mcmc.natural.mort.file, overwrite = TRUE)
use_data(mcmc.fishing.mort.u.file, overwrite = TRUE)
use_data(mcmc.vuln.biomass.file, overwrite = TRUE)
use_data(mcmc.proj.file, overwrite = TRUE)
use_data(mpd.proj.file, overwrite = TRUE)
use_data(iscam.exe.file, overwrite = TRUE)
use_data(iscam.starter.file, overwrite = TRUE)
#use_data(, overwrite = TRUE)
