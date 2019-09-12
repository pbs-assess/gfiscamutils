## Custom class types
model.class <- "model"
model.lst.class <- "model.list"

## RData file name for model storage
rdata.file <- "aaa_gfiscam.RData"

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
output.files <- c(rep.file,
                  par.file,
                  "admb_runtime.log",
                  "iscam.b*",
                  "iscam.p*",
                  "iscam.r*",
                  "iscam.log",
                  "iscam.ecm",
                  "iscam.hst",
                  "iscam.mc*",
                  "*.cov",
                  "*.cor",
                  "*.dep",
                  "*.eva",
                  "*.hes",
                  "*\\.re*",
                  "*.std",
                  "*.psv",
                  "*.RData",
                  "variance",
                  "eigv.rpt",
                  "fmin.log",
                  "mcmc",
                  "sims")

## iscam program filenames
iscam.exe.file <- "iscam.exe"
iscam.starter.file <- "iscam.dat"
retro.dir <- "retrospectives"

## Model run command line outputs
log.file <-  "runoutput.log"
.dos.pipe.stdout <- "1>"
.dos.pipe.stderr <- "2>&1"
.dos.append.stdout <- "1>>"
.dos.append.stderr <- "2>>&1"
.linux.append.stderr <- "2>&1"
linux.pipe.to.log <- paste(.dos.pipe.stdout, log.file, .dos.pipe.stderr)
linux.append.to.log <- paste(.dos.append.stdout, log.file, .linux.append.stderr)
dos.pipe.to.log <- paste(.dos.pipe.stdout, log.file, .dos.pipe.stderr)
dos.append.to.log <- paste(.dos.append.stdout, log.file, .dos.append.stderr)

use_data(model.class, overwrite = TRUE)
use_data(model.lst.class, overwrite = TRUE)
use_data(rdata.file, overwrite = TRUE)
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
use_data(output.files, overwrite = TRUE)
use_data(retro.dir, overwrite = TRUE)
use_data(iscam.exe.file, overwrite = TRUE)
use_data(iscam.starter.file, overwrite = TRUE)
use_data(retro.dir, overwrite = TRUE)
use_data(output.files, overwrite = TRUE)
use_data(log.file, overwrite = TRUE)
use_data(linux.pipe.to.log, overwrite = TRUE)
use_data(linux.append.to.log, overwrite = TRUE)
use_data(dos.pipe.to.log, overwrite = TRUE)
use_data(dos.append.to.log, overwrite = TRUE)
#use_data(, overwrite = TRUE)
