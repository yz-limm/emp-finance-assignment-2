base <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
files <- c("F-F_Research_Data_Factors_CSV.zip", "Portfolios_Formed_on_ME_CSV.zip", "Portfolios_Formed_on_BE-ME_CSV.zip", "25_Portfolios_5x5_CSV.zip")
out <- "latest_french"; dir.create(out, showWarnings=FALSE)
for (f in files) {
  z <- file.path(out,f)
  download.file(paste0(base,f), z, mode="wb")
  unzip(z, exdir=out)
  unlink(z)
}
cat("Saved current files to", normalizePath(out), "\n")
