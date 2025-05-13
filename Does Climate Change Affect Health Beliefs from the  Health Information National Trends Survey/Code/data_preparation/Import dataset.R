# Build packages
required_pkgs <- c("haven", "dplyr", "survey", "srvyr", "RStata", "flextable", "officer")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
rm(pkg, required_pkgs)

# Set stata path and version
options("RStata.StataPath" =  "D:/Software/Stata17/StataMP-64.exe")
options("RStata.StataVersion" = 17)

# Import dataset
raw <- read_dta("C:/Users/m1810/Desktop/Paper/HINTS/HINTS 6 (2022)/hints6_public.dta")