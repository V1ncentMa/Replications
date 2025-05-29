

#### Build packages --------------------------------------------------

# Package list
required_pkgs <- c("haven", "dplyr", "survey", "srvyr", "officer", "flextable", "broom")

# Check and install packages
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
rm(pkg, required_pkgs)




#### Import dataset --------------------------------------------------

hints_pooled <- read_dta("hints_pooled.dta")

