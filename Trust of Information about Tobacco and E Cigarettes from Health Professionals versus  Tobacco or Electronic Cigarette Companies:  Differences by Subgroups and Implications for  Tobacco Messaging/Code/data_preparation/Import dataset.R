# Build packages
required_pkgs <- c("haven", "dplyr", "survey", "flextable", "officer")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Import dataset
raw <- read_dta("C:/Users/m1810/Desktop/Paper/HINTS/Hints_fda_09052017_public.dta")
