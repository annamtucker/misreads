# load all packages needed for analysis and generating figures

pkgs = c("tidyverse", "cowplot", "marked", "RMark", "TMB", "RColorBrewer",
         "viridis", "bblme", "emdbook", "lubridate")

check <- sapply(pkgs, require,
                warn.conflicts = TRUE,
                character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}
