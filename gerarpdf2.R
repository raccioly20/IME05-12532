xaringan_to_pdf("https://ime05-12547.netlify.app/_site/content/slides04/index.html#1")


remotes::install_github("jhelvy/xaringanBuilder")
library(xaringanBuilder)
setwd("E:/OneDrive/GitHub/IME05-12532/content/slides06")
build_pdf("index.Rmd")
build_pdf("index.html")               
