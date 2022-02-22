xaringan_to_pdf("https://ime05-12547.netlify.app/_site/content/slides03/index.html#1")


remotes::install_github("jhelvy/xaringanBuilder")
library(xaringanBuilder)
build_pdf("index.Rmd")
build_pdf("index.html")               
