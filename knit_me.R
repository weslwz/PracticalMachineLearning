library(knitr)
library(markdown)
knit("write_up.Rmd")
markdownToHTML("write_up.md", "write_up.html")
