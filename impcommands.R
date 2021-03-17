# blogdown::new_site()

devtools::install_github("hadley/emo") ## Emotes https://github.com/hadley/emo

blogdown::hugo_version()

# remotes::install_github('rstudio/blogdown')

blogdown::new_post("Listar FIIs como um supermercado", ext = '.Rmd')


blogdown::build_site()
blogdown::serve_site()
# blogdown::stop_server()


rmarkdown::render("2021-03-15-listar-fiis.Rmd",encoding="UTF-8")
