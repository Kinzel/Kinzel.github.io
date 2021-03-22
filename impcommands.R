# blogdown::new_site()

devtools::install_github("hadley/emo") ## Emotes https://github.com/hadley/emo

blogdown::hugo_version()

# remotes::install_github('rstudio/blogdown')

blogdown::new_post("Teste 3", ext = '.Rmd')


blogdown::build_site()
blogdown::serve_site()
# blogdown::stop_server()

blogdown::hugo_cmd("--cleanDestinationDir")

rmarkdown::render("2021-03-15-listar-fiis.Rmd",encoding="UTF-8")


# git init
# git add .
# git commit -m "first commit"
# git branch -M master
# git remote add origin https://github.com/Kinzel/Kinzel.github.io.git
# git push -u origin master

## Apenas para mandar pro GitHub
# git init
# git add .
# git commit -m "first commit"
# git push -u origin master

## Como fiz isso rodar
# Você precisa fazer o "gh-pages.yml" como está já salvo aqui no .github > workflow
# e criar o .gitmodules com os detalhes já inseridos nele na pasta principal