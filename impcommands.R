# blogdown::new_site()

devtools::install_github("hadley/emo") ## Emotes https://github.com/hadley/emo

blogdown::hugo_version()

# remotes::install_github('rstudio/blogdown')

blogdown::new_post("1_Listando_FII", ext = '.Rmd')


blogdown::build_site()
blogdown::serve_site()
# blogdown::stop_server()

blogdown::hugo_cmd("--cleanDestinationDir")

## Para criar paginas, colocar o nome como "index", deletar o public/ e usar blogdown::build_site()

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