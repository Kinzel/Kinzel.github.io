##////////////////////////////
# Feito por Guilherme Kinzel
# Rio de Janeiro, 27 Março de 2021
#
# Código para coletar todos os FIIs disponíveis e indicadores usando mt5R e webscrapping
# Demora cerca de 15 minutos para rodar completamente

#////////////////////////////////////////////////////////////////
## Pacotes  ----

library(mt5R)
library(stringr) 
library(dplyr)

#////////////////////////////////////////////////////////////////
## Plataforma Funcionando?  ----

MT5.Ping()

#////////////////////////////////////////////////////////////////
## Todos os tickers  ----

## Pegaremos o Ticker de todos os ativos negociaveis na B3
sTodosTickers = MT5.AllSymbols() 

## Queremos somente aqueles que possuam '11' no nome 
sTodosTickers = sTodosTickers[str_detect(sTodosTickers, "11")] 

## Podem vir opções junto, tiramos pelo tamanho do nome
sTodosTickers = sTodosTickers[str_length(sTodosTickers) >= 5 & str_length(sTodosTickers) <= 6] 


head(sTodosTickers)

length(sTodosTickers)

#////////////////////////////////////////////////////////////////
## Filtrar FIIs  ----

## Criaremos um vetor para armazenar se é realmente FII
bQuaisSaoFII = rep(TRUE, length.out = length(sTodosTickers))

for(i in 1:length(sTodosTickers))
{
  ## Iremos de nome em nome
  sAtivo = sTodosTickers[i]
  
  scrape_url = paste0("https://statusinvest.com.br/fundos-imobiliarios/", str_to_lower(sAtivo))
  flat_html = base::readLines(con = scrape_url, encoding="UTF-8")
  
  ## Verificar se realmente é FII
  if(any(str_detect(flat_html, "o encontramos o que vo") == T))
  {
    ## Se o statusinvest.com.br informar que não encontrou o ticker
    bQuaisSaoFII[i] = FALSE
  }else
  {
    ## Caso contrário, é FII!
    bQuaisSaoFII[i] = TRUE
  }
}

## Filtrar aqueles que verdadeiramente são FIIs
sTodosOsFIIs = sTodosTickers[bQuaisSaoFII]

head(sTodosOsFIIs)

length(sTodosOsFIIs)

#////////////////////////////////////////////////////////////////
## Webscrapping  ----

PegarTextoNegrito = function(flat_html, sQualIndicador, iQualNaSequencia = 1)
{
  # flat_html: é a página obtida pelo base::readLines
  # sQualIndicador: qual indicador estamos atrás
  # iQualNaSequencia: o indicador pode aparecer diversas vezes na pagina. Escolha em qual você está interessado.
  
  iOnde_AproxIndicador = which(str_detect(flat_html, regex(sQualIndicador, ignore_case = T)))
  iOnde_EstaoNegritos = which(str_detect(flat_html, "</strong>"))
  
  sTexto = flat_html[iOnde_EstaoNegritos[iOnde_EstaoNegritos > iOnde_AproxIndicador[iQualNaSequencia]][1]]
  
  iMarcadores = str_locate_all(sTexto, ">")[[1]][,1]
  iOndeTermina = str_locate_all(sTexto, "</strong>")[[1]][1]
  iOndeComeca = tail(subset(iMarcadores, iMarcadores < iOndeTermina),1) 
  
  sTexto_Quero = str_sub(sTexto, iOndeComeca + 1, iOndeTermina - 1)
  if(length(sTexto_Quero)<1)sTexto_Quero = NA; ## não achou
  return(sTexto_Quero)
}

PegarSetor = function(flat_html)
{
  iOnde_AproxIndicador = which(str_detect(flat_html, regex(">Segmento</span>", ignore_case = T)))[1]
  iOnde_BarraInversa = which(str_detect(flat_html, '\"'))
  
  sTexto = flat_html[iOnde_BarraInversa[iOnde_BarraInversa > iOnde_AproxIndicador][1]]
  
  iOnde_BarraDir = str_locate_all(sTexto, "/")[[1]]
  
  ## Procura por aspas
  iOndeTermina = str_locate_all(sTexto, '"')[[1]][2]
  iOnde_Comeca = tail(iOnde_BarraDir[iOnde_BarraDir < iOndeTermina],1)
  
  sTexto_Quero = str_sub(sTexto, iOnde_Comeca + 1, iOndeTermina - 1)
  return(sTexto_Quero)
}

scrape_url = "https://statusinvest.com.br/fundos-imobiliarios/xpml11"
flat_html = base::readLines(con = scrape_url, encoding="UTF-8")

PVP = PegarTextoNegrito(flat_html, "P/VP")
print(PVP)

Setor = PegarSetor(flat_html)
print(Setor)

#////////////////////////////////////////////////////////////////
## Coleta indicadores 1  ----

## data frame em que iremos colocar todas as informacoes
df_todos_fiis = data.frame()

for(i in 1:length(sTodosOsFIIs))
{
  ## Iremos de ticker em ticker
  sFII = sTodosOsFIIs[i]
  
  scrape_url = paste0("https://statusinvest.com.br/fundos-imobiliarios/", str_to_lower(sFII))
  flat_html = base::readLines(con = scrape_url, encoding="UTF-8")
  
  df_fii = data.frame(Ticker = sFII,
                      Nome = NA,
                      Setor = NA,
                      Adm = NA,
                      Cotacao = NA,
                      MM200Delta = NA,
                      Vol = NA,
                      DY12m = NA,
                      P_VP = NA,
                      VALOR_EM_CAIXA = NA)
  
  ### Nome
  # Deixaremos para outro bloco usando o site clubefii
  
  ### Setor
  # Remover "-" caso tenha na string
  # Colocar primeira letra maiúscula
  # Remove espaços no inicio e no final, se atribuível
  
  sSetor = PegarSetor(flat_html) %>% str_replace_all(., "-", " ") %>% str_to_title(.) %>% str_trim(.)
  df_fii$Setor = sSetor
  
  ### Administrador
  # Usar somente as duas primeiras palavras
  
  sAdm = PegarTextoNegrito(flat_html, "ADMINISTRADOR") %>% word(., 1,2)
  df_fii$Adm = sAdm
  
  ### Cotacao
  ### MM200Delta
  ### Vol
  
  # Deixaremos para o próximo bloco, onde voltaremos a usar o MT5R. 
  
  ### DY12m
  # Trocar virgula por ponto para o R entender que é um numérico
  # Transformar em numerico
  
  fDY12 = PegarTextoNegrito(flat_html, "DIVIDEND YIELD") %>% str_replace_all(., "\\,", "\\.") %>% as.numeric(.)
  df_fii$DY12m = fDY12
  
  ### P/VP
  # Trocar virgula por ponto para o R entender que é um numérico
  # Transformar em numerico
  
  fPVP = PegarTextoNegrito(flat_html, "P/VP") %>% str_replace_all(., "\\,", "\\.") %>% as.numeric(.)
  df_fii$P_VP = fPVP
  
  ### Valor em caixa
  # Trocar virgula por ponto para o R entender que é um numérico
  # Transformar em numerico
  
  fValorEmCaixa = PegarTextoNegrito(flat_html, "VALOR EM CAIXA") %>% str_replace_all(., "\\,", "\\.") %>% as.numeric(.)
  df_fii$VALOR_EM_CAIXA = fValorEmCaixa
  
  ## Adiciona no data.frame final
  df_todos_fiis = rbind(df_todos_fiis, df_fii)
}

head(df_todos_fiis)

#////////////////////////////////////////////////////////////////
## Coleta indicadores 2  ----

## Como já temos o df_todos_fiis com todos os tickers, nós agora vamos fazer o looping em cima deste data.frame

for(i in 1:dim(df_todos_fiis)[1])
{
  sTicker = df_todos_fiis$Ticker[i]
  
  ## Verificamos se ele está no Marketwatch do MT5
  bEstaMW = MT5.SymbolInMarketwatch(sTicker)
  
  if(bEstaMW == FALSE)
  {
    MT5.MarketwatchAdd(sTicker)
    Sys.sleep(0.2) ##Esperamos o MT5 baixar os dados
  }
  
  ## 1440 é o diario e queremos 200 rows para fazer a média móvel de 200
  Cotacao = MT5.GetSymbol(sTicker, iTF = 1440, iRows = 200)
  
  ## Alguns FIIs podem nao ter nenhuma cotacao, como algum muito recente
  if(dim(Cotacao)[1]>0)
  {
    ## Cotacao
    df_todos_fiis$Cotacao[i] = tail(Cotacao$Close, 1)
    
    ## Cotacao atual sobre a média móvel de 200 períodos, caso tenha 200 linhas
    df_todos_fiis$MM200Delta[i] = ifelse(length(Cotacao$Close)<200, NA, tail(Cotacao$Close, 1) / mean(Cotacao$Close))
    
    ## Volume medio em 200 dias, em milhoes
    df_todos_fiis$Vol[i] = mean(Cotacao$Close * Cotacao$Volume) / 1000000
  }
  
  if(bEstaMW == FALSE)
  {
    ## Se nao estava no Marketwatch, remove
    MT5.MarketwatchRemove(sTicker)
  }
}

head(df_todos_fiis)

#////////////////////////////////////////////////////////////////
## Coletar nome  ----

PegarNome = function(sTickerFII)
{
  pagina_url = paste0("https://www.clubefii.com.br/fiis/", str_to_lower(sTickerFII))
  pagina_flat = base::readLines(con = pagina_url, encoding="UTF-8", warn=F)
  
  iOnde_AproxIndicador = which(str_detect(pagina_flat, regex(sTickerFII, ignore_case = T)))
  
  ##Verifiquei que é na 4a posicao
  sTexto = pagina_flat[iOnde_AproxIndicador[4]]
  
  ## Procura por aspas
  iOndeEstao_simbolo_menos = str_locate_all(sTexto, '-')[[1]][,1]
  sNomeFII = str_sub(sTexto, iOndeEstao_simbolo_menos[1] + 2, iOndeEstao_simbolo_menos[2] - 2)
  return(sNomeFII)
}

for(i in 1:dim(df_todos_fiis)[1])
{
  sTicker = df_todos_fiis$Ticker[i]
  
  ### Nome
  df_todos_fiis$Nome[i] = PegarNome(sTicker)
}


#////////////////////////////////////////////////////////////////
## Demais indicadores extras  ----
df_todos_fiis = df_todos_fiis[!is.na(df_todos_fiis$Cotacao),]

df_todos_fiis$MM200Delta = scale(df_todos_fiis$MM200Delta, center = 0) * 100

df_todos_fiis$PB_Anos = round(df_todos_fiis$Cotacao / (df_todos_fiis$DY12m / 100 * df_todos_fiis$Cotacao), 1)

head(df_todos_fiis)

#////////////////////////////////////////////////////////////////
## SALVAR  ----

write.csv2(df_todos_fiis, file = "tabela_fiis.csv", row.names = FALSE)

print(getwd())

#////////////////////////////////////////////////////////////////
## Analise de dados  ----

median(df_todos_fiis$DY12m, na.rm=T)

df_todos_fiis[which.max(df_todos_fiis$VALOR_EM_CAIXA), ]

df_todos_fiis[which.max(df_todos_fiis$MM200Delta), ]

Tabela_Dinamica = df_todos_fiis %>% 
  dplyr::group_by(Setor) %>% 
  dplyr::summarise(.groups = 'drop',
                   n = n(),
                   DY12m_median = median(DY12m, na.rm=TRUE),
                   MM200Delta_median = median(MM200Delta, na.rm=TRUE)
  ) %>% dplyr::filter(n >= 2) %>% dplyr::arrange(desc(MM200Delta_median)) %>% print(n = Inf) 

library(ggplot2)
g = ggplot(Tabela_Dinamica, aes(x=DY12m_median, y=MM200Delta_median)) +
  geom_point(size=2, shape=1) + 
  geom_text(label=Tabela_Dinamica$Setor, hjust=0.45, vjust=1.5) +
  xlab('Dividend Yield') +
  ylab('MM200Delta') +
  geom_hline(yintercept=100, linetype="dashed",color = "black", size=0.5, alpha = .4) +
  geom_vline(xintercept=5, linetype="dashed",color = "black", size=0.5, alpha = .4) +
  coord_cartesian(xlim = c(0, 11))
print(g)