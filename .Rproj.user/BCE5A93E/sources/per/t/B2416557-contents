library(shiny)
library(rgdal)
library(DT)
library('dygraphs')
library('xts')
library(leaflet)
library(tidyverse)
library(xts)
library(plotly)
library(readxl)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(shinydashboard)
library(ggrepel)
library(sf)
library(ps)
library(shinydashboardPlus)
library(shinyEffects)

############################################

mapa_ma_shp <- sf::st_read("21MUE250GC_SIR.shp", quiet = TRUE)

dplyr::glimpse(mapa_ma_shp)
mapa_ma_shp <- mapa_ma_shp %>% 
  mutate(municipio = str_to_lower(NM_MUNICIP)) # todas as cidades com letra minuscula

banco_luzivan <- read_excel("banco_modificado.xlsx")

banco_luzivan = banco_luzivan %>%   
  separate("Município", into = c("Codigo", "Cidade"), sep = 7)

banco <- banco_luzivan %>% 
  mutate(municipio = str_to_lower(Cidade))
banco$municipio = factor(banco$municipio)


banco_completo <- merge(banco, mapa_ma_shp, by.x = "municipio", by.y = "municipio") # une os dois bancos dado as cidades
dplyr::glimpse(banco_completo) 
banco_completo = st_as_sf(banco_completo)

banco_tabela=banco_luzivan
names(banco_tabela) = c("Codigo", "Municipio","Espinha bifida (Q05)",  "Outras malformacoes congenitas (Q80-Q89)",
                        "Aparelho circulatorio (Q20-Q28)", "Fenda labial e palatina (Q35-Q37)", 
                        "Ausencia atresia e estenose do intestino delgado (Q41)", "Aparelho digestivo (Q38-Q45)",
                        "Testiculo nao descido (Q53)", "Aparelho urinario (Q60-Q64)", 
                        "Deformidades congenitas do quadril (Q65)",
                        "Deformidades congenitas dos pes (Q66)", "Aparelho osteomuscular (Q65-Q79)",
                        "Sistema nervoso (Q00-Q07)", "Anomalias cromossomicas (Q90-Q99)",
                        "Hemangioma e linfangioma de qualquer localizacao (D18)", "Sem anomalia", 
                        "k")

banco_tabela2 = banco_tabela %>%
  gather(key = "tipo de anomalia", value = "n", 3:17) %>%
  mutate(prevalencia = (n/k)*10000)

names(banco_tabela2) = c("Código", "Municipio", "numero de nascidos vivos",
                         "tipo de anomalia", "numero de nascidos vivos com a anamalia", 
                         "prevalencia") 

banco_completo_trasformado <- st_transform(banco_completo, "+init=epsg:4326")

#########################################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Análise de nasc. vivos por tipo de anomalias congenitas no MA",
                  titleWidth = 600),
  dashboardSidebar
  (sidebarMenu(
    menuItem("Mapa da prevalencia", tabName = "mapa_porporcao") ,
    menuItem("Mapa nasc. vivos por tipo  de anomalia", tabName = "mapa_n_casos"),
    menuItem("Tabela com dados das prevalencias", tabName = "tabela"),
    menuItem("Sobre", tabName = "sobre")
  )
  ),
  
  dashboardBody(
    tabItems(
      #    
      tabItem("tabela",
              fluidPage(
                titlePanel("Tabela com dados da prevalencia por 10000 e numero de nascidos vivos por tipo de anomalia em municipios do MA"),
                
                
                fluidRow(
                  column(
                    width = 6,
                    tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                    tags$img(src="logo_ime.png", height = 100, width = 270),
                    tags$img(src="ppg_genetica.png", height = 80, width = 95)
                  )
                  
                ),
                
                mainPanel(
                  DTOutput(outputId = "tabela")
                )
              )
      ),
      #   
      tabItem("mapa_porporcao",
              fluidPage(
                titlePanel("Mapa das prevalencias por 10000 de diferentes tipos de anomalias congenitas no estado do Maranhao"),
                
                fluidRow(column(
                  6,
                  selectInput(
                    inputId = "y", 
                    label = "Escolha o tipo de anomalia a ser indicado no mapa",
                    choices = c("Hemangioma e linfangioma de qualquer localizacao (D18)", "Sistema nervoso (Q00-Q07)", "Espinha bifida (Q05)", 
                                "Aparelho circulatorio (Q20-Q28)", 
                                "Fenda labial e palatina (Q35-Q37)", "Aparelho digestivo (Q38-Q45)", "Ausencia atresia e estenose do intestino delgado (Q41)", 
                                "Testiculo nao descido (Q53)", "Aparelho urinario (Q60-Q64)", 
                                "Deformidades congenitas do quadril (Q65)", "Aparelho osteomuscular (Q65-Q79)", "Deformidades congenitas dos pes (Q66)", ##perguntar
                                "Outras malformacoes congenitas (Q80-Q89)", 
                                "Anomalias cromossomicas (Q90-Q99)"),
                    selected = "Hemangioma e linfangioma de qualquer localizacao (D18)"
                  )),
                  column(
                    width = 6,
                    tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                    tags$img(src="logo_ime.png", height = 100, width = 270),
                    tags$img(src="ppg_genetica.png", height = 80, width = 95)
                  )
                  
                ),
                
                mainPanel(
                  leafletOutput("grafico_mapa_proporcao", height = "600px"),
                  width = 12
                )
              )
      ),
      
      tabItem("mapa_n_casos",
              fluidPage(
                titlePanel("Mapa do numero de nascidos vivos por tipo de anomalia congenita no estado do Maranhao"),
                
                fluidRow(column(
                  6,
                  selectInput(
                    inputId = "x", 
                    label = "Escolha o tipo de anomalia a ser indicado no mapa",
                    choices = c("Hemangioma e linfangioma de qualquer localizacao (D18)", "Sistema nervoso (Q00-Q07)", "Espinha bifida (Q05)", 
                                "Aparelho circulatorio (Q20-Q28)", 
                                "Fenda labial e palatina (Q35-Q37)", "Aparelho digestivo (Q38-Q45)", "Ausencia atresia e estenose do intestino delgado (Q41)", 
                                "Testiculo nao descido (Q53)", "Aparelho urinario (Q60-Q64)", 
                                "Deformidades congenitas do quadril (Q65)", "Aparelho osteomuscular (Q65-Q79)", "Deformidades congenitas dos pes (Q66)", ##perguntar
                                "Outras malformacoes congenitas (Q80-Q89)", 
                                "Anomalias cromossomicas (Q90-Q99)"),
                    selected = "Hemangioma e linfangioma de qualquer localizacao (D18)"
                  )),
                  column(
                    width = 6,
                    tags$img(src="ufrgs_logo.png", height = 100, width = 127),
                    tags$img(src="logo_ime.png", height = 100, width = 270),
                    tags$img(src="ppg_genetica.png", height = 80, width = 95)
                  )
                  
                ),
                
                mainPanel(
                  leafletOutput("grafico_mapa_casos", height = "600px"),
                  width = 12
                )
              )
      ),
      tabItem("sobre",
              fluidPage(
                fluidRow(
                  
                  # setZoom(id = "covidMetrika",class = "small-box"),
                  #  setZoom(id = "git_covidMetrika",class = "small-box"),
                  
                  column(
                    width = 12,
                    valueBoxOutput("covidMetrika",width = 12)
                  ),
                  column(
                    width = 6,
                    valueBoxOutput("git_covidMetrika", width = 12)
                  ),
                  
                  widgetUserBox(title="Aplicativo desenvolvido por Márcia Helena Barbian 
                              com laboração de Luzivan Costa Reis, Lavínia Schuler-Faccini, Augusto Cardoso dos Santos, 
                              Elis Vanessa de Lima e Silva e Juliano Boquett",
                                type=2, collapsible = TRUE,color = "primary", width = 12,
                                tags$div(
                                  class="box box-widget widget-user-2", style="left: 407px;bottom: 207px;"
                                )),
                  
                  widgetUserBox(
                    title = tags$b("Márcia Helena Barbian"),
                    subtitle = "Professora do Departamento de Estatística da UFRGS",
                    type = 2,
                    width = 4,
                    src = 'marcia.png',
                    color = "blue",
                    "Contato: mhbarbian@ufrgs.br",
                    footer_padding = F
                  ),
                  
                  widgetUserBox(
                    title = tags$b("Luzivan Costa Reis"),
                    subtitle = "Aluno de Pós-graduação em Genética e Biologia molecular, UFRGS",
                    type = 2,
                    width = 4,
                    src = 'luzivan.jpg',
                    color = "red",
                    "Contato: luzivanreis@gmail.com",
                    footer_padding = F
                  ),
                  widgetUserBox(
                    title = tags$b("Lavínia Schuler-Faccini"),
                    subtitle = "Professora do programa de Pós-graduação em Genética e Biologia molecular, UFRGS",
                    type = 2,
                    width = 4,
                    src = 'ppg_genetica.png',
                    color = "red",
                    "Contato: lavinia.faccini@ufrgs.br",
                    footer_padding = F
                  ),
                  
                  widgetUserBox(
                    title = tags$b("Augusto Cardoso dos Santos"),
                    #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
                    type = 2,
                    width = 4,
                    src = 'ppg_genetica.png',
                    color = "red",
                    "Contato: santosaccd@gmail.com",
                    footer_padding = F
                  ),
                  widgetUserBox(
                    title = tags$b("Elis Vanessa de Lima e Silva"),
                    #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
                    type = 2,
                    width = 4,
                    src = 'ppg_genetica.png',
                    color = "red",
                    "Contato: evlsilva@hcpa.edu.br",
                    footer_padding = F
                  ),
                  widgetUserBox(
                    title = tags$b("Juliano Boquett"),
                    #subtitle = "Pós-graduação em Genética e Biologia molecular, UFRGS",
                    type = 2,
                    width = 4,
                    src = 'ppg_genetica.png',
                    color = "red",
                    "Contato: Juliano Boquett ",
                    footer_padding = F
                  ),
                  
                  widgetUserBox(title="Fonte de dados: Datasus",
                                type=2, collapsible = TRUE,color = "", width = 12,
                                tags$div(
                                  class="box box-widget widget-user-2", style="left: 407px;bottom: 207px;"
                                )),
                  # ),
                  # 
                  # 
                   tags$img(src = "logos.png", 
                           height = "130", width = "800")
                )
              ) #fluidpaige
      ) #Tabitem
      
      
    )
  )
)


server <- function(input, output) {
  
  output$grafico_mapa_proporcao <- renderLeaflet({
    
    if(input$y=="Aparelho osteomuscular (Q65-Q79)"){y <- banco_completo_trasformado$ostomuscular}
    if(input$y=="Deformidades congenitas dos pes (Q66)"){y <- banco_completo_trasformado$pes}
    if(input$y=="Outras malformacoes congenitas (Q80-Q89)"){y <- banco_completo_trasformado$outras}
    if(input$y=="Deformidades congenitas do quadril (Q65)"){y <- banco_completo_trasformado$quadril}
    if(input$y=="Aparelho circulatorio (Q20-Q28)"){y <- banco_completo_trasformado$aparelho_circulatorio}
    if(input$y=="Testiculo nao descido (Q53)"){y <- banco_completo_trasformado$testiculo}
    if(input$y=="Fenda labial e palatina (Q35-Q37)"){y <- banco_completo_trasformado$labial_fenda}
    if(input$y=="Anomalias cromossomicas (Q90-Q99)"){y <- banco_completo_trasformado$anomalias_cromossomicas}
    #if(input$y=="Sistema nervoso (Q00-Q07)"){y <- banco_completo_trasformado$?????}
    if(input$y=="Espinha bifida (Q05)"){y <- banco_completo_trasformado$espinha_bifida}
    if(input$y=="Aparelho urinario (Q60-Q64)"){y <- banco_completo_trasformado$geniturario}
    if(input$y=="Hemangioma e linfangioma de qualquer localizacao (D18)"){y <- banco_completo_trasformado$linfangioma}
    if(input$y=="Ausencia atresia e estenose do intestino delgado (Q41)"){y <- banco_completo_trasformado$ausencia_intestino}
    if(input$y=="Aparelho digestivo (Q38-Q45)"){y <- banco_completo_trasformado$outras_formacoes_digestivo}
    if(input$y=="Sistema nervoso (Q00-Q07)"){y <- banco_completo_trasformado$outras_ma_formacoes} 
    
    y= 10000*(y/banco_completo_trasformado$numero_nascidos)
    pal <- colorBin("YlOrRd", domain = y, bins = 6)
    
    
    leaflet(banco_completo_trasformado) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial= FALSE)) %>%
      addPolygons(fillColor = ~pal(y), 
                  weight = 1.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s - prevalencia %s", banco_completo_trasformado$Cidade, round(y, 6)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~y, opacity = 0.7, title = NULL,
                labFormat = labelFormat(digits = 5),
                position = "bottomright")
    
    
  })
  
  
  output$grafico_mapa_casos <- renderLeaflet({
    
    if(input$x=="Aparelho osteomuscular (Q65-Q79)"){x <- banco_completo_trasformado$ostomuscular}
    if(input$x=="Deformidades congenitas dos pes (Q66)"){x <- banco_completo_trasformado$pes}
    if(input$x=="Outras malformacoes congenitas (Q80-Q89)"){x <- banco_completo_trasformado$outras}
    if(input$x=="Deformidades congenitas do quadril (Q65)"){x <- banco_completo_trasformado$quadril}
    if(input$x=="Aparelho circulatorio (Q20-Q28)"){x <- banco_completo_trasformado$aparelho_circulatorio}
    if(input$x=="Testiculo nao descido (Q53)"){x <- banco_completo_trasformado$testiculo}
    if(input$x=="Fenda labial e palatina (Q35-Q37)"){x <- banco_completo_trasformado$labial_fenda}
    if(input$x=="Anomalias cromossomicas (Q90-Q99)"){x <- banco_completo_trasformado$anomalias_cromossomicas}
    #if(input$x=="Sistema nervoso (Q00-Q07)"){x <- banco_completo_trasformado$?????}
    if(input$x=="Espinha bifida (Q05)"){x <- banco_completo_trasformado$espinha_bifida}
    if(input$x=="Aparelho urinario (Q60-Q64)"){x <- banco_completo_trasformado$geniturario}
    if(input$x=="Hemangioma e linfangioma de qualquer localizacao (D18)"){x <- banco_completo_trasformado$linfangioma}
    if(input$x=="Ausencia atresia e estenose do intestino delgado (Q41)"){x <- banco_completo_trasformado$ausencia_intestino}
    if(input$x=="Aparelho digestivo (Q38-Q45)"){x <- banco_completo_trasformado$outras_formacoes_digestivo}
    if(input$x=="Sistema nervoso (Q00-Q07)"){x <- banco_completo_trasformado$outras_ma_formacoes} 
    
    #x= 10000*(x/banco_completo_trasformado$numero_nascidos)
    pal <- colorBin("YlOrRd", domain = x, bins = 6)
    
    leaflet(banco_completo_trasformado) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial= FALSE)) %>%
      addPolygons(fillColor = ~pal(x), 
                  weight = 1.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s - n de nascidos vivos %s", banco_completo_trasformado$Cidade, round(x, 6)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~x, opacity = 0.7, title = NULL,
                labFormat = labelFormat(digits = 5),
                position = "bottomright")
    
    
  })
  
  
  
  # 
  output$tabela <- renderDT(banco_tabela2)
  # 
  
}

shinyApp(ui, server)
