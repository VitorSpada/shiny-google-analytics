
# load the required packages
library(RGoogleAnalytics)
library(shiny)
library(shinydashboard) 
library(ggplot2)
library(tableHTML)
## app.R ##
library(shiny)
library(shinydashboard) 
library(dygraphs)
library(plotly)
library(dplyr)
library(forecast)
library('DT')
library(xts)
library(CausalImpact)
library(aws.s3)
library(shinycssloaders)
library(shinyWidgets)
Sys.setenv("AWS_ACCESS_KEY_ID" = aws_acess_key_id,
           "AWS_SECRET_ACCESS_KEY" = aws_secret_acess_key,
           "AWS_DEFAULT_REGION" = aws_default_region
)


ga.data<-aws.s3::s3read_using(read.csv, object = 's3://sia-shiny/gadata.csv')



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Smart G.A")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Modelagem", icon = icon("chart-bar"), tabName = "grafico"),
    menuItem('Análise de SEO Traffic', tabName='ani',icon=icon("chart-line"),
             badgeLabel = "new", badgeColor = "black")
  )
)

body<-dashboardBody(tags$head(tags$style(HTML(
  '
                             .box.box-solid.box-primary>.box-header {
  color:#fff;
  background: #39ac73 
                    }
.box.box-solid.box-primary{
border-bottom-color: #39ac73 ;
border-left-color: #39ac73 ;
border-right-color: #39ac73 ;
border-top-color: #39ac73 ;
}

/* logo */
  .skin-blue .main-header .logo {
    background-color: #39ac73;
  }

/* logo when hovered */
  .skin-blue .main-header .logo:hover {
    background-color: #39ac73;
  }

/* navbar (rest of the header) */
  .skin-blue .main-header .navbar {
    background-color: #39ac73;
  }
  
/* main sidebar */
  .skin-blue .main-sidebar {
    background-color: #ffffff;
  }

/* active selected tab in the sidebarmenu */
  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    background-color: #ffffff;
  }

/* other links in the sidebarmenu */
  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
    background-color:#ffffff;
      color: #000000;
  }

/* other links in the sidebarmenu when hovered */
  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
    background-color: #d1e0e0;
  }
/* toggle button when hovered  */
  .skin-blue .main-header .navbar .sidebar-toggle:hover{
    background-color: #d1e0e0;
  }

/* body */
  .content-wrapper, .right-side {
    background-color: #7da2d1;
  }

                                /* other links in the sidebarmenu when hovered */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                  background-color:  #39ac73;
                                }
                                /* toggle button when hovered  */
                                .skin-olive .main-header .navbar .sidebar-toggle:hover{
                                background-color: #ff69b4;
                                }
                                .small-box.bg-olive { background-color:  #39ac73  !important; color: #ffffff !important;}

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }'))),
  tabItems(tabItem(tabName = "dashboard",
                   fluidRow(
                     valueBoxOutput("value1",width=4)
                     ,valueBoxOutput("value2",width=4)
                     ,valueBoxOutput("value3")
                   ),
                   
                   fluidRow( 
                     
                     
                     box(
                       title = "Visitas vs rejeição"
                       ,status = "primary"
                       ,solidHeader = TRUE 
                       ,collapsible = TRUE 
                       ,dygraphOutput("revenuebyPrd", height = "375px") %>% withSpinner()
                     )
                     ,box(
                       title = "Navegadores"
                       ,status = "primary"
                       ,solidHeader = TRUE 
                       ,collapsible = TRUE 
                       ,plotlyOutput("revenuebyRegion", height = "375px") %>% withSpinner()
                     )
                     ,box(
                       title = "Tabela"
                       ,status = "primary"
                       ,solidHeader = TRUE 
                       ,collapsible = TRUE 
                       ,dataTableOutput("tabela", height = "375")%>% withSpinner(),width=12 
                     )
                   )
  ),
  tabItem(tabName = "ani",
          fluidRow(
            
            valueBoxOutput("ani1",width=6) 
            ,valueBoxOutput("ani2",width=6) 
            ,valueBoxOutput("ani3",width=12),
            box(
              title = "Efeito de SEO Keywords"
              ,status = "primary"
              ,solidHeader = TRUE 
              ,collapsible = TRUE 
              ,plotOutput("impacto", height = "550") %>% withSpinner()%>% withSpinner(),width=12 
            ),
            box(title="Nesta aba, o aplicativo realiza uma análise de efeito do SEO Traffic. O usuário poderá
                                  saber se após uma mudança nas palavras chaves do site em um dado dia(linha tracejada vertical), houve impacto significativo do ponto de vista estatístico sobre alguma métrica do Google Analytics,
                                  tais como as sessões ou acessos da página. Além de saber a significância do impacto, o aplicativo é capaz de estimar com margem de erro a magnitudade do mesmo. No exemplo acima, temos as sessões de tráfego orgânico em que houve um evento significativo na data 2018/04/28. O efeito acumulado no período foi de -1758 sessõoes",
                solidHeader = TRUE,width=12
            )
            
            
            
            
          )),
  tabItem(tabName = "grafico",
          fluidRow(
           
            
            dropdownButton(
              tags$h3("Seleção de parâmetros"),
              
              sliderInput("periodo", "Escolha o período da previsão em dias:",min = 1, max = 30, value = 30),
              selectInput("variavel", "Selecione uma variavel ",choice=c('sessions','pageViews','bounces'))
              ,
              circle = TRUE, status = "danger", icon = icon("gear"), width = "20px",
              tooltip = tooltipOptions(title = "Click to see inputs !")),
                       
              box(solidHeader = TRUE,
                  
              dygraphOutput("grafico", height = "400",width="100%") %>% withSpinner(),width=11.9)
            
            box
            

            
    
       #h6( "Nesta aba, o aplicativo constrói um modelo estatístico capaz de realizar previsões com margem de erro de métricas do Google Analytics. O usuário pode escolher a métrica, e qual o tamanho do período da previsão.")
            
            
          ))))


# combine the two fluid rows to make the body
#body <- dashboardBody(tabItems(tabItem(tabName = "widgets",frow1,frow2)),tabItem(tabName = "dashboard",modelo))
ui <- dashboardPage(title = 'Dashboard GA', header, sidebar, body, skin='blue')



###########################################################################



server <- function(input, output) { 
  #some data manipulation to derive the values of KPI boxes
  acessos2 <- sum(ga.data$pageViews)
  sessions <- sum(ga.data$sessions)
  bounce <- sum(ga.data$bounce)
  
  ####################################################################
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(acessos2, format="d", big.mark=',')
      ,"Acessos no Período"
      #,paste('Top Account:',sales.account$Account)
      ,icon = icon("mouse-pointer")
      ,color = "olive")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(sessions, format="d", big.mark=',')
      ,'Sessões no Período'
      ,icon = icon("user-friends")
      ,color = "olive")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(bounce, format="d", big.mark=',')
      #,paste('Top Product:',prof.prod$Product)
      ,"Rejeições no Período"
      ,icon = icon("user-slash")
      ,color = "olive")   
  })
  output$ani1 <- renderValueBox({
    valueBox(resultado,
             z
             ,icon = icon("thumbs-up")
             ,color = "olive")  
  })
  output$ani2 <- renderValueBox({
    valueBox(
      formatC(efeito, format="d", big.mark=',')
      ,"Sessões no Período"
      
      ,icon = icon("mouse-pointer")
      ,color = "olive")  
  })
  
  
  
  
  theTable <- within(ga.data, 
                     browser <- factor(browser, 
                                       levels=names(sort(table(browser), 
                                                         decreasing=TRUE))))
  output$revenuebyRegion <- renderPlotly({
    print(
      ggplotly(ggplot(theTable,aes(x=browser,fill=browser))+geom_bar(stat="count")  + scale_fill_brewer(palette="Reds") + theme_classic() +
                 theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())))
  })
  Acessos <- ts(ga.data$pageViews, start=c(2017, 9,13), end=c(2019, 08,16), frequency=12)
  #Rejeicao <- ts(ga.data$bounces, start=c(2017, 9,13), end=c(2019,  08,16), frequency=12)
  #grafico<-cbind(Acessos,Rejeicao)
  #output$revenuebyPrd<-renderDygraph({dygraph(grafico) %>% dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)})
  
  
  
  
  acessos=ga.data %>%
    group_by(date)%>%
    summarise(acessos=sum(pageViews))
  acessos<-as.data.frame(acessos)
  date <- seq(as.Date("2018-01-01"), length=365, by="days")
  data <- seq(as.Date("2018-01-01"), length=365, by="days")
  date<-as.data.frame(date)
  date$pageViews<-0
  date$date <- format(as.Date(date$date), "%Y%m%d")
  acessos<-merge(x = date, y = acessos, by = "date", all.x = TRUE)
  acessos$acessos[is.na(acessos$acessos)] <- 0
  Visitas <- xts(acessos$acessos, order.by=data)
  
  
  bounces=ga.data %>%
    group_by(date)%>%
    summarise(bounces=sum(bounces))
  bounces<-as.data.frame(bounces)
  date <- seq(as.Date("2018-01-01"), length=365, by="days")
  data <- seq(as.Date("2018-01-01"), length=365, by="days")
  date<-as.data.frame(date)
  date$pageViews<-0
  date$date <- format(as.Date(date$date), "%Y%m%d")
  bounces<-merge(x = date, y = bounces, by = "date", all.x = TRUE)
  bounces$bounces[is.na(bounces$bounces)] <- 0
  Rejeiçoes <- xts(bounces$bounces, order.by=data)
  grafico<-cbind(Visitas,Rejeiçoes)
  output$revenuebyPrd<-renderDygraph({dygraph(grafico) %>% dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)})
  
  
  
  
  d<-reactive({
    
    z<-as.data.frame((aggregate(list(ga.data$bounces,ga.data$pageViews,ga.data$sessions), by = (ga.data["date"]), sum)))
    colnames(z) <- c("date","bounces","pageViews","sessions")
    d<-z[input$variavel]
    
    ts_ga = ts(d,  start=c(2018, 1), end=c(2019,1),frequency = 365)
    fit <- auto.arima(ts_ga,D=1)
    fcast <- forecast(fit, input$periodo)
    serie<-as.data.frame(fcast$x)
    date <- seq(as.Date("2018-01-01"), length=366, by="days")
    #date$date <- format(as.Date(date$date), "%Y%m%d")
    plot=xts(serie$x,order.by=date)
    
    
    mean<-as.data.frame(fcast$mean)
    upper<-as.data.frame(fcast$upper)
    upper<-as.data.frame(upper$`95%`)
    lower<-as.data.frame(fcast$lower)
    lower<-as.data.frame(lower$`95%`)
    f<-cbind(mean,upper,lower)
    colnames(f) <- c("fit","upper","lower")
    date <- seq(as.Date("2019-01-01"), length=input$periodo, by="days")
    #date$date <- format(as.Date(date$date), "%Y%m%d")
    plot2=xts(f,order.by=date)
    final<-cbind(plot,plot2)
    return(final)
  })
  output$grafico<-renderDygraph({dygraph(d(), main = "Previsão métrica G.A") %>% dySeries("plot", label = "Ocorrido") %>% dySeries(c("lower", "fit", "upper"), label = "Previsão") %>% dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)})
  
  
  
  output$tabela<-DT::renderDataTable({ga.data})
  
  start = "2017-09-13"
  end = "2019-08-23"
  
  
  pre.period <- as.Date(c(start, "2018-08-04"))
  post.period <- as.Date(c("2018-08-05", end))
  direct=ga.data %>%
    filter(source=='(direct)')%>%
    group_by(date)%>%
    summarise(direct=sum(sessions))
  
  direct=ga.data %>%
    filter(source=='(direct)')%>%
    group_by(date)%>%
    summarise(direct=sum(sessions))
  direct<-as.data.frame(direct)
  date <- seq(as.Date("2017-09-13"), length=710, by="days")
  data <- seq(as.Date("2017-09-13"), length=710, by="days")
  date<-as.data.frame(date)
  date$pageViews<-0
  date$date <- format(as.Date(date$date), "%Y%m%d")
  direct<-merge(x = date, y = direct, by = "date", all.x = TRUE)
  direct$direct[is.na(direct$direct)] <- 0
  direct <- xts(direct$direct, order.by=data)
  
  
  
  google=ga.data %>%
    filter(source=='google')%>%
    group_by(date)%>%
    summarise(google=sum(sessions))
  google<-as.data.frame(google)
  date <- seq(as.Date("2017-09-13"), length=710, by="days")
  data <- seq(as.Date("2017-09-13"), length=710, by="days")
  date<-as.data.frame(date)
  date$pageViews<-0
  date$date <- format(as.Date(date$date), "%Y%m%d")
  google<-merge(x = date, y = google, by = "date", all.x = TRUE)
  google$google[is.na(google$google)] <- 0
  google <- xts(google$google, order.by=data)
  serie1 <- apply.weekly(as.xts(direct),FUN=sum)
  serie2 <- apply.weekly(as.xts(google),FUN=sum)
  
  serie<-cbind(serie1,serie2)
  impact_vw <- CausalImpact(serie, pre.period, post.period,model.args = list(niter = 1000, nseasons = 52))
  output$impacto<-renderPlot({plot(impact_vw)})
  
  p<-as.data.frame(impact_vw$summary)
  efeito=p$AbsEffect[2]
  p_value<-p$p[2]
  resultado<-"O imapcto é significativo"
  
  if(p_value > 0.05) {
    z<-'A probabilidade do efeito ter acontecido apenas devido a chance é alta'
  } else if (p_value < 0.05) {
    z<-'A probabilidade do efeito ter acontecido apenas devido a chance é pequena'
    return(z)
  }
  
  
  
  
  
}  
shinyApp(ui, server)

