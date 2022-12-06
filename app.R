library(ggplot2)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(DT)
library(data.table)
library(corrplot)
library(sf)
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")


df2015 = read.csv("2015.csv")
df2016 = read.csv("2016.csv")
df2017 = read.csv("2017.csv")
df2018 = read.csv("2018.csv")
df2019 = read.csv("2019.csv")
names(df2018) = c('Happiness.Rank','Country','Happiness.Score',
                  'Economy..GDP.per.Capita.', 'Family', 
                  'Health..Life.Expectancy.', 'Freedom', 'Generosity', 
                  'Trust..Government.Corruption.')
names(df2019) = c('Happiness.Rank','Country','Happiness.Score',
                  'Economy..GDP.per.Capita.', 'Family', 
                  'Health..Life.Expectancy.', 'Freedom', 'Generosity', 
                  'Trust..Government.Corruption.')
world <- ne_countries(scale = "medium", returnclass = "sf")
countries = sort( unique(c(df2015$Country,df2016$Country,df2017$Country,df2018$Country,df2019$Country)))
setdiff(countries,df2015$Country)
for (country in countries){
  if (!(country %in% df2015$Country)){
    df2015[nrow(df2015) + 1,] = c(country,rep(NA, ncol(df2015)-1))
  }
}
for (country in countries){
  if (!(country %in% df2016$Country)){
    df2016[nrow(df2016) + 1,] = c(country,rep(NA, ncol(df2016)-1))
  }
}
for (country in countries){
  if (!(country %in% df2017$Country)){
    df2017[nrow(df2017) + 1,] = c(country,rep(NA, ncol(df2017)-1))
  }
}
for (country in countries){
  if (!(country %in% df2018$Country)){
    df2018[nrow(df2018) + 1,] = c(NA,country,rep(NA, ncol(df2018)-2))
  }
}
for (country in countries){
  if (!(country %in% df2019$Country)){
    df2019[nrow(df2019) + 1,] = c(NA,country,rep(NA, ncol(df2019)-2))
  }
}
cdf2015 = df2015[order(df2015$Country),]
cdf2016 = df2016[order(df2016$Country),]
cdf2017 = df2017[order(df2017$Country),]
cdf2018 = df2018[order(df2018$Country),]
cdf2019 = df2019[order(df2019$Country),]


rdf = copy(df2016)
rdf[] = lapply(rdf, function(x) {
  x1 = type.convert(as.character(x), as.is=TRUE)
  ifelse(grepl("^[0-9.]+$", x1), round(as.numeric(x1), 3), x1)})
rdf$Country = NULL
rdf = rdf %>% group_by(Region) %>%
  summarise(Mean_Happiness = mean(Happiness.Score, na.rm=T),
            Mean_gdp = mean(Economy..GDP.per.Capita.,na.rm=T),
            Mean_family=mean(Family,na.rm=T),
            Mean_freedom=mean(Freedom,na.rm=T),
            Mean_LE = mean(Health..Life.Expectancy., na.rm=T))
rdf = rdf[1:10,]
names(rdf) = c("Region","Happiness", "GDP_per_capita", "Family", "Freedom", "Life_Expectancy")
rdf = melt(rdf, id='Region')
rdf[] = lapply(rdf, function(x) {
  x1 = type.convert(as.character(x), as.is=TRUE)
  ifelse(grepl("^[0-9.]+$", x1), round(as.numeric(x1), 3), x1)})


corr = df2019[1:156,3:ncol(df2019)]
names(corr) = c('Happiness','GDP_per_capita','Family','Life_Expectancy','Freedom',
                'Generosity','Trust in Government')
corr[] = lapply(corr, function(x) {
  x = type.convert(as.character(x), as.is=TRUE)})


scatter = df2019[1:156,3:ncol(df2019)]
names(scatter) = c('Happiness','GDP_per_capita','Family','Life_Expectancy','Freedom',
                   'Generosity','Trust in Government')
scatter[] = lapply(scatter, function(x) {
  x = type.convert(as.character(x), as.is=TRUE)})








ui <- dashboardPage(
  dashboardHeader(title="World Happiness Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem('Map Visualization', tabName='map'),
      menuItem("Regional Analysis", tabName = "region"),
      menuItem("Cross Factor Analysis", tabName = "scatter"),
      menuItem('Cross Country Analysis', tabName='country')
    )
  ),
  dashboardBody(
    tags$style(HTML(".sidebar-menu li a { font-size: 16px; }")),
    tabItems(
      tabItem('about',
              tags$h2('Overview', align='center'),
              tags$br(),
              tags$h4('The dashboard is a visual approach to analyze happiness across the world.
                     Through the use of multiple interactive plots, users can see the variance in 
                     happiness on a regional and country level, and also study the various factors
                     that affect it. The plots might take some time to render, apologies for that.'),
              tags$br()),
      tabItem("region",
              tabsetPanel(
                tabPanel("Region vs Region",
                         selectInput("regvar1", "Select Region 1", unique(df2016[,2])[1:10], selected='Australia and New Zealand'),
                         selectInput("regvar2", "Select Region 2", unique(df2016[,2])[1:10], selected='Eastern Asia'),
                         fluidRow(plotOutput("rvrplot"))
                ),
                tabPanel("Factor vs Factor",
                         selectInput("facvar1", "Select Factor 1", c("Happiness", "GDP_per_capita", "Family", "Freedom", "Life_Expectancy"),selected='Happiness'),
                         selectInput("facvar2", "Seelct Factor 2", c("Happiness", "GDP_per_capita", "Family", "Freedom", "Life_Expectancy"),selected='GDP_per_capita'),
                         fluidRow(plotOutput("fvfplot"))
                )
              )),
      tabItem("scatter",
              tabsetPanel(
                tabPanel("Factor vs Factor",
                         h3("Analyse 2 Factors against each other", align='center'),
                         fluidRow(plotOutput('corrplot'), align='center'),
                         selectInput("scatvar1", "Select Factor 1", c("Happiness", "GDP_per_capita", "Family", "Freedom", "Life_Expectancy"), selected='Happiness'),
                         selectInput("scatvar2", "Select Factor 2", c("Happiness", "GDP_per_capita", "Family", "Freedom", "Life_Expectancy"), selected='Family'),
                         fluidRow(plotOutput("scatplot")),
                         actionButton("fitline", "Fit Regression Line"),
                         actionButton("removefitline", "Remove Regression Line")
                )
              )),
      tabItem('country',
              tabsetPanel(
                tabPanel("Country",
                         h3("Trend in country's happiness across 5 years", align='center'),
                         selectInput('cvar','Select Country',countries, selected='Finland'),
                         selectInput('fvar1', 'Select Factor', names(cdf2019)[3:7],selected='Happiness.Score'),
                         fluidRow(plotOutput('lineplot'), align='center')
                ),
                tabPanel("Country vs Country",
                         h3("Compare the trends of two countries", align='center'),
                         selectInput('cvar1','Select Country 1',countries, selected='United States'),
                         selectInput('cvar2','Select Country 2',countries, selected='India'),
                         selectInput('fvar2', 'Select Factor', names(cdf2019)[3:7],selected='Happiness.Score'),
                         fluidRow(plotOutput('lvlplot'))
                ),
                tabPanel("Top and Bottom",
                         h3("Compare the trends of countries at the (current) top/bottom"),
                         radioButtons('tb','Select Top or Bottom',c('Top','Bottom'), selected='Top'),
                         numericInput('pvar','Select number of countries',5, min=1, max=100),
                         selectInput('fvar3', 'Select Factor', names(cdf2019)[3:7],selected='Happiness.Score'),
                         fluidRow(plotOutput('plvlplot'))
                )
              )),
      tabItem('map',
              h3("Visualize the trends globally", align='center'),
              selectInput('var', 'Select Factor', names(cdf2019)[3:7],selected='Happiness.Score'),
              fluidRow(plotOutput('mapplot'))
      )
    )
  )
)


server <- function(input, output) {
  rv = reactiveValues(ols=0)
  output$rvrplot = renderPlot({
    rvar1 = input$regvar1
    rvar2 = input$regvar2
    ggplot(rdf[rdf$Region %in% c(rvar1,rvar2),], aes(x=variable,y=value, fill=variable)) + 
      geom_bar(stat='identity', col='black', alpha=0.75) + facet_grid(Region~.)+
      geom_text( aes(label=value), vjust=1.25) + theme_classic() + 
      xlab('Factors')+ labs(fill='Factors')+
      ggtitle(paste0('Comparison plot of different factors between ', rvar1, ' and ', rvar2))+
      theme(axis.text.x  = element_blank(),
            text = element_text(size=11),
            legend.text = element_text(size=11),
            legend.title = element_text(size=13, hjust = 0.5), 
            strip.text.y = element_text(size=10),
            axis.text.y = element_text(size=15),
            legend.box.background=element_rect(),
            plot.title = element_text(hjust=0.5))
  })
  output$fvfplot = renderPlot({
    fvar1 = input$facvar1
    fvar2 = input$facvar2
    ggplot(rdf[rdf$variable %in% c(fvar1,fvar2),], aes(x=Region,y=value, fill=Region)) + 
      geom_bar(stat='identity', col='black', alpha=0.75) + facet_grid(variable~.)+
      geom_text( aes(label=value), vjust=1.25) + theme_classic() + 
      xlab('Regions')+labs(fill='Regions')+
      ggtitle(paste0('Comparison plot between ', fvar1, ' and ', fvar2, ' across regions'))+
      theme(axis.text.x  = element_blank(),
            legend.text = element_text(size=11),
            legend.title = element_text(size=13, hjust=0.5), 
            strip.text.y = element_text(size=10),
            axis.text.y = element_text(size=15),
            legend.box.background=element_rect(),
            plot.title = element_text(hjust=0.5))
  })
  output$corrplot = renderPlot({
    cormat = round(cor(corr),2)
    corrplot(cormat, method='square',type='lower', diag=FALSE
             ,addCoef.col = "black")
  }, width=500)
  output$scatplot = renderPlot({
    svar1 = input$scatvar1
    svar2 = input$scatvar2
    if(rv$ols) {
      g = ggplot(data=scatter, mapping=aes_string(x=svar1,y=svar2)) + 
        geom_point(color="black", alpha=0.5) + geom_smooth(method='lm', col='red')+ 
        theme_classic() + 
        ggtitle(paste0("Scatter plot between ", svar1, " and ", svar2))+
        theme(plot.title = element_text(hjust=0.5))
    }
    else{
      g = ggplot(data=scatter, mapping=aes_string(x=svar1,y=svar2)) + 
        geom_point(color="black", alpha=0.5) + theme_classic() + 
        ggtitle(paste0("Scatter plot between ", svar1, " and ", svar2)) +
        theme(plot.title = element_text(hjust=0.5))
    }
    g
  })
  observeEvent(input$fitline, {
    rv$ols = 1
  })
  observeEvent(input$removefitline, {
    rv$ols = 0
  })
  output$lineplot = renderPlot({
    fac1 = input$fvar1
    cvar = input$cvar
    df = as.data.frame(cbind(countries,cdf2015[fac1],cdf2016[fac1],
                             cdf2017[fac1],cdf2018[fac1],
                             cdf2019[fac1]))
    names(df) = c('Country','2015', '2016', '2017', '2018', '2019')
    for (country in countries){
      df[which(df$Country==country),][which(is.na(df[which(df$Country==country),]))]=median(as.numeric(df[which(df$Country==country),2:ncol(df)]), na.rm=T)
    }
    df = df[order(df$`2019`, decreasing =TRUE),]
    df[] = lapply(df, function(x) {
      x1 = type.convert(as.character(x), as.is=TRUE)
      ifelse(grepl("^[0-9.]+$", x1), round(as.numeric(x1), 3), x1)})
    ggplot(melt(df[df$Country==cvar,])) + geom_line(aes(x=variable, y=value,group=1)) + 
      geom_point(aes(x=variable, y=value, group=1),color='red') + 
      geom_text(aes(x=variable, y=value,label=value),vjust=2,hjust=1)+
      labs(x='Year',y=paste0(fac1), title=paste0('Trend in ',cvar, ' : ', fac1)) + 
      theme_classic()+ 
      theme(plot.title = element_text(hjust = 0.5),
            legend.box.background=element_rect())
  })
  output$lvlplot = renderPlot({
    fac2 = input$fvar2
    cvar1 = input$cvar1
    cvar2 = input$cvar2
    df = as.data.frame(cbind(countries,cdf2015[fac2],cdf2016[fac2],
                             cdf2017[fac2],cdf2018[fac2],
                             cdf2019[fac2]))
    names(df) = c('Country','2015', '2016', '2017', '2018', '2019')
    for (country in countries){
      df[which(df$Country==country),][which(is.na(df[which(df$Country==country),]))]=median(as.numeric(df[which(df$Country==country),2:ncol(df)]), na.rm=T)
    }
    df = df[order(df$`2019`, decreasing =TRUE),]
    df[] = lapply(df, function(x) {
      x1 = type.convert(as.character(x), as.is=TRUE)
      ifelse(grepl("^[0-9.]+$", x1), round(as.numeric(x1), 3), x1)})
    ggplot(melt(df[df$Country %in% c(cvar1,cvar2),])) + 
      geom_line(aes(x=variable, y=value,group=Country, color=Country)) + 
      geom_point(aes(x=variable, y=value, group=Country,color=Country)) + 
      geom_text(aes(x=variable, y=value,label=value),vjust=2,hjust=1)+
      labs(x='Year',y=paste0(fac2), title=paste0(cvar1 ,' vs ',cvar2, " : ", fac2, ' trend')) + 
      theme_classic()+ 
      theme(plot.title = element_text(hjust = 0.5),
            legend.box.background=element_rect())
  })
  output$plvlplot = renderPlot({
    n = as.integer(input$pvar)
    fac3 = input$fvar3
    df = as.data.frame(cbind(countries,cdf2015[fac3],cdf2016[fac3],
                             cdf2017[fac3],cdf2018[fac3],
                             cdf2019[fac3]))
    names(df) = c('Country','2015', '2016', '2017', '2018', '2019')
    for (country in countries){
      df[which(df$Country==country),][which(is.na(df[which(df$Country==country),]))]=median(as.numeric(df[which(df$Country==country),2:ncol(df)]), na.rm=T)
    }
    df = df[order(df$`2019`, decreasing =TRUE),]
    df[] = lapply(df, function(x) {
      x1 = type.convert(as.character(x), as.is=TRUE)
      ifelse(grepl("^[0-9.]+$", x1), round(as.numeric(x1), 3), x1)})
    if (input$tb == "Top"){
      ggplot(melt(df[1:n,])) + 
        geom_line(aes(x=variable, y=value,group=Country, color=Country)) + 
        geom_point(aes(x=variable, y=value, group=Country,color=Country)) + 
        geom_text(aes(x=variable, y=value,label=value),vjust=2,hjust=1)+
        labs(x='Year',y=paste0(fac3), 
             title=paste0(fac3,' : Trend in the top ', n ,' countries')) + 
        theme_classic()+
        theme(text = element_text(size=10)) + 
        theme(plot.title = element_text(hjust = 0.5),
              legend.box.background=element_rect())
    }
    else{
      ggplot(melt(df[(length(countries)+1-n):length(countries),])) + 
        geom_line(aes(x=variable, y=value,group=Country, color=Country)) + 
        geom_point(aes(x=variable, y=value, group=Country,color=Country)) + 
        geom_text(aes(x=variable, y=value,label=value),vjust=2,hjust=1)+
        labs(x='Year',y=paste0(fac3), 
             title=paste0(fac3,' : Trend in the bottom ', n ,' countries')) + 
        theme_classic() + 
        theme(plot.title = element_text(hjust = 0.5),
              legend.box.background=element_rect())
    }
  })
  output$mapplot = renderPlot({
    var = input$var
    fill = c()
    for (country in world$name){
      if (country %in% df2019$Country){
        fill = c(fill, df2019[df2019$Country==country,var])
      }
      else{
        fill = c(fill,NA)
      }
    }
    fill = lapply(fill, function(x){
      return (as.double(x))
    })
    fill=as.numeric(fill)
    ggplot(data = world) +
      geom_sf(aes(fill=fill)) +
      ggtitle(paste0("World map showing distribution of ", var, " in 2019"), 
              subtitle = paste0("(Grey countries are due a lack of data)")) +
      scale_fill_viridis_c(option='A')+
      theme(plot.title = element_text(hjust=0.5),
            plot.subtitle = element_text(hjust=0.5))
  }, height=800)
}

shinyApp(ui = ui, server = server)
