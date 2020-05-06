---
title: "Exam 2"
author: '[Name]'
date: "11/14/2019"
output: 
  html_document:
    keep_md: yes
---




```r
library(tidyverse)
library(jsonlite)
library(assertthat)
library(lubridate)
library(httr)
library(textdata)
```


## Task 1


```r
#Get API key
API <- "acf8bf99ddee4c18ac37631c749ec297"

#Ingredient for api
baseurl_source <- "https://newsapi.org/v2/sources?" 
category <- c("general","business")
language <- "en"
country <- "us"

#interest sources
news <- c("CNN","Fox News","The Wall Street Journal","Reuters")

#Data scrapping
url <- paste0(baseurl_source,"category=",category,"&",
              "language=",language,"&","country=",country,"&","apiKey=",API, sep = "")

data <- sapply(url,function(x){read_json(x)["sources", drop = F]}) 

sources <- lapply(data, function(x){map_df(map(x,unlist),as.list)}) %>% 
  bind_rows() %>% 
  filter(name %in% news)

sources
```

```
# A tibble: 4 x 7
  id       name     description           url     category language country
  <chr>    <chr>    <chr>                 <chr>   <chr>    <chr>    <chr>  
1 cnn      CNN      View the latest news~ http:/~ general  en       us     
2 fox-news Fox News Breaking News, Lates~ http:/~ general  en       us     
3 reuters  Reuters  Reuters.com brings y~ http:/~ general  en       us     
4 the-wal~ The Wal~ WSJ online coverage ~ http:/~ business en       us     
```



```r
#Ingredient for api
baseurl_head <- "https://newsapi.org/v2/top-headlines?"
id <- sources$id
q <- "taxes"

#Data scrapping
url <- paste0(baseurl_head,"q=",q,"&","sources=",id,"&","apiKey=",API, sep = "")

data <- sapply(url,function(x){read_json(x)["articles",drop = F]})

headline <- lapply(data, function(x){map_df(map(x,unlist),as.list)}) %>% 
  bind_rows()

headline
```

```
# A tibble: 0 x 0
```



```r
#Ingredient for api
baseurl_every <- "https://newsapi.org/v2/everything?"
qintitle <- "healthcare"
date <- "2019-11-01"
domain <- sources$url %>% str_remove_all("^h.*/[w]*\\.*[us\\.]*")
language <- "en"
country <- "us"

#Data scrapping
url <- paste0(baseurl_every,"qIntitle=",qintitle,"&","from=",date,"&","to=",date,"&","language=",language,"&",
             "domains=",domain,"&","apikey=", API, sep = "")

data <- sapply(url,function(x){read_json(x)["articles",drop = F]})

headline_11_01 <- lapply(data, function(x){map_df(map(x,unlist),as.list)}) %>% 
  bind_rows() %>% 
  select(title)

headline_11_01
```

```
# A tibble: 3 x 1
  title                                                 
  <chr>                                                 
1 Healthcare may trump Brexit in battle for British vote
2 Warren's big healthcare plan relies on big assumptions
3 Warren's big healthcare plan relies on big assumptions
```

## Task 2


```r
get_source <- function(category = "", api_key){
  
  #input test
  assert_that(is.string(category))
  assert_that(noNA(api_key), not_empty(api_key))
  
  #Ingredient for api
  baseurl <- "https://newsapi.org/v2/sources?" 
  language <- "en"
  country <- "us"
  news <- c("CNN","Fox News","The Wall Street Journal","Reuters")

  #Data scrapping
  url <- paste0(baseurl,"category=",category,"&",
                "language=",language,"&","country=",country,"&","apiKey=",api_key, sep = "")

  data <- sapply(url,function(x){read_json(x)$source}) 
  
  sources <- map_df(map(data,unlist),as.list) %>% 
    filter(name %in% news)
  
  return(sources)
}
```



```r
get_headlines <- function(sources = get_source(api_key = "acf8bf99ddee4c18ac37631c749ec297"), q = "", page_size = 20, page=1, api_key){
  #input test - data type
  assert_that(is.string(q), is.count(page_size), is.count(page), see_if(page_size<=100))
  #NA test
  assert_that(noNA(api_key), not_empty(api_key))
  
  #Ingredient for api
  baseurl <- "https://newsapi.org/v2/top-headlines?"
  id <- sources$id
  
  #Data scrapping
  url <- paste0(baseurl,"q=",q,"&","pageSize=",page_size,"&","page=",page,"&",
                "sources=",id,"&","apiKey=",api_key, sep = "")
  
  data <- sapply(url,function(x){read_json(x)["articles", drop = F]})
  
  headlines <- lapply(data, function(x){map_df(map(x,unlist),as.list)}) %>% 
    bind_rows()
  
  if(nrow(headlines) == 0) return(data.frame(status = "No data available"))
  return(headlines)
}
```



```r
get_historic <- function(q = "", q_title = "", 
                         sources = get_source(api_key = "acf8bf99ddee4c18ac37631c749ec297"), 
                         from = Sys.time() %m-% months(1), to = Sys.time(), sort_by = "publishedAt", 
                         page_size = 20, page = 1, api_key){
  
  #input test - data type
  options <- c("relavancy", "popularity", "publishedAt")
  from <- as.Date(from)
  to <- as.Date(to)
  assert_that(is.string(q), is.string(q_title), see_if(sort_by %in% options),is.count(page_size), is.count(page),
              see_if(page_size<=100),is.time(to)|is.date(to), is.time(from)|is.date(from))
  #NA test
  assert_that(noNA(api_key),not_empty(api_key))
  
  #Ingredient for api
  baseurl <- "https://newsapi.org/v2/everything?"
  domain <- sources$url %>% str_remove_all("^h.*/[w]*\\.*[us\\.]*")
  language <- "en"
  country <- "us"
  
  #Data scrapping
  url <- paste0(baseurl,"q=",q,"&","qIntitle=",q_title,"&","from=",from,"&","to=",to,"&","language=",language,
                "&","sortBy=",sort_by,"&","pageSize=",page_size,"&","page=",page,"&","domains=",domain,
                "&","apikey=",api_key, sep = "")
  data <- sapply(url, function(x){read_json(x)["articles", drop = F]})
  historic <- lapply(data, function(x){map_df(map(x,unlist),as.list)}) %>% 
    bind_rows()
  
  if(nrow(historic) == 0) return(data.frame(status = "No data available"))
  return(historic)
}
```


## Task 3


```r
library(shiny)
library(shinythemes)
library(tidytext)
library(wordcloud2)

if(interactive()){
ui <- fluidPage(theme = shinytheme("united"),
  navbarPage("News API!",
    #First panel
    tabPanel("Intro",
      titlePanel("Welcome to News search program"),
      sidebarLayout(
        sidebarPanel(
          h1("News API"),
          p("You can search News sources, Top headlines, and Articles in this app with summary and plots!"),
          br(),
          strong(textOutput("api")),
          br(),
          strong(em("Cautious!")),
          br(),
          p("Unfortunately, only 500 request call is provided per day. Moreover, if you restart app, previous call count is not recorded. Please be careful.")
        ),
        mainPanel(
          h2("Introduction"),
          p("This application provides search service of articles and information about source of news. News data is obtained by"),
          a("News API.", href = "https://newsapi.org"),
          br(),
          h2("Features"),
          h3("Articles"),
          p("You can search most recent headline articles by keyword, source, and pages. In addition, you can search every articles within a month. You can adjust your search by keyword, keyword in title, period, and pages. It can be sorted by relavancy, popularity, and time. Not only data table, summary plot and wordcloud plot are also provided which represent which words are mostly used in articles, and relavancy with keyword if you enter."),
          h3("Sources"),
          p("You can search news sources by category. The data table of new sources which provide name, description, url, and category is provided. At about tap, you can find specific information about new sources which is provided by"),
          a("Wikipedia.", href = "https://en.wikipedia.org/wiki/Main_Page")
        )
      )
      ),
    #Second panel         
    tabPanel("Articles",
      sidebarLayout(
        sidebarPanel(
          #Item choice user interest
          selectInput(inputId = "item", label = "Select Item",
                      choices = list("Recent Headlines" = 2,"Everything" = 3)),
          conditionalPanel(
          condition = "input.item == 2",
          #select source
          checkboxGroupInput(inputId = "source_head", label = "Select sourse",
                             choices = list("CNN" = "CNN", "Fox News" = "Fox News", 
                                            "The Wall Street Journal" = "The Wall Street Journal",
                                            "Reuters" = "Reuters"), selected = "CNN"),
          #input search keywords
          textInput(inputId = "keywords_head", label = "Search for", value = ""),
          #page size selection
          sliderInput(inputId = "pagesize_head", label = "page size", min = 1, max = 100, value = 20),
          #page number selection
          numericInput(inputId = "page_head", label = "To which pages", min = 1, value = 1)
        ),
        conditionalPanel(
          condition = "input.item == 3",
          #select source
          checkboxGroupInput(inputId = "source_every", label = "Select sourse",
                             choices = list("CNN" = "CNN", "Fox News" = "Fox News", 
                                            "The Wall Street Journal" = "The Wall Street Journal",
                                            "Reuters" = "Reuters"), selected = "CNN"),
          #input search keywords
          textInput(inputId = "keywords_every", label = "Search for", value = ""),
          #search keywords only in title
          checkboxInput(inputId = "title", label = "Only in title"),
          #set dates
          dateRangeInput(inputId = "date", label = "from to"),
          #sort_by
          selectInput(inputId = "sort", label = "Sort by",
                      choices = list("relavancy" = "relavancy", "popularity" = "popularity", 
                                     "publishedAt" = "publishedAt")),
          #page size selection
          sliderInput(inputId = "pagesize_every", label = "page size", min = 1, max = 100, value = 20),
          #page number selection
          numericInput(inputId = "page_every", label = "To which pages", min = 1, value = 1)
        ),
        div(align="right",
            actionButton(inputId = "run", label = "search")
        )
      ),
      
   mainPanel(
     tabsetPanel(
       tabPanel("Search",
          conditionalPanel(
           condition = "input.item == 2",
           DT::dataTableOutput("headline_table")
          ),
          conditionalPanel(
           condition = "input.item == 3",
           DT::dataTableOutput("every_table")
          )
        ),
       tabPanel("Summary plot",
          conditionalPanel(
            condition = "input.item == 2",
            plotOutput("head_sum")
          ),
          conditionalPanel(
            condition = "input.item == 3",
            plotOutput("every_sum")
          )
        ),
       tabPanel("Word cloud",
          conditionalPanel(
            condition = "input.item == 2",
            wordcloud2Output("head_cloud")
          ),
          conditionalPanel(
            condition = "input.item == 3",
            wordcloud2Output("every_cloud"))
          ))
        )
      )
    ),
  #Third panel
  tabPanel("Sources",
      sidebarLayout(
        sidebarPanel(
          #select category user interest
          checkboxGroupInput(inputId = "category", label = "Select Category",
                            choices = list("general" = "general", "business" = "business"), selected = "general"),
          div(align="right",
            actionButton(inputId = "run_source", label = "run")
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Search",
              DT::dataTableOutput("source_table")
            ),
            tabPanel("About",
              selectInput(inputId = "source", label = "Select Source want to see",
                          choice = list("CNN" = "CNN", "Fox News" = "Fox_News", 
                                            "The Wall Street Journal" = "The_Wall_Street_Journal",
                                            "Reuters" = "Reuters"), selected = "CNN"),
              htmlOutput("resource")
            )
          )
        )
      )
    )
  )
)
        

server <- function(input, output, session) {
  #API key
  api_key <- "acf8bf99ddee4c18ac37631c749ec297"
  #API call
  call_count <- reactiveVal(0)
  observeEvent(
    input$run | input$run_source,{call_count(call_count()+1)} )
  output$api <- renderText(paste("Used requests: ",call_count()))
  
  #search - headline table
  headline_table <- reactive({
    input$run
    isolate({
      temp <- get_headlines(q = input$keywords_head, page_size = input$pagesize_head,
                    page = input$page_head, api_key = api_key) 
      
      if(nrow(temp) == 1) return(temp)
      
      temp <- temp %>%  
       select(source.name, title, description, url) %>% 
       mutate(url = paste0("<a href='",url,"'>",url,"</a>")) %>% 
       filter(source.name %in% input$source_head) %>% 
       `colnames<-`(c("Author","Title","Description","URL"))
      
      return(temp)
    })
  })
  output$headline_table <- DT::renderDataTable({
     DT::datatable(headline_table(),options = list(pageLength = 4), escape = FALSE)
    })
  #modal diag for search failure
  head_row <- reactiveVal(1)
  observeEvent(headline_table(),{if(nrow(headline_table()) == 1) head_row(head_row()+1)}, ignoreInit = T)
  observeEvent(head_row(), {
    showModal(modalDialog(
      title = "Result",
      "Search result for keyword is 0",
      easyClose = T
    ))
  },ignoreNULL = T, ignoreInit = T) 
  #search - news table
  every_table <- reactive({
    input$run
    isolate({
      if(input$title){
       temp <- get_historic(q_title = input$keywords_every, from = input$date[1], to = input$date[2], 
                    sort_by = input$sort,page_size = input$pagesize_every, page = input$page_every, 
                    api_key = api_key) 
       if(nrow(temp) == 1) return(temp)
       temp <- temp %>%
         select(source.name, title, description, url, publishedAt) %>% 
         mutate(url = paste0("<a href='",url,"'>",url,"</a>")) %>% 
         filter(source.name %in% input$source_every) %>% 
         `colnames<-`(c("Author","Title","Description","URL","Time"))
       return(temp)
        
     } else {
       temp <- get_historic(q = input$keywords_every, from = input$date[1], to = input$date[2], 
                            sort_by = input$sort,page_size = input$pagesize_every, page = input$page_every,
                            api_key = api_key)
       if(nrow(temp) == 1) return(temp)
       temp <- temp %>%  
         select(source.name, title, description, url, publishedAt) %>% 
         mutate(url = paste0("<a href='",url,"'>",url,"</a>")) %>% 
         filter(source.name %in% input$source_every) %>% 
         `colnames<-`(c("Author","Title","Description","URL","Time"))
       return(temp)
     }
    })
  })
  
  output$every_table <- DT::renderDataTable({
    DT::datatable(every_table(),options = list(pageLength = 4), escape = FALSE)
  })
  #modal diag for search failure
  every_row <- reactiveVal(1)
  observeEvent(every_table(),{if(nrow(every_table()) == 1) every_row(every_row()+1)}, ignoreInit = T)
  observeEvent(every_row(), {
    showModal(modalDialog(
      title = "Result",
      "Search result for keyword is 0",
      easyClose = T
    ))
  },ignoreNULL = T, ignoreInit = T) 
  
  #summray - headlines
  headline_summary <- reactive({
    input$run
    isolate({
      get_headlines(q = input$keywords_head, page_size = input$pagesize_head,
                    page = input$page_head, api_key = api_key)[c(2,3,4,5,6)] %>%  
       mutate(url = paste0("<a href='",url,"'>",url,"</a>")) %>% 
       filter(source.name %in% input$source_head) %>%   
        unnest_tokens(word, description) %>% 
        anti_join(stop_words) %>% 
        count(word, sort = T)
    })
  })
  #summary - plot
  output$head_sum <- renderPlot(
    ggplot(headline_summary()[1:10,], aes(x = reorder(word,n), y = n, fill = n, label = n)) +
    geom_col() +
    coord_flip() +
    theme_bw() +
    scale_fill_gradient(low = "yellow", high = "red") +
    theme(legend.position = "none") +
    labs(title = "Top 10 words in top headlines", y = NULL, x = NULL) +
    geom_text(mapping = aes(label = n), hjust = -.5, col = "red", size = 4)
  )
  #cloud plot -headlines
  output$head_cloud <- renderWordcloud2(
    wordcloud2(headline_summary())
  )
  
  #summary - Every articles
  every_summary <- reactive({
    input$run
    isolate({
    if(input$title){
       get_historic(q_title = input$keywords_every, from = input$date[1], to = input$date[2], 
                    sort_by = input$sort,page_size = input$pagesize_every, page = input$page_every, 
                    api_key = api_key) %>%  
       mutate(url = paste0("<a href='",url,"'>",url,"</a>")) %>% 
       filter(source.name %in% input$source_every) %>% 
       unnest_tokens(word, description) %>% 
       anti_join(stop_words) %>% 
       count(word, sort = T)
        
     } else {
       get_historic(q = input$keywords_every, from = input$date[1], to = input$date[2], sort_by = input$sort,
                    page_size = input$pagesize_every, page = input$page_every, api_key = api_key) %>%  
       mutate(url = paste0("<a href='",url,"'>",url,"</a>")) %>% 
       filter(source.name %in% input$source_every) %>% 
       unnest_tokens(word, description) %>% 
       anti_join(stop_words) %>% 
       count(word, sort = T)
     }
    })
  })
  #summary - every plot
  output$every_sum <- renderPlot(
    ggplot(every_summary()[1:10,], aes(x = reorder(word,n), y = n, fill = n, label = n)) +
    geom_col() +
    coord_flip() +
    theme_bw() +
    scale_fill_gradient(low = "yellow", high = "red") +
    theme(legend.position = "none") +
    labs(title = "Top 10 words in articles", y = NULL, x = NULL) +
    geom_text(mapping = aes(label = n), hjust = -.5, col = "red", size = 4)
  )
  #cloud plot - every
  output$every_cloud <- renderWordcloud2(
    wordcloud2(every_summary())
  )
  
  #source - source table
  source_table <- reactive({
    input$run_source
    isolate({
    temp <- map_dfr(input$category,get_source,api_key)[2:5] %>% 
      mutate(url = paste0("<a href='",url,"'>",url,"</a>"))
    return(temp)})
  })
  
  output$source_table <- DT::renderDataTable({
    DT::datatable(source_table(),options = list(pageLength = 1),escape = FALSE)
    })
  #source - resources
  output$resource <- renderUI(
    includeHTML(paste0("https://en.wikipedia.org/wiki/",input$source))
  )
}

shinyApp(ui, server)
}
```

