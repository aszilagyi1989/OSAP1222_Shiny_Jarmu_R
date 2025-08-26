#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library("shiny")
library("shinyWidgets")
library("bslib")
library("shinyjs")
library("tidytable")
library("stringr")
library("lubridate")
library("ggplot2")
library("plotly")
library("GWalkR")
library("TSstudio")
library("xts")
library("plotrix")

DATA <- read.csv("https://raw.githubusercontent.com/aszilagyi1989/Shiny_CSV/refs/heads/main/OSAP1222_Jarmu.csv", sep = ";", row.names = NULL)
DATA$MHO <- str_pad(DATA$MHO, width = 2, pad = "0")
DATA$DATE <- ymd(paste(DATA$TEV, DATA$MHO, "01", sep = ";"))

ui <- page_sidebar(
  
  list(tags$head(HTML('<link rel = "icon", href = "https://map.ksh.hu/timea/images/shortcut.ico",
                                  type = "image/png" />'))),
  
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  titlePanel("Hazai és külföldi járművek eloszlása a határátkelés folyamán", windowTitle = "OSAP 1222 - Járműforgalom hazai és külföldi járművek eloszlása a határátkelés folyamán"),
  shinyjs::useShinyjs(),
  sidebar = sidebar(
    id = "sidebar",
    title = "Adatszűrő",
    width = "20%",
    height = "100%",
    dateRangeInput(
      inputId = "date",
      label = "Időszak kiválasztása",
      start = ymd("2019-01-01"),
      end = ymd(paste(str_sub(Sys.Date(), 1, 4), as.integer(str_sub(Sys.Date(), 6, 7)) - 1, "01", sep = "-")),
      min = ymd("2019-01-01"),
      max = ymd(paste(str_sub(Sys.Date(), 1, 4), as.integer(str_sub(Sys.Date(), 6, 7)) - 1, "01", sep = "-"))
    ),
    uiOutput("bordercity"),
    uiOutput("search"),
    selectInput("diagram", "Diagramtípus", c("Vonal", "Pont", "Kör", "Doboz", "Idősor", "Dekompozíciós modell"), selected = "Vonal"), # , "3D Kör"
    selectInput("forgalom", "Dekompozíció iránya", c("Belépő", "Kilépő"), selected = "Belépő")
  ),
  navset_card_underline(id = "navset", 
                        nav_panel("Vonal- vagy pontdiagram", plotlyOutput("linechart")),
                        nav_panel("Tableau", uiOutput("tableau")),
                        nav_spacer(),
                        nav_menu(title = "Hivatkozások", align = "right", 
                                 nav_item(tags$a("Személyforgalom", href = "https://aszilagyi.shinyapps.io/OSAP1222_Shiny_Szemely_R/", target = "_blank")), 
                                 nav_item(tags$a("TIMEA", href = "https://aszilagyi.shinyapps.io/TIMEA_Shiny_R/", target = "_blank"))
                        )
  )
)


server <- function(input, output, session) {
  
  output$bordercity <- renderUI({
    
    DATA %>% select(-c(TEV, MHO)) %>% group_by(DATE, MG05, MG58, MG64, MG60) %>% summarise(GADF201 = sum(GADF201 )) %>% arrange(DATE, MG05, MG58, MG64, MG60)
    
    selectInput(
      "bordercity",
      "Határátkelőhely",
      choices = unique(DATA$MG05),
      selected = unique(DATA$MG05)[1]
    )
    
  })
  
  
  output$search <- renderUI({
    
    pickerInput(
      "search",
      "Keresés",
      choices = c(unique(DATA[DATA$MG05 == input$bordercity & DATA$DATE >= input$date[[1]] & DATA$DATE <= input$date[[2]], "MG64"])),
      selected = c(unique(DATA[DATA$MG05 == input$bordercity & DATA$DATE >= input$date[[1]] & DATA$DATE <= input$date[[2]], "MG64"])),
      options = list("actions-box" = TRUE), multiple = TRUE
    )
    
  })
  
  
  output$diagram <- renderPrint({
    
    diagram <- get(input$diagram)
    
  })
  
  
  observeEvent(input$navset, {
    
    if(input$navset == "Tableau") {
      
      shinyjs::disable("diagram")
      shinyjs::disable("forgalom")
      
      subsetted <- reactive({
        
        DATA %>% select(-c(TEV, MHO)) %>% arrange(DATE, MG05, MG58, MG64, MG60) -> DATA
        DATA %>% filter(DATE >= input$date[[1]] & DATE <= input$date[[2]] & MG64 %in% input$search & MG05 %in% input$bordercity)
        
      })
      
      output$tableau <- renderUI({
        
        subsetted <- subsetted()
        colnames(subsetted) <- c("Irány", "Honosság", "Határátkelőhely", "Jármű", "Forgalom", "Dátum")
        
        renderGwalkr({ gwalkr(subsetted) })
        
      })
      
    } else {
      
      shinyjs::enable("diagram")
      if (input$diagram == "Dekompozíciós modell")
        shinyjs::enable("forgalom")
      
    }
  })
  
  
  observeEvent(input$diagram, {
    if(input$diagram != "Dekompozíciós modell") {
      
      shinyjs::disable("forgalom")
      
    } else {
      
      shinyjs::enable("forgalom")
      
    }
  })
  
  
  subsetted <- reactive({
    
    req(input$date)
    req(input$bordercity)
    req(input$search)
    req(input$diagram)
    
    DATA %>% select(-c(TEV, MHO)) %>% arrange(DATE, MG05, MG58, MG64, MG60) -> DATA
    
    if(input$navset != "Tableau" && (input$diagram == "Vonal" || input$diagram == "Pont" || input$diagram == "Kör" || input$diagram == "3D Kör" || input$diagram == "Doboz")){
      
      DATA %>% filter(DATE >= input$date[[1]] & DATE <= input$date[[2]] & MG64 %in% input$search & MG05 %in% input$bordercity)
      
    }else if(input$navset != "Tableau" && (input$diagram == "Idősor" || input$diagram == "Dekompozíciós modell")){
      
      DATA %>% filter(DATE >= input$date[[1]] & DATE <= input$date[[2]] & MG64 %in% input$search & MG05 %in% input$bordercity) -> DATA
      View(DATA)
      DATA %>% select(-c("MG05", "MG64", "MG60")) -> DATA
      DATA %>% group_by(MG58, DATE) %>% summarise(GADF201 = sum(GADF201)) -> DATA
      DATA %>% pivot_wider(names_from = "MG58", values_from = "GADF201")
      
    }else if(input$navset == "Tableau"){
      
      DATA
      
    }
    
  })
  
  output$linechart <- renderPlotly({
    
    req(input$date)
    req(input$bordercity)
    req(input$search)
    req(input$diagram)
    req(input$forgalom)
    
    if(input$navset != "Tableau"){
      
      tryCatch({
        
        subsetted <- subsetted()
        
        if(input$diagram == "Vonal"){
          
          colnames(subsetted) <- c("Irány", "Honosság", "Határátkelőhely", "Jármű", "Forgalom", "Dátum")
          map <- ggplot(data = subsetted, aes(x = Dátum, y = Forgalom, colour = Honosság, fill = Honosság)) + geom_line() + labs(x = "Dátum", y = "Forgalom", colour = "Honosság") + facet_wrap(facet = vars(subsetted$Irány, subsetted$Jármű), nrow = 2)
          ggplotly(map)
          
        }else if (input$diagram == "Pont"){
          
          colnames(subsetted) <- c("Irány", "Honosság", "Határátkelőhely", "Jármű", "Forgalom", "Dátum")
          map <- ggplot(data = subsetted, aes(x = Dátum, y = Forgalom, colour = Honosság, fill = Honosság)) + geom_point() + facet_wrap(facet = vars(subsetted$Irány, subsetted$Jármű), nrow = 2)
          ggplotly(map)
          
        }else if (input$diagram == "Kör"){
          
          colnames(subsetted) <- c("Irány", "Honosság", "Határátkelőhely", "Jármű", "Forgalom", "Dátum")
          pie1 <- plot_ly(subsetted, labels = subsetted[subsetted$Irány == "Belépő", "Jármű"], values = subsetted[subsetted$Irány == "Belépő", "Forgalom"], type = 'pie', title = "Belépő",  domain = list(x = c(0, 0.4), y = c(0.4, 1)))
          pie1 <- pie1 %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
          pie2 <- plot_ly(subsetted, labels = subsetted[subsetted$Irány == "Kilépő", "Jármű"], values = subsetted[subsetted$Irány == "Kilépő", "Forgalom"], type = 'pie', title = "Kilépő", domain = list(x = c(0.6, 1), y = c(0.4, 1)))
          pie2 <- pie2 %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
          subplot(pie1, pie2, nrows = 2, shareX = TRUE, shareY = FALSE)
          
          
        }else if (input$diagram == "3D Kör"){
          
          colnames(subsetted) <- c("Irány", "Honosság", "Határátkelőhely", "Jármű", "Forgalom", "Dátum")
          pie3D <- pie3D(subsetted[subsetted$Irány == "Belépő", "Forgalom"], labels = subsetted[subsetted$Irány == "Belépő", "Jármű"], explode = 0.1, main = "Belépő")
          pie3D
          
          
        }else if (input$diagram == "Doboz"){
          
          colnames(subsetted) <- c("Irány", "Honosság", "Határátkelőhely", "Jármű", "Forgalom", "Dátum")
          map <- ggplot(data = subsetted, aes(x = Jármű, y = Forgalom, fill = Irány, colour = Irány)) + geom_boxplot() + facet_wrap(facet = vars(subsetted$Honosság), nrow = 2)
          # map <- ggplot(data = subsetted, aes(x = Jármű)) + geom_bar() # + facet_wrap(facet = vars(subsetted$Irány), nrow = 2)
          ggplotly(map)
          
        }else if (input$diagram == "Idősor"){
          
          subsetted <- xts(subsetted[, c("Belépő", "Kilépő")], order.by = subsetted$DATE)
          # subsetted <- ts(subsetted, start = c(str_sub(input$date[[1]], 1, 4), str_sub(input$date[[1]], 6, 7)), end = c(str_sub(input$date[[2]], 1, 4), str_sub(input$date[[2]], 6, 7)), frequency = 12)
          map <- ts_plot(subsetted[, c("Belépő", "Kilépő")], title = "Idősor", Ytitle = "Forgalom", Xtitle = "Dátum")
          map
          
        }else if (input$diagram == "Dekompozíciós modell"){
          
          subsetted <- xts(subsetted[, c("Belépő", "Kilépő")], order.by = subsetted$DATE)
          # subsetted <- ts(subsetted, start = c(str_sub(input$date[[1]], 1, 4), str_sub(input$date[[1]], 6, 7)), end = c(str_sub(input$date[[2]], 1, 4), str_sub(input$date[[2]], 6, 7)), frequency = 12)
          if (input$forgalom == "Belépő")
            map <- ts_decompose(ts(subsetted[, "Belépő"], frequency = 12))
          else
            map <- ts_decompose(ts(subsetted[, "Kilépő"], frequency = 12))
          map
          
        }
        
      },
      error = function(error_message){
        
        showModal(modalDialog(
          title = "Figyelmeztetés!",
          "A kiválasztott időszak kezdő időpontja nem lehet nagyobb, mint a záróidőpont, vagy az adott időszakra még nincs adat, vagy csak egyelemű az idősor, vagy az idősori dekompozíció esetén meg kell lennie a két teljes évnek!",
          footer = modalButton("Rendben"),
          fade = TRUE
        ))
        
      })
      
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)