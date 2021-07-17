library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

url_mzcr <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19"

get_data_url <- function(urlx) {
    df <- data.table::fread(paste(url_mzcr, urlx, sep = "/"))
    return(df)
}
nakazeni_vyleceni_umrti_testy <- get_data_url("nakazeni-vyleceni-umrti-testy.csv")
ui <- dashboardPage(skin = "yellow",
    dashboardHeader(title = "Epi data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Nákaza kumulativní", tabName = "nakaza_cumul", icon = icon("medkit"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "nakaza_cumul",
                    fluidRow(
                        box(dateRangeInput("date_nakaza_cumul",
                                       "Zvolte datum",
                                       start = "2020-01-27",
                                       end = Sys.Date(),
                                       min = "2020-01-27",
                                       max = Sys.Date()), width = 3),
                        box(plotly::plotlyOutput("nakaza_kumulativni_plot"))
                    ))
        )
    )
)


server <- function(input, output, session) {
    nakaza_kumul_plot <- reactive({
        nakazeni_vyleceni_umrti_testy %>%
            filter(datum >= input$date_nakaza_cumul[1], datum <= input$date_nakaza_cumul[2])%>%
            ggplot(aes(datum, kumulativni_pocet_nakazenych))+
            geom_line(color = "gold")+
            scale_x_date(date_breaks = "1 month")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            xlab("Datum")+
            ylab("Kumulativní počet nakažených")
    })
    
    output$nakaza_kumulativni_plot <- plotly::renderPlotly({
        nakaza_kumul_plot()
    })
   
}

shinyApp(ui = ui, server = server)
