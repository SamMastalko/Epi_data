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
            menuItem("Nákaza", tabname = "nakaza", icon = icon("medkit"),
                menuSubItem("Nákaza kumulativní", tabName = "nakaza_cumul"),
                menuSubItem("Aktuálně nakažených", tabName = "nakaza_actual")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "nakaza_cumul",
                    fluidRow(
                        headerPanel("Kumulativní nákaza"),
                        box(title = "Možnosti", status = "warning", solidHeader = TRUE,
                            dateRangeInput("date_nakaza_cumul",
                                       "Zvolte datum",
                                       start = "2020-01-27",
                                       end = Sys.Date(),
                                       min = "2020-01-27",
                                       max = Sys.Date()),
                            textOutput("popis_cumul"),
                            width = 4),
                        box(title = "Graf", status = "primary", solidHeader = TRUE,
                            plotly::plotlyOutput("nakaza_kumulativni_plot"), width = 8)
                    )),
            tabItem(tabName = "nakaza_actual",
                    fluidRow(
                        headerPanel("Aktuální nákaza"),
                        box(title = "Možnosti", status = "warning", solidHeader = TRUE,
                            dateRangeInput("date_nakaza_actual",
                                           "Zvolte datum",
                                           start = "2020-01-27",
                                           end = Sys.Date(),
                                           min = "2020-01-27",
                                           max = Sys.Date()),
                            textOutput("popis_actual"),
                            width = 4),
                        box(title = "Graf", status = "primary", solidHeader = TRUE,
                            plotly::plotlyOutput("nakaza_aktualni_plot"), width = 8)
                    ))
        )
    )
)


server <- function(input, output, session) {
    nakaza_kumul_plot <- reactive({
        nakazeni_vyleceni_umrti_testy %>%
            filter(datum >= input$date_nakaza_cumul[1], datum <= input$date_nakaza_cumul[2])%>%
            ggplot(aes(datum, kumulativni_pocet_nakazenych))+
            geom_line(color = "gray16")+
            scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Datum")+
            ylab("Kumulativní počet nakažených")
    })
    
    output$nakaza_kumulativni_plot <- plotly::renderPlotly({
        nakaza_kumul_plot()
    })
    output$popis_cumul <- renderText({
        "Graf zobrazuje kumulativní nákazu podle hlášení hygienických stanic."
    })
    
    nakaza_aktualni_plot <- reactive({
        nakazeni_vyleceni_umrti_testy %>%
            mutate(nakazeni_aktualne = kumulativni_pocet_nakazenych - kumulativni_pocet_umrti - kumulativni_pocet_vylecenych)%>%
            filter(datum >= input$date_nakaza_actual[1], datum <= input$date_nakaza_actual[2])%>%
            ggplot(aes(datum, nakazeni_aktualne))+
            geom_line(color = "gray16")+
            scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Datum")+
            ylab("Aktuální počet nakažených")
    })
    
    output$nakaza_aktualni_plot <- plotly::renderPlotly({
        nakaza_aktualni_plot()
    })
    output$popis_actual <- renderText({
        "Graf zobrazuje aktuální nákazu podle hlášení hygienických stanic."
    })
   
}

shinyApp(ui = ui, server = server)
