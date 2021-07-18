library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

#data####
url_mzcr <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19"

get_data_url <- function(urlx) {
    df <- data.table::fread(paste(url_mzcr, urlx, sep = "/"), encoding = "UTF-8")
    return(df)
}
nakazeni_vyleceni_umrti_testy <- get_data_url("nakazeni-vyleceni-umrti-testy.csv")
obce <- get_data_url("obce.csv")
kraj_okres_nakazeni_vyleceni_umrti <- get_data_url("kraj-okres-nakazeni-vyleceni-umrti.csv")
umrti <- get_data_url("umrti.csv")

#ui####
ui <- dashboardPage(skin = "yellow",
    dashboardHeader(title = "Epi data"),
    dashboardSidebar(
        sidebarMenu( #Menu####
            menuItem("Nákaza", tabname = "nakaza", icon = icon("medkit"),
                menuSubItem("Nákaza kumulativní", tabName = "nakaza_cumul"),
                menuSubItem("Aktivní případy", tabName = "nakaza_actual")
                ),
            menuItem("Přehled podle obcí", tabName = "obce", icon = icon("map-marked-alt")),
            menuItem("Úmrtí", tabName = "umrti", icon = icon("cross"))
            
        )
    ),
    dashboardBody(
        tabItems( #Nakaza####
            tabItem(tabName = "nakaza_cumul",
                    fluidRow( #Kumulativni####
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
                    fluidRow( #Aktivni####
                        headerPanel("Aktivní případy"),
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
                    )),
            tabItem(tabName = "obce",
                    fluidRow( #Obce####
                        headerPanel("Data dle obcí"),
                        box(title = "Možnosti", status = "warning", solidHeader = TRUE,
                            selectizeInput("kraj", "Zvolte kraj", choices = sort(unique(obce$kraj_nazev))),
                            selectizeInput("okres", "Zvolte okres", choices = NULL),
                            selectizeInput("obec", "Zvolte obec", choices = NULL),
                            dateRangeInput("date_obce",
                                           "Zvolte datum",
                                           start = "2020-03-01",
                                           end = Sys.Date(),
                                           min = "2020-03-01",
                                           max = Sys.Date()),
                            actionButton("button_obec", "Zobrazit data"),
                            width = 3),
                        box(title = "Aktivní případy v obci", status = "primary", solidHeader = TRUE,
                            h3(textOutput("obec_title")),
                            plotly::plotlyOutput("aktivni_obec"), width = 9),
                        box(title = "Okresní data", status = "primary", solidHeader = TRUE,
                            h3(textOutput("okres_title")),
                            plotly::plotlyOutput("render_okres"), width = 12)
                    )
                
            ),
            tabItem(tabName = "umrti",
                    fluidRow( #Umrti####
                        headerPanel("Úmrtí"),
                        tabBox(width = 12,
                            tabPanel("Kumulativně",
                                     plotly::plotlyOutput("umrti_kumul")),
                            tabPanel("Přírůstkově",
                                     plotly::plotlyOutput("umrti_rust")),
                            tabPanel("Dle věku a pohlaví",
                                     plotly::plotlyOutput("umrti_vek_pohlavi"))
                        )
                    ))
        )
    )
)


server <- function(input, output, session) {
#Nakaza####
#Kumulativni####
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
#Aktualni#####  
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
    
#Input obce/okres####
    observe({
        updateSelectizeInput(session, "okres", choices = obce %>%
                                 filter(kraj_nazev == input$kraj)%>%
                                 pull(okres_nazev)%>%
                                 unique()%>%
                                 sort(), server = TRUE)
    })

    observe({
        updateSelectizeInput(session, "obec", choices = obce %>%
                                 filter(okres_nazev == input$okres)%>%
                                 pull(obec_nazev)%>%
                                 unique()%>%
                                 sort(), server = TRUE)
    })
#Obce plot#### 
    er_obec_title <- eventReactive(input$button_obec, {
        paste(input$obec)
        })
    output$obec_title <- renderText({
        er_obec_title()
    })
    aktivni_obec_plot <- eventReactive (input$button_obec,{
        obce %>%
            filter(kraj_nazev == input$kraj,
                   okres_nazev == input$okres,
                   obec_nazev == input$obec,
                   datum >= input$date_obce[1],
                   datum <= input$date_obce[2])%>%
            ggplot(aes(datum, aktivni_pripady))+
            geom_line(color = "gray16")+
            scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Datum")+
            ylab("Aktuální počet nakažených")
    })
    output$aktivni_obec <- plotly::renderPlotly({
        validate(
            need(input$obec != "", "Zadejte obec a potvrdte stisknutim tlacitka")
        )
        aktivni_obec_plot()
    })
#Okres####
    er_okres_title <- eventReactive(input$button_obec, {
        paste("Okres",input$okres)
    })
    output$okres_title <- renderText({
        er_okres_title()
    })
    okres_plot <- eventReactive(input$button_obec, {
        kraj_okres_nakazeni_vyleceni_umrti %>%
            filter(okres_lau_kod == obce%>%
                       filter(okres_nazev == (input$okres))%>%
                       pull(okres_lau_kod)%>%
                       unique(),
                   datum >= input$date_obce[1],
                   datum <= input$date_obce[2])%>%
            mutate(nakaza = diff(c(0,kumulativni_pocet_nakazenych)))%>%
            ggplot(aes(x = datum, y = nakaza))+
            geom_line()+
            scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Datum")+
            ylab("Přírůstková data")
    })
    output$render_okres <- plotly::renderPlotly({
        validate(
            need(input$obec != "", "Zadejte obec a potvrdte stisknutim tlacitka")
        )
        okres_plot()
    })
#umrti####
    umrti_kumul_plot <- nakazeni_vyleceni_umrti_testy %>%
        ggplot(aes(x = datum, y = kumulativni_pocet_umrti))+
        geom_line()+
        scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        xlab("Datum")+
        ylab("Kumulativní počet úmrtí")
    
    output$umrti_kumul <- plotly::renderPlotly({
        umrti_kumul_plot
    })
    
    umrti_rust_plot <- umrti %>%
        group_by(datum)%>%
        summarise(n = n())%>%
        ggplot(aes(datum, n))+
        geom_line()+
        scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        xlab("Datum")+
        ylab("Denní počet úmrtí")
    
    output$umrti_rust <- plotly::renderPlotly({
        umrti_rust_plot
    })
    
    umrti_vek_pohlavi_plot <- umrti %>%
        ggplot(aes(x = as.factor(vek), fill = pohlavi))+
        geom_bar(position = "dodge")+
        scale_fill_manual(values = c("cyan4", "firebrick4"))+
        xlab("Věk")+
        ylab("Počet úmrtí")+
        scale_x_discrete(breaks = as.character(seq(10, 100, by = 10)))
    
    output$umrti_vek_pohlavi <- plotly::renderPlotly({
        umrti_vek_pohlavi_plot
    })
}

shinyApp(ui = ui, server = server)


