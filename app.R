library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sp)
library(leaflet)
library(httr)
library(jsonlite)


#data####
url_mzcr <- "https://onemocneni-aktualne.mzcr.cz/api/v3/"
cnfg <- "?page=1&itemsPerPage=10000"
token <- "&apiToken=TOKEN"

get_data_url <- function(urlx) {
    if (urlx == "obce") {
        cnfg <- paste(cnfg, "&datum%5Bafter%5D=", Sys.Date(), sep = "")
    }
    data <- GET(paste(url_mzcr, urlx, cnfg, token, sep = ""))
    data <- rawToChar(data$content)
    Encoding(data) <- "UTF-8"
    list <- fromJSON(data, flatten = TRUE)
    df <- list$`hydra:member`
    df <- mutate(df, datum = as.Date(datum))
    return(df)
}
nakazeni_vyleceni_umrti_testy <- get_data_url("nakazeni-vyleceni-umrti-testy")
obce <- get_data_url("obce")
kraj_okres_nakazeni_vyleceni_umrti <- get_data_url("kraj-okres-nakazeni-vyleceni-umrti")
umrti <- get_data_url("umrti")
testy <- get_data_url("testy-pcr-antigenni")
shp_list <- readRDS("./shp-list.rds")

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
            menuItem("Úmrtí", tabName = "umrti", icon = icon("cross")),
            menuItem("Testování", tabName = "testy", icon = icon("vial")),
            menuItem("Mapa incidence", tabName = "incidence_mapa", icon = icon("map"))
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
                            plotly::plotlyOutput("nakaza_kumulativni_plot"), width = 8),
                        box(title = "Data", status = "primary", solidHeader = TRUE,
                            DT::DTOutput("nakaza_kumulativni_table"), width = 12)
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
                            plotly::plotlyOutput("nakaza_aktualni_plot"), width = 8),
                        box(title = "Data", status = "primary", solidHeader = TRUE,
                            DT::DTOutput("nakaza_aktualni_table"), width = 12)
                    )),
            tabItem(tabName = "obce",
                    fluidRow( #Obce####
                        headerPanel("Data dle obce a okresu - MOMENTÁLNĚ FUNKČNÍ JEN TABULKA AKTUÁLNÍ NÁKAZY"),
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
                        tabBox(title = "Okresní přírůstky",
                            tabPanel("Denní",
                                     h3(textOutput("okres_title")),
                                     plotly::plotlyOutput("render_okres")),
                            tabPanel("Týdenní",
                                     h3(textOutput("okres_title2")),
                                     plotly::plotlyOutput("render_okres_agr")),
                            tabPanel("Tabulka aktuální nákazy v kraji",
                                     DT::DTOutput("nakaza_kraj")),width = 12)
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
                        ),
                        box(title = "Data", status = "primary", solidHeader = TRUE,
                            DT::DTOutput("umrti_table"), width = 12)
                    )),
            tabItem(tabName = "testy",
                    fluidRow( #Testy####
                    headerPanel("Testy"),
                    tabBox(width = 12,
                           tabPanel("Celkový počet testů",
                                    h2("Veškeré testy včetně incidence"),
                                    plotly::plotlyOutput("testy_sum"),
                                    h2("Podíl pozitivních testů"),
                                    plotly::plotlyOutput("podil_pozitivnich"),
                                    h2(textOutput("korelace_text")),
                                    plotly::plotlyOutput("korelace_testu"),
                                    h2(textOutput("korelace_text2")),
                                    plotly::plotlyOutput("korelace_testu2"))
                        )
                    )
            ),
            tabItem(tabName = "incidence_mapa",
                    fluidRow( #Mapa Incidence####
                    box(title = "Mapa sedmidenní incidence dle obcí", status = "primary", solidHeader = TRUE,
                        leaflet::leafletOutput("mapa_incidence"), width = 12))
                    )
        )
    ),
    tags$head(tags$style(HTML('* {font-family: "Verdana"};')))
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
    nakaza_kumul_dt <- reactive({DT::datatable(nakazeni_vyleceni_umrti_testy %>%
                                         filter(datum >= input$date_nakaza_cumul[1],
                                                datum <= input$date_nakaza_cumul[2])%>%
                                         select(datum:kumulativni_pocet_testu),
                                         options = list(scrollX = TRUE)
                                     )})
    output$nakaza_kumulativni_table <- DT::renderDT({
        nakaza_kumul_dt()
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
            ylab("Aktivní případy")
    })
    
    output$nakaza_aktualni_plot <- plotly::renderPlotly({
        nakaza_aktualni_plot()
    })
    output$popis_actual <- renderText({
        "Graf zobrazuje aktivní případy podle hlášení hygienických stanic."
    })
    nakaza_aktual_dt <- reactive({DT::datatable(nakazeni_vyleceni_umrti_testy %>%
                                                    filter(datum >= input$date_nakaza_cumul[1],
                                                          datum <= input$date_nakaza_cumul[2])%>%
                                                    mutate(nakazeni_aktualne = kumulativni_pocet_nakazenych - kumulativni_pocet_umrti - kumulativni_pocet_vylecenych)%>%
                                                    select(datum, prirustkovy_pocet_nakazenych:nakazeni_aktualne),
                                               options = list(scrollX = TRUE)
    )})
    output$nakaza_aktualni_table <- DT::renderDT({
        nakaza_aktual_dt()
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
        paste("okres",input$okres)
    })
    output$okres_title <- renderText({
        er_okres_title()
    })
    output$okres_title2 <- renderText({
        er_okres_title()
    })
    okres_plot_agr <- eventReactive(input$button_obec, {
        kraj_okres_nakazeni_vyleceni_umrti %>%
            filter(okres_lau_kod == obce%>%
                       filter(okres_nazev == (input$okres))%>%
                       pull(okres_lau_kod)%>%
                       unique(),
                   datum >= input$date_obce[1],
                   datum <= input$date_obce[2])%>%
            mutate(nakaza = diff(c(0,kumulativni_pocet_nakazenych)))%>%
            group_by(datum = round_date(datum, unit = "week")) %>% 
            summarise(nakaza = sum(nakaza, na.rm = TRUE)) %>%
            ggplot(aes(x = datum, y = nakaza))+
            geom_line()+
            scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Datum")+
            ylab("Okresní přírůstková data - týdenní")
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
            ylab("Okresní přírůstková data - denní")
    })
    output$render_okres <- plotly::renderPlotly({
        validate(
            need(input$obec != "", "Zadejte obec a potvrdte stisknutim tlacitka")
        )
        okres_plot()
    })
    output$render_okres_agr <- plotly::renderPlotly({
        validate(
            need(input$obec != "", "Zadejte obec a potvrdte stisknutim tlacitka")
        )
        okres_plot_agr()
    })
    
    nakaza_kraj_table <- eventReactive(input$button_obec, {
        DT::datatable(obce%>%
                          filter(kraj_nazev == input$kraj,
                                 datum == Sys.Date())%>%
                          group_by(okres_nazev)%>%
                          mutate(aktivni_pripady = sum(aktivni_pripady))%>%
                          ungroup()%>%
                          distinct(okres_nazev, aktivni_pripady, datum)
        )
        

    })
    
    output$nakaza_kraj <- DT::renderDT(
        nakaza_kraj_table()
    )
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
    output$umrti_table <- DT::renderDT({
        DT::datatable(umrti, options = list(scrollX = TRUE))
    })
    
#Testy####
    output$testy_sum <- plotly::renderPlotly(
        testy %>%
            mutate(pocet_testu = pocet_PCR_testy + pocet_AG_testy)%>%
            ggplot()+
            geom_line(aes(datum, y = pocet_testu, color = "Pocet testu"))+
            geom_line(aes(datum, y = incidence_pozitivni, color = "Incidence pozitivni"))+
            scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Datum")+
            ylab("Počet testů")
    )
    
    testy <- testy %>%
        mutate(pocet_testu = pocet_PCR_testy + pocet_AG_testy)%>%
        mutate(podil_pozitivnich_testu = incidence_pozitivni / pocet_testu * 100)
    
    
    output$podil_pozitivnich <- plotly::renderPlotly(
        testy %>%
            ggplot(aes(datum, podil_pozitivnich_testu))+
            geom_line()+
            scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")+
            scale_y_continuous(labels = function(x) paste0(x, "%"))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Datum")+
            ylab("Podíl pozitivních testů")
    )
    
    output$korelace_testu <- plotly::renderPlotly(
        testy %>%
            ggplot(aes(pocet_testu, podil_pozitivnich_testu))+
            geom_point(shape = "1")+
            geom_smooth(method = "lm", se = FALSE)+
            scale_y_continuous(labels = function(x) paste0(x, "%"))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Počet testů")+
            ylab("Podíl pozitivních testů")
    )
    
    output$korelace_text <- renderText(
            paste("Korelace množství testů a podílu pozitivních, R = ",
                  cor(testy$pocet_testu, testy$podil_pozitivnich_testu)))
    output$korelace_testu2 <- plotly::renderPlotly(
        testy %>%
            ggplot(aes(pocet_testu, incidence_pozitivni))+
            geom_point(shape = "1")+
            geom_smooth(method = "lm", se = FALSE)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("Počet testů")+
            ylab("Počet pozitivních testů")
    )
    
    output$korelace_text2 <- renderText(
        paste("Korelace množství testů a počtu pozitivních, R = ",
              cor(testy$pocet_testu, testy$incidence_pozitivni)))
# mapa####
df_obce <- shp_list$obce
obce_dnes <- obce %>% filter(datum == Sys.Date())
df_obce@data <- df_obce@data %>% left_join(obce_dnes, by = c("id" = "obec_kod"))
df_obce@data <- df_obce@data %>% mutate(incidence = nove_pripady_7_dni / (people_count / 10^5))
natural.interval = classInt::classIntervals(df_obce$incidence[!is.na(df_obce$incidence)], n = 6, style = "kmeans")$brks %>% unique %>% round(0)
pal <- leaflet::colorBin("YlOrRd", domain = df_obce$incidence, bins = natural.interval, na.color = "white", pretty = FALSE)
popup <- sprintf("<strong>%s</strong><br/>
              %s sedmidenní Incidence na 100 000 obyvatel", df_obce@data$obec_nazev, df_obce@data$incidence)
sl = as(shp_list$obce, "SpatialLines")
xsl <- rgeos::gUnion(sl,sl)
output$mapa_incidence <- leaflet::renderLeaflet({
    leaflet::leaflet(df_obce) %>%
        addProviderTiles("CartoDB.Positron") %>% addPolygons(fillColor = ~pal(incidence), weight = 2, opacity = 1, color = "white", dashArray = "", fillOpacity = 0.8,
                                                             highlight = highlightOptions(weight = 3, color = "#666", dashArray = "",
                                                                                          fillOpacity = 0.8, bringToFront = TRUE, sendToBack = TRUE),
                                                             popup = ~popup) %>%
        leaflet::addLegend( pal = pal, values = factor(df_obce$incidence %>% unique), opacity = 0.7,  position = "topright")%>%
        addPolylines(data = xsl, weight = 1, opacity = 0.1, color="black")
})
}



shinyApp(ui = ui, server = server)


