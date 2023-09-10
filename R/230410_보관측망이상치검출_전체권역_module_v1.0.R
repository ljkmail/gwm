library(httr)
library(rvest)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(progress)
library(shiny)

# 보 코드 크롤링을 위한 테이블 ----------------------------------------------------------------

weir_code <- tibble::tribble(
    ~ area, ~ code,
    "hangang", "GCM",
    "hangang", "YJM",
    "hangang", "IPM",
    "gmgang", "SZM",
    "gmgang", "GZH",
    "gmgang", "BZH",
    "yongsum", "JSM",
    "yongsum", "SCM",
    "nakdong", "SJM",
    "nakdong", "NDM",
    "nakdong", "GMM",
    "nakdong", "CGM",
    "nakdong", "GJM",
    "nakdong", "DSM",
    "nakdong", "HCM",
    "nakdong", "HAM"
)

# ui  ---------------------------------------------------------------------

boUI <- function(id) {
    sidebarLayout(
        sidebarPanel(
            radioButtons(NS(id, "area"), label = "유역을 선택해 주세요",
                         choiceNames = c("한강", "금강", "영섬", "낙동강"),
                         choiceValues = c("hangang", "gmgang", "yongsum", "nakdong"),
                         selected = "yongsum", 
                         inline = TRUE),
            actionButton(NS(id, "weir_detiction"), label = "미수신 검출 시작",                    
                         class = "btn-success",
                         icon = icon("magnifying-glass"))
        ),
        mainPanel(
            h3("미수신 보관측정 리스트"),
            tableOutput(NS(id, "df_error")),
            h3("전체 보관측정 리스트"),
            dataTableOutput(NS(id, "df_url4"))
        )
        
    )
}

# server ------------------------------------------------------------------
boServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        
        # 보 코드 크롤링 ----------------------------------------------------------------
        
        ch_weir_code <- reactive(
            weir_code %>% filter(area %in% input$area) %>% 
                mutate(code_text = paste0(code, "-\\d\\d\\d")))
        
        url_weir <- "http://www.gims.go.kr/odmUnderground?resultId=JSM-008&fromDate=2023-04-01&toDate=2023-04-03"
        
        get_url_weir <- GET(url_weir)
        
        df_url_weir <- reactive(read_html(get_url_weir) %>% 
                                    html_nodes(xpath = '//*[@id="bo"]/comment()') %>% 
                                    html_text() %>% 
                                    str_extract_all(pattern = ch_weir_code()$code_text) %>% 
                                    unlist() %>% 
                                    unique()
        )
        
        observeEvent(input$weir_detiction, {
            shiny::withProgress(message = "미수신관정을 검색중입니다.", {
                
                n <- length(df_url_weir())
                
                df_url <- list()
                
                # 관측자료 불러오기 ---------------------------------------------------------------     
                
                for (i in df_url_weir()) {
                    
                    tryCatch({
                        
                        shiny::incProgress(1/n)
                        
                        url <- paste0("http://www.gims.go.kr/odmUndergroundChartJson?resultId=", 
                                      i, "&fromDate=", Sys.Date(), "&toDate=", Sys.Date())
                        
                        get_url <- GET(url)
                        
                        df_url[[i]] <- fromJSON(as.character(get_url)) %>% 
                            .[["list"]] %>% 
                            mutate_at(vars(valuedatetimech), ymd_hm) %>% 
                            mutate(gennum = i) %>% 
                            dplyr::select(gennum, everything(), -valueid, -resultid, -valuedatetime , -type)
                    }, error = function(e) {
                        df_url[[i]] <- NULL
                        warning(paste0("관측소의 자료가 없습니다.: ", i))  
                    })
                    
                }
                
                
            })
            
            df_url2 <- do.call(rbind, df_url) %>% 
                pivot_wider(names_from = datatype, values_from = datavalue) %>% 
                rename(gw_level = `01`, river_up = `02`, river_down = `03`, 
                       rain = `04`, temper = `05`)
            
            # 이상관측값 확인 ----------------------------------------------------------------
            
            func_str <- function(x) str_sub(x, start = 1, end = 10)
            
            df_url3 <- df_url2 %>% 
                mutate_at(vars(valuedatetimech), func_str) %>% 
                group_by(gennum, valuedatetimech) %>% 
                summarise(gw_level = round(mean(gw_level, na.rm = T), 2), 
                          river_up = round(mean(river_up, na.rm = T), 2), 
                          river_down = round(mean(river_down, na.rm = T), 2), 
                          rain = round(sum(rain), 2), 
                          temper = round(mean(temper, na.rm = T), 2)) %>% 
                rename(관정명 = gennum, 날짜 = valuedatetimech, 지하수위 = gw_level, 
                       상류하천 = river_up, 하류하천 = river_down, 강우량 = rain,
                       수온 = temper)
            
            output$df_url4 <- renderDataTable(df_url3)
            
            # 에러 목록
            output$df_error <- renderTable({
                df_url3 %>% 
                    filter(지하수위 %in% NaN)
            })
            
        })
    })
}

