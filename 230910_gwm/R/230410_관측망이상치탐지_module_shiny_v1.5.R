###############################################################################
#                                                                             #
#            실시간 영섬권역 국가지하수관측망 이상치 검색 서비스              #
#                                                                             #
###############################################################################

# 데이터 삭제 -----------------------------------------------------------------
# rm(list = ls())

# 라이브러리 불러오기 및 경로 설정 ------------------------------------------------------

require(shiny)
require(jsonlite)     # json 양식 파일 읽어옴
require(lubridate)    # 날짜자료 제어
require(tidyverse)
require(anomalize)    # 이상치탐지
# require(data.table)   # 고속으로 자료읽어오기
require(cowplot)      # 출력된 여러개의 그림을 한개의 파일로 합치기
require(sf)           # 이상관측소 지도 추가
# require(plotly)       # 이상관측소 지도 추가
# require(doParallel)   # CPU 병렬작업
# require(rstudioapi)   # 작업경로 자동설정
require(ggh4x)        # ggplot 이중축 쉽게 생성

# shell("cls")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# CPU 병열작업 --------------------------------------------------------------------

# all_cores <- parallel::detectCores(logical = FALSE)

# doParallel::registerDoParallel(cores = all_cores)

# 샤이니 ui  ------------------------------------------------------------------

ngwUI <- function(id) {
    fluidPage(
        sidebarLayout(
            sidebarPanel(
                sliderInput(NS(id, "max_anoms"),                                # 이상값 퍼센트 설정 UI
                            label = "최대 이상치 검출 비율:",
                            min = 0.005, max = 0.1, value = 0.01, step = 0.01),
                sliderInput(NS(id, "anomal_day"),
                            label = "이상치 검출 과거일수(일수):",               # 이상값이 발생한 과거 일수 검색 UI
                            min = 1, max = 14, value = 7, step = 1),
                actionButton(NS(id, "detection"), "이상값 검출 시작",           # 이상값 탐지 버튼 UI
                             class = "btn-success",
                             icon = icon("magnifying-glass")),
                downloadButton(NS(id, "downloadtable"), "테이블 다운로드"),
                downloadButton(NS(id, "downloadgraph"), "그래프 다운로드")
                # br(),                                                          # 한줄 띄기
                # br(),                                                          # 한줄 띄기
                # h5(strong("이상치가 발생한 관측정 위치")),
                # plotlyOutput("shp")                                            # 이상치 관측정 지도
            ),
            mainPanel(
                tableOutput(NS(id, "tab1")),                                    # 이상값 결과값 출력
                br(),                                                            # 한줄 띄기
                plotOutput(NS(id, "plot"), height = "auto")                     # 이상치가 발생한 측정망 그래프 출력
            )
        )
    )
}

# 샤이니 server  -------------------------------------------------------------

ngwServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$detection, {                                                        # "Anomaly Detection!" 버튼을 누루면 아래가 실행됨
            
            # 자료 불러오기 ---------------------------------------------------------------
            # 연보자료 불러오기(현재까지는 2021년12월31일까지의 자료만 저장되어 있음)
            # df_yunbo <- data.table::fread("input/gw_yunbo.csv")                              # 기존 지하수측정연보자료를 읽어옴
            # 영섬관측망 리스트 불러오기
            # df_obs_list <- read.csv("input/230216_염섬관측정현황.csv", fileEncoding = 'euc-kr')
            # 기상(ASOS)관측위치도 불러오기
            # df_asos_list <- read.csv(file = "input/230217_META_관측지점정보_asos.csv",
            #                         fileEncoding = "euc-kr")
            
            today_ <- as.character(lubridate::today()) |>
                str_replace_all("-", "")
            
            # 지도추가
            # shp_watershed <- st_read("input/수자원단위지도(최종,GRS80)/WKMBBSN.shp")
            
            # shp_gwm <- st_read("input/230125_국가지하수_관리측정망_700개소/국가지하수관리측정망_700개소.shp")
            
            # 용량이 커서 메모리 절약을 위해 자료를 저장
            # save.image(file = "input/230220_data_all.RData")
            load("R/input/230313_data_all.RData")
            # 영섬관측망 리스트 정리
            df_gennum <- na.omit(unique(df_obs_list$GENNUM))
            # # openAPI에서 자료 불러오기 ---------------------------------------------------------------
            
            # 자료 정리하기 ---------------------------------------------------------------
            # DB자료 정리
            
            df_real <- data.frame()
            
            df_gennum <- as.data.frame(df_gennum)
            
            # 위치자료 변환
            # asos 위치
            sf_asos_list <- st_as_sf(x = df_asos_list, coords = c("경도", "위도"),
                                     crs = 4326, remove = FALSE)
            
            sf_obs_list <- st_as_sf(x = df_obs_list |> drop_na(경도),
                                    coords = c("경도", "위도"),
                                    crs = 4326, remove = FALSE)
            
            # 지하수관측정에서 가장 가까운 기상관측소 찾기 ------------------------------------------------
            
            df_obs_list_near <- sf_obs_list |>
                st_join(sf_asos_list |>
                            select(지점, 시작일, 종료일, 지점명, 위도, 경도) |>
                            rename(기상_지점 = 지점, 기상_시작일 = 시작일, 기상_종료일 = 종료일,
                                   기상_지점명 = 지점명, 기상_위도 = 위도, 기상_경도 = 경도),
                        join = st_nearest_feature) |>
                as.data.frame()
            
            # 에러나는 관측정 제외
            shiny::withProgress(message = "DB에서 자료를 불러오고 있습니다.", {         # DB에서 불러오는데 시간이 많이 걸리므로 진행바 생성
                n <- nrow(df_gennum)

                for(i in 1:nrow(df_gennum)) {

                    shiny::incProgress(1/n)

                    key <- "L0eM%2BBjtmlHYcF5I5lOvfJ%2F0TBcPs11GMO1P0ENzjkoPaJQMrx3pVqPQ%2FXNBWYBQ"
                    gennum <- df_gennum[i,]
                    startdate <- "20220101"
                    enddate <- today_

                    df_real_temp <- jsonlite::fromJSON(paste0("http://www.gims.go.kr/api/data/observationStationService/getGroundwaterMonitoringNetwork?KEY=",
                                                              key,
                                                              "&type=JSON&gennum=", gennum,
                                                              "&begindate=", startdate,
                                                              "&enddate=", enddate)) |>
                        as.data.frame()
                    df_real <- rbind(df_real, df_real_temp)
                }
            })

            df_real <- df_real |>
                rename(GENNUM = response.resultData.gennum,
                       ELEV = response.resultData.elev,
                       WTEMP = response.resultData.wtemp,
                       LEV = response.resultData.lev,
                       EC = response.resultData.ec,
                       YMD = response.resultData.ymd) |>
                select(GENNUM, YMD, ELEV, LEV, WTEMP, EC) |>
                mutate_at(vars(c("GENNUM", "ELEV", "LEV", "WTEMP", "EC")), as.numeric) |>
                mutate_at(vars("YMD"), ymd)
            
            # 연보자료와 DB자료를 합치기
            
            df_yunbo <- df_yunbo |>
                mutate_at(vars(YMD), ymd) |>
                mutate_if(is.character, as.numeric) |>
                select(-WELLNUM)
            
            df <- reactive({
                rbind(df_yunbo, df_real)
            })
            
            # 미수신 관정 확인 ---------------------------------------------------------------
            
            df_today <- reactive({
                df() |>
                    filter(YMD %in% ymd(today()))
            })
            
            df_obs_list_omit_na <- drop_na(df_obs_list, c(GENNUM))
            
            # 이상값 조회(S-ESD) 제외 관측정(2022년에 설치 또는 시설개선한 관측정)
            # 2022년에 설치한 관정의 GENNUM이 엑셀에 추가되면 추가할 것
            df_not_obs <- data.frame(not_obs = NULL)
            
            # 센서가 설치안되어 있는 관측정 제외
            df_not_obs2 <- data.frame(not_obs = c("남원산곡"))
            
            df_anomal_obs_ <- reactive({
                df_obs_list_omit_na |>
                    bind_cols(data.frame(miss = ifelse(df_obs_list_omit_na$GENNUM %in%
                                                           df_today()$GENNUM, '정상', '비정상'))) |>
                    # left_join(df_today()[, c("GENNUM")], by = 'GENNUM') |>         # 필요없는거 같은데 왜있는지 모르겠음
                    filter(miss == "비정상") |>
                    mutate(LEV = NA, ELEV = NA, WTEMP = NA, EC = NA, 이상사유 = '결측') |>
                    select(관측소명, 구분, LEV, ELEV, WTEMP, EC, GENNUM, 이상사유)
            })
            
            # 조건에 따른 금일 이상관측소 추출 ----------------------------------------------
            
            # 센서기준 물기둥 0.5m 아래에 있는 관측소(이상 관측소)
            # OPENAPI에서 센서 상부 높이 자료 제공이 없어 검색 불가
            # df_anomal_obs_temp_0 <- reactive({
            #   df_today() |>
            #     filter(LEV <= 0.5) |>
            #     left_join(df_obs_list[, c("GENNUM", "관측소명", "구분")], by = "GENNUM") |>
            #     mutate(이상사유 = "물기둥 0.5m 이하") |>
            #     select(관측소명, 구분, LEV, ELEV, WTEMP, EC, GENNUM, 이상사유)
            # })
            
            
            # 전기전도도가 0인 관측소(이상 관측소)
            df_anomal_obs_temp_1 <- reactive({
                df_today() |>
                    filter(EC == 0) |>
                    left_join(df_obs_list[, c("GENNUM", "관측소명", "구분")], by = "GENNUM") |>
                    mutate(이상사유 = "전기전도도 0") |>
                    select(관측소명, 구분, LEV, ELEV, WTEMP, EC, GENNUM, 이상사유)
            })
            
            # 수온이 0인 관측소(이상 관측소)
            df_anomal_obs_temp_2 <- reactive({
                df_today() |>
                    filter(WTEMP  ==0) |>
                    left_join(df_obs_list[, c("GENNUM", "관측소명", "구분")], by = "GENNUM") |>
                    mutate(이상사유 = "수온 0") |>
                    select(관측소명, 구분, LEV, ELEV, WTEMP, EC, GENNUM, 이상사유)
            })
            
            df_anomal_obs <- reactive({
                rbind(df_anomal_obs_(), df_anomal_obs_temp_1(), df_anomal_obs_temp_2())
            })
            
            # 자료 이상치탐지(Seasonal Hybrid ESD (S-H-ESD)) ---------------------------------
            
            # 2023년 부터 관측을 시작한 관측정의 관측자료는 삭제
            
            GENNUM <- drop_na(df_obs_list, GENNUM) |>
                filter(!관측소명 %in% df_not_obs$not_obs) |>
                select(GENNUM)
            
            GENNUM <- as.vector(GENNUM$GENNUM)
            
            df_anomal_gennum <- data.frame(NULL)
            
            shiny::withProgress(message = "이상치를 검색중입니다.", {                        # 이상치를 탐색하는데 시간이 많이 걸리므로 진행바 생성
                n <- length(GENNUM)
                
                for (i in GENNUM) {
                    
                    shiny::incProgress(1/n)
                    
                    df_temp <- reactive({
                        as_tibble(df()) |>
                            filter(GENNUM == i) |>
                            arrange(YMD) |>
                            drop_na(ELEV)
                    })
                    
                    if (nrow(df_temp()) != 0) {
                        df_anomal <- reactive({df_temp() |>
                                time_decompose(ELEV, message = FALSE) |>
                                anomalize(remainder, method = "gesd", max_anoms = input$max_anoms) |>
                                time_recompose()
                        })
                        
                        if (sum(tail(df_anomal()$anomaly, input$anomal_day) %in% "Yes") >= 1) {
                            df_anomal_gennum <- rbind(df_anomal_gennum, data.frame(gennum = i))
                        }
                    }
                }
            })
            
            # 이상값이 있는 관측소만 뽑아서 그래프로 출력 ------------------------------------------------
            id <- showNotification("표와 그래프를 그리고 있습니다.", duration = NULL)
            
            df_anomal_gennum2 <- reactive({
                as.data.frame(df_anomal_gennum) |>
                    left_join(df_obs_list[, c("관측소명", "구분", "GENNUM")], by = c("gennum" = "GENNUM")) |>
                    left_join(df_obs_list_near[, c("GENNUM", "기상_지점", "기상_시작일", "기상_지점명")],
                              by = c("gennum" = "GENNUM"))
            })
            
            authKey <- "_NL4EYPlSCOS-BGD5VgjvQ" # 인증키
            
            df_asos <- as.data.frame(NULL)
            
            for (i in 1:nrow(as.data.frame(df_anomal_gennum2()))) {
                tryCatch({
                    tm1 <- df() |> dplyr::filter(GENNUM == df_anomal_gennum2()$gennum[i]) |> # 시작일
                        filter(row_number() == 1) |>
                        select(YMD) |>
                        format("%Y%m%d") |>
                        as.character()
                    
                    tm2 <- df() |> dplyr::filter(GENNUM == df_anomal_gennum2()$gennum[i]) |> # 종료일
                        filter(row_number() == n()) |>
                        select(YMD) |>
                        format("%Y%m%d") |>
                        as.character()
                    
                    stn <- df_anomal_gennum2()$기상_지점[i]                                  # 관측지점번호
                    
                    obs <- "RN"                                                              # 관측항목
                    
                    df_asos_temp <- read.table(paste0("https://apihub.kma.go.kr/api/typ01/url/kma_sfcdd3.php?tm1=",
                                                      tm1, "&tm2=", tm2, "&obs=", obs, "&stn=", stn, "&authKey=", authKey))
                    
                    
                    df_asos <- rbind(df_asos, df_asos_temp)
                }, error = function(e) {
                    df_asos_temp <- NULL
                    warning(paste0("Error in asos staion: ", df_anomal_gennum2()$기상_지점[i]))
                })
                
            }
            
            
            
            names(df_asos)  <- c("TM", "STN", "WS_AVG", "WR_DAY", "WD_MAX", "WS_MAX",
                                 "WS_MAX_TM", "WD_INS", "WS_INS", "WS_INS_TM", "TA_AVG",
                                 "TA_MAX", "TA_MAX_TM", "TA_MIN", "TA_MIN_TM", "TD_AVG",
                                 "TS_AVG", "TG_MIN", "HM_AVG", "HM_MIN", "HM_MIN_TM",
                                 "PV_AVG", "EV_S", "EV_L", "FG_DUR", "PA_AVG", "PS_AVG",
                                 "PS_MAX", "PS_MAX_TM", "PS_MIN", "PS_MIN_TM", "CA_TOT",
                                 "SS_DAY", "SS_DUR", "SS_CMB", "SI_DAY", "SI_60M_MAX",
                                 "SI_60M_MAX_TM", "RN_DAY", "RN_D99", "RN_DUR", "RN_60M_MAX",
                                 "RN_60M_MAX_TM", "RN_10M_MAX", "RN_10M_MAX_TM", "RN_POW_MAX",
                                 "RN_POW_MAX_TM", "SD_NEW", "SD_NEW_TM", "SD_MAX", "SD_MAX_TM",
                                 "TE_05", "TE_10", "TE_15", "TE_30", "TE_50")
            
            df_asos <- df_asos |>
                select(TM, STN, RN_DAY)
            
            for (i in 1:nrow(df_anomal_gennum2())) {
                
                if (df_anomal_gennum2()$기상_지점[i] %in% unique(df_asos$STN)) {  # ASOS 값이 없는 경우는 강우자료를 제외하고 그래프를 출력
                    
                    assign(paste0("sec_", df_anomal_gennum2()[i, 1]),
                           ggh4x::help_secondary(data = drop_na(as_tibble(df()), ELEV) |>
                                                     filter(GENNUM == df_anomal_gennum2()$gennum[i]) |>
                                                     left_join(df_asos |>
                                                                   filter(STN == df_anomal_gennum2()$기상_지점[i],
                                                                          !RN_DAY == -9) |>
                                                                   mutate(YMD_ASOS = ymd(TM)) |>
                                                                   select(-TM),
                                                               by = c("YMD" = "YMD_ASOS")) |>
                                                     distinct() |>
                                                     rename(RAIN_mm = RN_DAY) |>
                                                     drop_na(RAIN_mm),
                                                 primary = ELEV, secondary = RAIN_mm)
                    )
                    
                    assign(paste0("graph_", df_anomal_gennum2()[i, 1]), # 이상값을 검색하려는 기간전 만을 대상으로 이상치를 탐색하고 제거함
                           drop_na(as_tibble(df()), ELEV) |>
                               filter(GENNUM == df_anomal_gennum2()$gennum[i]) |>
                               time_decompose(ELEV, message = TRUE) |>
                               anomalize(remainder,  method = "gesd", max_anoms = input$max_anoms) |>
                               time_recompose() |>
                               left_join(df_asos |>
                                             filter(STN == df_anomal_gennum2()$기상_지점[i],
                                                    !RN_DAY == -9) |>
                                             mutate(YMD_ASOS = ymd(TM)) |>
                                             select(-TM),
                                         by = c("YMD" = "YMD_ASOS")) |>
                               distinct() |>
                               rename(RAIN_mm = RN_DAY) |>
                               mutate(GENNUM = df_anomal_gennum2()$gennum[i]) |>
                               ggplot(aes(x = YMD, color = anomaly)) + theme_minimal() +
                               geom_point(aes(y = get(paste0("sec_",GENNUM[1]))$proj(RAIN_mm)),
                                          color = "seagreen2", shape = 8) +
                               geom_ribbon(aes(ymin = recomposed_l1, ymax = recomposed_l2),
                                           fill = "grey70", alpha = 0.5, color = NA) +
                               scale_y_continuous(sec.axis = get(paste0("sec_", df_anomal_gennum2()[i, 1]))) +
                               geom_point(aes(y = observed)) +
                               scale_color_manual(values = c("steelblue", "orangered"))+
                               theme(axis.title.x = element_blank(),
                                     axis.title.y.right = element_text(color = "seagreen2"),
                                     legend.position = "bottom") +
                               ylab("groundwater level(EL.m)")
                    )
                } else {
                    assign(paste0("graph_", df_anomal_gennum2()[i, 1]), # 이상값을 검색하려는 기간전 만을 대상으로 이상치를 탐색하고 제거함
                           drop_na(as_tibble(df()), ELEV) |>
                               filter(GENNUM == df_anomal_gennum2()$gennum[i]) |>
                               time_decompose(ELEV, message = TRUE) |>
                               anomalize(remainder,  method = "gesd", max_anoms = input$max_anoms) |>
                               time_recompose() |>
                               mutate(GENNUM = df_anomal_gennum2()$gennum[i]) |>
                               ggplot(aes(x = YMD, color = anomaly)) + theme_minimal() +
                               geom_ribbon(aes(ymin = recomposed_l1, ymax = recomposed_l2),
                                           fill = "grey70", alpha = 0.5, color = NA) +
                               geom_point(aes(y = observed)) +
                               scale_color_manual(values = c("steelblue", "orangered"))+
                               theme(axis.title.x = element_blank(),
                                     axis.title.y.right = element_text(color = "seagreen2"),
                                     legend.position = "bottom") +
                               ylab("groundwater level(EL.m)")
                    )
                }
            }
            
            # 한페이지에 모든 이상 관측소 그래프를 넣기 위해 cowplot 패키지 사용
            graph <- cowplot::plot_grid(plotlist = mget(paste0("graph_", df_anomal_gennum2()[1:nrow(df_anomal_gennum2()), 1])),  
                                       ncol = 1, align="v",                                                                
                                       labels = paste0(df_anomal_gennum2()[1:nrow(df_anomal_gennum2()), 1], "_",
                                                       df_anomal_gennum2()[1:nrow(df_anomal_gennum2()), 2], "_",
                                                       df_anomal_gennum2()[1:nrow(df_anomal_gennum2()), 3])
            )
            
            output$plot <- shiny::renderPlot({
                graph
            }, height = 300 * nrow(df_anomal_gennum2()), res = 96)
            
            df_anomal_obs_temp <- reactive({
                df_today() |> 
                    filter(GENNUM %in% df_anomal_gennum2()$gennum) |> 
                    mutate(이상사유 = "수위 이상값") |> 
                    left_join(df_obs_list[, c("GENNUM", "관측소명", "구분")], by = "GENNUM") |> 
                    select(관측소명, 구분, LEV, ELEV, WTEMP, EC, GENNUM, 이상사유)
            })
            
            df_anomal_obs2 <- reactive({
                rbind(df_anomal_obs(), df_anomal_obs_temp())
            })
            
            df_anomal_obs3 <- reactive({
                df_anomal_obs2() |> 
                    filter(!관측소명 %in% df_not_obs2$not_obs) |> 
                    arrange(관측소명, 구분) 
            })
            
            on.exit(removeNotification(id))
            
            # 테이블 출력 -----------------------------------------------------------------
            
            output$tab1 <- shiny::renderTable(df_anomal_obs3(), width = "100%")
            
            # 이상관측소 지도 출력 -------------------------------------------------------------
            # shp_anomal <- reactive({
            #     shp_gwm |> 
            #         left_join(df_obs_list[, c("관측소명", "GENNUM")], by = c("측정망" = "관측소명")) |> 
            #         filter(GENNUM %in% df_anomal_obs3()$GENNUM)
            # })
            # 
            # output$shp <- renderPlotly({
            #     ggplotly(ggplot() + theme_minimal() +
            #                  geom_sf(data = shp_watershed, color = "grey") + 
            #                  geom_sf(data = shp_anomal(), aes(fill = 측정망), color = "orangered") +
            #                  theme(legend.position = "none", axis.text = element_blank()), 
            #              res = 96)
            # })
            
            # 자료저장 --------------------------------------------------------------------
            
            # 이상치 테이블 저장 
            output$downloadtable <- shiny::downloadHandler(                                  
                filename = function() {
                    paste0(Sys.Date(), "_anomaly.csv")
                },
                content = function(file) {
                    write.csv(df_anomal_obs3(), file, row.names = FALSE, fileEncoding = "euc-kr")
                }
            )
            
            # 이상치 그래프 저장
            output$downloadgraph <- shiny::downloadHandler(
                filename = function() {
                    paste0(Sys.Date(), "_anomaly.png")
                },
                content = function(file) {
                    cowplot::save_plot(graph, filename = file, 
                                       base_height = nrow(df_anomal_gennum2()) * 4,
                                       limitsize = FALSE)
                }
            )
            
        })
    })
}

