#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(RSelenium)
library(curl)
library(stringr)
library(DT)
library(data.table)
library(reshape2)
library("dplyr")
library(rhandsontable)

urllist <- read.delim("0_urllist - 1.tsv")
nofi2.2 <-""
nofi.2 <- ""
point <- ""
hotel <- ""
checkintime <- ""
score <- ""
kutikomi <- ""
koukuken <- ""
l <- ""
back_ref <- "" 
repstr <- ""
temp.r <- "";temp.j <- ""
elistR <- NULL;elistJ <- NULL
t1 <<- NULL;t1.1 <- NULL;t1.2 <- NULL;t1.3 <- NULL;t1.3.2 <- NULL
rt <- NULL;jt <- NULL;ot <- NULL

#プロセス削除
#MacとWindowsでWevdriver起動方法が異なります。
if(length(grep("apple",sessionInfo()$platform )) > 0){
    command <- "lsof -i:4444 | grep java | awk '{print $2}'"
    port <- system("lsof -i:4444 | grep java | awk '{print $2}'",intern=TRUE)[1]
    system(paste("kill",port),intern=TRUE)[1]
    binman::list_versions("chromedriver")
    rD <- rsDriver(chromever = "77.0.3865.40",port=4444L)
    remDr <- rD[["client"]]
}else{
    remDr <- remoteDriver(port=4444L, browserName = "chrome")
    remDr$open()
}

#
r_logic <- function(hotel,plancapmode,aftermonth,elistR,kakoyoyakuFlag,kutikomiFlag,koukukenFlag,additionalFlag){
    Sys.sleep(1)
    homenurl <- remDr$getCurrentUrl() 
    if(length(remDr$findElements(value = paste("//*[@id='htlCntntArea']/div/article[1]/ul/li[@data-locate='hotel-checkin']/dl/dd",sep="") )) > 0){
        checkintime <-  remDr$findElement(value = paste("//*[@id='htlCntntArea']/div/article[1]/ul/li[@data-locate='hotel-checkin']/dl/dd",sep="") )$getElementText()
    } else {
        checkintime <- "null"
    }
    if(length(remDr$findElements(value = paste("//*[@id='navPlan']/a",sep=""))) > 0){
        remDr$findElement(value = paste("//*[@id='navPlan']/a",sep="") )$clickElement()
    }
    koukuken <- ""
    if(koukukenFlag){ 
        if(length(remDr$findElements(value = paste("//*[@class='rtconds packagebox']",sep="") )) >0){
            koukuken <- remDr$findElement(value = paste("//*[@class='rtconds packagebox']",sep="") )$getElementText()
            remDr$findElement(value = paste("//*[@class='rtconds packagebox']",sep="") )$clickElement()
            Sys.sleep(1)    
            remDr$screenshot(display = FALSE, file =paste("./WWW/Rakuten",hotel,"koukuken.png",sep="_"))
        }
    }
    
    no <-  length(remDr$findElements(value = paste("//*[@class='htlPlnCsst mnmLstVw']/li",sep="") ))
    j <- 1
    j2 <- 1
    homenurl <- remDr$getCurrentUrl() 
    if(no > 0){
        for (j2 in 1: no) {        
            no2 <-  length(remDr$findElements(value = paste("//*[@class='htlPlnCsst mnmLstVw']/li[", j2 ,"]/div/div[2]/ul/li",sep="") ))
            for (j in 1: no2) {
                name  <- remDr$findElement(value = paste("//*[@id='RthNameArea']/h2/a",sep="") )$getElementText()   
                planId <- remDr$findElement(value = paste( "//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]",sep="") )$getElementAttribute('id')
                plan  <- remDr$findElement(value = paste( "//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/h4",sep="") )$getElementText()    
                date  <- ifelse(length(remDr$findElements(value = paste( "//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[1]/p",sep="") )) >0,
                    remDr$findElement(value = paste( "//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[1]/p",sep="") )$getElementText() ,"Room sale")
                
                roomId  <- ifelse(length(remDr$findElements(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/h6",sep=""))) >0,
                                remDr$findElement(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]",sep=""))$getElementAttribute('id'),"Room sale")
                
                room  <- ifelse(length(remDr$findElements(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/h6",sep=""))) >0,
                                remDr$findElement(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/h6",sep=""))$getElementText(),"Room sale")
                
                meal   <- ifelse(length(remDr$findElements(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/dl/dd[2]/span[1]",sep=""))) >0,
                                 remDr$findElement(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/dl/dd[2]/span[1]",sep=""))$getElementText(),"-")
                
                ppl   <-    ifelse(length(remDr$findElements(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/dl/dd[2]/span[2]",sep=""))) >0,
                                   remDr$findElement(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/dl/dd[2]/span[2]",sep=""))$getElementText(),"-")
                
                paymentmethod <- ifelse(length(remDr$findElements(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/dl/dd[2]/span[3]",sep=""))) >0,
                                        remDr$findElement(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[", j2,"]/div/div[2]/ul/li[",  j  ,"]/div/dl/dd[2]/span[3]",sep=""))$getElementText(),"-")

                cancelPolicy<- "-";noshowPolicy<- "-";receiptPolicy<-"-"
                if(additionalFlag){
                    cale  <- remDr$findElement(value = paste("//*[@id='htlCntntArea']/div[1]/ul/li[",j2,"]/div/div[2]/ul/li[",  j  ,"]/ul/li/dl/dd/span[2]/a",sep="") )$getElementAttribute('href')
                    remDr$navigate(as.character(cale))
                    Sys.sleep(1)
                    cancelPolicy <-  gsub("\n","",remDr$findElement(value = paste("//*[@data-locate='cancelPolicy']/parent::node()",sep="") )$getElementText()) 
                    noshowPolicy <-  gsub("\n","",remDr$findElement(value = paste("//*[@data-locate='noshowPolicy']/parent::node()",sep="") )$getElementText() )
                    receiptPolicy <-  gsub("\n","",remDr$findElement(value = paste("//*[@data-locate='receiptPolicy']/parent::node()",sep="") )$getElementText()) 
                    remDr$goBack() 
                    Sys.sleep(2)
                }
                elistR <- rbind(elistR,c("Rakuten",j, hotel,name,paste(planId,plan,sep=":"), paste(roomId,room,sep=":"), ppl, meal, date,checkintime,"" ,koukuken,paymentmethod, homenurl,"no yet",cancelPolicy,noshowPolicy,receiptPolicy))                  
            }
        }
    }else{
        if(length(remDr$findElements(value = paste("//*[@id='errorArea']/p",sep=""))) >0){
            error <- remDr$findElement(value = paste("//*[@id='errorArea']/p",sep=""))$getElementText()
            error.2 <- gsub("\n","",error)
            elistR <- rbind(elistR, c("Rakuten","E1", "hotel", "-", "-",error.2 ,"-" ,"-","-" ,"-" ,"-","-" ,"-","-"))                           
            t1.3 <<- "a"
        } else{
            error <- remDr$findElement(value = paste("//*[@id='listError']/p",sep=""))$getElementText()
            error.2 <- gsub("\n","",error)
            elistR <- rbind(elistR, c("Rakuten","E1", "hotel", "-", "-",error.2 ,"-" ,"-","-" ,"-" ,"-","-" ,"-","-"))                    
            t1.3 <<- "b"
        }
    }
    return(elistR)
}
j_logic <- function(hotel,plancapmode,aftermonth,elistJ,kutikomiFlag,koukukenFlag,pplFlag,additionalFlag){
    #登録中を考慮
    Sys.sleep(1)
    homenurl <- remDr$getCurrentUrl() 
    errorFlag2  <- length(remDr$findElements(value = paste("//*[@id='check-infobox-txt01']",sep="")))
    if(errorFlag2 == 0){  
        score <- "-"
        kutikomi <- "-"
        #ホテル-プラン表示
        if(length(remDr$findElements(value = paste("//*[@id='yado_header_tab_menu']/ul/li[2]/a/span",sep="") )) > 0){
            remDr$findElement(value = paste("//*[@id='yado_header_tab_menu']/ul/li[2]/a/span",sep="") )$clickElement()
        }
        planurl <- remDr$getCurrentUrl() 
        
        koukuken <- ""
        if(koukukenFlag){             
            if(length(remDr$findElements(value = paste("//*[@class='jpackyodonm clearfix']",sep="") )) >0){
                koukuken <- "Has package"
                remDr$screenshot(display = FALSE, file =paste("./WWW/Jalan",hotel,"koukuken.png",sep="_"))
            }
        }
        
        no <- length(remDr$findElements(value ="//*[@class='styleguide-scope p-searchResults']/ul/li"))    
        Sys.sleep(1)  
        homenurl <- remDr$getCurrentUrl() 
        l  <- 1
        l2  <- 1
        if(no == 0){
            no <- 1
        }
        for (l2 in 1 : no) {
            no2 <- length(remDr$findElements(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[2]/table/tbody/tr",sep=""))) -1  
            for (l in 1: no2) {
                Sys.sleep(1) 
                name  <- remDr$findElement(value = paste("//*[@id='yado_header_hotel_name']/a",sep="") )$getElementText()   
                planId  <- ifelse(length(remDr$findElements(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]",sep="")) ) >0 ,
                                  remDr$findElement(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]",sep=""))$getElementAttribute('data-plancode'),"-")
                
                plan  <- ifelse(length(remDr$findElements(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[1]/p",sep=""))  ) >0 ,
                                remDr$findElement(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[1]/p",sep=""))$getElementText()  ,"-")
                date  <- "XXX"
                room  <- ifelse(length(remDr$findElements(value = paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[2]/table/tbody/tr[",l + 1,"]/td[1]/div[1]/a",sep=""))) >0,
                                remDr$findElement(value = paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[2]/table/tbody/tr[",l + 1,"]/td[1]/div[1]/a",sep=""))$getElementText(),"-")
                
                roomId  <- ifelse(length(remDr$findElements(value = paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[2]/table/tbody/tr[",l + 1,"]/td[1]/div[1]/a",sep=""))) >0,
                                  remDr$findElement(value = paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[2]/table/tbody/tr[",l + 1,"]",sep=""))$getElementAttribute('id'),"-")
                
                meal   <- ifelse(length(remDr$findElements(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2,"]/div[2]/div/div[2]/div[2]/dl[1]/dd",sep="" ))) >0,
                                 remDr$findElement(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2,"]/div[2]/div/div[2]/div[2]/dl[1]/dd",sep="" ))$getElementText(),"-")
                checkintime <-  ifelse(length(remDr$findElements(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2,"]/div[2]/div/div[2]/div[2]/dl[2]/dd",sep="" ))) >0,
                                       remDr$findElement(value =paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2,"]/div[2]/div/div[2]/div[2]/dl[2]/dd",sep="" ))$getElementText(),"-")
                point   <-ifelse(length(remDr$findElements(value = paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[2]/table/tbody/tr[",l + 1,"]/td[1]/div[1]/ul/li[@class='c-label c-label--orange p-searchResultItem__horizontalLabel overwritePointLabel']",sep="") )) >0,
                                 remDr$findElement(value = paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[2]/table/tbody/tr[",l + 1,"]/td[1]/div[1]/ul/li[@class='c-label c-label--orange p-searchResultItem__horizontalLabel overwritePointLabel']",sep="") )$getElementText(),"-")
                ppl   <-    "XXX"                
                if(pplFlag){
                    cale <-remDr$findElement(value = paste("//*[@class='styleguide-scope p-searchResults']/ul/li[", l2, "]/div[2]/table/tbody/tr[",l + 1,"]/td[1]/div[1]/a",sep=""))$getElementAttribute('href')
                    remDr$navigate(as.character(cale))
                    pplLength <- length(remDr$findElements(value = paste( "//*[@id='dyn_adult_num']/option",sep="") ))
                    if(pplLength == 9){
                        i3 <- 1
                        ppltemp <- NULL
                        for(i3 in 1 :9){
                            remDr$findElement(value = paste( "//*[@id='dyn_adult_num']",sep="") )$clickElement()    
                            remDr$findElement(value = paste( "//*[@id='dyn_adult_num']/option[", i3,"]",sep="") )$clickElement()    
                            remDr$findElement(value = paste( "//*[@id='feeBtn']",sep="") )$clickElement()
                            if(length(remDr$findElements(value = paste( "//*[@class='p-details01__chargeInfo__title']",sep="") )) == 0){
                                ppltemp <- rbind(ppltemp,i3)
                            }else{
                                #paste(i3, remDr$findElement(value = paste( "//*[@class='p-details01__chargeInfo__title']",sep="") )$getElementText()   )
                            }
                            remDr$goBack() 
                        }
                        min <- min(ppltemp)
                        max <- max(ppltemp)
                    }else{
                        min  <- remDr$findElement(value = paste( "//*[@id='dyn_adult_num']/option[1]",sep="") )$getElementText()    
                        max  <- remDr$findElement(value = paste( "//*[@id='dyn_adult_num']/option[", pplLength, "]",sep="") )$getElementText()    
                    }
                    ppl <- paste(min,max,sep="~")
                    remDr$goBack() 
                }    
                paymentmethod <- "-";child <- "-";checkinby<- "-";cancelPolicy <- "-";noshowPolicy<- "-";receiptPolicy<- "-"
                #決済
                if(additionalFlag){
                    cale <-remDr$findElement(value = paste("//*[@class='p-planTable p-searchResultItem__planTable']/tbody/tr[",l + 1,"]/td[1]/div[1]/a",sep=""))$getElementAttribute('href')
                    remDr$navigate(as.character(cale))
                    Sys.sleep(1)
                    
                    paymentmethod   <-ifelse(length(remDr$findElements(value = paste("//*[@class='s12_30b']/parent::node()/parent::node()",sep="") )) >0,
                                             remDr$findElement(value = paste("//*[@class='s12_30b']/parent::node()/parent::node()",sep="") )$getElementText() %>% str_replace("\n"," : "),"たぐなし")
                    
                    child   <-ifelse(length(remDr$findElements(value = paste("//*[@class='box02 s12_30']",sep="") )) >0,
                                     remDr$findElement(value = paste("//*[@class='box02 s12_30']",sep="") )$getElementText() %>% str_replace_all("\n","_"),"") %>% str_replace("小学生 受け入れなし","X") %>% str_replace("幼児：食事・布団あり 受け入れなし","X") %>% str_replace("幼児：食事あり 受け入れなし","X") %>% str_replace("幼児：布団あり 受け入れなし","X") %>% str_replace("幼児：食事・布団なし 受け入れなし","X")
                    
                    checkinby <- ifelse(length(remDr$findElements(value = paste("//*[@class='s12_30']",sep="") )) >0,
                                        remDr$findElement(value = paste("//*[@class='s12_30']",sep="") )$getElementText()  %>% str_replace("幼児：食事・布団なし 受け入れなし","X"),"-")
                    
                    #cancelPolicy　<- remDr$findElement(value = paste("//*[contains(text(), '予約金')][@class='td03']/parent::node()",sep="") )$getElementText() %>% str_replace_all("\n","_")
                    #cancelPolicyLoc　<- remDr$findElement(value = paste("//*[contains(text(), '予約金')][@class='td03']/parent::node()",sep="") )
                    #noshowPolicy  <- remDr$findElement(value = paste("//*[contains(text(), '無連絡キャンセル')][@class='td03']/parent::node()/parent::node()",sep="") )$getElementText() %>% str_replace_all("\n","_")
                    
                    #if(length(remDr$findElements(value = paste("//*[contains(text(), '無連絡キャンセル')][@class='td03']/parent::node()/parent::node()/parent::node()/parent::node()/table",sep="")))){
                    #    noshowPolicy <- paste(noshowPolicy,
                    #                          remDr$findElement(value = paste("//*[contains(text(), '無連絡キャンセル')][@class='td03']/parent::node()/parent::node()/parent::node()/parent::node()/table[2]",sep="_"))$getElementText(),
                    #                          sep="_")
                    #}
                    
                    receiptPolicy <- ""
                    
                    #remDr$executeScript(paste("window.scrollTo(1, ",cancelPolicyLoc$getElementLocation()$y -500 ,");"))
                    #remDr$screenshot(display = FALSE, file =paste("./WWW/Jalan",hotel,gsub("/","・",room),"policy",Sys.Date(),".png",sep="_"))
                    remDr$goBack() 
                }
                elistJ <- rbind(elistJ,  c("Jalan",l, hotel,name,paste(planId,plan,sep=":"),paste(roomId,room,sep=":"), paste(ppl,child,sep="_"), meal, date,checkintime,point,koukuken,paymentmethod,homenurl,checkinby,cancelPolicy,noshowPolicy,receiptPolicy))
            }
        }
    }else{
        Sys.sleep(0.5)  
        elistJ <- rbind(elistJ,c("Jalan","E1" , hotel, "-", "-",gsub("\n","",remDr$findElement(value = paste("//*[@id='check-infobox-txt01']",sep=""))$getElementText())  ,"-" ,"-" ,"-","-","-","-","-","-","-" ))                     
    }
    return (elistJ)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("sample"),
    tabsetPanel(
        tabPanel("Plan info",
                 fluidRow(
                     column(1,helpText("Must")),
                     column(2,
                            selectizeInput("selectOTA", label = "OTA", 
                                           choices = c("Rakuten","Jalan"), options = list(maxItems = 5, placeholder = 'Select a name'), selected = c("Rakuten") )),
                     column(3,
                            selectizeInput("propertyList", label = "Property", 
                                           choices = urllist$propertyName, options = list(maxItems = 200, placeholder = 'Select a name'), selected = urllist$propertyName[1])),
                     column(1,actionButton("do2", "Get Plan Info"))
                 ),fluidRow(
                     column(1,helpText("if needed")),
                     column(1,checkboxInput("koukukenFlag", "Koukuken", value = FALSE, width = NULL)),
                     column(1,checkboxInput("pplFlag", "Occumancy(J)", value = FALSE, width = NULL)),
                     column(1,checkboxInput("additionalFlag", "Polycy(R/J), Paymentmethod,Childrate,CheckinKanou(J", value = FALSE, width = NULL))
                 ),fluidRow(
                     column(4,
                            selectizeInput("parameta", label = "hensu", 
                                           choices = ls(), options = list(maxItems = 1, placeholder = 'Select a name'), selected = "urllist" ))),
                     column(12,verbatimTextOutput("out2")
                 ),fluidRow(
                     column(12,dataTableOutput("out1"))
                 )
        ) 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    options(encoding = 'UTF-8')
    observeEvent(input$do2, {
        if(TRUE ){
            #①リストの項目追加
            elistR <<- c("OTA","page","name","OTAname","plan","room","ppl","meal","date","checkintime","campaign","koukuken","Payment method" ,"URL","Checkinby","cancelPolicy","noshowPolicy","receiptPolicy")
            elistJ <<- c("OTA","page","name","OTAname","plan","room","ppl","meal","date","checkintime","campaign","koukuken","Payment method", "URL","Checkinby","cancelPolicy","noshowPolicy","receiptPolicy")
            elistR <<- rbind(elistR,c("OTA","page","name","OTAname","plan","room","ppl","meal","date","checkintime","campaign","koukuken","Payment method","URL","Checkinby","cancelPolicy","noshowPolicy","receiptPolicy") )
            elistJ <<- rbind(elistJ,c("OTA","page","name","OTAname","plan","room","ppl","meal","date","checkintime","campaign","koukuken","Payment method","URL","Checkinby","cancelPolicy","noshowPolicy","receiptPolicy"))
            colnames(elistR) <<- elistR[1,]
            colnames(elistJ) <<- elistJ[1,]
            elistR <<- elistR[-1:-2,]
            elistJ <<- elistJ[-1:-2,]
        }        
        for (hotel in input$propertyList) {
            #楽天が選択されている場合
            if(! is.na(match("Rakuten",input$selectOTA)) ){
                propertyNo <- match(hotel,urllist$propertyName)
                url2 <- urllist$rakutenUrl2[propertyNo]
                j <- 0
                if(url2 != "https://hotel.travel.rakuten.co.jp/hotelinfo/plan/?f_teikei=&f_heya_su=1&f_otona_su=1&f_s1=0&f_s2=0&f_y1=0&f_y2=0&f_y3=0&f_y4=0&f_kin=&f_kin2=&f_squeezes=&f_tscm_flg=&f_tel=&f_static=1"){
                    remDr$navigate(url2)
                    
                    rakutencnt <- 0
                    while(rakutencnt < 5){
                        if(length(remDr$findElements(value = paste("//*[@class='reserve_error']",sep=""))) > 0){
                            remDr$findElement(value = paste("//*[@alt='楽天トラベル']",sep="") )$clickElement()
                            Sys.sleep(1)
                            remDr$navigate(url2)
                        }else {
                            break;
                        }
                    }
                    elistR <<- r_logic(hotel,input$plancapmode,input$aftermonth,elistR,input$kakoyoyakuFlag, input$kutikomiFlag,input$koukukenFlag,input$additionalFlag) 
                } else {
                    url <- "https://travel.rakuten.co.jp/" 
                    remDr$navigate(url)
                    #ホテル名入力
                    remDr$findElement(value = paste("//*[@id='f_query']",sep="") )$sendKeysToElement(list(hotel))
                    #検索
                    remDr$findElement(value = paste("//*[@id='kw-submit']",sep="") )$clickElement()
                    errorFlag  <- length(remDr$findElements(value = paste("//*[@id='messageArea']/p",sep="")))
                    if(errorFlag == 0){
                        #ホテル画面表示
                        remDr$findElement(value = paste("//*[@id='result']/div[2]/h2/a/span",sep="") )$clickElement()
                        elistR <<- r_logic(hotel,input$plancapmode,input$aftermonth,elistR,input$kakoyoyakuFlag, input$kutikomiFlag,input$koukukenFlag,input$additionalFlag)          
                    } else if (urllist$rakutenUrl[propertyNo] !=   "https://travel.rakuten.co.jp/HOTEL//.html"){
                        elistR <<- rbind(elistR,c("Rakuten","E2", hotel, "-", "-", gsub("\n","",remDr$findElement(value = paste("//*[@id='messageArea']/p",sep="") )$getElementText())   ,"-" ,"-" ,"-","-","-","-" ,"-","-","-","-","-","-","-"))        
                        remDr$navigate(urllist$rakutenUrl[propertyNo])   
                        elistR <<- r_logic(hotel,input$plancapmode,input$aftermonth,elistR,input$kakoyoyakuFlag,input$kutikomiFlag,input$koukukenFlag,input$additionalFlag)      
                    } else{
                        elistR <<- rbind(elistR,c("Rakuten","E2", hotel, "-", "-",gsub("\n","",remDr$findElement(value = paste("//*[@id='messageArea']/p",sep="") )$getElementText())   ,"-" ,"-" ,"-","-","-","-","-","-","-","-","-","-","-"))        
                    }
                }
            }
            if(! is.na(match("Jalan",input$selectOTA))){
                propertyNo <- match(hotel,urllist$propertyName)
                url2 <- urllist$jalanUrl2[propertyNo]
                j <- 0
                if(url2 != "https://www.jalan.net/yad/plan/?screenId=UWW3001&stayDay=&yadNo=&maxPrice=999999&dateUndecided=1&rootCd=7701&stayMonth=&stayYear=&contHideFlg=1&minPrice=0&callbackHistFlg=1&smlCd=140202&distCd=01&ccnt=yads2"){
                    remDr$navigate(url2)
                    elistJ <<- j_logic(hotel,input$plancapmode,input$aftermonth,elistJ,input$kutikomiFlag,input$koukukenFlag,input$pplFlag,input$additionalFlag)        
                }else{
                    #Jalan表示
                    url <- "https://www.jalan.net/" 
                    remDr$navigate(url)
                    #ホテル名入力
                    remDr$findElement(value = paste("//*[@id='searchAreaStn']",sep="") )$sendKeysToElement(list(hotel))
                    #検索
                    remDr$findElement(value = paste("//*[@id='image1']",sep="") )$clickElement()
                    l <- 0
                    errorFlag  <- length(remDr$findElements(value = paste("//*[@id='check-infobox-txt01']",sep="")))
                    if(errorFlag == 0){   
                        #ホテル画面表示
                        remDr$findElement(value = paste("//*[@id='fw']/div[3]/ol/li/div[2]/div[1]/div/div[1]/h2/a",sep="") )$clickElement()
                        elistJ <<- j_logic(hotel,input$plancapmode,input$aftermonth,elistJ,input$kutikomiFlag,input$koukukenFlag,input$pplFlag,input$additionalFlag)
                    }else if(urllist$jalanUrl[propertyNo] !=  "https://www.jalan.net/yad/"){
                        Sys.sleep(0.5)  
                        remDr$navigate(urllist$jalanUrl[propertyNo])
                        elistJ <<- j_logic(hotel,input$plancapmode,input$aftermonth,elistJ,input$kutikomiFlag,input$koukukenFlag,input$pplFlag,input$additionalFlag)        
                    }else{
                        Sys.sleep(0.5)  
                        elistJ <<- rbind(elistJ,c("Jalan","E2", hotel, "-", "-",gsub("\n","",remDr$findElement(value = paste("//*[@id='check-infobox-txt01']",sep=""))$getElementText())  ,"-" ,"-" ,"-","-","-","-" ,"-","-","","","-","-"))           
                    }
                }
            }
            today <-  Sys.Date() +1 
            write.table(rbind(elistR,elistJ),paste("rakuten-jalan_",today,".csv"),sep="\t")
        }
        t1.1 <<-  read.delim(paste("rakuten-jalan_",today,".csv") , header= T,sep="\t",row.names=NULL)
    })
    output$out1 <- renderDataTable({
        eval(parse(text =input$parameta))
    })
    output$out2 <- renderPrint({
        eval(parse(text =input$parameta))
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
