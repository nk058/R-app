#install.packages("highcharter")
#install.packages("devtools")

#devtools::install_github("jbkunst/highcharter")


library(shiny)
library(highcharter)
library(data.table)
library(RMySQL)
library(DBI)
library(lubridate)
library(dplyr)
library("data.tree")


#################################
####   to json        ####
#################################

to_json = function(df, orient = "columns", json = T){
  dl = as.list(df)
  dl = switch(orient, 
              columns = dl,
              records = do.call('zip_vectors_', dl),
              values = do.call('zip_vectors_', setNames(dl, NULL))
  )
  if (json){
    dl = rjson::toJSON(dl)
  }
  return(dl)
}

zip_vectors_ = function(..., names = F){
  x = list(...)
  y = lapply(seq_along(x[[1]]), function(i) lapply(x, pluck_(i)))
  if (names) names(y) = seq_along(y)
  return(y)
}

pluck_ = function (element){
  function(x) x[[element]]
}

##-----------------------Query Database------##
#q2="15/16 Q4"
#q1="16/17 Q1"

##-------get all quarters list from database--------##
get_quarter <- function(){
  
  print("get_quarter called")
  cat("get_quarter called!\n", file = stderr())
  
  on.exit(dbDisconnect(conn)) ## important to close connection
  conn <- dbConnect(MySQL(), user='Glm_Test_adm', password='kJkiTBOvVf_wkNB', dbname='Glm_Test', host='testux-db.vih.infineon.com', port=3306)
  
  query<- sprintf("select distinct(Quarter) from Glm_Test.CostCenter_Rawdata;") 
  query1<- sprintf("select distinct(Quarter) as Q2 from Glm_Test.CostCenter_Rawdata WHERE  Quarter< (SELECT MAX(Quarter) from Glm_Test.CostCenter_Rawdata);") 
  
  q1 <- dbGetQuery(conn,query)## get queried data
  q2 <- dbGetQuery(conn,query1)## get queried data
  q1
  
}

##-------------get max quarter from database---------##
max_quarter<- function(){
  
  print("get_quarter called")
  cat("get_quarter called!\n", file = stderr())
  
  on.exit(dbDisconnect(conn)) ## important to close connection
  conn <- dbConnect(MySQL(), user='Glm_Test_adm', password='kJkiTBOvVf_wkNB', dbname='Glm_Test', host='testux-db.vih.infineon.com', port=3306)
  query<- sprintf("select max(Quarter) from Glm_Test.CostCenter_Rawdata;") 
  q1 <- dbGetQuery(conn,query)## get queried data
  q1
  
}

##-------get second max quarter --------------##
second_max_quarter<- function(){
  
  print("get_quarter called")
  cat("get_quarter called!\n", file = stderr())
  
  on.exit(dbDisconnect(conn)) ## important to close connection
  conn <- dbConnect(MySQL(), user='Glm_Test_adm', password='kJkiTBOvVf_wkNB', dbname='Glm_Test', host='testux-db.vih.infineon.com', port=3306)
  
  query<- sprintf("select max(Quarter) as Q1 from Glm_Test.CostCenter_Rawdata;") 
  query1<- sprintf("select max(Quarter) from Glm_Test.CostCenter_Rawdata WHERE  Quarter< (SELECT MAX(Quarter) from Glm_Test.CostCenter_Rawdata);") 
  q1 <- dbGetQuery(conn,query)## get queried data
  q2 <- dbGetQuery(conn,query1)## get queried data
  q2
  
}



get_min <- function(){
  
  print("get_min called")
  cat("get_min called!\n", file = stderr())
  
  on.exit(dbDisconnect(conn)) ## important to close connection
  conn <- dbConnect(MySQL(), user='Glm_Test_adm', password='kJkiTBOvVf_wkNB', dbname='Glm_Test', host='testux-db.vih.infineon.com', port=3306)
  
  #query<- sprintf("select max(Quarter) as Q1 from Glm_Test.CostCenter_Rawdata;") 
  query1<- sprintf("select max(Quarter) as Q2 from Glm_Test.CostCenter_Rawdata WHERE  Quarter< (SELECT MAX(Quarter) from Glm_Test.CostCenter_Rawdata);") 
  
  #q1 <- dbGetQuery(conn,query)## get queried data
  q2 <- dbGetQuery(conn,query1)## get queried data
  
  #q1<-as.character(q1)
  q2<-as.character(q2)
  
  query2<- sprintf("select costcenter,sum(CostTotal)as sum from Glm_Test.CostCenter_Rawdata where quarter in ('%s') group by  Costcenter;",q2) 
  
  qd <- dbGetQuery(conn,query2)## get queried data
  
  qd<-as.data.table(qd)
  
  min<-qd[,.(min=min(sum)),]
  
  min<-round(min,0)
  min
  #sqldata<- qd
}




get_data <- function(q1,q2){
  
  print("get_data called")
  cat("get_data called!\n", file = stderr())
  
  on.exit(dbDisconnect(conn)) ## important to close connection
  conn <- dbConnect(MySQL(), user='Glm_Test_adm', password='kJkiTBOvVf_wkNB', dbname='Glm_Test', host='testux-db.vih.infineon.com', port=3306)
  
  query<- sprintf("select Costcenter, Vendor,Tool,(cc.sumq3)-(cc.sumq2) as diff, cc.sumq3 as SumA, cc.sumq2 as SumB,Division,InvoiceRecipient,NoOfUsers from 
                  (select Costcenter, Vendor,Tool,Division,InvoiceRecipient,NoOfUsers,
                  sum(case when quarter = '%s' then costtotal else 0 end) as sumq3,
                  sum(case when quarter = '%s' then costtotal else 0 end) as sumq2
                  from Glm_Test.CostCenter_Rawdata
                  where quarter in ('%s', '%s')
                  group by Costcenter , Vendor ,Tool) as cc;",q1,q2,q1,q2) 
  
  qd <- dbGetQuery(conn,query)## get queried data
  qd<-data.table(qd)
  #View(qd)
  qd[is.na(SumA)]<-0
  qd[is.na(SumB)]<-0
  
  qd[Division %in% c("0","#N/A"),Division:="UNKNOWN"]
  qd[Division %in% c("OP FE","OP general","OP BE","OP ","OP Support"),Division:="OP"]
  
  qd
  #sqldata<- qd
}
#summary(qd)

unique(sqldata$Division)
#got_data<-get_data("16/17 Q1","15/16 Q4")

#sqldata<-got_data

#sqldata[Division=="UNKNOWN"]
#sqldata[Division=="OP "]
#choice= ("Division")


pass_data<-function(sqldata,choice= "Division"){    
  print("pass_data called")
  cat("pass_data called\n", file = stderr())
  #----------------------------------------------------------------------------------
  #sqldata$pathString <- paste("IFX" ,sqldata$Costcenter, sqldata$Vendor, sqldata$Tool,  sep = "/")
  
  ###-----------selecting heirarchy of treemap on basis of input of radio buttons ------###
  
  if(choice == "Division"){pathString= paste("IFX", sqldata$Division,sqldata$Costcenter,sqldata$Vendor,sqldata$Tool, sep = "$")
  (pathString)}else{
    if (choice == "Tool"){pathString = paste("IFX",sqldata$Tool, sqldata$Vendor,sqldata$Division,sqldata$Costcenter, sep = "$")
    (pathString)
    }else{
      if(choice== "Vendor"){pathString = paste("IFX",sqldata$Vendor,sqldata$Tool,sqldata$Division, sqldata$Costcenter, sep = "$")
      }else{
        if(choice== "Costcenter"){pathString = paste("IFX",sqldata$Costcenter,sqldata$Division,sqldata$Vendor,sqldata$Tool, sep = "$")
        
        (pathString)       
        }
        
        (pathString)
      }}}
  
  ##---- creating another coloumn in sqldata---#
  sqldata$pathString <-paste(pathString)
  
  ##-- creating nodes 
  st <- Sys.time()
  s <- as.Node(sqldata ,mode="table", pathDelimiter = "$")
  en <- Sys.time()
  print(paste("as.node time taken(sec):",en-st))
  cat(paste("as.node time taken(sec):",en-st))
  
  ##--- aggregate data on nodes ----##
  
  s$Do(function(node) node$value <- Aggregate(node , attribute = "diff", aggFun = sum), traversal = "post-order")
  s$Do(function(node) node$A <- Aggregate(node , attribute = "SumA", aggFun = sum), traversal = "post-order")
  s$Do(function(node) node$B <- Aggregate(node , attribute = "SumB", aggFun = sum), traversal = "post-order")
  s$Do(function(node) node$NoOfUsers <- Aggregate(node , attribute = "NoOfUsers", aggFun = sum), traversal = "post-order")
  
  #s$Do(function(node) node$change <- Aggregate(node , attribute = "percent_change", aggFun = sum), traversal = "post-order")
  #print(s,"value","A","B","Division","NoOfUsers")
  
  # assign ids to all nodes
  s$Set(id = 1:s$totalCount)
  # calculate parent ids for all children, using parent1 as parent is system reserved by data.tree
  s$Set(parent1 = c(function(self) GetAttribute(self$parent, "id", format = identity)))
  
  #copy the data tree structure to a data.frame
  st <- Sys.time()
  test <- ToDataFrameTree(s,"value", "level", "id",colorValue="value",actualvalue="value","parent1","A","B","Division","InvoiceRecipient","NoOfUsers")
  en <- Sys.time()
  print(paste("ToDataFrameTree time taken(sec):",en-st))
  cat(paste("ToDataFrameTree time taken(sec):",en-st,"\n"), file = stderr())
  # converting data frame to datatable for fast operation
  test <- data.table(test)
  # calculation of percentages at all level
  #test$change<-((test$A-test$B)/test$B)*100
  #if(test$A==0 | test$B==0){
  #  test$change<-(((test$A)/(test$A+test$B))-((test$B)/(test$A+test$B)))*100     
  
  #}else{test$change<-((test$A-test$B)/test$B)*100}
  
  #------------------------------------
  test[test$B<=0,B:=1]
  test[test$A<=0,A:=1]
  
  test$change<-((test$A-test$B)/((test$B))*100)
  #View(test)
  #test$change<-(((test$A)/(test$A+test$B))- ((test$B)/(test$A+test$B)))*100
  
  #test[change>3000, change:=3000]
  #manip to get the correct level name
  test[,name:= gsub("(*UCP)(*UTF)[^[:alnum:]]", " ", levelName,perl=T)]
  test[,name:= gsub('\u00A6|\u002D|\u00B0|\u0022|\u00b0|\u00a6',' ', levelName,perl=T)]
  #test[,name:= levelName]
  #write.csv(test$name,"name before.csv")
  test[,name:= trimws(name)]
  #write.table(test, "mydata_test.txt", sep="\t")
  test[,parent:= as.character(parent1)] # parent and ids should of character for highcharts
  test[,id:=as.character(id)] # parent and ids should of character for highcharts
  ## show boxes only for vendors and tools even if they are negative 
  
  #test[level >= 3, value:=abs(value)]
  #test
  
  list<-test[level==2]$name
  t2=NULL
  for(i in 1: length(list)){
    #for(i in 1: 2){
    print(list[i])        
    #cc_filtered<-test[name=="3717 02101"]
    #cc_filtered<-test[name=="1000 63531"]
    cc_filtered<-test[name==list[i]]
    print(cc_filtered$name)
    #a<-t[level=='5']
    a<-test[level=='3'& parent1 %in% cc_filtered$id, ]
    b<-test[level=='4'& parent1 %in% a$id,]
    c<-test[level=='5'& parent1 %in% b$id,]
    
    if(length(c$levelName)==0){ print("y")
      tt<-test[id %in% c(cc_filtered$id,a$id, b$id )]
      
      tt$div<-tt[level==4]$Division
      tt$site<-tt[level==4]$InvoiceRecipient
      
      
      
    }else{print("n")
      tt<-test[id %in% c(cc_filtered$id,a$id, b$id ,c$id)]
      
      tt$div<-tt[level==5]$Division
      tt$site<-tt[level==5]$InvoiceRecipient
    }
    
    
    t2<-rbind(t2,tt)
  }
  
  
}

# removing unecesary columns and processing list_data to either percentage or absolute views
# calculating col_stops
#test<-t2
#dd1<-function(test,checkbox){   
dd1<-function(test,checkbox){   
  if(checkbox== 'TRUE'){  
    
    test<-as.data.table(test)
    #deleting unnecessary columns
    test[,value:= as.numeric(trimws(change))]
    test[level >= 4, value:=abs(change)]
  }else{
    
    #deleting unnecessary columns
    test[,value:= as.numeric(trimws(value))]
    test[level >= 4, value:=abs(value)]
  }
  
  test[,levelName:=NULL]
  test[,parent1:=NULL]
  setkey(test, id)
  # removing IFX as a level
  test <- test[!(id==1)]
  # assigning NA to all nodes whose parent is IFX
  test <- test[parent==1, parent:=NA]
  ## show boxes only for vendors and tools even if they are negative 
  #test[level >= 3, value:=abs(value)]
  test
  
}

#dd2<-test
#cc_input="1000-66253"
#------------------------------------------------
dd3<-function(cc_input,dd2,slider,slider1,choice){
  dd2<-as.data.table(dd2)
  if(cc_input!=""){
    # filtering data on basis of slider input
    dd2[,n:= trimws(sub("^\\s*<U\\+\\w+>|-", " ", name,perl=T)),]
    inp<-gsub("-", " ",cc_input)
    
    test<-dd2
    if (choice == "Division"){
      
      cc_filtered<-test[n == inp,]
      #a<-t[level=='5']
      a<-test[level=='2'& id %in% cc_filtered$parent, ]
      b<-test[level=='4'& parent %in% cc_filtered$id,]
      c<-test[level=='5'& parent %in% b$id,]
      e<-test[id %in% c(cc_filtered$id,a$id, b$id ,c$id)]
      
    }else if (choice == "Costcenter")  {
      cc_filtered<-test[n == inp,]
      a<-test[level=='3'& parent %in% cc_filtered$id, ]
      b<-test[level=='4'& parent %in% a$id,]
      c<-test[level=='5'& parent %in% b$id,]
      e<-test[id %in% c(cc_filtered$id,a$id, b$id ,c$id)]
      
      
    }else {
      
      cc_filtered<-test[n== inp,]
      #print(cc_filtered$name)
      #a<-t[level=='5']
      a<-test[level=='4'& id %in% cc_filtered$parent, ]
      b<-test[level=='3'& id %in% a$parent,]
      c<-test[level=='2'& id %in% b$parent,]
      e<-test[id %in% c(cc_filtered$id,a$id, b$id,c$id )]
    }
  }
  else{
    
    
    aa<-dd2[ B>slider1,]
    a<-aa[level=='3' & actualvalue>slider,]
    
    b <- dd2[level=='4' & parent %in% a$id ,]
    d <- dd2[level=='5' & parent %in% b$id ,]
    #d1 <- dd2[level=='5' & parent %in% d$id ,]
    d1<-dd2[level=='2' & id %in% a$parent]
    e <- dd2[id %in% c(a$id,b$id,d$id,d1$id)]     
  }
  e
  
}



server= shinyServer(   
  function(input, output,session) {
    
    print("serverCalled")
    cat("serverCalled\n", file = stderr())
    
    output$chart<- renderHighchart({  
      
      
      progress <- shiny::Progress$new(session, min=1, max=15)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...')
      
      # type of heirarchy
      cho <- input$dist
      # checkbox input
      view <-input$checkbox
      
      # evaluating checbox input and passing to vv variable 
      if(view==TRUE){
        vv<-'TRUE'
      }else{
        vv<-'FALSE'
      }
      # taking inputs from radio buttons
      # quering the database with selected quarters as input 
      # calling main treemap script with data and type of heirarchy
      
      savedListFileName <- sprintf("%s%s%s.rds",input$quarter1,input$quarter2,cho)
      #savedListFileNameColstop <- sprintf("%s%s%scol.rds",input$quarter1,input$quarter2,cho)
      #savedListFileName<- sprintf("%s%s%s.rds","15/16 Q4","15/16 Q3","Costcenter")
      savedListFileName <- gsub("\\/", " ", savedListFileName) #remove slash
      #savedListFileNameColstop <- gsub("\\/", " ", savedListFileNameColstop) #remove slash
      print(paste("file name:",savedListFileName) )
      cat(paste("file name:",savedListFileName,"\n"), file = stderr())
      
      #cache data
      savedListFileName <- paste("cache/",savedListFileName)
      #savedListFileNameColstop <- paste("cache/",savedListFileNameColstop )
      
      if(file.exists(savedListFileName)){ # if file exists
        list_data <- readRDS(savedListFileName)  # true: load the file
        # colstops <- readRDS(savedListFileNameColstop)  # true: load the file
        
        print(paste(savedListFileName," loaded"))
        #print(paste(savedListFileNameColstop," loaded"))
      } else {
        got_data<- get_data(input$quarter1,input$quarter2);
        list_data<-pass_data(got_data,cho) #false: calculate list newly
        
        saveRDS(list_data,savedListFileName)
        #saveRDS(colstops,savedListFileNameColstop)
        print("Saved")
      }
      ##########filtering
      # Calling dd1 function with view option 
      dd2<-dd1(list_data,vv)
      # Slider range input
      slider<-input$slide
      slider1<-input$slide1
      
      # filtering data on basis of slider input
      
      
      e<-dd3(input$cc_input,dd2,slider,slider1,cho)
      # creating colstops 
      #e<-list_data
      max <- max(e$colorValue, na.rm=T)
      min <- min(e$colorValue, na.rm=T)
      res=abs((min))/(max- min);
      colstops<- data.frame(q = c(0.01,res - 0.0001,res,res + 0.0001,1),
                            c = c('#1a9641','#a6d96a','#ffffbf','#fdae61','#d7191c'))
      
      colstops<- list.parse2(colstops)
      #write.table(colstops, "mydata_colstops.txt", sep="\t")
      
      print("colstops from global variable")
      cat("colstops from global variable\n", file = stderr())
      # creating list format data fro treemap input
      list <- to_json(e, orient = "records", json = F)
      #write.table(list, "mydata_list.txt", sep="\t")
      # for number formating in highcharter
      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- ","
      options(highcharter.lang = hcoptslang)
      
      ##--------- Main treeamp plot function--------------##
      hc<-highchart() %>% 
        hc_title(text = "Costcenter Deviation between Quarters (A-B)") %>%
        #hc_subtitle(text="Q1 and Q2 is default selection")%>%
        hc_colorAxis( stops = colstops, endOnTick=FALSE,startOnTick=FALSE)  %>%
        #hc_colorAxis( minColor = '#1a9641', maxColor= '#a6d96a')  %>%
        hc_add_series(
          type = "treemap",
          layoutAlgorithm = "squarified",
          allowDrillToNode = T,
          dataLabels= list(enabled= F), levelIsConstant= F,
          
          
          levels =list( 
            list(
              level = 1,
              dataLabels = list(
                enabled = TRUE
              ),
              borderWidth = 3, borderColor= 'black'
              
            )
          ),
          
          data= list
          
        ) %>% 
        hc_tooltip(pointFormat = "<b>{point.name}</b>
                   <br> Site:{point.site} </br> <br> Div:{point.div} </br> <br>EUR: {point.actualvalue:,.2f}</br> <br> Change:{point.change}% </br>",borderWidth=1, shared= F, followPointer=F , enabled=T, borderRadius=5) %>%
        hc_plotOptions(borderWidth=15,borderColor='#fdae61', animationLimit=1000,turboThreshold=5000)
      # <br> No. of Users :{point.NoOfUsers} </br>  
      for (i in 1:15) {
        progress$set(value = i)
        # Sys.sleep(0.5)
      }
      
      hc
      
    })
  })


ui <- fluidPage(
  
  
  #h1("Treemap"),
  fluidRow(
    column(2,
           class = "panel",
           h1("Treemap"),
           selectInput("quarter1", label = "Compare invoice of Quarter (A)",
                       choices = get_quarter(),selected = max_quarter()),
           selectInput("quarter2", label = "with Quarter (B)",
                       choices = get_quarter(),selected = second_max_quarter()),
           wellPanel(checkboxInput("checkbox","% View",TRUE),
                     conditionalPanel( condition = "input.smooth == true"),
                     sliderInput("slide", "Min CC Diff Limit (A-B):", 
                                 min =-30000, max = 100000, value = 10000, step = 20000,
                                 format="$#,##0", locale="us"),
                     
                     sliderInput("slide1", "Min CC Invoice (B):", 
                                 min =get_min(), max = 100000, value = 10000, step = 20000,
                                 format="$#,##0", locale="us"),
                     
                     
                     radioButtons("dist", "Top level -> Drill down",
                                  c("Division", "Costcenter" ,
                                    "Tool" ,"Vendor"),selected = "Costcenter"),
                     textInput("cc_input", label = h3("Search Costcenter"), value = ""),
                     submitButton("update"))
           
    ),
    column( 10,highchartOutput("chart" , width = "100%", height = "800px")), 
    h4("Costcenter Trend!",a("Link",target="_blank",href="http://172.20.154.94:8597/apps/trend/"))
    
    
  )
)       


shinyApp(ui = ui, server = server)

