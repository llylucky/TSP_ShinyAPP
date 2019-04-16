library("shiny")
library("httr")
library("jsonlite")
library("png")
function(input, output,session) {
	mykey <- "f0f9cf3da940f8ac359e2f112a0bf150"
	#判断地址是否正确
    judge1 <- eventReactive(input$add,{	
        api <- "http://restapi.amap.com/v3/geocode/geo?"
        getsummary <- paste0(api,"address=",input$text1,"&key=",mykey)
		get <- GET(getsummary)
		content <-content(get,as="text")
		result<-fromJSON(content)
        if(result$count==0){
            return("Address Error")
		}else if(length(which(sapply(as.list(unlist(strsplit(input$text3,split="\n"))),function(x)which(x==input$text1))!=0))!=0){
			return("Address Already Exists")
		}else{
			return("Success")
		}
    })
    value= reactiveVal(0)
    #存储已正确的地址
    checked <- reactive({
        check <-c()
        if(judge1()=="Success")
            check <-c(check,input$text1)
        return(check)
    })
    observeEvent(input$add,{
        if(judge1()=="Success"){
            newvalue=value()+1
            value(newvalue)
        }
    })
	#显示判断的结果
	output$text2 <-renderText({
		return(judge1())
	})
	#如果地址正确更新编辑框中的地址
	observeEvent(input$add,{
		if(judge1()=="Success"){
			if(input$text3==""){
				res <-input$text1
				updateTextAreaInput(session,"text3", value = res)
			}else{
				res <-paste0(input$text3,"\n",input$text1)
				updateTextAreaInput(session,"text3", value = res)
			}
		}
	})
	#点击clear按钮清除编辑框中内容
	observeEvent(input$clear,{
		updateTextAreaInput(session,"text3", value = "")
	})
	#显示编辑框中的内容并标号
	output$text4 <- renderPrint({
		data.frame(address=unlist(strsplit(input$text3,split="\n")))
	})
    #更新出发点选择框
	observe({
        num=1:length(unlist(strsplit(input$text3,split="\n")))
        updateSelectInput(session,"number",choices=num)
    })
    #对编辑框中未检验的地址进行判断
	judge2 <- eventReactive(input$run,{
        tep<-unlist(strsplit(input$text3,split="\n"))
        if(value()==0){
            diff<-tep
        }else{
        	diff<- setdiff(tep,checked())
        }
		error="incorrect address:"
		for(i in 1:length(diff)){
			api <- "http://restapi.amap.com/v3/geocode/geo?"
			getsummary <- paste0(api,"address=",diff[i],"&key=",mykey)
			get <- GET(getsummary)
			content <-content(get,as="text")
			result<-fromJSON(content)
			if(result$count==0){
				error=paste0(error,"\n",diff[i])
			}
		}		
		return(error)		
    })
	output$text8 <- renderText({
		if(judge2()!="incorrect address:")
			return(judge2())
	})
	#获取所有点的坐标，最短距离，最佳序列以及最佳序列对应的坐标流
	geting <- eventReactive(input$run,{
		if(judge2()=="incorrect address:"){
			withProgress(message = '获取坐标和距离矩阵',{
				address <- unlist(strsplit(input$text3,split="\n"))
				#获取各地点的坐标
				n=length(address)
				location <- c()
				api <- "http://restapi.amap.com/v3/geocode/geo?"
				x=1:n
				for(i in 1:length(split(x,ceiling(x/10)))){
					if(i!=length(split(x,ceiling(x/10)))){
						getsummary <- paste0(api,"address=",paste(address[(10*i-9):(10*i)],collapse="|"),"&batch=true","&key=",mykey)
						get <- GET(getsummary)
						content <-content(get,as="text")
						result <- fromJSON(content)
						location <- c(location,result$geocodes$location)
					}else{
						getsummary <- paste0(api,"address=",paste(address[(10*(i-1)+1):n],collapse="|"),"&batch=true","&key=",mykey)
						get <- GET(getsummary)
						content <-content(get,as="text")
						result <- fromJSON(content)
						location <- c(location,result$geocodes$location)
					}
				}
				#获取两地点之间的距离
				dis <- function(i,j){
					api <-"http://restapi.amap.com/v4/direction/bicycling?"
					getsummary <- paste0(api,"origin=",location[i],"&destination=",location[j],"&key=",mykey)
					get <- GET(getsummary)
					content <-content(get,as="text")
					result <- fromJSON(content)
					distance <- result$data$paths$distance
					return(distance)
				}
				#距离矩阵和坐标流矩阵
				d <- matrix(nrow=n,ncol=n)    #距离矩阵
				for(i in 1:n){
					for(j in i:n){
						if(i==j){
							d[i,j]=0
						}else{
							d[i,j]=d[j,i]=as.numeric(dis(i,j))
						}	
					}
				}
			})
			withProgress(message = '计算最佳序列',{
				#用C—W节约法求解TSP
				#计算一个排列所代表的环路长度
				S=function(x) 
				{
					y=c(x[-1],x[1])

					s=0
					for(i in 1:length(x)) s[i]=d[x[i],y[i]]
					return(sum(s))
				}
				#选取一个基点，实施C-W节约算法
				CW=function(B)
				{
					#计算除基点以外，其它点两两相连后能带來的路程节约值
					x=1:n
					com=combn(x[-B],2)	#取2的组合问题
					com=rbind(com,0)	#第三行记录节约值

					for(k in 1:ncol(com)){
						i=com[1,k]
						j=com[2,k]
						saving = d[B,i] + d[B,j] - d[i,j]
						com[3,k]=saving
					}

					com=com[,order(com[3,],decreasing=TRUE)] #按节约值从大到小排序

					y=as.list(x[-B])	#初始的n-1条线路

					for(h in 1:ncol(com)) #依次取最节约的线路，判断能否插入
					{
						i=com[1,h]
						j=com[2,h]
				
						q=sapply(y,function(x)which(x==i))	#判断一个数字v在list(y)的哪一个slice
						si=which(q != 0)
				
						q=sapply(y,function(x)which(x==j))
						sj=which(q != 0)
				
						if(si != sj) #两点不在一条线路上
						{
							wi=which(y[[si]]==i)
							Li=length(y[[si]])
					
							wj=which(y[[sj]]==j)
							Lj=length(y[[sj]])

							if(wi==Li && wj==1){ #尾、头型
								y[[si]]=c(y[[si]],y[[sj]])
								y=y[-sj]
							}else if(wi==Li && wj==Lj){	#尾、尾型
								y[[si]]=c(y[[si]],rev(y[[sj]]))
								y=y[-sj]
							}else if(wi==1 && wj==1){ #头、头型
								y[[si]]=c(rev(y[[si]]),y[[sj]])
								y=y[-sj]
							}else if(wi==1 && wj==Lj){ #头、尾型
								y[[si]]=c(rev(y[[si]]),rev(y[[sj]]))
								y=y[-sj]
							}
						}		
						if(length(y)==1) break
					}
					return(c(B,unlist(y)))
				}
				#获取最短距离以及最短距离所对应的地点序列
				mt <- cbind()
				for(i in 1:n){
					z=CW(i)
					juli=S(z)
					xulie=c(z,juli)
					mt <- cbind(mt,xulie)
				}
				z1=mt[1:n,which.min(mt[n+1,])]          #最佳序列
				if(which(z1==input$number)!=1)
					z1=c(z1[which(z1==input$number):n],z1[1:(which(z1==input$number)-1)])
			})
			withProgress(message = '获取最佳序列对应的路径距离、坐标和文字导航',{
				z2=c(z1[-1],z1[1])
				julimin=mt[n+1,which.min(mt[n+1,])]     #最小距离
				route <- function(i,j){
					api <-"http://restapi.amap.com/v4/direction/bicycling?"
					getsummary <- paste0(api,"origin=",location[i],"&destination=",location[j],"&key=",mykey)
					get <- GET(getsummary)
					content <-content(get,as="text")
					result <- fromJSON(content)
					steps <- as.data.frame(result$data$paths$steps)
					polyline <- paste(steps$polyline,collapse=";")
					instruction <- paste(steps$instruction,collapse=";")
					return(c(polyline,instruction))
				}
				p1 <- c() 
				p2 <- c()
				for(i in 1:length(z1)){
					p1[i]<-route(z1[i],z2[i])[1]
					p2[i]<-route(z1[i],z2[i])[2]
				}
				return(cbind(z1,p1,p2,location,julimin))
			})
		}
	})
	
	#显示最短距离
	output$text5 <- renderText({
        paste0(geting()[1,5],"米")
	})
	#显示最佳序列
	output$text6 <- renderText({
		geting()[,1]
	})
	#显示总路径
	output$outplot <-renderPlot({
        if(judge2()=="incorrect address:"){
            gp <- geting()[,2]
            glocation <- geting()[,4]
            coor1=matrix(as.numeric(unlist(strsplit(unlist(strsplit(gp,split=";")),split=","))),ncol=2,byrow=TRUE)
            coor2=matrix(as.numeric(unlist(strsplit(glocation,split=","))),ncol=2,byrow=TRUE)
            plot(coor1,type="n",xlab="",ylab="")
            lines(coor1,lwd=2)	
            points(coor2,pch=21,bg="red",cex=1.8)
            text(coor2,labels=1:length(glocation),adj=c(-0.3,-0.3),col="blue",pos=2,cex=1.8)
        }
	      
	})
    #更新路径选择框
    chose <- reactive({
            gz1 <- geting()[,1]
            gz2=c(gz1[-1],gz1[1])
            x <- c()
            for(i in 1:length(geting()[,1])){
                x[i]=paste0(gz1[i],"-",gz2[i])
            }
            return(x)
    })
    observe({
        updateSelectInput(session,"select",choices=chose())
	})
	#单路径导航
	output$text7 <- renderUI({
        if(judge2()=="incorrect address:"){
            gi=geting()[,3]
            lapply(1:length(gi),function(i){
                tagList(
                    renderText({
                        chose()[i]
                    }),
                    renderText({
                        gi[i]
                    })
                )
            })
        }
	})

    #选中某条路径图像输出
    num <- eventReactive(input$generate,{
        which(chose()==input$select)
    })
    output$image <- renderUI({
        if(judge2()=="incorrect address:"){
            tagList(
                renderImage({
                    i=num()
                    outfile <- tempfile(fileext = ".png")
                    gz1 <- as.numeric(geting()[,1])
                    gz2=c(gz1[-1],gz1[1])
                    gp <- geting()[,2]
                    glocation <- geting()[,4]
                    api <-"http://restapi.amap.com/v3/staticmap?"
                    getsummary <- paste0(api,"labels=",gz1[i],",2,0,16,0xFFFFFF,0x008000:",glocation[gz1[i]],"|",gz2[i],",2,0,16,0xFFFFFF,0x008000:",glocation[gz2[i]],"&paths=2,0x0000ff,1,,:",gp[i],"&key=",mykey)
                    get <- GET(getsummary)
                    content <-content(get,as="raw")
                    img <- readPNG(content)
                    writePNG(img,target = outfile)
                    list(src=outfile,contentType='image/png',alt = "This is alternate text")       
                },deleteFile = TRUE)
            )
        }
    })
}