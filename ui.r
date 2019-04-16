library("shiny")
library("httr")
library("jsonlite")
library("png")
fluidPage(
    titlePanel(includeHTML("logo.html"),windowTitle ="路迅" ),
	  sidebarLayout(
    sidebarPanel(
		includeHTML("shuoming.html"),
    hr(),
		#地址输入框
		div(class="text1_type",textInput("text1","Address Input:")),
		#地址判断结果
    textOutput("text2"),
    actionButton("add","Add",height="60px",width="80px",style="font-size:20px"),
		#编辑框（框中有默认地址）
    div(class="text3_type",textAreaInput("text3","Address Edit:",value=paste0("上海市闵行区剑川路910号","\n","上海市闵行区富卓路340号","\n","上海市闵行区莘庄镇莘凌路211号","\n","上海市闵行区浦佳路91号","\n","上海市闵行区梅陇镇集心路201号","\n","上海市闵行区紫凤路350号"),resize="vertical",rows=4)),
		actionButton("clear","Clear",height="60px",width="80px",style="font-size:20px"),
		h3("Address Show:"),
		#标序号地址展示
		verbatimTextOutput("text4"),
    div(class="number_type",selectInput("number","Select Starting Point:","")),
		tags$head(tags$style(type="text/css", ".text1_type {
		                     font-size: 25px; }", ".text3_type {
		                     font-size: 25px; }",
		                     ".number_type {
		                     font-size: 25px; }")),
    actionButton("run","Run",height="60px",width="80px",style="font-size:20px"),
		textOutput("text8"),
		tags$style(type='text/css',"#text3 {font-size: 25px !important} ", "#text4 {font-size: 25px !important} ","#text8 {font-size: 25px !important} ")
	),
	mainPanel( 
		h2("Route Distance:"),
		verbatimTextOutput("text5"),
		h2("Route Sequence:"),
		verbatimTextOutput("text6"),
		h2("Total Path Image:"),
		plotOutput("outplot"),
		h2("Route Instruction:"),
		uiOutput("text7"),
    h2("Single Path Map:"),
    div(class="select_type",selectInput("select","Select Route:","")),
    actionButton("generate","Generate",height="60px",width="110px",style="font-size:20px"),
    uiOutput("image"),
		tags$head(tags$style(type="text/css", ".select_type {
		                     font-size: 25px; }")),
		tags$style(type='text/css', "#text5 {font-size: 25px !important} ","#text6{font-size: 25px !important} ","#text7{font-size:25px !important} ")
	)),
	hr(),
	div(class="footer",	includeHTML("mianze.html"),align="center")

)