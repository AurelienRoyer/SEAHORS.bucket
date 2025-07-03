# modFunctionUI <- function(id) {
#   ns <- NS(id)
#   DTOutput(ns("tablepalyno2"))
# }

app_ui <- function(){
  shiny::addResourcePath("SEAHORS", system.file("R", package="SEAHORS"))
    

##### script R shiny
options(shiny.reactlog = TRUE)
css <- "
.radio-inline {
  padding: 0 3px;
  text-align: center;
  margin-left: 0 !important;
  line-height: 30px;

}

.radio-inline input {
  top: 20px;
  left: 50%;
  margin-left: -6px !important;
  line-height: 30px;
}"



ui <- shinyUI(
  navbarPage(
  windowTitle = "SEAHORS.BUCKET",
  fluidPage(
    useShinyjs(),
    theme = shinytheme(theme = "journal"),
    sidebarLayout(
      sidebarPanel(span(img(src = "www/logobucket1.png", height = 110)),
                   tags$header(
                     tags$a(href = "https://github.com/AurelienRoyer/SEAHORS/",
                            "Spatial Exploration of ArcHaeological Objects in R Shiny")),
                   tags$hr(),
                   tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                   # tags$script('<script src="https://kit.fontawesome.com/29eb425cd7.js" crossorigin="anonymous"></script>'),
                   # HTML('<script src="https://kit.fontawesome.com/29eb425cd7.js" crossorigin="anonymous"></script>'),
                   tags$script('<script src="https://kit.fontawesome.com/04cc3aa3ca.js" crossorigin="anonymous"></script>'),
                   HTML('<script src="https://kit.fontawesome.com/04cc3aa3ca.js" crossorigin="anonymous"></script>'),
                   tags$style(HTML(css)),
                   radioButtons(
                     "bt2", h4("QUICK SIDEBAR"),
                     choices = c("Data site" = 1,
                                 "The variable modalities"=6,
                                 "Point size" = 2,
                                 "Point color" = 3,
                                 "Point shape" = 4,
                                 "Fig. options"=5,
                                 "Suppl. data"=7),
                     selected = "1", inline=TRUE), style = "font-size:90%",
                   tags$hr(),
                   conditionalPanel(condition="input.bt2==1",
                                    h4(style = "color: red;","Subsetting dataset"),
                                    tags$br(),
                                    uiOutput("Date"),
                                    uiOutput("xlimits"),
                                    uiOutput("ylimits"),
                                    uiOutput("zlimits"),
                                    uiOutput("liste.sector"),
                                    uiOutput("liste.UAS"),
                   fluidRow(conditionalPanel("output.nb6", 
                                                              column(5,
                                                                     actionButton("all_UAS_entry", label = "ALL"),),
                                                              column(2,
                                                                     actionButton("reset_UAS_entry", label = "reset"),)
                                    )),#end fluidrow
                                    uiOutput("liste.Nature"),
                   fluidRow(conditionalPanel("output.nb6", 
                                                              column(5,
                                                                     actionButton("all_nature_entry", label = "ALL"),),
                                                              column(2,
                                                                     actionButton("reset_nature_entry", label = "reset"),))
                                    ), #end fluidrow
                                    
                                    uiOutput("liste.passe"),
                                    width=4, 
                   ), # end of conditionalPanel
                   conditionalPanel(condition="input.bt2==2",
                                    h4(style = "color: red;","Modifying point size according to a variable"),
                                          column (6,numericInput("point.opa", "Default point opacity", 1, min = 0, max=1, width="50%",step=0.05), ),  
                                    tags$br(),
                                    fluidRow(
                                          #  column (6,numericInput("minsize", "Minimal point size", 0.25, min = 0.1, max=10, width="50%")),
                                             column (6,numericInput("point.size", "Default point size", 2, min = 1, max=20, width="50%"), ),  ),
                                    #  tags$br(),
                                    #  h5(style = "color: blue;","Only available with unique ID: "),        
                                    # uiOutput("sectionXx2"),
                                    # uiOutput("sectionXy2"),
                                    # uiOutput("sectionXz2"),
                                    # uiOutput("var.gris.2D"),
                                    # uiOutput("var.gris.2D.1"),
                   ), # end of conditionalPanel
                   conditionalPanel(condition="input.bt2==3",
                                    h4(style = "color: red;","Modifying point color according to a variable"),
                                    tags$h5(style = "color: blue;","you can import color ramp from panel 'additional settings'"),
                                    tags$br(),
                                    tags$br(),
                                    tags$h5(style = "color: blue;","The variable colored corresponds to requested data : nature/species'"),
                                    tags$hr(),
                                    column(7,div(style = "height:2px"),
                                           h4("Select colors"),
                                           uiOutput("colors2")),
                                    tags$br(),
                                    tags$br(),
                   ), # end of conditionalPanel
                   conditionalPanel(condition="input.bt2==4",
                                    h4(style = "color: red;","Modifying point shape"),
                                    tags$br(),
                                    column(9,
                                           column(6,selectInput("shape", "main shape",
                                                                choices= list("circle"='circle',
                                                                              "square"= 'square',
                                                                              "triangle"='triangle',
                                                                              "diamond"='diamond',
                                                                              "star"='star')),
                                           ),
                                           column(6, style = "margin-top: 25px;", actionButton("do.shape1", "Set main shape ")),
                                    ),
                                    column(10,
                                           
                                           column(7, uiOutput("shape2"),),
                                           column(6,  style = "margin-top: 3px;",uiOutput("shape2.var1"),),
                                           column(6,  style = "margin-top: 3px;",uiOutput("shape2.var2"),),
                                           br(),
                                           column(6, actionButton("do.shape2", "Set secondary shape")),
                                           tagAppendAttributes(textOutput("text.shape"), style="white-space:pre-wrap;"),
                                    )
                                    
                   ),# end of conditionalPanel
                   conditionalPanel(condition="input.bt2==6",
                                    h4(style = "color: red;","Subsetting the variable modalities"),
                                    tags$br(),
                                    fluidRow(conditionalPanel("output.nb6", 
                                                              column(5,
                                                                     actionButton("all_artifact_entry", label = "ALL"),),
                                                              column(2,
                                                                     actionButton("reset_artifact_entry", label = "reset"),))
                                    ), #end fluidrow
                                    tags$br(),
                                    uiOutput("liste.species"),
                                    
                                    
                   ),# end of conditionalPanel
                   conditionalPanel(condition="input.bt2==7",
                                    h4(style = "color: red;","Supplementary data"),
                                    tags$br(),
                                    radioButtons(
                                      "bt5", h4(),
                                      choices = c("Nature" = 1,
                                                  "color"=2),
                                      selected = "1", inline=TRUE), style = "font-size:70%",
                                    tags$br(),
                                    conditionalPanel(condition="input.bt5==1",
                                                     uiOutput("liste.Nature.xyz"),),
                                    conditionalPanel(condition="input.bt5==2",
                                                     column(7,div(style = "height:2px"),
                                                            h4("Select colors"),
                                                            uiOutput("colors.sup")),
                                                     tags$br()
                                                     ) # end of conditionalPanel
                   ),# end of conditionalPanel
                   conditionalPanel(condition="input.bt2==5",
                                    h4(style = "color: red;","Figure options"),
                                    tags$br(),
                                    column(10,
                                           sliderInput("jitter", "jitter modifications",0, min = 0, max=2, step=0.01),),
                                    tags$br(),
                                    fluidRow(column (7,radioButtons("optioninfosfigplotly", "Show figure legend", choices = c(right = "right",
                                                                                                                              left = "left",
                                                                                                                              top = "top",
                                                                                                                              bottom = "bottom",
                                                                                                                              none="none"),
                                                                    selected = "right"),
                                                     )),
                                    tags$br(),
                                    column(10,column(5,numericInput("height.size.b", label = h5("Figure height"), value = 800),),
                                           column(5,numericInput("width.size.b", label = h5("Figure width"), value = 1000),),
                                           tags$hr(),),
                                    tags$br(),
                                    column(10,
                                           column(4,numericInput("fontsizeaxis", "Axis font size",12, min = 1, max=40),),
                                           column(4,numericInput("fontsizetick", "tick font size",12, min = 1, max=40),),),
                                    column(10,
                                           column(4,textInput("Name.X", label="Legends name of X",value = "X"),),
                                           column(4,textInput("Name.Y", label="Legends name of Y",value = "Y"),),
                                           column(4,textInput("Name.Z", label="Legends name of Z",value = "Z"),),),
                                    column(10,
                                           column(4,numericInput("Xtickmarks", "Position of X tick marks",1, min = 0, max=40),),
                                           column(4,numericInput("Ytickmarks", "Position of Y tick marks",1, min = 0, max=40),),
                                           column(4,numericInput("Ztickmarks", "Position of Z tick marks",1, min = 0, max=40),),),
                                    column(10,
                                           column(4,numericInput("Xminor.breaks", "Position of X minor breaks",1, min = 0, max=40),),
                                           column(4,numericInput("Yminor.breaks", "Position of Y minor breaks",1, min = 0, max=40),),
                                           column(4,numericInput("Zminor.breaks", "Position of Z minor breaks",1, min = 0, max=40),),),
                                    column(12,br(),
                                           hr(),),
                                    uiOutput("themeforfigure"),
                   )# end of conditionalPanel
      ), #end sidebarpanel
      
      mainPanel(
        shiny::tabsetPanel(type = "tabs",id="mainpanel",
                    tabPanel("Overview", 
                             tags$div(
                               h2(" Welcome to the", em("SEAHORS.BUCKET"), "application",align="center", style = "font-family: 'Times', serif;
    font-weight: 500; font-size: 500; text-shadow: 3px 3px 3px #aaa; line-height: 1; 
     color: #404040;"),
                               tags$br(),
                               column(12, 
                                      column(3,),
                                      column(6,span(img(src = "www/logobucket2.png", height = 200)),),
                                      tags$br(),
                                      tags$br(),     
                               ),
                               column(12, br(),
                                      column(1,),
                                      column(9,
                                             br(),
                                             
                                             HTML(
                                               " <div style=width:100%;, align=left>
    <font size=3>
   <span style='text-transform:none'>
   
   <i>SEAHORS.BUCKET</i> package is dedicated to the intra-site spatial analysis of archaeological piece-plotted objects coming from bucket.</p>
    <i>SEAHORS.BUCKET</i> function is designed to vizualize specifically objects not directly coordinated, but coming from buckets.</p>
   <p>It makes it possible to explore the spatial organisation of not coordinated points taken on archaeological fields, as for example small flints or small vertebrate remains.</p>
       <p>This is an open and free software, 
   <ul>
      <li>it is available as an R package on the <a href=https://cran.r-project.org/package=SEAHORS target=_blank>CRAN</a>, and</li>
      <li> its source code is published on a <a href=https://github.com/AurelienRoyer/SEAHORS/ target=_blank>github repository</a>.</li>
    </ul>
    </p>
    <br>
    <p>Try <i>SEAHORS.BUCKET</i> now with the <a href=https://hal.science/hal-02190243 target=_blank> XXXX </a> Paleolithic site dataset:</p>
    </span> 
   
      </font size>           
                                               <br>
                                               <br>
                                               <br>
                                               <br>
                                               <br></div>" )
                                      ), ), #end of column
                             ), # end div()
                             column(12,  column(8,),column(2, HTML(
                               "  <div style=height:50%;, align=rigth> 
                          <font size=2>
                          <p>To import an example:</p>
                          </font size>
                          </div>"),#end html
                               # actionButton("button_example","Click to load the dataset",style="height:50%")
                               ),
                               tags$br(),
                               tags$br(),),
                             
                    ), #end of tabPanel
                    tabPanel("Data upload", 
                             value = "tab1", 
                             shiny::tabsetPanel(id = "tab1inner",
                                         type = "tabs",
                                         tabPanel(tags$h5("Import data"), 
                                                  tags$br(),
                                                  tags$br(),
                                                  htmlOutput("nb6"),
                                                  tags$br(),
                                                  tags$hr(),
                                         fluidRow(column(9,  
                                                  bs4Dash::tooltip(
                                                    shinyWidgets::actionBttn(
                                                      inputId = "save_up_file",
                                                      label = "Step 1. Upload data",
                                                      style = "unite",
                                                      color = "danger",
                                                      icon = icon("fas fa-save",lib = "font-awesome")
                                                    ),
                                                    title = "To upload the database",
                                                    placement = "top"
                                                  ),
                                                  tags$style("#save_file1 .modal-content{ width:100%,height:100%}     "),
                                                  bsModal(
                                                    id = "save_file1",
                                                    title = "Uploading file",
                                                    trigger = "save_up_file",size = "Default",
                                                    tags$h4(style = "color: red;","Options for loading file"), 
                                                    radioGroupButtons(
                                                      inputId = "trackChr",
                                                      label = tags$div(
                                                        HTML("<table><tr>
                                                    <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Step 1.1 : XYZ data and bucket data coming from 1 or 2 files?</b></font></div></td>
                                                    </tr></table>")
                                                      ),
                                                      choices = c("1 file" = 1, "2 files" = 2)
                                                      
                                                    ),
                                                    
                                                    radioGroupButtons(
                                                      inputId = "line_column",
                                                      label = tags$div(
                                                        HTML("<table><tr>
                                                    <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Step 1.2 : Bucket data in lines or in columns?</b></font></div></td>
                                                    
                                                    </tr></table>")
                                                      ),
                                                      #choices = c("in lines" = 1, "in columns" = 2)
                                                      choices = c("in lines" = 1)
                                                    ),
                                                    # checkboxInput("set.dataoneID", "Check this option if one line correspond to one ID", F),
                                                    
                                                    conditionalPanel(
                                                    condition = "input.trackChr == 1",
                                                    #fileInput("file1", 
                                                    fileInput("file_xyzspecies",
                                                      HTML("<table><tr>
                                                    <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Step 2 :  Choose the file (.csv/.xls/.xlsx)</b></font></div></td></tr></table>"),
                                                              multiple = TRUE,
                                                              accept = c("text/csv",
                                                                         "text/comma-separated-values",
                                                                         ".csv",
                                                                         ".xlsx",".xls")),
                                                    tags$br(),

                                                    selectInput(inputId = "worksheet_xyzspecies", label="Worksheet Name", choices =''),
                                                    tags$br(),
                                                    actionButton(inputId = "getData_xyzspecies",label="Get site data"),
                                                    uiOutput("set.ID"),

                                                    ), # end of conditionnal panel
                                                    
                                                    conditionalPanel(
                                                      condition = "input.trackChr == 2",
                                                      fluidRow(column(6,column(6,
                                                      fileInput("file1",HTML("<table><tr>
                                                    <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Step 2.1 : Choose the file with field XYZ data </b></font></div></td></tr></table>"),
                                                                multiple = TRUE,
                                                                accept = c("text/csv",
                                                                           "text/comma-separated-values",
                                                                           ".csv",
                                                                           ".xlsx",".xls")),
                                                      selectInput(inputId = "worksheet", label="Worksheet Name", choices =''),
                                                      tags$br(),
                                                      actionButton(inputId = "getData",label="Get site data"),
                                                      uiOutput("set.ID2"),
                                                      ),
                                                      tags$br(),),
                                                      column(6,

                                                      ),
                                                      column(6,
                                                             fileInput("file1.species", HTML("<table><tr>
                                                    <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Step 2.2 : Choose the file with bucket data </b></font></div></td></tr></table>"),
                                                                       multiple = TRUE,
                                                                       accept = c("text/csv",
                                                                                  "text/comma-separated-values",
                                                                                  ".csv",
                                                                                  ".xlsx",".xls")),
                                                             selectInput(inputId = "worksheet.species", label="Worksheet Name", choices =''),
                                                             actionButton(inputId = "getData.species",label="Get species data"),  
                                                             uiOutput("set.ID.species"),
                                                             tags$br(),
                                                             tags$br(),
                                                             
                                                    #          radioGroupButtons(
                                                    #            inputId = "trackdataoneID",
                                                    #            label = tags$div(
                                                    #              HTML("<table><tr>
                                                    # <td><div><font color='red'><h5><i class='fa-solid fa-play'></i><b> Data of one ID  </b></font></div></td>
                                                    # </tr></table>")
                                                    #            ),
                                                    #            choices = c("in one line" = 1, "in several lines" = 2)
                                                    #            
                                                    #          ),
                                                             
                                                             tags$br(),
                                                             tags$br(),
                                                    tags$br(),
                                                    tags$br(),
                                                    tags$br(),
                                                    
                                                             actionButton('merge.data.tables', 'Merge data tables', class = "btn-danger")
                                                      ), # end of column
                                                      ),
                                                    ), # end of conditionnal panel
                                                    tags$br(),
                                                    tags$hr(),
                                                    fluidRow(column(6,
                                                                             
                                                    actionButton('reset.BDD', 'Reset Input'),),
                                                   
                                                    # column(6,
                                                    # conditionalPanel(
                                                    #   condition = "input.trackChr == 2",
                                                    #   div(
                                                    #     shinyWidgets::actionBttn(
                                                    #       inputId = "chr_merging",
                                                    #       label = 
                                                    #         HTML("
                                                    # <font color='red'><h4><i class='fa-solid fa-folder-tree'></i> Check merging / to finish</font>
                                                    # "),
                                                    #       style = "unite",
                                                    #       color = "danger",
                                                    #       icon=NULL
                                                    #     ),
                                                    #     style = 'margin-right:0px'
                                                    #   ), # end of div  
                                                    # 
                                                    #   
                                                    #   bsModal(
                                                    #     id = "bsmodal_merginginfo",
                                                    #     title = "Information on table merging",
                                                    #     trigger = "chr_merging",size = "large",
                                                    #     
                                                    #     tags$h4(style = "color: red;","Information on table merging") 
                                                    #     
                                                    #   ),
                                                    #   ),#end of conditional panel
                                                    #     
                                                    # ),
                                                    
                                                    )
                                                  ),
                                                  
                                                  #div(
                                                  #  class = "right-button-class",
                                                  shinyWidgets::actionBttn(
                                                      inputId = "chr_setting",
                                                      label = NULL,
                                                      style = "unite",
                                                      color = "danger",
                                                      icon = icon("fas fa-cogs",lib = "font-awesome")
                                                    ),
                                                    tags$style("#bsmodal_param .modal-dialog{ width:1200px} 
                                                                .modal-backdrop {
                                                                                    display: none;
                                                                                    z-index: 1040 !important;
                                                                                }
                                                                                
                                                                                .modal-content {
                                                                                    margin: 2px auto;
                                                                                    z-index: 1100 !important;
                                                                                }
                                                                                
                                                                                "),

                                                     bsModal(
                                                       id = "bsmodal_param",
                                                      title = "Options for loading file",
                                                      trigger = "chr_setting",size = "large",

                                                      tags$h4(style = "color: red;","Options for loading file"), 
                                                      checkboxInput("header", "Header", TRUE),
                                                    checkboxInput("set.dec", "Check this option to automatically correct for the presence of comma in decimal numbers", TRUE)
                                                      ),
                                                  )),
                                                  
                                                  fluidRow(
                                                    tags$br(),
                                                    
                                                    column (7, 
                                                            bs4Dash::tooltip(
                                                              shinyWidgets::actionBttn(
                                                                inputId = "request_file",
                                                                label = "Step 2. Settings of request data",
                                                                style = "unite",
                                                                color = "danger",
                                                                icon = icon("fas fa-save",lib = "font-awesome")
                                                              ),
                                                              title = "To parametrize the necessary variable",
                                                              placement = "top"
                                                            ),
                                                    ),),
                                                            
                                                            tags$style("#request1 .modal-content{ width:1200px}  "),
                                                            fluidRow(
                                                              tags$br(),
                                                              
                                                              column (7,      
                                                            
                                                            bs4Dash::tooltip(
                                                              shinyWidgets::actionBttn(
                                                                inputId = "request_file2",
                                                                label = "Step 3. Settings of the squaring",
                                                                style = "unite",
                                                                color = "danger",
                                                                icon = icon("fas fa-save",lib = "font-awesome")
                                                              ),
                                                              title = "To parametrize the necessary variable",
                                                              placement = "top"
                                                            ),
                                                            tags$hr(),
                                                            tags$br(),
                                                            htmlOutput("nb7"),
                                                            tags$br(),
                                                            tags$h4(style = "color: red;","Variable for quick sidebar selection"), 
                                                            tags$br(),
                                                            column (7, 
                                                                    uiOutput("set.levels"),
                                                                    ),
                                                            column (3, 
                                                                    bs4Dash::tooltip(
                                                                      shinyWidgets::actionBttn(
                                                                        inputId = "chr_lvl_setting",
                                                                        label = NULL,
                                                                        style = "unite",
                                                                        color = "danger",
                                                                        icon = icon("fas fa-cogs",lib = "font-awesome")
                                                                      ),
                                                                      title = "To organize the order of levels",
                                                                      placement = "right"
                                                                    ),
                                                                    tags$style("#bsmodal_lvl .modal-dialog{ width:1200px} 
                                                                .modal-backdrop {
                                                                                    display: none;
                                                                                    z-index: 1040 !important;
                                                                                }
                                                                                
                                                                                .modal-content {
                                                                                    margin: 2px auto;
                                                                                    z-index: 1100 !important;
                                                                                }
                                                                                
                                                                                "),
                                                                    
                                                                    bsModal(
                                                                      id = "bsmodal_lvl",
                                                                      title = "To order the stratigraphic level organisation",
                                                                      trigger = "chr_lvl_setting",size = "large",
                                                                      
                                                                      tags$h4(style = "color: red;","Factor level order: Drag the button order with the mouse"), 
                                                                      actionButton("reverse.button", label = "Reverse the order"),
                                                                      uiOutput("order.level")
                                                                      ),
                                                            ), # end of column
                                                            # column (7, 
                                                            # uiOutput("set.square")
                                                            # ),
                                                            # column (3, 
                                                            #         
                                                            #         bs4Dash::tooltip(
                                                            #           shinyWidgets::actionBttn(
                                                            #             inputId = "chr_sq_setting",
                                                            #             label = NULL,
                                                            #             style = "unite",
                                                            #             color = "danger",
                                                            #             icon = icon("fas fa-cogs",lib = "font-awesome")
                                                            #           ),
                                                            #           title = "To configure the type of square",
                                                            #           placement = "right"
                                                            #         ),
                                                            #         tags$style("#bsmodal_lvl2 .modal-dialog{ width:1200px} 
                                                            #     .modal-backdrop {
                                                            #                         display: none;
                                                            #                         z-index: 1040 !important;
                                                            #                     }
                                                            #                     
                                                            #                     .modal-content {
                                                            #                         margin: 2px auto;
                                                            #                         z-index: 1100 !important;
                                                            #                     }
                                                            #                     
                                                            #                     "),
                                                            #         
                                                            #         bsModal(
                                                            #           id = "bsmodal_lvl2",
                                                            #           title = "To configure the type of square",
                                                            #           trigger = "chr_sq_setting",size = "large",
                                                            #            radioButtons("format.square", "Format of the square:",
                                                            #                         choices = c("A ou 1" = "A",
                                                            #                                     "A1 ou D5" = "B",
                                                            #                                     "A1a ou B5b" = "C",
                                                            #                                     "A1-SC1"="D"),
                                                            #                         selected = "C", inline=TRUE)
                                                            #         ),
                                                            # ),
                                                            column (7, 
                                                            uiOutput("set.sector"), 
                                                            uiOutput("set.anat"),
                                                            uiOutput("set.vol"),
                                                            uiOutput("set.date"),
                                                            uiOutput("set.nature"),
                                                            uiOutput("set.passe"),
                                                            ),
                                                    ), #endcolumn
                                                  ),#end of fluidrow   
                                                  tags$hr(),
                                                  tableOutput("contents")
                                         ), #end of tabPanel
                                    # tabPanel(tags$h5("Import supplementary coordinated data"),
                                    #               tags$br(),
                                    #               tags$br(),
                                    #               tags$hr(),
                                    #               fluidRow(column(10,
                                    #                               tags$h4(style = "color: red;","To load supplementary materials having XYZ coordinates"),
                                    #                               tags$hr(),
                                    #               ),#endcolumn
                                    #               ),#end of fluidrow
                                    #               fluidRow(column(12,
                                    #                               fileInput("file1.XYZ", "Choose File (.csv/.xls/.xlsx)// it is not yet included. load good. still to add or merge",
                                    #                                         multiple = TRUE,
                                    #                                         accept = c("text/csv",
                                    #                                                    "text/comma-separated-values",
                                    #                                                    ".csv",
                                    #                                                    ".xlsx",".xls")),
                                    #                               selectInput(inputId = "worksheet.XYZ", label="Worksheet Name", choices =''),
                                    #                               actionButton(inputId = "getData.XYZ",label="Get data"),
                                    # 
                                    #               )),
                                    #               fluidRow(
                                    #                 tags$br(),
                                    # 
                                    #                 column (7,
                                    #                         tags$hr(),
                                    #                         tags$h4(style = "color: red;","Option parameters"),
                                    #                         tags$h5(style = "color: blue;","Defined one variable to be colored"),
                                    #                         tags$br(),
                                    #                         uiOutput("set.nature.xyz"),
                                    #                         tags$br(),
                                    #                         tags$hr(),
                                    #                         tags$h5(style = "color: blue;","X, Y and Z data should be defined in both datasets"),
                                    #                         tags$br(),
                                    #                         uiOutput("set.x.xyz"),
                                    #                         uiOutput("set.y.xyz"),
                                    #                         uiOutput("set.z.xyz"),
                                    #                         uiOutput("set.levels.xyz"),
                                    #                         uiOutput("set.square.xyz"),
                                    #                         uiOutput("set.sector.xyz"), 
                                    #                         uiOutput("set.date.xyz"),
                                    #                         tags$br(),
                                    #                         tags$hr(),
                                    #                         htmlOutput("nb.temp"),
                                    #                         tags$br(),
                                    #                         tags$br(),
                                    #                 ), #endcolumn
                                    #               ),#end of fluidrow
                                    #               tags$hr(),
                                    #               tableOutput("contents.species"),
                                    #      ), #end of tabPanel
                                    tabPanel(tags$h5("Concatenate two columns"), 
                                             tags$br(),
                                             column(12,textInput("Merge.groupe", "Choose the name of the new column",value = "new.concatenate.col"),),
                                             tags$br(),
                                             column(8, uiOutput("set.col1"),),
                                             tags$br(),
                                             column(8,radioButtons("separatormerge", "separator between the two names",
                                                                   choices = c("." = ".",
                                                                               "_" = "_",
                                                                               "-" = "-",
                                                                               "," =",",
                                                                               "nospace"= ""),
                                                                   selected = "_", inline=TRUE),),
                                             tags$br(),
                                             tags$br(),
                                             column(8, uiOutput("set.col2"),),
                                             tags$br(),
                                             checkboxInput("checkbox.index", label = "To put this new groups as ID", value = TRUE),
                                             tags$br(),
                                             column(8,actionButton("Merge2", "Concatenate the two columns"),),
                                    ),
                                    tabPanel(tags$h5("Rename group modality"), 
                                             tags$br(),
                                             column (12,
                                                    # hr(style = "border-top: 1px solid #000000;"), 
                                                     br(),
                                                     tags$h3("Rename group modality"),
                                                     uiOutput("liste.newgroup2"),
                                                    tags$hr(),
                                                     uiOutput("liste.newgroup4"),
                                                     textInput("text.new.group2", label=h5("New name of the modality"),value = "new.modality"),
                                                     uiOutput("btt.modify"),
                                                    tags$br(),
                                                    tags$hr(),
                                                    tags$br(),
                                                    fileInput("file.renaming.table", "Choose File (.csv/.xls/.xlsx)// it is not yet included. load good. still to add or merge",
                                                              multiple = TRUE,
                                                              accept = c("text/csv",
                                                                         "text/comma-separated-values",
                                                                         ".csv",
                                                                         ".xlsx",".xls")),
                                                    selectInput(inputId = "worksheet.renaming.table", label="Worksheet Name", choices =''),
                                                    actionButton(inputId = "getData.renaming.table",label="Get the data"),
                                                    
                                                    uiOutput("liste.newgroup.table"),
                                                    uiOutput("liste.newgroup.table2"),
                                                    uiOutput("btt.modify2"),
                                                    ),
                                                     #actionButton("go.ng2", "Modify"),),
                                             
                                             ),
                                    tabPanel(tags$h5("Additional settings"),
                                             shiny::tabsetPanel(type = "tabs",
                                                         tabPanel(tags$h5("Merge additional data"), 
                                                                  tags$br(),
                                                                  tags$h5(style = "color: blue;","This allows you to merge additional data with the XYZ data, using a Unique object ID (recorded in a column in both datasets). First, load the additional data below:"),
                                                                  tags$br(),
                                                                  fileInput("file.extradata", "Choose File (.csv/.xls/.xlsx)",
                                                                            multiple = TRUE,
                                                                            accept = c("text/csv",
                                                                                       "text/comma-separated-values",
                                                                                       ".csv",
                                                                                       ".xlsx",".xls")),
                                                                  tags$h5(style = "color: blue;","Choose the column with the same Unique object ID, then press Merge "),
                                                                  uiOutput("set.columnID"),
                                                                  tags$br(),
                                                                  actionButton("goButton.set.columnID", "Merge"),
                                                                  tags$br(),
                                                                  tags$br(),
                                                                  tags$hr(),
                                                                  tags$h5(style = "color: red;","Report on the merging"),
                                                                  tags$h5(style = "color: blue;","IDs that are NOT unique in the XYZ dataset"),
                                                                  verbatimTextOutput("notunique"),
                                                                  tags$h5(style = "color: blue;","IDs that are NOT unique in the additional dataset"),
                                                                  verbatimTextOutput("notunique2"),
                                                                  tags$h5(style = "color: blue;","Objects present in the XYZ dataset that have no correspondence in the additional dataset"),
                                                                  verbatimTextOutput("suppl.no.include"),
                                                                  tags$h5(style = "color: blue;","Objects present in the additional dataset that have no correspondence in the XYZ dataset"),
                                                                  verbatimTextOutput("ID.no.suppl.data"),
                                                                  tags$br(),
                                                         ),#end tabpanel
                                                         tabPanel(tags$h5("Import orthophoto "),
                                                                  tags$br(),
                                                                  tags$h5(style = "color: red;","Orthophoto file must be in .tiff format"),
                                                                  tags$br(),
                                                                  fluidRow(
                                                                    fileInput("file2", "For x/y section",
                                                                              multiple = F,
                                                                              accept = c(".tif",".tiff")),
                                                                    uiOutput("liste.ortho.file2"),
                                                                    fileInput("file5", "For y/x section",
                                                                              multiple = F,
                                                                              accept = c(".tif",".tiff")),
                                                                    uiOutput("liste.ortho.file5"),
                                                                    fileInput("file3", "For x/z section",
                                                                              multiple = F,
                                                                              accept = c(".tif",".tiff")),
                                                                    uiOutput("liste.ortho.file3"),
                                                                    fileInput("file4", "For y/z section",
                                                                              multiple = F,
                                                                              accept = c(".tif",".tiff")),
                                                                    uiOutput("liste.ortho.file4"),
                                                                  )#end fluidrow
                                                         ),#end tabpanel
                                                         
                                                         tabPanel(tags$h5("Color ramps"),
                                                                  br(),
                                                                  column(6,
                                                                         column(6, downloadButton("save.col", "Save color ramp")),
                                                                         br(),
                                                                         br(),
                                                                         br(),
                                                                         br(),
                                                                         column(6,   fileInput("file.color", "Choose File to import color ramp (.csv)",
                                                                                               multiple = TRUE,
                                                                                               accept = c("text/csv",
                                                                                                          "text/comma-separated-values,text/plain",
                                                                                                          ".csv")), ),
                                                                  )#end of column
                                                         ),
                                                         
                                                         tabPanel(tags$h5("Slider parameters"),
                                                                  br(),
                                                                  
                                                                  tags$hr(),
                                                                  fluidRow(column (7,numericInput("stepXsize", "Steps used for the X slider in the quick sidebar", 0.1, min = 0.1, step=0.1, max=100, width="50%")),
                                                                           column (7,numericInput("stepYsize", "Steps used for the Y slider in the quick sidebar", 0.1, min = 0.1, step=0.1, max=100, width="50%")),
                                                                           column (7,numericInput("stepZsize", "Steps used for the Z slider in the quick sidebar", 0.1, min = 0.1, step=0.1, max=100, width="50%")),
                                                                  ),
                                                                  tags$hr(),
                                                         ),
                                                         tabPanel(tags$h5("Information shown while hovering points"),
                                                                  br(),
                                                                  uiOutput("liste.infos"),
                                                                  br(),
                                                                  actionButton("listeinfos.go", "Update"),
                                                         )
                                                         # tabPanel(tags$h5("Record new group"), 
                                                         #          br(),
                                                         #          br(),
                                                         #          # column (12,tags$h3("Record new group"),
                                                         #          #         br(),
                                                         #          #         textInput("text.new.group", label="Name of the new group variable",value = "new.group"),
                                                         #          #         uiOutput("liste.newgroup"),
                                                         #          #         actionButton("go.ng", "Create"),
                                                         #          #         br(),
                                                         #          #         br(),),
                                                         # 
                                                         #          
                                                         # ), #end tabpanel
                                                         # tabPanel(tags$h5("Export settings"), 
                                                         #          br(),
                                                         #          radioButtons("docpdfhtml", "Export format",
                                                         #                       choices = c(html = "html"),
                                                         #                       
                                                         #                       selected = "html", inline=TRUE),
                                                         #          br(),
                                                         #          fluidRow(
                                                         #            column(6, downloadButton("export.Rmarkdown", "Export settings as Rmarkdown document")),
                                                         #          )
                                                         # ), #end tabpanel
                                                         
                                             ),#end tabsetpanel temp
                                    ) # end of tabpanel
                             #       tabPanel(tags$h5("Generator of XY data"),  
                             #                tags$h5("Generate XY coordinates according to the squaring"),
                             #                radioButtons("format.square2", tags$h5("Format of the square:"),
                             #                             choices = c("A//notdone" = "A",
                             #                                         "J3" = "B",
                             #                                         "D5c" = "C",
                             #                                         "H8-SC1//notdone"="D"),
                             #                             selected = "C", inline=TRUE),
                             #                uiOutput("select.square"),
                             #                actionButton("go.gen", "Generate"),
                             # )
                             ),#end tabSETPanel
                    ),#end tabpanel
                    
                    tabPanel("Verif data", 
                             shiny::tabsetPanel(type = "tabs",         
                                       tabPanel(tags$h5("Basic data"),

                                                                       tags$h4(style = "color: blue;","summary of basic data"),
                                                                       tags$br(),
                                                                       uiOutput("sum.species2"),
                                                                       tags$br(),
                                                                       uiOutput("sum.bucket"),
                                                                       tags$br(),
                                                                       uiOutput("sum.remain"),
                                                                       tags$br(),
                                                                       uiOutput("sum.remain2")),
                                         tabPanel(tags$h5("Raw table"), 
                                                 column(10,
                                                           DTOutput("table")),
                                                  column(11, downloadButton("downloadData_rawdata", "Download")),
                                         ),#end tabpanel
                                         # tabPanel(tags$h5("Pivot table"),
                                         #          fluidRow(
                                         #            uiOutput("liste.summary"),
                                         #            column(5,
                                         #                   h4("Remains class "),
                                         #                   tableOutput("summary")),
                                         #            column(11, downloadButton("downloadData_pivotdata", "Download")),
                                         #          ) #end fluidrow
                                         # ), #end tabpanel
                                         tabPanel(tags$h5("Data material table per levels"),
                                                  fluidRow(
                                                    tags$br(),
                                                    tags$h5(style = "color: blue;","Correspond to nature object data per level, as defined in both select input in 'Data upload' window"),
                                                  
                                                    tags$br(),
                                                    uiOutput("sum.species"),
                                                    tags$br(),
                                                    tags$br(),
                                                    tags$hr(),
                                                    column(5,
                                                           #h4("Remains clas"),
                                                           DTOutput("table.species")),
                                                    column(11, downloadButton("downloadData_speciesdata", "Download")),
                                                  ) #end fluidrow
                                         ), #end tabpanel
                                       tabPanel(tags$h5("Data material table per ID"),
                                                tags$br(),
                                                tags$hr(),
                                                column(5,
                                                       #h4("Remains clas"),
                                                       DTOutput("table.dec.species")),
                                       ),#end tabpanel 
                                       tabPanel(tags$h5("Bucket verification panel"),
                                                tags$h4(style = "color: red;","This panel is dedicated to list some basic possible incongruency in the database"),
                                                tags$h5(style = "color: blue;","Here, the list of ID(s) that is associated with different bucket XYZ coordinates"),
                                                ## ici calculer avec les XYZ raws des buckets. save a part input$x etc
                                                
                                                tags$h5(style = "color: blue;","Here, the list of ID(s) that is associated with different bucket squares"),
                                                
                                                tags$h5(style = "color: blue;","Here, the list of spits associated with different bucket IDs"),
                                                
                                                tags$h5(style = "color: blue;","Here, the list of bucket XYZ coordinates not fitting with theorical coordinates calculated based on squares "),
                                                
                                       ),
                                       tabPanel(tags$h5("Table bucket comparison"),
                                                tags$br(),
                                                tags$h5(style = "color: blue;","To compare the data merged"),
                                                tags$br(),
                                                 uiOutput("radiolist1"),
                                                 uiOutput("radiolist2"),
                                                tags$br(),
                                                DTOutput("tablecomp") ,
                                                tags$br(),
                                                
                                       ), #end tabpanel #end tabPanel
                             ), #end tabset panel
                    ), #end tabPanel
                    
                    tabPanel("Plot - splits",
                             shiny::tabsetPanel(type = "tabs",
                                         id = "tabs3",
                                         shiny::navbarMenu(
                                         column (7, 
                                                 shinyWidgets::actionBttn(
                                                   inputId = "chr_lvl_setting3",
                                                   label = "To configure the squaring organization",
                                                   style = "unite",
                                                   color = "danger",
                                                   icon = icon("fas fa-cogs",lib = "font-awesome")
                                                 ),
                                                 tags$style("#bsmodal_lvl3 .modal-dialog{ width:1200px} 
                                                                .modal-backdrop {display: none;
                                                                                    z-index: 1040 !important;}
                                                                                .modal-content {margin: 2px auto;
                                                                                    z-index: 1100 !important;}"),
                                                 bsModal(
                                                   id = "bsmodal_lvl3",
                                                   title = "To order the stratigraphic level organisation",
                                                   trigger = "chr_lvl_setting3", size = "large",
                                                   tags$h4(style = "color: red;","Factor level order: Drag the button order with the mouse"), 
                                                   #myComplexTableUI,
                                                   orderInput('BC', 'Blank cell', items = c("Blank","Blank","Blank","Blank","Blank"),  connect = c("A","B","C","D","E"), as.source=TRUE),
                                                   uiOutput("order.level3"),
                                                   orderInput('A', 'Column1', items = NULL, connect = c( "levels.order2.orga1","B","C","D","E","BC")),
                                                   orderInput('B', 'Column2', items = NULL, connect = c("levels.order2.orga1","A","C","D","E","BC")),
                                                   orderInput('C', 'Column3', items = NULL, connect = c("levels.order2.orga1","A","B","D","E","BC")),
                                                   orderInput('D', 'Column4', items = NULL, connect = c("levels.order2.orga1","A","B","C","E","BC")),
                                                   orderInput('E', 'To remove', items = NULL, connect = c("levels.order2.orga1","A","B","C","D","BC")),
                                                   actionButton("go.oder", "Validate")
                                                   #uiOutput("order.level4")
                                                 ),
                                         ) , # end of column
                                         column (4,
                                                 bs4Dash::tooltip(
                                                   shinyWidgets::actionBttn(
                                                     inputId = "chr_lvl_setting2",
                                                     label = "To configure the split order",
                                                     style = "unite",
                                                     color = "danger",
                                                     icon = icon("fas fa-cogs",lib = "font-awesome")    ),
                                                   title = "To organize the order of splits (or any defined group, such as levels)",
                                                   placement = "right"   ),
                                                 tags$style("#bsmodal_lvl23 .modal-dialog{ width:1200px}
                                                                .modal-backdrop {display: none;
                                                                                    z-index: 1040 !important;}
                                                                                .modal-content {margin: 2px auto;
                                                                                    z-index: 1100 !important;}"),
                                                 bsModal(id = "bsmodal_lvl23",
                                                         title = "To order the stratigraphic level organisation",
                                                         trigger = "chr_lvl_setting2", size = "large",
                                                         tags$h4(style = "color: red;","Factor level order: Drag the button order with the mouse"),
                                                         #actionButton("reverse.button", label = "Reverse the order"),
                                                         uiOutput("order.level2")
                                                 ),
                                         ), # end of column
                                         
                                         ),
                                         tabPanel(tags$h5("barplot frequency"),
                                                  tags$br(),
                                                  
                                                  tags$br(),
                                                  column(12,
                                                         column(6,radioButtons("var.barplot", "Choice between using the splits or the altitude (z)",
                                                                               choices = c(split = "name",
                                                                                           Z = "Z"),
                                                                               selected = "name", inline=TRUE),),
                                                         tags$hr(),
                                                         
                                                  ),
                                                  tags$br(),
                                                  column(12,
                                                         uiOutput("plot.histo.freq"),
                                                         tags$br(),
                                                         tags$br(),
                                                         downloadButton("download_plot.histo.freq", "Download as .pdf"))
                                                  # uiOutput("plot.histo.freq.plotly")
                                         ),#end tabpanel   
                                         tabPanel(tags$h5("plot abundance data"),
                                                  tags$br(),
                                                  column(4,radioButtons("var.plot.abundance", "",
                                                                        choices = c(nr = "NR",
                                                                                    nr_corr = "NR per volum"), inline=TRUE),),
                                                  column(6,radioButtons("var.barplot2", "Choice between using the splits or the altitude (z)",
                                                                        choices = c(split = "name",
                                                                                    Z = "Z"),
                                                                        selected = "name", inline=TRUE),),
                                                  tags$br(),
                                                  
                                                  uiOutput("plot.abondance.graph"),
                                                  tags$br(),
                                                  tags$br(),
                                                  #downloadButton("download_plot.abundance", "Download as .pdf")
                                         ),#end tabpanel  
                                         tabPanel(tags$h5("plot square density"),
                                                  tags$br(),
                                                  tags$h4("in progress"),
                                                  uiOutput("plot.sqd"),
                                         )#end tabpanel         
                             ),#end tabsetpanel temp
                             
                    ), #end tabPanel
                    tabPanel("Plot - bucket xyz coord", 
                             shiny::tabsetPanel(type = "tabs",

                                         
                                         tabPanel(tags$h5("point plot"),
                                                  tags$h4(style = "color: blue;","To plot the coordinates of the buckets"),
                                                  column(12,
                                                  radioButtons("var.plot.simple", "section",
                                                               choices = c(xy = "xy",
                                                                           yx = "yx",
                                                                           yz = "yz",
                                                                           xz = "xz"),
                                                               selected = "xy", inline=TRUE),
                                                  tags$hr(),),
                                                  column(4,radioButtons("var.density.curves", "include density curves",
                                                                        choices = c(no = "no",
                                                                                    yes = "yes"),selected = "no"),),
                                                  fluidRow(tags$br(),
                                                           tags$br(),
                                                           column(12,
                                                                  uiOutput("plot.point"),),
                                                           tags$br(),
                                                  ),#end of fluidrow
                                         ),#end tabPanel 
                                         tabPanel(tags$h5("advanced point plot"),
                                                  tags$h4(style = "color: blue;","To plot the coordinates of the buckets"),
                                                  
                                                  column(12,      
                                                         radioButtons("var.plot.simple.adv", "section",
                                                                      choices = c(xy = "xy",
                                                                                  yx = "yx",
                                                                                  yz = "yz",
                                                                                  xz = "xz"),
                                                                      selected = "xy", inline=TRUE),
                                                         tags$br(),),
                                                  fluidRow(tags$br(),
                                                           tags$br(),
                                                           column(12,
                                                                  uiOutput("plot.point.adv"),),
                                                           tags$br(),
                                                  ),#end of fluidrow
                                         ),#end tabPanel 
                                          tabPanel(tags$h5("Pie chart"),
                                                  fluidRow(tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           column(12,sliderInput("scale.to.pie", label = h5("Scale of pie chart"), min=1, max=2000, value = 150,ticks = FALSE),),
                                                          column(12,uiOutput("limit.nbindiv.pie.simple"),),
                                                           tags$br(),
                                                           column(12,      
                                                                  radioButtons("var1.simple", "section",
                                                                               choices = c(xy = "xy",
                                                                                           yx = "yx",
                                                                                           yz = "yz",
                                                                                           xz = "xz"),
                                                                               selected = "xy", inline=TRUE),
                                                                  tags$br(),),
                                                           
                                                           column(12,
                                                                  uiOutput("plot2Dbox.simple"),),
                                                           tags$br(),),
                                                  fluidRow(
                                                    tags$br(),
                                                    tags$br(),
                                                    hr(style = "border-top: 1px solid #000000;"), 
                                                    column(12,
                                                           column(2,numericInput("ratio.to.coord.simple", label = h5("Ratio figure"), value = 1),),
                                                           column(2),
                                                    ),
                                                  ),
                                                  column(12,
                                                         column(2,radioButtons("var.ortho.simple", "include ortho",
                                                                               choices = c(no = "no",
                                                                                           yes = "yes"),
                                                                               selected = "no", inline=TRUE),  ),
                                                         # column(2, radioButtons("var.fit.table.simple", "Include refits",
                                                         #                        choices = c(no = "no",
                                                         #                                    yes = "yes"),
                                                         #                        selected = "no", inline=TRUE),),
                                                         column(2),
                                                         column(6,downloadButton("downloadData2D.simple", "Download as .pdf")), 
                                                         hr(style = "border-top: 0.5px solid #000000;"), ),
                                                  tags$br(),
                                                  
                                                  
                                         ),#end tabpanel
                                            

                            tabPanel(tags$h5("Swarm plot"),
                             tags$br(),
                             column(12,
                                    column(6,radioButtons("var.2d.slice", "section",
                                                          choices = c(xy = "xy",
                                                            yx = "yx",
                                                            yz = "yz",
                                                            xz = "xz"),
                                                          selected = "yz", inline=TRUE),
                                           tags$br(),),
                                    column(2),
                             ),
                              fluidRow(
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                column(12,
                                       uiOutput("plot.2dsw")),
                                tags$br(),      
                              ),# end of fluidrow
                             hr(style = "border-top: 1px solid #000000;"), 
                             fluidRow(column(12,
                                             column(2, uiOutput("ratiotocoorsimple2"),),
                                            # column(12, downloadButton("downloadData2d.slice", "Download as .HTML")),
                                             tags$br(),
                                             column(12, uiOutput("download.slice.output")),
                             ),),#end of fluidrow
                    ),#end tabPanel 2D slice
                    #tabPanel(tags$h5("Advanced pie chart plot"),
                             # fluidRow(tags$br(),
                             #          tags$br(),
                             #          tags$br(),
                             #          column(12,sliderInput("scale.to.pie.adv", label = h5("Scale"), min=1, max=200, value = 50,ticks = FALSE),),
                             #          column(12,uiOutput("limit.nbindiv.pie.adv"),),
                             #          tags$br(),
                             #          tags$br(),
                             #          column(12,      
                             #                 radioButtons("var.pie.adv", "section",
                             #                              choices = c(xy = "xy",
                             #                                          yx = "yx",
                             #                                          yz = "yz",
                             #                                          xz = "xz"),
                             #                              selected = "xy", inline=TRUE),
                             #                 tags$br(),),
                             #          
                             #          column(12,
                             #                 leafletOutput("plot2D.pie.adv"),),
                             #          tags$br(),),
                             
                   #),#end tabpanel    
                    # tabPanel(tags$h5("leaflet plot"),
                    #          tags$br(),
                    #          column(12,
                    #                 column(6,radioButtons("modehistoplot", "mode",
                    #                                       choices = c("pie chart" = 1,
                    #                                                   "histogram" = 2),
                    #                                       selected = 1, inline=TRUE),
                    #                        tags$br(),),
                    #                 column(2),
                    #          ),
                    #          tags$br(),
                    #          conditionalPanel(condition="input.modehistoplot==2",
                    #                           
                    #          
                    #          
                    #          fluidRow(
                    #            tags$br(),
                    #            tags$br(),
                    #            tags$br(),
                    #            column(12,sliderInput("scale.to.hist.adv", label = h5("Scale"), min=1, max=200, value = 50,ticks = FALSE),),
                    #            column(12,uiOutput("limit.nbindiv.histo.adv"),),
                    #            tags$br(),
                    #            tags$br(),
                    #            column(12,
                    #                   column(6,radioButtons("var.2d.histo", "section",
                    #                                         choices = c(xy = "xy",
                    #                                                     yx = "yx",
                    #                                                     yz = "yz",
                    #                                                     xz = "xz"),
                    #                                         selected = "yz", inline=TRUE),
                    #                          tags$br(),),
                    #            ), #end of column
                    #            column(12,
                    #                   leafletOutput("plot2D.histo.adv"),),
                    #            tags$br(),      
                    #          ),# end of fluidrow
                    #          hr(style = "border-top: 1px solid #000000;"), 
                    #          fluidRow(column(12,
                    #                          column(2, uiOutput("ratiotocoord.histo"),),
                    #                          tags$br(),
                    #                          column(12, uiOutput("download.histo.output")),
                    #          ),),#end of fluidrow
                    #          ),# end of conditionalPanel
                    #          conditionalPanel(condition="input.modehistoplot==1",
                    #                           fluidRow(tags$br(),
                    #                                    tags$br(),
                    #                                    tags$br(),
                    #                                    column(12,sliderInput("scale.to.pie.adv", label = h5("Scale"), min=1, max=200, value = 50,ticks = FALSE),),
                    #                                    column(12,uiOutput("limit.nbindiv.pie.adv"),),
                    #                                    tags$br(),
                    #                                    tags$br(),
                    #                                    column(12,      
                    #                                           radioButtons("var.pie.adv", "section",
                    #                                                        choices = c(xy = "xy",
                    #                                                                    yx = "yx",
                    #                                                                    yz = "yz",
                    #                                                                    xz = "xz"),
                    #                                                        selected = "yz", inline=TRUE),
                    #                                           tags$br(),),
                    #                                    
                    #                                    column(12,
                    #                                           leafletOutput("plot2D.pie.adv"),),
                    #                                    tags$br(),),           
                    #                           
                    #          )# end of conditionalPanel
                    # ),#end tabPanel 
                     
                    tabPanel(tags$h5("Density plot"), 
                             fluidRow(tags$br(),
                                      tags$br(),
                                      column(12,
                                             uiOutput("plotdens"),),
                                      tags$br(),
                             ),#end of fluidrow
                             tags$br(),
                             hr(style = "border-top: 1px solid #000000;"), 
                             fluidRow(column(12,
                                             column(4,radioButtons("var3", "section",
                                                                   choices = c(xy = "xy",
                                                                               yx = "yx",
                                                                               yz = "yz",
                                                                               xz = "xz"), inline=TRUE),),
                                             column(5, downloadButton("downloadDatadensity", "Download as .pdf")),
                                             column(2,numericInput("ratio.to.coord", label = h5("Ratio figure"), value = 1),),
                             ),
                             tags$hr(),
                             column(12,
                                    column(6,radioButtons("var.ortho2", "Include ortho",
                                                          choices = c(no = "no",
                                                                      yes = "yes"),
                                                          selected = "no", inline=TRUE),),
                                    tags$hr(),
                                    
                             ),
                             
                             column(12,
                                    column(4, radioButtons("var.point.size.density", "point size proportional",
                                                           choices = c(no = "no",
                                                                       yes = "yes"),selected = "no"),),
                                    column(12,sliderInput("size.dens", label = h5("Point scale"), min=1, max=10, value = 1,ticks = FALSE),),
                                    column(4, radioButtons("black.col.point", "point color = black",
                                                           choices = c(no = "no",
                                                                       yes = "yes"),selected = "no"),),
                                    
                                    column(4, radioButtons("var.plotlyg.lines", "include density lines",
                                                           choices = c(no = "no",
                                                                       yes = "yes"),selected = "no"),),
                                    column(4, radioButtons("fill_density", "include density fill",
                                                           choices = c(no = "no",
                                                                       yes = "yes"),selected = "no"),),
                                    column(4,radioButtons("var.density.curves", "include density curves",
                                                          choices = c(no = "no",
                                                                      yes = "yes"),selected = "no"),),),
                             column(4, sliderInput("alpha.density", "Point transparency",  min = 0, max=1, value=1, width="50%"),),
                             tags$br(),
                             tags$br(),
                             tags$hr(),
                             column(12,
                                    tags$h5(style = "color: blue;","Density calculated by mass::ke2D package, using Kernel density"),),
                             ) #end fluidrow
                    ) #end tabPanel
                    
                  ), #end tabset panel
          ), #end tabPanel    
                    
                    
                    
          # tabPanel("Plot - random coordinate",
          #          tabsetPanel(type = "tabs",
          #                      id = "tabs2",
          #                      tabPanel(tags$h5("request data"),
          #                               column(12,
          #                                      htmlOutput("nb.sub"),
          #                                      column(10,
          #                               tags$h4(style = "color: red;","Request data to parametrize"), ),
          #                               column(10,tags$br(),
          #                               tags$br(),),
          #                               column(10,
          #                              
          #                               # uiOutput("select.square.random"),
          #                               # bs4Dash::tooltip(
          #                               #   shinyWidgets::actionBttn(
          #                               #     inputId = "chr_sq2_setting",
          #                               #     label = NULL,
          #                               #     style = "unite",
          #                               #     color = "danger",
          #                               #     icon = icon("fas fa-cogs",lib = "font-awesome")
          #                               #   ),
          #                               #   title = "To configure the type of square",
          #                               #   placement = "right"
          #                               # ),
          #                               # tags$style("#bsmodal_lvl28 .modal-dialog{ width:1200px} 
          #                               #                         .modal-backdrop {
          #                               #                                             display: none;
          #                               #                                             z-index: 1040 !important;
          #                               #                                         }
          #                               #                                         
          #                               #                                         .modal-content {
          #                               #                                             margin: 2px auto;
          #                               #                                             z-index: 1100 !important;
          #                               #                                         }
          #                               #                                         
          #                               #                                         "),
          #                               
          #                               # bsModal(
          #                               #   id = "bsmodal_lvl28",
          #                               #   title = "To configure the type of square",
          #                               #   trigger = "chr_sq2_setting",size = "large",
          #                               #   radioButtons("format.square.random", "Format of the square:",
          #                               #                choices = c("A ou 1" = "A",
          #                               #                            "A1 ou D5" = "B",
          #                               #                            "A1a ou B5b" = "C",
          #                               #                            "A1-SC1"="D"),
          #                               #                selected = "C", inline=TRUE)
          #                               # ),
          #                               
          #                               ),
          #                               
          #                               
          #                               column(10,
          #                                     # actionButton("go.generate.square2", "Step1. Generate XY coordinate according to square"),
          #                                      tags$br(),
          #                               tags$br(),
          #                               shinyWidgets::actionBttn(
          #                                 inputId = "chr_lvl_settingrandom",
          #                                 label = "Step2. Configure the settings",
          #                                 style = "unite",
          #                                 color = "danger",
          #                                 icon = icon("fas fa-cogs",lib = "font-awesome")),
          #                               
          #                               tags$style("#bsmodal_lvlrandom .modal-dialog{ width:1200px}
          #                                                       .modal-backdrop {display: none;
          #                                                                           z-index: 1040 !important;}
          #                                                                       .modal-content {margin: 2px auto;
          #                                                                           z-index: 1100 !important;}"),
          #                               bsModal(id = "bsmodal_lvlrandom",
          #                                       title = "To configure the settings",
          #                                       trigger = "chr_lvl_settingrandom", 
          #                                       size = "large",
          #                                       radioButtons("var.random", "Choice the size of the squaring",
          #                                                    choices = c("m²" = "a",
          #                                                                "1/2m²" = "b",
          #                                                                "1/16m²"="c"),
          #                                                    selected = "b", inline=TRUE),
          #                                       
          #                                       radioButtons("var.data", "Unity of the coordinates ",
          #                                                    choices = c(m = "1",
          #                                                                cm = "100",
          #                                                                mm="1000"),
          #                                                    selected = "1", inline=TRUE),
          #                                       
          #                                   
          #                                       column(3,
          #                                       numericInput("val.epaisseur.dec", label = h5("Tickness mean values of the splits in cm"), value = 2))
          #                                       
          #                               ),),
          #                               column(10,tags$br(),),
          #                               column(10,actionButton("goButton.generate.random.data", "Step3. Generate random coordinates"),),
          #                               ), #end column
          #                               ),#end tabpanel
          #                      tabPanel(tags$h5("Advanced 2D plot"),
          #                               fluidRow(tags$br(),
          #                                        tags$br(),
          #                                        #column(1,  actionButton("run_button", "Display/refresh", icon = icon("play")),),
          #                                        tags$br(),
          #                                        tags$br(),
          #                                        tags$br(),
          #                                        column(12,      
          #                                               radioButtons("var1.random", "section",
          #                                                            choices = c(xy = "xy",
          #                                                                        yx = "yx",
          #                                                                        yz = "yz",
          #                                                                        xz = "xz"),
          #                                                            selected = "xy", inline=TRUE),
          #                                               tags$br(),),
          #                                        
          #                                        column(12,
          #                                               uiOutput("plot2Dbox.random"),),
          #                                        tags$br(),),
          #                               fluidRow(
          #                                 tags$br(),
          #                                 tags$br(),
          #                                 hr(style = "border-top: 1px solid #000000;"), 
          #                                 
          #                                 tags$br(),
          # 
          #                                 column(2),
          #                                 column(6,downloadButton("downloadData2D.random", "Download as .HTML")), ),
          #                               hr(style = "border-top: 0.5px solid #000000;"), 
          #          
          #                               tags$br(),
          #                               column(8,
          #                                      tags$br(),
          #                                      tags$br(), 
          #                               ),
          #                               ),#end fluidrow
          # 
          #                     # ), #end sub-tabpanel
          #                      
          #                      tabPanel(tags$h5("Simple 2D plot"),
          #                               fluidRow(tags$br(),
          #                                        tags$br(),
          #                                        column(6,radioButtons("var.rand.densi", "Show density",
          #                                                              choices = c(yes = "yes",
          #                                                                          no = "no"),
          #                                                              selected = "no", inline=TRUE),),
          #                                        tags$br(),
          #                                        tags$br(),
          #                                        column(12,      
          #                                               radioButtons("var1.simple.random", "section",
          #                                                            choices = c(xy = "xy",
          #                                                                        yx = "yx",
          #                                                                        yz = "yz",
          #                                                                        xz = "xz"),
          #                                                            selected = "xy", inline=TRUE),
          #                                               tags$br(),),
          #                                        
          #                                        column(12,
          #                                               uiOutput("plot2Dbox.simple.random"),),
          #                                        tags$br(),),
          #                               fluidRow(
          #                                 tags$br(),
          #                                 tags$br(),
          #                                 hr(style = "border-top: 1px solid #000000;"), 
          #                                 column(12,
          #                                        column(2,numericInput("ratio.to.coord.simple.random", label = h5("Ratio figure"), value = 1),),
          #                                        column(2),
          #                                 ),
          #                               ),
          #                               column(12,
          #                                      column(2,radioButtons("var.ortho.simple.random", "include ortho",
          #                                                            choices = c(no = "no",
          #                                                                        yes = "yes"),
          #                                                            selected = "no", inline=TRUE),),
          #                                      column(2),
          #                                      column(6,downloadButton("downloadData2D.simple.random", "Download as .pdf")), 
          #                                      hr(style = "border-top: 0.5px solid #000000;"), ),
          #                               tags$br(),
          #                      )     
          #          ),#end tabsetpanel temp
          # ), #end tabPanel
          # tabPanel("Microfauna treatment",
          #          tabsetPanel(type = "tabs",
          #                      id = "tabs.graph",
          #                      # tabPanel(tags$h5("request data"),
          #                      #
          #                      #          tags$h4(style = "color: red;","Request data to parametrize"),
          #                      #          tags$br(),
          #                      #
          #                      #
          #                      # ),#end tabpanel
          #                      tabPanel(tags$h5("Basic data"),
          # 
          #                               tags$h4(style = "color: blue;","summary of basic data"),
          #                               tags$br(),
          #                               uiOutput("sum.species2"),
          #                               tags$br(),
          #                               uiOutput("sum.bucket"),
          #                               tags$br(),
          #                               uiOutput("sum.remain"),
          #                               tags$br(),
          #                               uiOutput("sum.remain2")
          # 
          #                      ),#end tabpanel
          #                      tabPanel(tags$h5("rarity curves"),
          #                               tags$br(),
          #                                materialSwitch(
          #                                 inputId = "Id075",
          #                                 label = "Inext graph"),
          #                               tags$br(),
          #                               uiOutput("rarefactionplotui"),
          #                               tags$br(),
          #                               tags$br(),
          #                               column(11, downloadButton("downloadData_rarefactiongraph", "Download")),
          #                               tags$br(),
          #                               tags$br(),
          #                               DTOutput("table.species.perlevels"),
          #                               column(11, downloadButton("downloadData_rarefactiondata", "Download")),
          #                      ),#end tabpanel
          #                      tabPanel(tags$h5("Bioclim data"),
          #                               uiOutput("sum.bucket2"),
          #                               tags$br(),
          #                               tags$br(),
          #                               radioButtons("var.bioclim", "",
          #                                            choices = c("Rodent" = FALSE,
          #                                                        "Rodent + Eulipotyphla" = TRUE),
          #                                            selected = TRUE, inline=TRUE),
          #                               tags$br(),
          #                               DTOutput("bioclim.react"),
          #                               tags$br(),
          #                               column(11, downloadButton("downloadData_bioclim.react", "Download"),
          #                                      tags$br(),),
          #                               tags$br(),
          #                               DTOutput("bioclim.react2"),
          #                               tags$br(),
          #                               column(11, downloadButton("downloadData_bioclim.react2", "Download")),
          #                               tags$br(),
          #                               tags$h5(style = "color: black;","species name(s) not included:"),
          #                               uiOutput("bioclim.names_noused"),
          #                               tags$h5(style = "color: blue;","Be careful to have well written the species name"),
          #                               tags$h5(style = "color: blue;","exemple: Microtus_arvalis"),
          # 
          #                      ),#end tabpanel
          #                      tabPanel(tags$h5("Bioclim graph"),
          #                      ),#end tabpanel
          #                      tabPanel(tags$h5("Ratio data"),
          #                               uiOutput("Ratio.data.list"),
          #                               tags$br(),
          #                               uiOutput("name.of.dig.element"),
          #                               uiOutput("dig.col"),
          #                               tags$br(),
          #                               tags$br(),
          #                               uiOutput("Ratio.data.graph"),
          #                               )#end tabpanel
          #          ),#end tabsetpanel temp
          # 
          # ), #end tabPanel
          tabPanel("Palynograph",
                   tags$br(),
                   column(6,
                   uiOutput("squaretoselect"),
                   ),
                   column(2,
                   shinyWidgets::actionBttn(
                     inputId = "chr_settingsquare",
                     label = NULL,
                     style = "unite",
                     color = "danger",
                     icon = icon("fas fa-cogs",lib = "font-awesome")
                   ),
                   tags$style("#bsmodal_sq .modal-dialog{ width:1200px}
                                                                .modal-backdrop {
                                                                                    display: none;
                                                                                    z-index: 1040 !important;
                                                                                }

                                                                                .modal-content {
                                                                                    margin: 2px auto;
                                                                                    z-index: 1100 !important;
                                                                                }

                                                                                "),

                   bsModal(
                     id = "bsmodal_sq",
                     title = "To reorganize square order",
                     trigger = "chr_settingsquare",size = "large",

                   uiOutput("order.sq")

                   ),
                   
                   
                   ),
                   
                   column(12,
                   uiOutput("selectspecies"),
                   ),
                   
                   radioGroupButtons(
                     inputId = "trackObjDens",
                     label = NULL,
                     choices = c("Density" = 1, "Density species" = 2)
                     
                   ),
                   column(2,
                          shinyWidgets::actionBttn(
                            inputId = "chr_settingX",
                            label = NULL,
                            style = "unite",
                            color = "danger",
                            icon = icon("fas fa-cogs",lib = "font-awesome")
                          ),
                          tags$style("#bsmodal_X .modal-dialog{ width:1200px}
                                                                .modal-backdrop {
                                                                                    display: none;
                                                                                    z-index: 1040 !important;
                                                                                }

                                                                                .modal-content {
                                                                                    margin: 2px auto;
                                                                                    z-index: 1100 !important;
                                                                                }

                                                                                "),
                          
                          bsModal(
                            id = "bsmodal_X",
                            title = "To modify range of X",
                            trigger = "chr_settingX",size = "large",
                            
                            uiOutput("order.X")
                            
                          ),
                          
                          
                   ),
                   
                   conditionalPanel(
                     condition = "input.trackObjDens == 1",
                     tags$br(),
                     uiOutput("densitytoselect2"),
                     
                   ), # end of conditionnal panel
                   conditionalPanel(
                     condition = "input.trackObjDens == 2",
                     tags$br(),
                     uiOutput("densitytoselect"),
                     DTOutput("tablepalyno3"),
                   ), # end of conditionnal panel
                   
                   
                   
                   tags$head(
                     tags$link(
                       rel = "stylesheet",
                       href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
                     ),
                     tags$script(
                       src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
                     )
                   ),
                   # DTOutput("table.palyno"),
                   # modFunctionUI("tablepalyno2"),
                   DTOutput("tablepalyno2"),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   uiOutput("Palynograph.plot.output.simple"),
                   tags$br(),
                   column(5, downloadButton("downloadpalynograph", "Download as .pdf")),
                   # uiOutput("Palynograph.plot.output")
          ) #end tabPanel

        ) #end tabset panel
      ) #end main panel
      ,fluid=FALSE) #sidebarLayout  
  )#endfluidPage
) #end navbarPage
) #end  shinyUI
}
