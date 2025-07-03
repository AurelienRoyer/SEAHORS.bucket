css <- HTML(
  ".input-hidden {",
  "  position: absolute;",
  "  left: -9999px;",
  "}",
  "input[type=radio] + label>img {",
  "  width: 50px;",
  "  height: 50px;",
  "  transition: 500ms all;",
  "}",
  "input[type=radio]:checked + label>img {",
  "  border: 1px solid #fff;",
  "  box-shadow: 0 0 3px 3px #090;",
  "  transform: rotateZ(-10deg) rotateX(10deg);", 
  "}"
  
) 
 

callback2 <- JS("
  table.on('click', 'tbody td.editable', function(e) {
    var cell = table.cell(this);
    var colIndex = cell.index().column;
    if ($(this).hasClass('editing')) return;

    // Colonne forme (3e, index 2)
    if (colIndex === 2) {
      var currentValue = cell.data();
      var options = ['circle', 'triangle', 'square', 'diamond'];
      var select = $('<select></select>');
      for (var i = 0; i < options.length; i++) {
        var selected = options[i] === currentValue ? 'selected' : '';
        select.append('<option ' + selected + '>' + options[i] + '</option>');
      }
      $(this).addClass('editing').empty().append(select);
      select.focus();
      select.on('change blur', function() {
        var newValue = $(this).val();
        var edit = {
          row: cell.index().row,
          col: cell.index().column,
          value: newValue
        };
        Shiny.setInputValue('tablepalyno2_cell_edit', edit, {priority: 'event'});
      });
    }

    // Colonne couleur (4e, index 3)
    else if (colIndex === 3) {
      var currentValue = cell.data();
      var input = $('<input type=\"color\" value=\"' + currentValue + '\" style=\"width:100%; border:none; background:none;\">');
      $(this).addClass('editing').empty().append(input);
      input.focus();
      input.on('input blur', function() {
        var newValue = $(this).val();
        var edit = {
          row: cell.index().row,
          col: cell.index().column,
          value: newValue
        };
        Shiny.setInputValue('tablepalyno2_cell_edit', edit, {priority: 'event'});
      });
    }
  });
")

callback3 <- JS("
  table.on('click', 'tbody td.editable', function(e) {
    var cell = table.cell(this);
    var colIndex = cell.index().column;
    if ($(this).hasClass('editing')) return;

    // Colonne forme (3e, index 2)
    if (colIndex === 2) {
      var currentValue = cell.data();
      var options = ['dens', 'remove'];
      var select = $('<select></select>');
      for (var i = 0; i < options.length; i++) {
        var selected = options[i] === currentValue ? 'selected' : '';
        select.append('<option ' + selected + '>' + options[i] + '</option>');
      }
      $(this).addClass('editing').empty().append(select);
      select.focus();
      select.on('change blur', function() {
        var newValue = $(this).val();
        var edit = {
          row: cell.index().row,
          col: cell.index().column,
          value: newValue
        };
        Shiny.setInputValue('tablepalyno3_cell_edit', edit, {priority: 'event'});
      });
    }

    // Colonne couleur (4e, index 3)
    else if (colIndex === 3) {
      var currentValue = cell.data();
      var input = $('<input type=\"color\" value=\"' + currentValue + '\" style=\"width:100%; border:none; background:none;\">');
      $(this).addClass('editing').empty().append(input);
      input.focus();
      input.on('input blur', function() {
        var newValue = $(this).val();
        var edit = {
          row: cell.index().row,
          col: cell.index().column,
          value: newValue
        };
        Shiny.setInputValue('tablepalyno3_cell_edit', edit, {priority: 'event'});
      });
    }
  });
")


app_server <- function(input, output, session) {
  base::options(digits=9)
 
##### set variable to avoid notes in R package----
  .stretch <- NULL
  layer2 <- NULL
  level <- NULL
  point.size <- NULL
  

##### necessary settings----
  font.size <- "8pt"
  minsize<-reactiveVal(0.25) ##for min point
  size.scale<-reactiveVal(3) ##for point
  stepX<-reactiveVal(0.1) ## step size sliders
  stepY<-reactiveVal(0.1) ## step size sliders
  stepZ<-reactiveVal(0.1) ## step size sliders
  transpar<-reactiveVal(1) ## alpha for density plot
  #shape_all<-reactiveVal("circle") ##for import fit data
  setXX<-reactiveVal(NULL) ##input$setx
  setYY<-reactiveVal(NULL) ##input$sety
  setZZ<-reactiveVal(NULL) ##input$setz
  setID<-reactiveVal(NULL) ##input$setID
  height.size<-reactiveVal(800) ## default size of figure
  width.size<-reactiveVal(1000) ## default size of figure
  listinfosmarqueur<-reactiveVal(NULL) ## for listinfos to be null at the beginning. 
  legendplotlyfig<-reactiveVal("right") ##for legends.
  inputcolor<-reactiveVal("null")
  save.col.react.fit<-reactiveVal()
  nnrow.df.df<-reactiveVal(0) ##nrow df$df
  ratiox<-reactiveVal(1) ## aspectratio X
  ratioy<-reactiveVal(1) ## aspectratio y
  ratioz<-reactiveVal(1) ## aspectratio z
  ratio.simple<-reactiveVal(1)
  font_size<-reactiveVal(12)
  font_tick<-reactiveVal(12)
  nameX<-reactiveVal("X")
  nameY<-reactiveVal("Y")
  nameZ<-reactiveVal("Z")
  Xtickmarks.size<-reactiveVal()
  Ytickmarks.size<-reactiveVal()
  Ztickmarks.size<-reactiveVal()
  Xminorbreaks<-reactiveVal(1)
  Yminorbreaks<-reactiveVal(1)
  Zminorbreaks<-reactiveVal(1)
  ID.no.suppl.data.txt<-reactiveVal("no data")
  notunique.txt<-reactiveVal("no data")
  notunique2.txt<-reactiveVal("no data")
  suppl.no.include.txt<-reactiveVal("no data")
  ##for import
  natureofobjects.list<-reactiveVal()
  natureofobjects.list.sup<-reactiveVal()
  # 1 file
  input_file_xyzspecies.datapath<-reactiveVal()
  input_file_xyzspecies.name<-reactiveVal()
  getdata.launch.xyzspecies<-reactiveVal()
  # 2 files
  input_file1.name<-reactiveVal()
  input_file1.datapath<-reactiveVal()
  getdata.launch<-reactiveVal()
  input_file1.species.name<-reactiveVal()
  input_file1.species.datapath<-reactiveVal()
  getdata.launch.species<-reactiveVal()
  fileisupload<-reactiveVal(NULL)
  fileisupload.species<-reactiveVal(NULL)
  listinfosmarqueur.species<-reactiveVal(0)
  merging.was.done<-reactiveVal(NULL)
  squareisdone<-reactiveVal(0)
  readytosub<-reactiveVal(0)
  number.species.per.levels<-reactiveVal(NULL)
 
##### creation of empty reactiveValues () 
session_store <- reactiveValues()  ## for save  plot 
df<-reactiveValues( #creation df 
    df=NULL,
    df.temp=NULL,
    df.data=NULL,
    df.species=NULL) # end reactivevalues 
factor.order.square.split<-reactiveValues(A=NULL,
                                          B=NULL,
                                          C=NULL,
                                          D=NULL,
                                          E=NULL) 
orga.square.histo.freq<-reactiveValues(nnnnncol=NULL,
                                       nnnnrow=NULL,
                                       list.nom.pp=NULL,
                                       pp=NULL,
                                       typ.trav=NULL,
                                       typ.subsquare=NULL,
                                       typ.al=NULL
)
data.column<-reactiveValues(data.df.xy=NULL,
                            data.graph=NULL,
                            data.graph.indiv=NULL,
                            data.graph.indiv.random=NULL)
values <- reactiveValues(newgroup = NULL)


##### reset xyz data ----
observeEvent(input$reset.BDD, { 
  fileisupload(NULL)
  shinyjs::refresh()
  shinyjs::reset('file1')
  df$df <- NULL
  df$df.temp <- NULL
  df$df.data <- NULL
  input_file1.name(NULL)
  input_file1.datapath(NULL)
}, priority = 1000)

##### reset species data ----
observeEvent(input$reset.BDD.species, { 
  fileisupload.species(NULL)
  shinyjs::refresh()
  shinyjs::reset('file1.species')
  df$df.species <- NULL
  df$temp2 <- NULL
  input_file1.species.name(NULL)
  input_file1.species.datapath(NULL)
}, priority = 1000)

##function
createdCell <- function(dat2){
  dat2_json <- jsonlite::toJSON(dat2, dataframe = "values")
  c(
    "function(td, cellData, rowData, rowIndex, colIndex){",
    sprintf("  var matrix = %s;", dat2_json),
    "  var tmatrix = matrix[0].map((col, i) => matrix.map(row => row[i]));", # we transpose
    "  $(td).attr('data-levels', JSON.stringify(tmatrix[colIndex]));",
    "}"
  )
}


##### import 1 file----  
observeEvent(input$file_xyzspecies, {
  input_file_xyzspecies.name(input$file_xyzspecies$name)
  input_file_xyzspecies.datapath(input$file_xyzspecies$datapath)
})
observe({
  req(!is.null(input_file_xyzspecies.datapath()))
  extension <- tools::file_ext(input_file_xyzspecies.name())
  switch(extension,
         csv = {updateSelectInput(session, "worksheet_xyzspecies", choices = input_file_xyzspecies.name())},
         xls =   {    selectionWorksheet <-excel_sheets(path = input_file_xyzspecies.datapath())
         updateSelectInput(session, "worksheet_xyzspecies", choices = selectionWorksheet)},
         xlsx =  {      selectionWorksheet <-excel_sheets(path = input_file_xyzspecies.datapath())
         updateSelectInput(session, "worksheet_xyzspecies", choices = selectionWorksheet)})
})
observeEvent(input$getData_xyzspecies, {
  getdata.launch.xyzspecies(input$getData_xyzspecies)
})
observeEvent(getdata.launch.xyzspecies(), {
  req(!is.null(input_file_xyzspecies.datapath()))
  df$df.data <-importdatafunction(input_file_xyzspecies.datapath(),input_file_xyzspecies.name(),df$df.data,input$worksheet_xyzspecies)
  df$df<-df$df.data  
  null<-as.numeric(1)
  df$df<-cbind.data.frame(null,df$df)
  fileisupload(1) 
  fileisupload.species(1)
  merging.was.done(1)
})# 


##### import 2 files----
###### import xyz data----
observeEvent(input$file1, {
    input_file1.name(input$file1$name)
    input_file1.datapath(input$file1$datapath)
  })
observe({
    req(!is.null(input_file1.datapath()))
    extension <- tools::file_ext(input_file1.name())
    switch(extension,
           csv = {updateSelectInput(session, "worksheet", choices = input_file1.name())},
           xls =   {    selectionWorksheet <-excel_sheets(path = input_file1.datapath())
           updateSelectInput(session, "worksheet", choices = selectionWorksheet)},
           xlsx =  {      selectionWorksheet <-excel_sheets(path = input_file1.datapath())
           updateSelectInput(session, "worksheet", choices = selectionWorksheet)})

  })
observeEvent(input$getData, {
    getdata.launch(input$getData)

  })
observeEvent(getdata.launch(), {
    req(!is.null(input_file1.datapath()))
  
    df$df.temp <-importdatafunction(input_file1.datapath(),input_file1.name(),df$df.temp,input$worksheet)
    df$df.data <- df$df.temp

    fileisupload(1)
   
  })# end observe of df$df2

###### import species data----
observeEvent(input$file1.species, {
   input_file1.species.name(input$file1.species$name)
   input_file1.species.datapath(input$file1.species$datapath)
 })
 observeEvent(input$getData.species, {
   getdata.launch.species(input$getData.species)
 })
 
 observe({
   req(!is.null(input_file1.species.datapath()))
   extension <- tools::file_ext(input_file1.species.name())
   switch(extension,
          csv = {updateSelectInput(session, "worksheet.species", choices = input_file1.species.name())},
          xls =   {    selectionWorksheet <-excel_sheets(path = input_file1.species.datapath())
          updateSelectInput(session, "worksheet.species", choices = selectionWorksheet)},
          xlsx =  {      selectionWorksheet <-excel_sheets(path = input_file1.species.datapath())
          updateSelectInput(session, "worksheet.species", choices = selectionWorksheet)})
 })
 
 observeEvent(getdata.launch.species(), {
   req(!is.null(input_file1.species.datapath()))
   df$df.temp2 <-importdatafunction(input_file1.species.datapath(),input_file1.species.name(),df$df.temp2,input$worksheet.species)
   df$df.species<-df$df.temp2[,!sapply(df$df.temp2, function(x) is.logical(x))] ##remove column without data
   nb.specimen<-"1"
   df$df.species<-cbind(nb.specimen,df$df.species)
   fileisupload.species(1)

 })# end observe of df.species$df2

 
###### merging the two tables----
 observeEvent(input$merge.data.tables,{
   req(!is.null(fileisupload()))
   req(!is.null(fileisupload.species()))
   req(!is.null(setID()))
   colnames(df$df.species)[colnames(df$df.species)==input$setID3]<-setID()
   df$df<-inner_join(df$df.data,df$df.species, by=setID())
   nnrow.df.df(nrow(df$df))
   null<-as.numeric(1)
   df$df<-cbind.data.frame(null,df$df)
   # if (input$set.dataoneID== TRUE) {
   #   
   # }
   merging.was.done(1)
 })

 
 
observeEvent(input$request_file, {
  req(!is.null(fileisupload()))
  showModal(
    modalDialog(
      title = tags$h4(style = "color: red;","Request data"),
      easyClose = T,
       fluidRow(column(6,
                       selectInput("setx", h4("x of the bucket (Default name: x)"),
                                   choices = c(names(df$df)[c(1:ncol(df$df))]),
                                   selected = liste.x()),
       ),
       column(2,
              checkboxInput("checkbox.invX", label = "Inversion of x", value = F),),),
       fluidRow(column(6,
                       selectInput("sety", h4("y of the bucket (Default name: y)"),
                                   choices = names(df$df)[c(1:ncol(df$df))],
                                   selected = liste.y())  ),
                column(2,
                       checkboxInput("checkbox.invY", label = "Inversion of y", value = F),),),
      fluidRow(
         column(6,
                       selectInput("setz", h4("z of the bucket (Default name: z)"),
                                   choices = names(df$df)[c(1:ncol(df$df))],
                                   selected = liste.z()),),
                column(2,
                       checkboxInput("checkbox.invZ", label = "Inversion of z", value = F),),
               ),
      fluidRow(
      column (6, 
       selectInput("setspecies", h4("The variable to explore (Default name: species, nature)"),
                    choices = names(df$df)[c(1:ncol(df$df))],
                    selected = liste.species2()),
      ),
      column(3,
      selectInput("setnb", h4("Number of objects"),
                  choices = names(df$df)[c(1:ncol(df$df))],
                  selected = liste.nb2())
      ),),
      
       
     
    
    )
  )
  readytosub(1)
})

#### step 3 generation of the squaring ----
activationofgeneration<-reactiveVal(NULL)

formatsquare<-reactiveVal("C")


observeEvent(input$request_file2, {
  req(!is.null(fileisupload()))
  print("rrrr")
  showModal(
    modalDialog(
      title = tags$h4(style = "color: red;","Setting of the squaring"),
      easyClose = T,
      fluidRow(column (6, 
             # uiOutput("set.square"),
             selectInput("setsquare", h4("Square (Default name: square)"),
                         choices = names(df$df)[c(1:ncol(df$df))],
                         selected = liste.square()),
      ),
      column (3,

              radioButtons("format.square", "Format of the square:",
                           choices = c("A ou 1" = "A",
                                       "A1 ou D5" = "B",
                                       "A1a ou B5b" = "C",
                                       "A1-SC1"="D"),
                           selected="C")
      ), #end of column
      fluidRow(column (6,uiOutput("set.dec"),),), #end of column
      fluidRow(column (6, 
                       wellPanel(
                       tags$label("To generate a squaring:"),
                       tags$label("1-Choose the subsquare with 0,0 coordinates:"),
                       radioButtons("varsub0", "",
                                    choices = c(A = "a",
                                                B = "b",
                                                C = "c",
                                                D = "d"),
                                    selected = "a", inline=TRUE),
                         tags$label("2-Choose the subsquare organisation:"),
                 
                         radioImages(
                           "radio",
                           images = c("www/ABCD.png", "www/ADBC.png","www/ABDC.png", "www/ACBD.png"),
                           values = c("ABCD", "ADBC", "ABDC","ACBD")
                         )),
      ),
),
      actionButton("go.gen", "Generate")
      ) 
    )
    )
  readytosub(2)
  activationofgeneration("0")
})

observeEvent(c(input$format.square),{
  formatsquare(input$format.square)
})

observeEvent(input$go.gen, {
  req(!is.null(merging.was.done()))
  req(!is.null(activationofgeneration()))
  req(!is.null(input$setsquare))
  
    df.sub<-df$df
    assign("temp.data.gat", df.sub, envir = .GlobalEnv)
    setsquare<-input$setsquare
    list.sq<-levels(as.factor(df.sub[[setsquare]]))
    nbsquare<-length(list.sq)
    typ.subsquare<-0
    xyz.data.sq<-as.data.frame(matrix(data=list.sq,byrow=F))
    xyz.data.sq$Y_new<-"0"
    xyz.data.sq$Z_new<-"0"
    bb<-str_split((xyz.data.sq[,1]), "\\d+")
    
    
    bb<-t(as.data.frame(lapply(bb, "[", c(1,2))))
    cc<-as.numeric(str_extract(xyz.data.sq[,1], "\\d+"))
    xyz.data.sq<-cbind.data.frame(bb,cc,xyz.data.sq)
    xyz.data.sq$Y_new<-xyz.data.sq$cc+0.5
    xyz.data.sq[["1"]]<-str_to_lower(xyz.data.sq[["1"]])
    typ.trav<-levels(as.factor(df.sub[[setsquare]]))
    typ.al<-levels(as.factor(as.numeric(str_extract(df.sub[[setsquare]], "\\d+"))))
    indice<-cbind.data.frame(letters,seq(1:26))
    colnames(indice)<-c("1","X_new")
    xyz.data.sq<-inner_join(xyz.data.sq,indice)
    xyz.data.sq$X_new<-xyz.data.sq$X_new+0.5
    
    if (formatsquare() == "C") {
      xyz.data.sq$X_new[xyz.data.sq[["2"]]=="A"]<-xyz.data.sq$X_new-0.25
      xyz.data.sq$Y_new[xyz.data.sq[["2"]]=="A"]<-xyz.data.sq$Y_new-0.25
      xyz.data.sq$X_new[xyz.data.sq[["2"]]=="a"]<-xyz.data.sq$X_new-0.25
      xyz.data.sq$Y_new[xyz.data.sq[["2"]]=="a"]<-xyz.data.sq$Y_new-0.25
      
      xyz.data.sq$X_new[xyz.data.sq[["2"]]=="B"]<-xyz.data.sq$X_new+0.25
      xyz.data.sq$Y_new[xyz.data.sq[["2"]]=="B"]<-xyz.data.sq$Y_new-0.25
      xyz.data.sq$X_new[xyz.data.sq[["2"]]=="b"]<-xyz.data.sq$X_new+0.25
      xyz.data.sq$Y_new[xyz.data.sq[["2"]]=="b"]<-xyz.data.sq$Y_new-0.25
      
      xyz.data.sq$X_new[xyz.data.sq[["2"]]=="C"]<-xyz.data.sq$X_new+0.25
      xyz.data.sq$Y_new[xyz.data.sq[["2"]]=="C"]<-xyz.data.sq$Y_new+0.25
      xyz.data.sq$X_new[xyz.data.sq[["2"]]=="c"]<-xyz.data.sq$X_new+0.25
      xyz.data.sq$Y_new[xyz.data.sq[["2"]]=="c"]<-xyz.data.sq$Y_new+0.25
      
      xyz.data.sq$X_new[xyz.data.sq[["2"]]=="D"]<-xyz.data.sq$X_new-0.25
      xyz.data.sq$Y_new[xyz.data.sq[["2"]]=="D"]<-xyz.data.sq$Y_new+0.25
      xyz.data.sq$X_new[xyz.data.sq[["2"]]=="d"]<-xyz.data.sq$X_new-0.25
      xyz.data.sq$Y_new[xyz.data.sq[["2"]]=="d"]<-xyz.data.sq$Y_new+0.25
    }
    colnames(xyz.data.sq)[4]<-setsquare
    temp<-inner_join(df.sub,xyz.data.sq[,4:7])
    df$df<-temp
    setXX("X_new")
    setYY("Y_new")

    updateSelectInput(session,"setx",choices = names(df$df)[c(1:ncol(df$df))],
                      selected = "X_new")
    xmax = df$df[,"X_new"] %>% ceiling() %>% max(na.rm = TRUE)
    xmin=df$df[,"X_new"] %>% floor() %>% min(na.rm = TRUE)
    updateSliderInput(session,'xslider','x limits',min=xmin,max=xmax,value=c(xmin,xmax),step=stepX())
    x2min=input$xslider[1]
    x2max=input$xslider[2]
    # updateSliderInput(session,'ssectionXx2','x (point size): min/max',min=x2min,max=x2max,value=c(x2min,x2max),step=stepX())
    # 
    updateSelectInput(session,"sety",choices = names(df$df)[c(1:ncol(df$df))],
                      selected = "Y_new")
    ymax = df$df[,setYY()] %>% ceiling() %>% max(na.rm = TRUE)
    ymin=df$df[,setYY()] %>% floor() %>% min(na.rm = TRUE)
    updateSliderInput(session,'yslider','y limits',min=ymin,max=ymax,value=c(ymin,ymax),step=stepY())
    y2min=input$yslider[1]
    y2max=input$yslider[2]
    # updateSliderInput(session,'ssectionXy2','y (point size): min/max',min=y2min,max=y2max,value=c(y2min,y2max),step=stepY())
    
    print("aaaa")
    
      setID<-setID()
     # setsquare<-input$setsquare
      setdec<-input$setdec
      #setspecies<-input$setspecies
      data.annex<-df.sub[,c(setID,setsquare,setdec)]
      print(data.annex)
      data.annex<-dplyr::distinct(data.annex)
      list.sq<-levels(as.factor(data.annex[[setsquare]]))
      nbsquare<-length(list.sq)
      typ.subsquare<-NULL
      switch(formatsquare(),
             A= { typ.trav<-levels(as.factor(data.annex[[setsquare]]))
             count.nb.al<-length(typ.trav)
             count.nb.trav<-1
             typ.al<-1},
             B= { typ.trav<-levels(as.factor(str_replace(data.annex[[setsquare]],str_extract(data.annex[[setsquare]], "\\d+"),"")))
             count.nb.trav<-length(typ.trav)
             typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
             count.nb.al<-length(typ.al)
             },
             C= {
               aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "\\d+")
               aa<-t(as.data.frame(aa))
               typ.trav<-levels(as.factor(aa[,1]))
               
        
               
               typ.subsquare<-levels(as.factor(aa[,2]))
               count.nb.trav<-length(typ.trav)
               typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
               count.nb.al<-length(typ.al)

             },
             D={

               aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "\\d+")
               aa2<-lapply(aa, "[", 1)
               aa2<-t(as.data.frame(aa2))
               typ.trav<-levels(as.factor(aa2[,1]))
               aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "-")
               aa2<-lapply(aa, "[", 2)
               aa2<-str_replace(aa2, "SC", "")
               typ.subsquare<-levels(as.factor(aa2))
               count.nb.trav<-length(typ.trav)
               typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
               count.nb.al<-length(typ.al)
             })
      
      
      print(typ.subsquare)
      nnnnncol<-count.nb.al
      nnnnrow<-count.nb.trav
      theo<-nnnnncol*nnnnrow
      # ici rajotuer sécurité si trompe de square (genre D) et typ.subsquare == null ou autre chose, faisant planter lignes ci dessous
    
 if (!is.null(typ.subsquare)){
        typ.subsquare<-str_replace_all(typ.subsquare,"[:punct:]|[:space:]|[:math:]","")
        
        theo<-theo*length(typ.subsquare)
        
        nnnnncol<-nnnnncol*length(typ.subsquare)
      }

      pp<-vector("list", theo)
      list.nom.pp<-vector(length = theo)
      k=0
      for (i in 1:count.nb.trav) {
        k<-k
        for (j in 1:count.nb.al) {
          k<-k
          if (!is.null(typ.subsquare)){
            for (y in 1:length(typ.subsquare)) {
              k<-k+1
              list.nom.pp[[k]]<-paste0(typ.trav[[i]],typ.al[[j]],typ.subsquare[[y]])
            }
          } else {
            k<-k+1
            list.nom.pp[[k]]<-paste0(typ.trav[[i]],typ.al[[j]])
          }
        }
      }

      orga.square.histo.freq$nnnnncol<-nnnnncol
      orga.square.histo.freq$nnnnrow<-nnnnrow
      names(pp)<-list.nom.pp
      orga.square.histo.freq$list.nom.pp<-list.nom.pp
      orga.square.histo.freq$pp<-pp
      orga.square.histo.freq$typ.trav<-typ.trav
      orga.square.histo.freq$typ.subsquare<-typ.subsquare
      orga.square.histo.freq$typ.al<-typ.al
    squareisdone(1)
    showModal(modalDialog(
      title = "New XYZ data",
      HTML(paste(" New XY data have been generated using <br>
                        ", input$setsquare, "<br>",
                 "Here is new data: <br>", 
                 xyz.data.sq[1,c(4)],xyz.data.sq[1,c(5)],xyz.data.sq[1,c(6)],xyz.data.sq[1,c(7)]
      ))
    ))
 }) ## end generator XYZ




#### output loading slide ----
 
liste.x<-reactiveVal(c("X_new","x","X","null","SPATIAL..X"))

 observeEvent(input$setx,{
   liste.x(c(input$setx))
 })
liste.y<-reactiveVal(c("Y_new","y","Y","null","SPATIAL..Y"))
observeEvent(input$sety,{
   liste.y(c(input$sety))
 })
 liste.z<-reactiveVal(c("z","Z","null","SPATIAL..Z","Z_new"))
 observeEvent(input$setz,{
   liste.z(c(input$setz))
 })
 liste.date<-reactiveVal(c("Years","periods","SPATIAL..Year","Year","year_exca"))
 observeEvent(input$setdate,{
   liste.date(c(input$setdate))
 })
 liste.nature2<-reactiveVal(c("Type","null","Nature","Code","name_taxa"))
 observeEvent(input$setnature,{
   liste.nature2(c(input$setnature))
 })
 liste.levels<-reactiveVal(c("UAS","Levels","null","couche","SPATIAL..USfield","Assemblage","name_level","name_us"))
 observeEvent(input$setlevels,{
   #ajout a tester
   df$df[[input$setlevels]]<-str_replace_all(df$df[[input$setlevels]],"[:punct:]|[:space:]|[:math:]",".")
   ##
   liste.levels(c(input$setlevels))
   factor.order.level(input$setlevels)
 })
 liste.dec<-reactiveVal(c("dec","Dec","split","null","name_dec"))
 observeEvent(input$setdec,{
   liste.dec(c(input$setdec))
 })
 liste.square<-reactiveVal(c("carré","square","null"))
 observeEvent(input$setsquare,{
   liste.square(c(input$setsquare))
 })
 liste.passe2<-reactiveVal(c("Passe","null"))
 observeEvent(input$setpasse,{
   liste.passe2(c(input$setpasse))
 })
liste.ID<-reactiveVal(c("ID","Point","null","fieldID","ID_dec"))
liste.sector2<-reactiveVal(c("null","context","localisation","Sector","SPATIAL..Square_field","Square","name_square"))
observeEvent(input$setsector,{
   liste.sector2(c(input$setsector))
 })
liste.species2<-reactiveVal(c("species","ESP7CE","nature","name_species"))
observeEvent(input$setspecies,{
   liste.species2(c(input$setspecies))
 })
liste.nb2<-reactiveVal(c("nb","nb_remains"))
observeEvent(input$setnb,{
  liste.nb2(c(input$setnb))
 })
 
 observeEvent(input$setx,{
   df$df[,input$setx]<-df$df[,input$setx] %>% as.numeric()
   setXX(input$setx)
   nameX(input$setx)
 })
 
 observeEvent(input$sety,{
   df$df[,input$sety]<-df$df[,input$sety] %>% as.numeric()  
   setYY(input$sety)
   nameY(input$sety)
 })
 
 observeEvent(input$setz,{ 
   df$df[,input$setz]<-df$df[,input$setz] %>% as.numeric() 
   setZZ(input$setz)
   nameZ(input$setz)
 })
 
 output$set.nature=renderUI({
   req(!is.null(merging.was.done()))
   selectInput("setnature", h4("Type (Default name: Type)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = liste.nature2())
 }) 
 output$set.ID=renderUI({
   req(!is.null(fileisupload()))

   selectInput("setID", h4("Unique object ID (Default name: ID)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = liste.ID())
 })   
 
 output$set.ID2=renderUI({
   req(!is.null(fileisupload()))
   selectInput("setID2", h4("Unique object ID (Default name: ID)"),
               choices = names(df$df.data)[c(1:ncol(df$df.data))],
               selected = liste.ID())
 })
 
 output$set.ID.species=renderUI({
   req(!is.null(fileisupload.species()))
   selectInput("setID3", h4("Unique object ID (Default name: ID)"),
               choices = names(df$df.species)[c(1:ncol(df$df.species))],
               selected = liste.ID())
 })
 
 
 observeEvent(input$setID2,{
   setID(input$setID2)
   liste.ID(c(input$setID2))
 })
 observeEvent(input$setID,{
   setID(input$setID)
   liste.ID(c(input$setID))
 })
 output$set.vol=renderUI({
   req(!is.null(merging.was.done()))
 
   selectInput("setvol", h4("Volume of sediment (Default name: Volume)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = c("Vol","Volume","Litres","Volume/litres"))
 }) 
 output$set.anat=renderUI({
   req(!is.null(merging.was.done()))
   selectInput("setanat", h4("Anatomy of remains (Default name: anat)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = c("anat","name_anat"))
 }) 
 observeEvent(input$setnb,{ 
   req(!is.null(merging.was.done()))
   df$df[,input$setnb]<-df$df[,input$setnb] %>% as.numeric() 
 })

### output after merging ----
 output$set.levels=renderUI({
   req(!is.null(merging.was.done()))
   selectInput("setlevels", h4("Levels (Default name: Levels)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = liste.levels())
 }) 
 
 output$set.date=renderUI({
   req(!is.null(merging.was.done()))
   selectInput("setdate", h4("years : format years (Default name: Years)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = liste.date()) 
 }) 

 output$set.passe=renderUI({
   req(!is.null(merging.was.done()))
   selectInput("setpasse", h4("others (No default name)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = liste.passe2())
 }) 

 output$set.sector=renderUI({
   req(!is.null(merging.was.done()))
   selectInput("setsector", h4("Context/sector (Default name: Context, Sector)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = liste.sector2())
 }) 
 
 output$set.dec=renderUI({
   req(!is.null(merging.was.done()))
   selectInput("setdec", h4("Splits (Default name: dec)"),
               choices = names(df$df)[c(1:ncol(df$df))],
               selected = liste.dec())
 }) 
 
 # output$set.square=renderUI({
 #   req(!is.null(merging.was.done()))
 #   selectInput("setsquare", h4("Square (Default name: square)"),
 #               choices = names(df$df)[c(1:ncol(df$df))],
 #               selected = liste.square())
 # }) 

 observeEvent(c(input$checkbox.invX), {
   req(!is.null(setXX()))
   req(input$checkbox.invX==TRUE)
   df$df[,setXX()]<-df$df[,setXX()]*-1
   updateSelectInput(session,"setx",choices = names(df$df)[c(1:ncol(df$df))],
                     selected = liste.x())
   xmax = df$df[,setXX()] %>% ceiling() %>% max(na.rm = TRUE)
   xmin=df$df[,setXX()] %>% floor() %>% min(na.rm = TRUE)
   updateSliderInput(session,'xslider','x limits',min=xmin,max=xmax,value=c(xmin,xmax),step=stepX())
 })
 observeEvent(input$checkbox.invY, {
   req(!is.null(setYY()))
   req(input$checkbox.invY==TRUE)
   df$df[,setYY()]<-df$df[,setYY()]*-1
   updateSelectInput(session,"sety",choices = names(df$df)[c(1:ncol(df$df))],
                     selected = liste.y())
   ymax = df$df[,setYY()] %>% ceiling() %>% max(na.rm = TRUE)
   ymin=df$df[,setYY()] %>% floor() %>% min(na.rm = TRUE)
   updateSliderInput(session,'yslider','y limits',min=ymin,max=ymax,value=c(ymin,ymax),step=stepY())
 })
 observeEvent(input$checkbox.invZ, {
   req(!is.null(setZZ()))
   req(input$checkbox.invZ==TRUE)
   df$df.data[,setZZ()]<-df$df[,setZZ()]*-1
   updateSelectInput(session,"setz",choices = names(df$df.data)[c(1:ncol(df$df))],
                     selected = liste.z())
   zmax = df$df[,setZZ()] %>% ceiling() %>% max(na.rm = TRUE)
   zmin=df$df[,setZZ()] %>% floor() %>% min(na.rm = TRUE)
   updateSliderInput(session,'zslider','z limits',min=zmin,max=zmax,value=c(zmin,zmax),step=stepZ())
})

  observeEvent(input$Name.X, {
    req(input$setx)
    nameX(input$Name.X)
  })
  observeEvent(input$Name.Y, {
    req(input$sety)
    nameY(input$Name.Y)
  })
  observeEvent(input$Name.Z, {
    req(input$setz)
    nameZ(input$Name.Z)
  })
  

#### others options ----
observeEvent(input$setspecies,{
   updateSelectInput(session,"Colors",choices = names(df$df)[c(1:ncol(df$df))],
                     selected = input$setspecies)
   inputcolor(input$setspecies)
 })
 
observeEvent(input$minsize, {
    minsize(input$minsize)
  })
observeEvent(input$alpha.density,{
    transpar(input$alpha.density)
  })
observeEvent(input$point.size,{
    size.scale(input$point.size)
  })
opacity<-reactiveVal(1)
observeEvent(input$point.opa,{
  opacity(input$point.opa)
})

#### button for png dl
icon_svg_path = "M10,6.536c-2.263,0-4.099,1.836-4.099,4.098S7.737,14.732,10,14.732s4.099-1.836,4.099-4.098S12.263,6.536,10,6.536M10,13.871c-1.784,0-3.235-1.453-3.235-3.237S8.216,7.399,10,7.399c1.784,0,3.235,1.452,3.235,3.235S11.784,13.871,10,13.871M17.118,5.672l-3.237,0.014L12.52,3.697c-0.082-0.105-0.209-0.168-0.343-0.168H7.824c-0.134,0-0.261,0.062-0.343,0.168L6.12,5.686H2.882c-0.951,0-1.726,0.748-1.726,1.699v7.362c0,0.951,0.774,1.725,1.726,1.725h14.236c0.951,0,1.726-0.773,1.726-1.725V7.195C18.844,6.244,18.069,5.672,17.118,5.672 M17.98,14.746c0,0.477-0.386,0.861-0.862,0.861H2.882c-0.477,0-0.863-0.385-0.863-0.861V7.384c0-0.477,0.386-0.85,0.863-0.85l3.451,0.014c0.134,0,0.261-0.062,0.343-0.168l1.361-1.989h3.926l1.361,1.989c0.082,0.105,0.209,0.168,0.343,0.168l3.451-0.014c0.477,0,0.862,0.184,0.862,0.661V14.746z"
dl_button <- list(
    name = "Download as .png",
    icon = list(
      path = icon_svg_path,
      transform = "scale(0.84) translate(-1, 0)"
    ),
    click = htmlwidgets::JS('function(gd) {Plotly.downloadImage(gd, {format: "png"}
                          ) }') )

###### option figures ----
  observeEvent(input$fontsizeaxis, {
    font_size(input$fontsizeaxis)
  }) 
  observeEvent(input$fontsizetick, {
    font_tick(input$fontsizetick)
  }) 
  
  observeEvent(input$Xtickmarks, {
    Xtickmarks.size(input$Xtickmarks)
  }) 
  observeEvent(input$Ytickmarks, {
    Ytickmarks.size(input$Ytickmarks)
  }) 
  observeEvent(input$Ztickmarks, {
    Ztickmarks.size(input$Ztickmarks)
  }) 
  
  observeEvent(input$Xminor.breaks, {
    Xminorbreaks(input$Xminor.breaks)
  }) 
  observeEvent(input$Yminor.breaks, {
    Yminorbreaks(input$Yminor.breaks)
  }) 
  observeEvent(input$Zminor.breaks, {
    Zminorbreaks(input$Zminor.breaks)
  })   
  
output$themeforfigure=renderUI({
    req(!is.null(fileisupload()))
  print("ezrerze")
    themes <- c("theme_bw", "theme_classic", "theme_dark", "theme_grey", "theme_light", "theme_linedraw", "theme_minimal")
    selectInput("themeforfigure.list", h4("Theme for 'Simple 2Dplot'"),
                choices = themes,
                selected = "theme_classic")
  })
themeforfigure.choice<-reactiveVal(c("theme_classic"))
observeEvent(input$themeforfigure.list,{
    themeforfigure.choice(c(input$themeforfigure.list))
  })

###### option size and ratio of figure ----
  
  observeEvent(input$height.size.a, {
    height.size(input$height.size.a)
  })
  #
  observeEvent(input$height.size.b, {
    height.size(input$height.size.b)
  })
  observeEvent(input$width.size.b, {
    width.size(input$width.size.b)
  })
  #
  observeEvent(input$height.size.b.simple, {
    height.size(input$height.size.b.simple)
  })
  observeEvent(input$width.size.b.simple, {
    width.size(input$width.size.b.simple)
  })
  #
  observeEvent(input$height.size.c, {
    height.size(input$height.size.c)
  })
  observeEvent(input$width.size.c, {
    width.size(input$width.size.c)
  }) 
  #
  observeEvent(input$height.size.d, {
    height.size(input$height.size.d)
  })
  observeEvent(input$width.size.d, {
    width.size(input$width.size.d)
  })  
  #
  observeEvent(input$height.size.e, {
    height.size(input$height.size.e)
  })
  observeEvent(input$width.size.e, {
    width.size(input$width.size.e)
  })   
  
  observeEvent(input$ratiox, {
    ratiox(input$ratiox)
  })   
  observeEvent(input$ratioy, {
    ratioy(input$ratioy)
  })  
  observeEvent(input$ratioz, {
    ratioz(input$ratioz)
  })  
  observeEvent(input$ratio.to.coord.simple, {
    ratio.simple(input$ratio.to.coord.simple)
  })  
  observeEvent(input$ratio.to.coord.simple.2, {
    ratio.simple(input$ratio.to.coord.simple.2)
  })    
    observeEvent(input$ratio.to.coord, {
    ratio.simple(input$ratio.to.coord)
  })    
  
### function used in the script ----
#### function to import ----
    importdatafunction<- function(datapath,data.name, df.name,name.worksheet) {
      req(!is.null(datapath))
      extension <- tools::file_ext(data.name)
      df.name <- switch(extension,
                        csv =  {    
                          sep2 <- if( ";" %in% strsplit(readLines(datapath, n=1)[1], split="")[[1]] ){";"
                          } else if( "," %in% strsplit(readLines(datapath, n=1)[1], split="")[[1]] ){","
                          } else if ( "\t" %in% strsplit(readLines(datapath, n=1)[1], split="")[[1]] ){"\t"
                          } else {";"}
                          utils::read.csv(datapath,
                                          header = T,
                                          sep = sep2, stringsAsFactors = F,  fileEncoding="latin1",
                                          dec=".")},
                        xls = readxl::read_xls(datapath, sheet=name.worksheet),
                        xlsx = readxl::read_xlsx(datapath, sheet=name.worksheet))

      df.name<-df.name[,!sapply(df.name, function(x) is.logical(x))] ##remove column without data
      if (input$set.dec == TRUE){
        df.name[] <- lapply(df.name, function(x) gsub(',', '.', x)) ### all is as.matrix, not numeric  
      } else{}
      if(!is.null(df.name[sapply(df.name, function(x) !is.numeric(x))])) {
        df.name[sapply(df.name, function(x) !is.numeric(x))] <- mutate_all(df.name[sapply(df.name, function(x) !is.numeric(x))], .funs=stringr::str_to_lower)}
      df.name
    }
#### function for density ----    
    get_density <- function(x, y,w, ...) {
      dens <- kde2d.weighted2(x, y, w=w,...)
      ix <- findInterval(x, dens$x)
      iy <- findInterval(y, dens$y)
      ii <- cbind(ix, iy)
      return(dens$z[ii])
    }
#### function for newgroup ----       
  # #function for newgroup
  # dataModal <- function() {
  #   if (!is.null(vv) && !is.null(values$newgroup)) { 
  #     modalDialog(
  #       selectInput("select.new.group", label = h3("Select the new group"), 
  #                   choices = values$newgroup, 
  #                   selected = values$newgroup[1]),
  #       textInput("NewGroup", "Choose the name of assignement",value = "new.variable"),
  #       footer = tagList(
  #         modalButton("Cancel"),
  #         actionButton("Change", "OK")
  #       )
  #     )
  #   }
  # }
  # 


### function for color ----
  color.function<-function (levelofcolor,name,selected_rainbow,loadingfile){  
    uvalues <-levels(as.factor(levelofcolor))
    n <- length(uvalues)
    choices <- as.list(uvalues)

    if (!is.null (loadingfile)) {
      mycolors <-unlist(loadingfile)
      selected_rainbow<-1
    } else {
      mycolors <- list("darkgreen", "blue","purple", "green","pink","orange","grey","aquamarine","chartreuse", 
                       "mintcream","salmon","brown","lightblue","red","gold",'#e6194b', '#3cb44b', '#ffe119', 
                       '#4363d8', '#f58231',"maroon1","hotpink3", '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', 
                       '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', 
                       '#808080', '#ffffff', '#000000')}
    colors <- paste0("background:",mycolors,";")
    colors <- paste0(colors,"color:black;")
    colors <- paste0(colors,"font-family: Arial;")
    colors <- paste0(colors,"font-weight: bold;")
    selected2 <-mycolors
    
    nk <- length(mycolors)  ## to repeat colors when there are more bars than the number of colors
    tagList(
      div(br()),
      div(
        lapply(1:n, function(i){
          k <- i %% nk
          if (k==0) k=nk
          if (selected_rainbow == "1") {
            selected2 <-mycolors[i]  }
            shinyWidgets::spectrumInput(
            inputId = paste0(name,i),
            label = paste0(uvalues[i], ": " ),
            choices = list(mycolors,
                           as.list(rainbow(10)),
                           as.list(heat.colors(10)),
                           as.list(terrain.colors(10)),
                           as.list(cm.colors(10)),
                           as.list(topo.colors(10)
                           )
            ),
            selected = selected2,
            options = list(`toggle-palette-more-text` = "Show more")
          )
          
        }),
        
      )
    )
  } # end of color.function
  

#### function for switching axis ----
var.function<-function(var.xyz,random=0){
      setXX<-setXX()
      setYY<-setYY() 
      setZZ<-setZZ()
      if (random ==1) {
        setXX<-"x_random"
        setYY<-"y_random"
        setZZ<-"z_random"
      }
      var<-setXX
      var2<-setYY
      axis.var.name<-nameX()
      axis.var2.name<-nameY()
      Xtickmarks.size<-Xtickmarks.size()
      Ytickmarks.size<-Ytickmarks.size()
      Xminorbreaks<-Xminorbreaks()
      Yminorbreaks<-Yminorbreaks()
      if (var.xyz != "xy"){
        switch(var.xyz,
               yz={ var<-setYY
               var2<-setZZ
               axis.var.name<-nameY()
               axis.var2.name<-nameZ()
               Xtickmarks.size<-Ytickmarks.size()
               Ytickmarks.size<-Ztickmarks.size()
               Xminorbreaks<-Yminorbreaks()
               Yminorbreaks<-Zminorbreaks()
               },
               xz={var<-setXX
               var2<-setZZ
               axis.var.name<-nameX()
               axis.var2.name<-nameZ()
               Xtickmarks.size<-Xtickmarks.size()
               Ytickmarks.size<-Ztickmarks.size()
               Xminorbreaks<-Xminorbreaks()
               Yminorbreaks<-Zminorbreaks()
               },
               yx={var<-setYY
               var2<-setXX
               axis.var.name<-nameY()
               axis.var2.name<-nameX()
               Xtickmarks.size<-Ytickmarks.size()
               Ytickmarks.size<-Xtickmarks.size()
               Xminorbreaks<-Yminorbreaks()
               Yminorbreaks<-Xminorbreaks()
               }
        ) } else {} # enf of if
      
      new.list.parameter<-list(var,var2,axis.var.name,axis.var2.name,Xtickmarks.size,Ytickmarks.size,Xminorbreaks,Yminorbreaks)
      return(new.list.parameter)
    }
#### function for minor grid ----
  # minor.grid.info.function<-function(var.xyz,var,var2,Xminorbreaks,Xtickmarks.size,Yminorbreaks,Ytickmarks.size){
  #   Xtval<-seq(floor(min(var.xyz[[var]])),max(var.xyz[[var]]), Xminorbreaks)
  #   Xttxt <- rep("",length(Xtval)) 
  #   Xttxt[seq(1,length(Xtval),Xtickmarks.size)]<-as.character(Xtval)[seq(1,length(Xtval),Xtickmarks.size)]
  #   
  #   Ytval<-seq(floor(min(var.xyz[[var2]])),max(var.xyz[[var2]]), Yminorbreaks)
  #   Yttxt <- rep("",length(Ytval)) 
  #   Yttxt[seq(1,length(Ytval),Ytickmarks.size)]<-as.character(Ytval)[seq(1,length(Ytval),Ytickmarks.size)]
  #   
  #   Ztval<-seq(floor(min(var.xyz[[setZZ()]])),max(var.xyz[[setZZ()]]), Zminorbreaks())
  #   Zttxt <- rep("",length(Ztval)) 
  #   Zttxt[seq(1,length(Ztval),Ztickmarks.size())]<-as.character(Ztval)[seq(1,length(Ztval),Ztickmarks.size())]
  # 
  # minor.grid.info<-list(Xtval,Xttxt,Ytval,Yttxt,Ztval,Zttxt)
  # return(minor.grid.info)
  # }
  

##### verification ----
val.ID<-reactiveVal(0)
val.ID2<-reactiveVal(0)

observeEvent( c(setID(),val.ID2()), {
    req(!is.null(merging.was.done()))
    req(!is.null(setID()))
    req(!is.null(setXX()))
    
    df.subx<-df$df[,c(setID(),setXX(),setYY(),setZZ())]
    temp<-plotly::distinct(df.subx)
    
    val.ID(0)
    #if (input$set.dataoneID== TRUE) {
    if (isTRUE(any(duplicated(temp[[1]]))) == TRUE) {
    val.ID(1)
      showModal(modalDialog(
             title = "Issues with loaded data",
             HTML(paste(" Some object IDs own different X, Y and Z values !<br>
                        Try to generate new XYZ"))
           ))
         } 
    #}
})
  
  # observeEvent(ignoreInit = TRUE, c(setXX(),setYY(),setZZ(),setID()), {
  #   if( sum(is.na(as.numeric(df$df[,input$setx])))>0 || sum(is.na(as.numeric(df$df[,input$sety])))>0 || sum(is.na(as.numeric(df$df[,input$setz])))>0 || (dim(df$df[duplicated(df$df[,input$setID]),])[1]>0 & input$setID != "null")) {
  # 
  #     showModal(modalDialog(
  #       title = "Issues with loaded data",
  #       if( sum(is.na(as.numeric(df$df[,setXX()])))>0) {
  #         HTML(paste(sum(is.na(as.numeric(df$df[,setXX()]))), " X value(s) was/were not included as not numerical <br>"))},
  #       if( sum(is.na(as.numeric(df$df[,setYY()])))>0) {
  #         HTML(paste(sum(is.na(as.numeric(df$df[,setYY()]))), " Y value(s) was/were not included as not numerical<br>"))},
  #       if( sum(is.na(as.numeric(df$df[,setZZ()])))>0) {
  #         HTML(paste(sum(is.na(as.numeric(df$df[,setZZ()]))), " Z value(s) was/were not included as not numerical<br>"))},
  #       if(setID() != "null" & dim(df$df[duplicated(df$df[,setID()]),])[1]>0) {
  #         HTML(paste(dim(df$df[duplicated(df$df[,setID()]),])[1], " object ID(s) is/are not unique !<br> "))
  #       }
  #     ))
  #   }
  # })
  # 

##### import supp data to show XYZ ----
  input_file_extradata.name<-reactiveVal(NULL)
  input_file_extradata.datapath<-reactiveVal(NULL)
  extradatafileisupload<-reactiveVal(NULL)
  observeEvent(input$file1.XYZ, {
    input_file_extradata.name(input$file1.XYZ$name)
    input_file_extradata.datapath(input$file1.XYZ$datapath)
  })
  observe({
    req(!is.null(input_file_extradata.datapath()))
    extension <- tools::file_ext(input_file_extradata.name())
    switch(extension,
           csv = {updateSelectInput(session, "worksheet.XYZ", choices = input_file_extradata.name())},
           xls =   {    selectionWorksheet <-excel_sheets(path = input_file_extradata.datapath())
           updateSelectInput(session, "worksheet.XYZ", choices = selectionWorksheet)},
           xlsx =  {      selectionWorksheet <-excel_sheets(path = input_file_extradata.datapath())
           updateSelectInput(session, "worksheet.XYZ", choices = selectionWorksheet)})

  }) #end observe 

observeEvent(input$getData.XYZ, {
    req(!is.null(input_file_xyzspecies.datapath()))
    df$df.extradata <-importdatafunction(input_file_extradata.datapath(),input_file_extradata.name(),df$df.extradata,input$worksheet.XYZ)
    df$df.extradata <-cbind.data.frame("null", df$df.extradata)
    extradatafileisupload(1)  
  })# 
  
output$set.levels.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    req(input$setlevels != "null")
    selectInput("setlevels.xyz", h4("Levels"),
                choices = names(df$df.extradata)[c(1:ncol(df$df.extradata))],
                selected = input$setlevels)
  }) 
  
output$set.date.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    req(input$setdate != "null")
    selectInput("setdate.xyz", h4("years"),
                choices = names(df$df.extradata)[c(1:ncol(df$df.extradata))],
                selected = input$setdate) 
  }) 
 output$set.sector.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    req(input$setsector != "null")
    selectInput("setsector.xyz", h4("Context/sector"),
                choices = names(df$df.extradata)[c(1:ncol(df$df.extradata))],
                selected = input$setsector)
  }) 
  
  output$set.square.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    req(input$setsquare != "null")
    selectInput("setsquare.xyz", h4("Square"),
                choices = names(df$df.extradata)[c(1:ncol(df$df.extradata))],
                selected = input$setsquare)
  })
  
  
  output$set.x.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    req(input$setx != "null")
    selectInput("setx.xyz", h4("X"),
                choices = names(df$df.extradata)[c(1:ncol(df$df.extradata))],
                selected = setXX())
  })
  output$set.y.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    req(input$sety != "null")
    selectInput("sety.xyz", h4("Y"),
                choices = names(df$df.extradata)[c(1:ncol(df$df.extradata))],
                selected = setYY())
  }) 
  output$set.z.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    req(input$setz != "null")
    selectInput("setz.xyz", h4("Z"),
                choices = names(df$df.extradata)[c(1:ncol(df$df.extradata))],
                selected = setZZ())
  }) 
  
  output$set.nature.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    selectInput("setnature.xyz", h4("Variable to be colored"),
                choices = names(df$df.extradata)[c(1:ncol(df$df.extradata))])
  })
  output$liste.Nature.xyz=renderUI({
    req(!is.null(extradatafileisupload()))
    req(input$setnature.xyz)
    checkboxGroupInput("Nature.xyz", h4(),
                       choices = levels(as.factor(df$df.extradata[,input$setnature.xyz])),selected = factor(df$df.extradata[,input$setnature.xyz]))
  })
  
df.sub.supp.xyz <- reactive({
    req(!is.null(df.sub()))
    req(!is.null(extradatafileisupload()))
    req(c(setXX()!="null", input$setx.xyz!="null"))
    req(c(setYY()!="null",input$sety.xyz!="null"))
    req(c(setZZ()!="null", input$setz.xyz!="null"))

      df.sub.supp.xyz <-df$df.extradata
      df.sub.supp.xyz<-as.data.frame(df.sub.supp.xyz)

 
      if(input$setdate!="null" && input$setdate.xyz!="null"){
        df.sub.supp.xyz[,df.sub.supp.xyz$set.date.xyz] <-as.numeric(df.sub.supp.xyz[,input$set.date.xyz])
        df.sub.supp.xyz[,input$set.date.xyz][is.na(df.sub.supp.xyz[,input$set.date.xyz])]<-0
        if (!is.null(input$Date2)) {
          df.sub.supp.xyz<-df.sub.supp.xyz %>%
            filter(df.sub.supp.xyz[,input$set.date.xyz] >= input$Date2[1], df.sub.supp.xyz[,input$set.date.xyz] <= input$Date2[2])}}

      if (input$setsector!="null" && input$setsector.xyz!="null"){
      df.sub.supp.xyz <- df.sub.supp.xyz[df.sub.supp.xyz[,input$setsector.xyz] %in% input$localisation, ]}

      if (input$setlevels!="null" && input$setlevels.xyz!="null"){
      df.sub.supp.xyz <- df.sub.supp.xyz[df.sub.supp.xyz[,input$setlevels.xyz] %in% input$UAS, ]}

      if (input$setnature.xyz!="null"){
        df.sub.supp.xyz <- df.sub.supp.xyz[df.sub.supp.xyz[,input$setnature.xyz] %in% input$Nature.xyz, ]}

      
      df.sub.supp.xyz[,input$setx.xyz]<-df.sub.supp.xyz[,input$setx.xyz] %>% as.numeric()
      df.sub.supp.xyz[,input$sety.xyz]<-df.sub.supp.xyz[,input$sety.xyz] %>% as.numeric()
      df.sub.supp.xyz[,input$setz.xyz]<-df.sub.supp.xyz[,input$setz.xyz] %>% as.numeric()


    df.sub.supp.xyz<-df.sub.supp.xyz %>% 
      filter(.data[[input$setx.xyz]] >= input$xslider[1], .data[[input$setx.xyz]] <= input$xslider[2]) %>% 
      filter(.data[[input$sety.xyz]] >= input$yslider[1], .data[[input$sety.xyz]] <= input$yslider[2]) %>% 
      filter(.data[[input$setz.xyz]] >= input$zslider[1], .data[[input$setz.xyz]] <= input$zslider[2])

    assign("df.sub.supp.xyz",df.sub.supp.xyz, envir=.GlobalEnv)
    df.sub.supp.xyz
    shiny::validatevalidate(need(nrow(df.sub.supp.xyz)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters, or change the supplementary dataset."))

  })

 observeEvent(input$setnature.xyz, {
   req(!is.null(extradatafileisupload()))
  natureofobjects.list.sup(input$setnature.xyz)

 }) 

observeEvent(input$Nature.xyz, {
  req(!is.null(extradatafileisupload()))
  natureofobjects.list.sup(input$Nature.xyz)

}) 

  
basiccolor.sup<-reactive({

  req(!is.null(natureofobjects.list.sup()))
  name<-"colorvar"
  color.function(natureofobjects.list.sup(),name,1,mypaletteofcolors())

}) 
output$colors.sup <- renderUI({
  req(!is.null(extradatafileisupload()))
  basiccolor.sup()
})

myvaluesx.supp<-reactive({
  req(!is.null(extradatafileisupload()))
  myvaluesx.supp <-NULL
  n <- length(unique(natureofobjects.list.sup()))
  val <- list()
  if (!is.null(input[[paste0("colorvar",1)]])) {
    myvaluesx.supp <- lapply(1:n, function(i) {
      if (i==1) val <- list(input[[paste0("colorvar",i)]])
      else val <- list(val,input[[paste0("colorvar",i)]])
    })}else{
      myvaluesx.supp <-as.list(rainbow(length(levels(as.factor(df$df.extradata[[input$setnature.xyz]])))))
      
    }
}) # end of myvaluexS.sup


  
  
##### import extradata ----
observe({
    req(input$file.extradata)
    extension <- tools::file_ext(input$file.extradata$name)
    df$file.extradata <- switch(extension,
                                csv = {    
                                  sep2 <- if( ";" %in% strsplit(readLines(input$file.extradata$datapath, n=1)[1], split="")[[1]] ){";"
                                  } else if( "," %in% strsplit(readLines(input$file.extradata$datapath, n=1)[1], split="")[[1]] ){","
                                  } else if ( "\t" %in% strsplit(readLines(input$file.extradata$datapath, n=1)[1], split="")[[1]] ){"\t"
                                  } else {";"}
                                  utils::read.csv(input$file.extradata$datapath,
                                                  header = input$header,
                                                  sep = sep2, stringsAsFactors = F,fileEncoding="latin1", 
                                                  dec=".")},
                                xls = readxl::read_xls(input$file.extradata$datapath),
                                xlsx = readxl::read_xlsx(input$file.extradata$datapath))
  }) #end observe 

output$set.columnID=renderUI({
    req(input$file1.XYZ)
    req(!is.null(setID()))
    selectInput("setcolumnID", h4("Select the unique objects ID)"),
                choices = names(df$file.extradata))
  }) 
observeEvent(input$setcolumnID, { ## add two necessary columns for the rest of manipulations
    df$file.extradata2<-df$file.extradata[,!sapply(df$file.extradata, function(x) is.logical(x))] ##remove column whitout data
    df$file.extradata2[sapply(df$file.extradata2, function(x) !is.numeric(x))] <- mutate_all(df$file.extradata2[sapply(df$file.extradata2, function(x) !is.numeric(x))], .funs=str_to_lower)
    temp.data<-df$df[duplicated(df$df[,setID()]) | duplicated(df$df[,setID()], fromLast = T),]
    if (nrow(temp.data) >0 ) {notunique.txt(temp.data)
    } else {notunique.txt("All IDs are unique")}
    temp.data2<-df$file.extradata2[duplicated(df$file.extradata2[,input$setcolumnID]) | duplicated(df$file.extradata2[,input$setcolumnID], fromLast = T),]
    if (nrow(temp.data2) >0 ) {notunique2.txt(temp.data2)
    } else {notunique2.txt("All IDs are unique")}
  }) #end observe 
  
observeEvent(input$goButton.set.columnID, {
    req(input$setcolumnID)
    if(setID() == "null"){ 
      showModal(modalDialog(
        title = "Issues with merging data", 
        HTML(paste("No unique ID has been defined in the XYZ dataset"))
      ))
      
      return()
    }
    if(dim(df$df[duplicated(df$df[,setID()]),])[1]>0){ 
      showModal(modalDialog(
        title = "Issues with merging data", 
        HTML(paste("Object IDs from the XYZ dataset are not unique. <br>
                 Import refit data required absolutely a unique ID per object"))
      ))
      return()
    }
 names(df$file.extradata2)[match(paste(input$setcolumnID),names(df$file.extradata2))]<-paste(input$setID)
    if(dim(df$file.extradata2[duplicated(df$file.extradata2[,setID()]),])[1]>0){ 
      showModal(modalDialog(
        title = "Issues with merging data", 
        HTML(paste("Object IDs from the imported dataset are not unique. <br>
                 Import refitXXXXX data required absolutely a unique ID per object"))
      ))
      return()
    }
    same.column.to.remove<-intersect(colnames(df$df),colnames(df$file.extradata2)) # remove column with same name
    same.column.to.remove<-same.column.to.remove[same.column.to.remove!=input$setID]
    df$file.extradata3<-df$file.extradata2[!names(df$file.extradata2)%in% c(same.column.to.remove)]
    df$file.extradata3[,input$setID]<-as.character(df$file.extradata3[,input$setID]) ## same format to avoid pb
    df$df[,setID()]<-as.character(df$df[,setID()]) ## same format to avoid pb
    
    temp.data2<-setdiff(df$df[,setID()],df$file.extradata3[,setID()])
    if(length(temp.data2)==0){ 
      ID.no.suppl.data.txt("perfect")} else {suppl.no.include.txt(temp.data2)}
    
    temp.data<-setdiff(df$file.extradata3[,setID()],df$df[,setID()])
    if(length(temp.data)==0){ 
      suppl.no.include.txt("perfect")} else {ID.no.suppl.data.txt(temp.data)}
    
    df$df<-full_join(df$file.extradata3,df$df)%>% 
      relocate(c("shapeX","text","null"))
  })
  
## table to show import extradata ----
  output$notunique<- renderPrint({notunique.txt()})
  output$notunique2<- renderPrint({notunique2.txt()})
  output$suppl.no.include<- renderPrint({suppl.no.include.txt()})
  output$ID.no.suppl.data<- renderPrint({ID.no.suppl.data.txt()})
  
 
#### merge two columns ----
output$set.col1=renderUI({
    req(!is.null(fileisupload()))

    selectInput("setcol1", h4("Choose a first column"),
                choices = names(df$df)[c(3:ncol(df$df))],
                selected = "")
  })   
output$set.col2=renderUI({
    req(!is.null(fileisupload()))

    selectInput("setcol2", h4("Choose a second column"),
                choices = names(df$df)[c(3:ncol(df$df))],
                selected = "")
  })  
  
observeEvent(input$Merge2, {
    new.group<-paste0(df$df[,input$setcol1],input$separatormerge,df$df[,input$setcol2])
    df$df<-cbind(df$df,new.group)
    colnames(df$df)[ncol(df$df)]<-c(input$Merge.groupe)
    
    if (input$checkbox.index==TRUE){
      setID(input$Merge.groupe)
      liste.ID(c(input$Merge.groupe))
      updateSelectInput(session,"setID",
                  choices = names(df$df)[c(1:ncol(df$df))],
                  selected = liste.ID())
    }
    showModal(modalDialog(
      HTML(paste("Data have been merged. <br>
               The first value obtained is",df$df[,input$Merge.groupe][1] ))
    ))
  })
  
##### output sidebar ----
  output$liste.Nature=renderUI({
    req(!is.null(merging.was.done()))
    req(input$setnature)
    checkboxGroupInput("Nature", h4("Type"),
                       choices = levels(as.factor(df$df[,input$setnature])),selected = factor(df$df[,input$setnature]))
  })

  output$liste.passe=renderUI({
    req(!is.null(merging.was.done()))
    req(input$setpasse)
    checkboxGroupInput("Passe", h4(paste(input$setpasse)),
                       choices = levels(as.factor(df$df[,input$setpasse])),selected = levels(as.factor(df$df[,input$setpasse])))
  })
  output$liste.sector=renderUI({
    req(!is.null(merging.was.done()))
    req(input$setsector)
    checkboxGroupInput("localisation", h4("Context"),
                       choices = levels(as.factor(df$df[,input$setsector])),selected = factor(df$df[,input$setsector]))
  })
  output$liste.UAS=renderUI({
    req(!is.null(merging.was.done()))
    req(input$setlevels)
    checkboxGroupInput("UAS", h4("Levels"),
                       choices = levels(as.factor(df$df[,input$setlevels])),selected = factor(df$df[,input$setlevels]))
  })
  output$liste.species=renderUI({
    req(!is.null(merging.was.done()))
    req(input$setspecies)
    checkboxGroupInput("natureofobjects", h4(),
                       choices = liste.of.nature.of.objects(),selected = factor(df$df[,input$setspecies]))
  })
  
liste.of.nature.of.objects<-reactive({
  ## to avoid issues when include data without numeric for xyz and distinct "species"
  req(input$setspecies)
  req(!is.null(setXX()))
  temp.list<- dplyr::filter(df$df,!is.na(as.numeric(as.character(.data[[setXX()]]))))
  temp.list<- dplyr::filter(temp.list,!is.na(as.numeric(as.character(.data[[setYY()]]))))
  temp.list<- dplyr::filter(temp.list,!is.na(as.numeric(as.character(.data[[setZZ()]]))))
  liste.of.nature.of.objects<-levels(as.factor(temp.list[,input$setspecies]))
  liste.of.nature.of.objects
})
  
 observeEvent(input$setspecies, {
   req(input$setspecies)
   req(!is.null(liste.of.nature.of.objects()))
   natureofobjects.list(liste.of.nature.of.objects())
 }) 
 
 observeEvent(input$natureofobjects, {
   req(input$natureofobjects)
   natureofobjects.list(input$natureofobjects)
}) 
 
output$nb6=renderUI({
    if (readytosub()!=1){
      HTML(paste("The below steps 1, 2 and 3 must be completed before the next"))
    } else {
    req(!is.null(fileisupload()))
    req(!is.null(df.sub()))
    sum_tot_indiv<-sum(nrow(df$df)-(max(sum(is.na(as.numeric(df$df[,setXX()]))),sum(is.na(as.numeric(df$df[,setYY()]))),sum(is.na(as.numeric(df$df[,setZZ()]))))))
    val.ID2(sum_tot_indiv)
    HTML(paste("Number of rows imported:",sum_tot_indiv,"for a total of", nrow(df$df), "rows present in the dataset"))
    }
    })

output$nb.temp=renderUI({
  if (readytosub()!=1){
    HTML(paste("Steps 1 and 2 below must be completed before the next"))
  } else {
    req(!is.null(fileisupload()))
    req(!is.null(df.sub()))
    req(!is.null(df.sub.supp.xyz()))

    sum_tot_indiv<-nrow(df.sub.supp.xyz())
    HTML(paste("Number of rows imported:",sum_tot_indiv,"for a total of", nrow(df$df.extradata), "rows present in the dataset"))
  }
})

output$nb.sub=renderUI({
  req(!is.null(df.sub()))
  HTML("")
})

output$nb7=renderUI({
  req(!is.null(merging.was.done()))
  req(!is.null(setID()))
  if (val.ID()==1){
    tagList(h4(style = "color: blue;",HTML(paste("WARNING: some object IDs own different X, Y and Z values !<br>"))))
  }
})

output$ylimits=renderUI({
    #req(!is.null(merging.was.done()))
  req(!is.null(squareisdone()))
    req(!is.null(setYY()))
    ymax= df$df[,setYY()] %>% as.numeric() %>%ceiling() %>% max(na.rm = TRUE)
    ymin=df$df[,setYY()] %>% as.numeric() %>% floor() %>% min(na.rm = TRUE)
    sliderInput('yslider','y limits',min=ymin,max=ymax,value=c(ymin,ymax),step=stepY())
  })
output$xlimits=renderUI({
  #req(!is.null(merging.was.done()))
  req(!is.null(squareisdone()))
    req(!is.null(setXX()))
    xmax = df$df[,setXX()] %>% ceiling() %>% max(na.rm = TRUE)
    xmin=df$df[,setXX()] %>% floor() %>% min(na.rm = TRUE)
    sliderInput('xslider','x limits',min=xmin,max=xmax,value=c(xmin,xmax),step=stepX())
  })
  output$zlimits=renderUI({
    #req(!is.null(merging.was.done()))
    req(!is.null(squareisdone()))
    req(!is.null(setZZ()))
    zmax = df$df[,setZZ()] %>% ceiling() %>% max(na.rm = TRUE)
    zmin=df$df[,setZZ()] %>% floor() %>% min(na.rm = TRUE)
    sliderInput('zslider','z limits',min=zmin,max=zmax,value=c(zmin,zmax),step=stepZ())
  })
  output$Date=renderUI({
    req(!is.null(merging.was.done()))
    req(input$setdate)
    dmin=min(as.numeric(df$df[,input$setdate]), na.rm=T)
    dmax=max(as.numeric(df$df[,input$setdate]), na.rm=T)
    if((dmax!="inf")==TRUE){
      sliderInput('Date2','Year(s) :',min=dmin,max=dmax,value=c(dmin,dmax),step=1,sep='')
    } else {}

  })

##### output additional Setting slide ----
tt<-reactiveVal() 

  observeEvent(input$stepXsize, {
    stepX(input$stepXsize)
  })
  observeEvent(input$stepYsize, {
    stepY(input$stepYsize)
  })
  observeEvent(input$stepZsize, {
    stepZ(input$stepZsize)
  })
  output$liste.infos=renderUI({
    req(!is.null(merging.was.done()))
    checkboxGroupInput("listeinfos", h4("Choose the variable information to be shown while hovering points on plots"),
                       choices = names(df$df)[c(4:ncol(df$df))], selected = NULL)
  })
  
  observeEvent(input$optioninfosfigplotly, {
    legendplotlyfig(input$optioninfosfigplotly)
  })
  
  output$ratiotocoorsimple2=renderUI({ 
    req(input$advanced.slice==FALSE)
    numericInput("ratio.to.coord.simple.2", label = h5("Ratio figure"), value = 1)
  })
  
###### output shape Setting  ----
  output$shape2=renderUI({
    req(!is.null(merging.was.done()))
    req(input$shape)
    s2<-list("circle","square","triangle","diamond","star")
    s2<-s2[s2!=input$shape]
    selectInput("setshape2", h4("Secondary shape"),
                choices = s2)
  }) 
  output$shape2.var1=renderUI({ 
    req(!is.null(merging.was.done()))
    selectInput("setshape2.1", h5("Select variable for secondary shape"),
                choices = names(df$df)[c(3:ncol(df$df))])
  })
  output$shape2.var2=renderUI({ 
    req(!is.null(merging.was.done()))
    df$Sh2<-df$df
    selectInput("setshape2.2", h5("Select variable modality for secondary shape"),
                choices = levels(as.factor(df$Sh2[,input$setshape2.1])),selected = factor(df$Sh2[,input$shape2.var1]))
  })
  
  
  observeEvent(input$do.shape2, {
    tt2<-paste(input$setshape2,input$setshape2.1," ", input$setshape2.2, " ") 
    tt3<-paste(tt2, tt(), sep="\n") 
    tt(tt3)
    
  })
  observeEvent(input$do.shape1, {
    tt3<-NULL
    tt(tt3)
  })
  
  output$text.shape <- renderText({
    paste(tt())}
  )
  
  observeEvent(input$do.shape1, {
    df$df$shapeX<-input$shape
  })
  
  observeEvent(input$do.shape2, {
    df$df$shapeX[df$df[,input$setshape2.1] %in% input$setshape2.2]<-input$setshape2
  })
  


#### liste infos # not used yet due to no plotly ----

observeEvent(req(!is.null(listinfosmarqueur())),{
    df$df$text<-""}
  )
observeEvent(input$listeinfos.go, {
    req(!is.null(merging.was.done()))
    selected = c()
    for (s in 1:length(input$listeinfos)) {
      selected = c(selected, input$listeinfos[s])
    }
    if (is.null(selected)) {
      selected = character(0)
    }
    
    df$df$text<-paste("<br><b>Z</b>:", df$df[,setZZ()],"<br><b>ID</b>:", df$df[,input$setID])
    if (length(input$listeinfos)>0){
      for (ii in 1:length(input$listeinfos)){
        text5<-paste("<br>",input$listeinfos[ii],": ",df$df[,input$listeinfos[ii]], sep="")
        df$df$text<-paste(df$df$text,text5)
      }
    }
    updateCheckboxGroupInput(session, "listeinfos", selected = selected)
    listinfosmarqueur(NULL)
  }) #end of observeevent
 
### output ratio ---- 
# output$dig.col=renderUI({ 
#     req(!is.null(merging.was.done()))
#   req(input$select.ratio ==3)
#     selectInput("digcol", h5("Select variable with digestion information"),
#                 choices = names(df$df)[c(3:ncol(df$df))],selected = "")
#   })
  
# output$name.of.dig.element=renderUI({ 
#   req(!is.null(merging.was.done()))
#   req(input$select.ratio ==3)
#   selectInput("nameofdigelement", h5("Select variable with digestion information"),
#               choices = levels(as.factor(df$df[,input$setanat])),selected = factor(df$df[,input$setanat]))
# 
# })


#### ordering levels ----
factor.order.level<-reactiveVal(NULL)
factor.order.level.activation<-reactiveVal(NULL)
output$order.level <- renderUI({
    orderInput(inputId = "levels.order", label = "",
               items = levels(as.factor(df$df[,input$setlevels])), item_class = 'info',
               class = "btn-group-vertical"
             )
  })

observeEvent(input$levels.order, {
    factor.order.level(input$levels.order)
    df$df[[input$setlevels]]<-factor(df$df[[input$setlevels]], levels = factor.order.level())
    factor.order.level.activation(1)
  })



### A finir . ne veux pas marcher ----
observeEvent(input$reverse.button,{ 
  factor.order.level(fct_rev(factor.order.level()))
  df$df[[input$setlevels]]<-factor(df$df[[input$setlevels]], levels = factor.order.level())
  updateOrderInput(session,"levels.order",
                   items = levels(as.factor(df$df[,input$setlevels])), 
                   item_class = 'info')
  })
### order level of split/dec
factor.order.split<-reactiveVal(NULL)
output$order.level2 <- renderUI({
  orderInput(inputId = "levels.order2", label = "",
             items = levels(as.factor(df.sub()[,input$setdec])), item_class = 'info',
             class = "btn-group-vertical"
  )
  
})
observeEvent(input$levels.order2, {
  factor.order.split(rev(input$levels.order2))
  #print(df.sub()[[input$setdec]])
  #print(factor(df.sub()[[input$setdec]]))
  #print(factor.order.split())
  
})

### order organisation of squares for splits

output$order.level3 <- renderUI({
  #nnnnncol<- orga.square.histo.freq$nnnnncol<-nnnnncol
  # orga.square.histo.freq$nnnnrow<-nnnnrow
  # orga.square.histo.freq$list.nom.pp<-list.nom.pp
  # orga.square.histo.freq$typ.trav<-typ.trav
  # orga.square.histo.freq$typ.subsquare<-typ.subsquare
  # orga.square.histo.freq$typ.al<-typ.al
  
  #nnnnncol<-length(orga.square.histo.freq$typ.al+1)
  
  pp1<-orga.square.histo.freq$list.nom.pp

list.level<-paste0("levels.order2.orga_", 1:3)

# my_list_of_tags <- lapply(list.level, function(x){div(
#                                                         orderInput(inputId = x, label = x, 
#                                                             items = levels(as.factor(df.sub()[,input$setdec])), item_class = 'info',connect=list.level
#                                                             ),
#                                                      class = "grid-cell") 
#                                                         })

 #  tagList(
   orderInput(inputId = "levels.order2.orga1", label = "levels.order2.orga1",
              items = pp1, item_class = 'info',connect = c("A","B","C","D","E","levels.order2.orga2"))
#    orderInput(inputId = "levels.order2.orga2", label = "Column 2",
#               items = levels(as.factor(df.sub()[,input$setdec]))[1], item_class = 'info',connect = c("levels.order2.orga1", "levels.order2.orga3")),
# 
  #  orderInput(inputId = "levels.order2.orga3", label = "Remove squares",
   #            items = NULL,item_class = 'info',connect = c("levels.order2.orga1", "levels.order2.orga2"), placeholder = 'Drag item here to remove it...', as.source=T)
# )
   })
# output$order.level4 <- renderUI({
#   orderInput(inputId = "levels.order2.orga2", label = "levels.order2.orga2",
#              items = levels(as.factor(df.sub()[,input$setdec]))[11:20], item_class = 'info',connect = c("A","B","levels.order2.orga1" ))
# })
# list.level<-NULL
# user_suggestion <- do.call(shiny::reactiveValues, setNames(vector(mode = "list", length = length(list.level)), list.level))
# observe({
#   lapply(list.level, function(x) {
#     user_suggestion[[x]] <- input[[x]]})
#   })
#

#### squaring configuration ----

observeEvent(input$go.oder, {
  factor.order.square.split$A<-input$A
  factor.order.square.split$B<-input$B
  factor.order.square.split$C<-input$C
  factor.order.square.split$D<-input$D
  factor.order.square.split$E<-input$E
  
  m.order<-max(length(factor.order.square.split$A), 
               length(factor.order.square.split$B),
               length(factor.order.square.split$C),
               length(factor.order.square.split$D))
  bb<-4
  if(m.order>0) {
    tab.temp<-as.data.frame(matrix("blank",nrow=4,ncol=m.order))
    bi<-1
    if (!is.null(factor.order.square.split$A)){tab.temp[bi,1:length(factor.order.square.split$A)]<-factor.order.square.split$A
    bi<-bi+1} else {
      
      tab.temp<-tab.temp[-c(bi),]}
    tab.temp<-as.data.frame(tab.temp)
    if (!is.null(factor.order.square.split$B)){tab.temp[bi,1:length(factor.order.square.split$B)]<-factor.order.square.split$B
    bi<-bi+1} else {
      tab.temp<-tab.temp[-c(bi),]}
    tab.temp<-as.data.frame(tab.temp)
    if (!is.null(factor.order.square.split$C)){tab.temp[bi,1:length(factor.order.square.split$C)]<-factor.order.square.split$C
    bi<-bi+1} else {
      tab.temp<-tab.temp[-c(bi),]}
    tab.temp<-as.data.frame(tab.temp)
    if (!is.null(factor.order.square.split$D)){tab.temp[bi,1:length(factor.order.square.split$D)]<-factor.order.square.split$D
    bi<-bi+1} else {
      tab.temp<-tab.temp[-c(bi),]}
    tab.temp<-as.data.frame(tab.temp)
    bi<-bi-1
    tab.list<-as.vector(t(tab.temp))
    
    nnnnncol<-m.order
    nnnnrow<-bi
    theo<-nnnnncol*nnnnrow
    pp<-vector("list", theo)
    pp<-pp[1:length(tab.list)]
    names(pp)<-tab.list
    orga.square.histo.freq$nnnnncol<-nnnnncol
    orga.square.histo.freq$nnnnrow<-nnnnrow
    orga.square.histo.freq$list.nom.pp<-tab.list
    orga.square.histo.freq$pp<-pp
  }
})
  

##### colors configuration ----
  save.col.react<-reactiveVal()
  mypaletteofcolors<-reactiveVal()
  observeEvent(df$file.color,{ 
    mypaletteofcolors(df$file.color[2])
  })
  
basiccolor= reactive({
    req(!is.null(merging.was.done()))
    name<-"colorvar"
print('basicolor action')
    color.function(natureofobjects.list(),name,1,mypaletteofcolors())

  }) 
  
save.col2<-observeEvent(myvaluesx(),{ 
    if (length(unlist(myvaluesx()))>1) {
      color<-levels(as.factor(df$df[,inputcolor()]))
      names_of_the_variable<-unlist(myvaluesx())
      length(color)<-max(c(length(color),length(names_of_the_variable))) ## to avoid problem of different row
      length(names_of_the_variable)<-max(c(length(color),length(names_of_the_variable)))
      save.col.react(cbind.data.frame(color,names_of_the_variable))
    }
  })
observe({
    req(input$file.color)
    extension <- tools::file_ext(input$file.color$name)
    df$file.color <- switch(extension,
                            csv = {    
                              sep2 <- if( ";" %in% strsplit(readLines(input$file.color$datapath, n=1)[1], split="")[[1]] ){";"
                              } else if( "," %in% strsplit(readLines(input$file.color$datapath, n=1)[1], split="")[[1]] ){","
                              } else if ( "\t" %in% strsplit(readLines(input$file.color$datapath, n=1)[1], split="")[[1]] ){"\t"
                              } else {";"}
                              utils::read.csv(input$file.color$datapath,
                                              header = input$header,
                                              sep = sep2, stringsAsFactors = F, 
                                              dec=".")},
                            xls = readxl::read_xls(input$file.color$datapath),
                            xlsx = readxl::read_xlsx(input$file.color$datapath))
  })
  
myvaluesx<-reactive({
    req(!is.null(merging.was.done()))
    req(!is.null(natureofobjects.list()))
    myvaluesx <-NULL
    n <- length(unique(natureofobjects.list()))
    val <- list()
    if (!is.null(input[[paste0("colorvar",1)]])) {
      myvaluesx <- lapply(1:n, function(i) {
        if (i==1) val <- list(input[[paste0("colorvar",i)]])
        else val <- list(val,input[[paste0("colorvar",i)]])
      })}else{
        myvaluesx <-as.list(rainbow(number.total.of.species()))
        
      }
  }) # end of myvaluexS
  
output$colors2 <- renderUI({
    basiccolor()
  })

### new group slide # not used due to no plotly----
 # output$liste.newgroup=renderUI({
 #    req(!is.null(merging.was.done()))
 #    selectInput("listenewgroup", h4("Copy data from another variable (select NULL for a default value of zero)"),
 #                choices = names(df$df)[c(3:ncol(df$df))],
 #                selected = c("null"))
 #  })
 #  
 #  create.newgroup <- observeEvent(input$go.ng, {
 #    new.group<-df$df[,input$listenewgroup]
 #    req(!isTruthy(input$text.new.group == values$newgroup)) ## block if two same names exist because problems later
 #    values$newgroup <- c(values$newgroup, input$text.new.group)
 #    df$df<-cbind(df$df,new.group)
 #    colnames(df$df)[ncol(df$df)]<-c(input$text.new.group)
 #    
 #  })
 #  
 #  output$brushed<- renderPrint({
 #    g1 <- df$df
 #    d <- event_data('plotly_selected')
 #    if (is.null(d)) return()
 #    if (length(d)==0) {
 #         vv(NULL)
 #      return()
 #    }
 #    dd <- cbind(d[[3]],d[[4]])
 #    
 #    list.parameter.info<-var.function(input$var1)
 #    var<-list.parameter.info[[1]]
 #    var2<-list.parameter.info[[2]]         
 #    WW<-which(g1[[var]] %in% dd[,1] & g1[[var2]] %in% dd[,2]) 
 #    vv<-df$df[WW,4:ncol(df$df)]
 #    vv(vv)
 #    vv
 #  })  
 #  
 #  observeEvent(input$Change2, {
 #    showModal(dataModal())
 #  })
 #  observeEvent(input$Change, {
 #    req(!is.null(input$Change))
 #    df$df[which(row.names(df$df) %in% row.names(vv())),][input$text.new.group] <-
 #      input$NewGroup
 #    removeModal()
 #  }) # end of Observe Event

### renaming ---
limitnbindiv<-reactiveVal(1)
output$liste.newgroup2=renderUI({
    req(!is.null(merging.was.done()))
    selectInput("liste.newgroup.rename", label = h5("Select the variable"), 
                choices = colnames(df$df), 
                selected = c('null'))
  })
  
output$liste.newgroup.table=renderUI({
    req(input$liste.newgroup.rename != "null")
    selectInput("liste.newgroup3", label = h5("Select the modality to rename"), 
                choices = factor(df$df[,input$liste.newgroup.rename]))
  })

observeEvent(input$go.ng2, { 
    req(input$liste.newgroup.rename != "null")
    req(!is.null(input$liste.newgroup3))
    df$df[,input$liste.newgroup.rename][df$df[,input$liste.newgroup.rename]==input$liste.newgroup3]<-input$text.new.group2
  })
output$btt.modify=renderUI({
    req(input$liste.newgroup.rename != "null")
    req(!is.null(input$liste.newgroup3))
    actionButton("go.ng2", "Modify")
  })

### renaming table ---

input_file.renaming.table.name<-reactiveVal(NULL)
input_file.renaming.table.datapath<-reactiveVal(NULL)

observeEvent(input$file.renaming.table, {
  input_file.renaming.table.name(input$file.renaming.table$name)
  input_file.renaming.table.datapath(input$file.renaming.table$datapath)
})

tabletorenameimport<-reactiveVal(NULL)
observeEvent(input$getData.renaming.table, {
  req(!is.null(input$file.renaming.table$datapath))
  df$df.temp.rename <-importdatafunction(input$file.renaming.table$datapath,input$file.renaming.table$name,df$df.temp.rename,input$worksheet.renaming.table)
  tabletorenameimport(1)  
})# 
 output$liste.newgroup.table=renderUI({
   req(!is.null(tabletorenameimport()))
   req(input$liste.newgroup.rename != "null")
   selectInput("liste.newgroup.renaming", label = h5("Select the variable with modality name to rename, as in the database already imported"), 
               choices = names(df$df.temp.rename)[c(1:ncol(df$df.temp.rename))])
 })
output$liste.newgroup.table2=renderUI({
  req(!is.null(tabletorenameimport()))
  req(input$liste.newgroup.rename != "null")
  selectInput("liste.newgroup.renaming2", label = h5("Select the variable with new modality name"), 
              choices = names(df$df.temp.rename)[c(1:ncol(df$df.temp.rename))])
})
output$btt.modify2=renderUI({
  req(!is.null(tabletorenameimport()))
  req(input$liste.newgroup.rename != "null")
  actionButton("go.ng.renaming", "Modify")
})

observeEvent(input$go.ng.renaming, { 
  req(input$liste.newgroup.rename != "null")
  req(!is.null(input$liste.newgroup.renaming))
  req(!is.null(input$liste.newgroup.renaming2))
  req(!is.null(tabletorenameimport()))
  df$df.temp.rename[,input$liste.newgroup.renaming]
  df$df.temp.rename[,input$liste.newgroup.renaming2]
  aa<-df$df.temp.rename[,input$liste.newgroup.renaming]
  bb<-df$df.temp.rename[,input$liste.newgroup.renaming2]
  mylist<-as.list(bb)
  names(mylist)<-aa
  ccc<-as.data.frame(ccc) 
  temp.file.renaming<-df$df %>% mutate(new.value = coalesce(unlist(mylist)[get(input$liste.newgroup.rename)], get(input$liste.newgroup.rename)))
  df$df[,input$liste.newgroup.rename]<-temp.file.renaming$new.value

## ajouter un rafraichiissement de input$setspecies

  })

##### ----
output$limit.nbindiv.pie.simple=renderUI({
    req(!is.null(merging.was.done()))
    req(!is.null(readytosub()))
    nindiv.max<-max(somme.indiv())
    sliderInput("limitnbindivpiesimple", label = h5("filter according to sum of 'individuals' per bucket"), min=1, max=nindiv.max, value = 1,ticks = FALSE)
  })
observeEvent(input$limitnbindivpiesimple, {
  limitnbindiv(input$limitnbindivpiesimple)})

output$limit.nbindiv.pie.adv=renderUI({
  req(!is.null(merging.was.done()))
  req(!is.null(readytosub()))
  nindiv.max<-max(somme.indiv())
  sliderInput("limitnbindivpieadv", label = h5("filter according to sum of 'individuals' per bucket"), min=1, max=nindiv.max, value = 1,ticks = FALSE)
})
observeEvent(input$limitnbindivpieadv, {
  limitnbindiv(input$limitnbindivpieadv)})

output$limit.nbindiv.histo.adv=renderUI({
  req(!is.null(merging.was.done()))
  req(!is.null(readytosub()))
  nindiv.max<-max(somme.indiv())
  sliderInput("limitnbindivhistoadv", label = h5("filter according to sum of 'individuals' per bucket"), min=1, max=nindiv.max, value = 1,ticks = FALSE)
})
observeEvent(input$limitnbindivhistoadv, {
  limitnbindiv(input$limitnbindivhistoadv)})

##### simplification to checkboxgroupinput ----
observeEvent(input$all_artifact_entry, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
    updateCheckboxGroupInput(session, "natureofobjects", 
                             selected = levels(as.factor(df$df[,input$setspecies]))) })
observeEvent(input$reset_artifact_entry, {
    updateCheckboxGroupInput(session, "natureofobjects", 
                             selected = FALSE)}) 
  observeEvent(input$all_UAS_entry, {
    req(input$setlevels)
    updateCheckboxGroupInput(session, "UAS",
                             selected = levels(as.factor(df$df[,input$setlevels])))})
  observeEvent(input$reset_UAS_entry, {
    updateCheckboxGroupInput(session, "UAS",
                             selected = FALSE)})
  observeEvent(input$all_nature_entry, {
    req(input$setnature)
    updateCheckboxGroupInput(session, "Nature",
                             selected = levels(as.factor(df$df[,input$setnature])))})
  observeEvent(input$reset_nature_entry, {
    updateCheckboxGroupInput(session, "Nature",
                             selected = FALSE)})
  
  

##### creation df.sub that would be used to create plot ----

somme.indiv<-reactiveVal()
df.species.table<-reactiveVal()
number.total.of.species<-reactiveVal(1)
  
df.sub <- reactive({ 
    req(!is.null(input$xslider))
    req(!is.null(merging.was.done()))
    req(inputcolor())
    req(!is.null(readytosub()))
    df.sub<-df$df
    plotcol<-df.sub[,inputcolor()]
    df.sub$layer2 <- factor(plotcol)
    df.sub$point.size <- size.scale()
    #df.sub$point.size2<-size.scale()
    #df.sub<-df.sub %>% relocate(layer2, point.size, point.size2)
    df.sub<-df.sub %>% relocate(layer2, point.size)

    if (input$setdate!="null"){

      df.sub[,input$setdate] <-as.numeric(df.sub[,input$setdate])

      df.sub[,input$setdate][is.na(df.sub[,input$setdate])]<-0

      if (!is.null(input$Date2)) {
        df.sub<-df.sub %>%
          filter(df.sub[,input$setdate] >= input$Date2[1], df.sub[,input$setdate] <= input$Date2[2])}}
    if (input$setsector!="null"){
      df.sub <- df.sub[df.sub[,input$setsector] %in% input$localisation, ]}
    if (input$setlevels!="null"){
      df.sub <- df.sub[df.sub[,input$setlevels] %in% input$UAS, ]}
    if (input$setnature!="null"){
      df.sub <- df.sub[df.sub[,input$setnature] %in% input$Nature, ]}
    if (input$setpasse!="null"){
      df.sub <- df.sub[df.sub[,input$setpasse]%in% input$Passe, ]}
    if (input$setspecies!="null"){
      df.sub <- df.sub[df.sub[,input$setspecies]%in% natureofobjects.list(), ]}
    df.sub<-df.sub %>% 
      filter(.data[[setXX()]] >= input$xslider[1], .data[[setXX()]] <= input$xslider[2]) %>% 
      filter(.data[[setYY()]] >= input$yslider[1], .data[[setYY()]] <= input$yslider[2]) %>% 
      filter(.data[[setZZ()]] >= input$zslider[1], .data[[setZZ()]] <= input$zslider[2])
    df.sub
 
    shiny::validate(need(nrow(df.sub)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
    # df.sub <- df.sub %>%
    #   mutate(nb_remains = as.numeric(input$setnb)) ## forcer que ce soit numeric
    print(str(df.sub))
    df.sub <- df.sub %>%filter(!is.na(input$setnb))
    
    #### ajout tableaux data.column$data.graph.indiv et data.column$data.graph et data.column$data.df.xy
    data.df3<-df.sub %>% group_by(.data[[setID()]],.data[[input$setspecies]],.data[[setXX()]],.data[[setYY()]],.data[[setZZ()]])%>%
      # summarize(nb_total = sum(!!sym(input$setnb)))
      summarize(nb_total = sum(.data[[input$setnb]]), .groups = "drop")
    data.df3<-data.df3 %>%
      filter(!is.na(nb_total))
    data.column$data.graph.indiv<-data.df3 ##save for swarnplot

    assign("data.df3",data.df3,envir = globalenv())
    assign("df.sub",df.sub,envir = globalenv())
    
    # df.sub3<-df.sub[,c(setID(),setXX(),setYY(),setZZ())]
    # df.sub3.1<-df.sub[,c(setID(),input$setsector,setXX(),setYY(),setZZ())]
    
    # df.sub3.2<-df.sub[,c(setID(),input$setsector,input$setnature,input$setanat,setXX(),setYY(),setZZ()),input$setnb]
   
    # assign("df.sub3.2",df.sub,envir = globalenv())
    
    # df.sub4<-dplyr::distinct(df.sub3)
    # data.column$data.graph.indiv<-dplyr::inner_join(data.df3,df.sub4)  ##save for swarnplot
    data.column$data.graph.indiv.square<-df.sub %>% group_by(.data[[setID()]],.data[[input$setspecies]],.data[[input$setsector]],.data[[setXX()]],.data[[setYY()]],.data[[setZZ()]])%>%
      # summarize(nb_total = sum(!!sym(input$setnb))) ##save for swarnplot for square
      summarize(nb_total = sum(.data[[input$setnb]]), .groups = "drop") ##save for swarnplot for square
    
    data.df3.2<-df.sub %>% group_by(.data[[setID()]],.data[[input$setsector]],.data[[input$setnature]],.data[[input$setanat]],.data[[setXX()]],.data[[setYY()]],.data[[setZZ()]])%>%
      # summarize(nb_total = sum(!!sym(input$setnb)))
      summarize(nb_total = sum(.data[[input$setnb]]), .groups = "drop")
    
    data.df3.2<-data.df3.2 %>%
      filter(!is.na(nb_total))
    
    data.column$data.graph.indiv.square2 <- data.df3.2 %>%
      tidyr::uncount(weights = nb_total)
    assign("data.df3.2",data.df3.2,envir = globalenv())
    # df.sub4<-dplyr::distinct(df.sub3.1)
    # data.column$data.graph.indiv.square<-dplyr::inner_join(data.df3,df.sub4)  ##save for swarnplot for square
    # 
    # df.sub4<-dplyr::distinct(df.sub3.2)
    # data.column$data.graph.indiv.square2<-dplyr::inner_join(data.df3,df.sub4)  ##save for swarnplot for square
    # data.column$data.graph.indiv.square2<-df.sub4  ##save for swarnplot for square
    
     assign("df.sub2",data.column$data.graph.indiv.square2,envir = globalenv())
    
    
     data_graph<-data.df3
    # data_graph<-dplyr::inner_join(data.df3,df.sub3) 
    
    data_graph[[input$setspecies]] <- factor(data_graph[[input$setspecies]])
    data_graph <- left_join(data_graph,
                            data_graph %>% 
                              group_by(.data[[setID()]]) %>%
                              summarize(nb_total2 = sum(nb_total))) %>%
      group_by(setID()) %>%
      mutate(nb_frac = 2*pi*cumsum(nb_total)/nb_total,
             start = lag(nb_frac, default = 0))
     myFormula <- as.formula(paste0(setID(), " + ", setXX(), " + ",setYY(), " + ",setZZ(), " ~ ",input$setspecies))
     data.dgraph2<-reshape2::dcast(data_graph, myFormula , fill = 0L)
     myFormula2 <- as.formula(paste0(setID(), " ~ ",input$setspecies))
     data.df.xy<-left_join(data.dgraph2[,1:4],dcast(data.df3, myFormula2, fill = 0L))
     somme<-apply(data.df.xy[,5:ncol(data.df.xy)],1,sum)
     scale = .03/sqrt(max(data_graph$nb_total2))
     somme<-as.data.frame(somme)
     data.df.xy<-cbind.data.frame(data.df.xy,somme)
   
    data.column$data.df.xy<-data.df.xy ##save for pie chart
    data.column$data.graph<-data_graph ## save au cas ou # pour l'instant pas utilisé
    somme.indiv(somme)                 ## save for limit slider
    
    #### creation de df.species.table
    data.df.tot<-df.sub
    data.df.tot<-data.df.tot %>% relocate(null)
    data.df.tot2<-data.df.tot %>% group_by(.data[[input$setlevels]],.data[[input$setspecies]])  %>% 
      summarise(total = sum(!!sym(input$setnb)))
    myFormula <- as.formula(paste0(input$setlevels, " ~ ",input$setspecies))
    df.species.table<-reshape2::dcast(data.df.tot2,myFormula, fill = 0L)
    df.species.table(df.species.table)
    number.total.of.species(ncol(df.species.table)-1)
    ####
    assign("df.subX",data.column$data.graph.indiv.square,envir = globalenv())
    print("done")
    df.sub
  })  # end of df.sub reactive
 
##### generator of random XYZ ----
var.random<-reactiveVal("b")
var.data<-reactiveVal(1)
val.epaisseur.dec<-reactiveVal(2)
randomisdone<-reactiveVal(NULL)
observeEvent(input$val.epaisseur.dec, {
  val.epaisseur.dec(input$val.epaisseur.dec)
})
output$select.square.random=renderUI({
  req(!is.null(merging.was.done()))
 selectInput("selectsquare2", h4("select the column"),
              choices = names(df$df)[c(1:ncol(df$df))],
              selected = liste.square())
}) 

observeEvent(input$var.random, {
  var.random(input$var.random)
})
observeEvent(input$var.data, {
  var.data(input$var.data)
})
observeEvent(input$goButton.generate.random.data, { 

  if (setXX()=="null" || setYY()=="null" || setZZ()=="null") {
    showModal(modalDialog(
      title = "Issue to generate random data",
      HTML(paste(" New random XYZ coordinates cannot be generated using <br>", 
                 "verify bucket X, Y, or Z coordinate(s) has/have been settled"))
      ))
  }
  
  req(setXX()!="null")
  req(setYY()!="null")
  req(setZZ()!="null")
  #data.graph<-data.column$data.graph.indiv
  #data.graph <- data.graph[rep(row.names(data.graph), data.graph$nb_total),]

  switch(var.random(),
         a = { val.to.random = 0.50
         },
         b = { val.to.random = 0.25},
         c ={ val.to.random = 0.125})
  
  val.to.random <- val.to.random*as.numeric(var.data())
  
  ep<-as.numeric(val.epaisseur.dec())*as.numeric(var.data())*0.01/2
  req(!is.null(squareisdone))

  df.sub2<-data.column$data.graph.indiv
  df.sub2 <- df.sub2[rep(row.names(df.sub2), df.sub2$nb_total),]
  
  df.sub2 <- df.sub2 %>%
    rowwise() %>%
    mutate(x_random = runif(1, min=-val.to.random, max=val.to.random)+get("X_new"),
           y_random = runif(1, min=-val.to.random, max=val.to.random)+get("Y_new"),
           z_random = runif(1, min=-ep, max=ep)+get(setZZ())
          )

  df.sub2<-as.data.frame(df.sub2)
  #setXX("x_random")
  #setYY("y_random")
  #setZZ("z_random")
  #liste.x(c("x_random"))
  #liste.y(c("y_random"))
  #liste.z(c("z_random"))
  
    df.sub<-df.sub()
    setID<-setID()
    setsquare<-input$setsquare
    setdec<-input$setdec
    setspecies<-input$setspecies
    data.annex<-df.sub[,c(setID,setsquare,setdec)]
    data.annex<-dplyr::distinct(data.annex)
    list.sq<-levels(as.factor(data.annex[[setsquare]]))
    nbsquare<-length(list.sq)
    typ.subsquare<-NULL
    switch(formatsquare(),
           A= { typ.trav<-levels(as.factor(data.annex[[setsquare]]))
           count.nb.al<-length(typ.trav)
           count.nb.trav<-1
           typ.al<-1},
           B= { typ.trav<-levels(as.factor(str_replace(data.annex[[setsquare]],str_extract(data.annex[[setsquare]], "\\d+"),"")))
           count.nb.trav<-length(typ.trav)
           typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
           count.nb.al<-length(typ.al)
           },
           C= { 
             aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "\\d+")
             aa<-t(as.data.frame(aa))
             typ.trav<-levels(as.factor(aa[,1]))
             typ.subsquare<-levels(as.factor(aa[,2]))
             count.nb.trav<-length(typ.trav)
             typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
             count.nb.al<-length(typ.al)
             
           },
           D={
             
             aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "\\d+")
             aa2<-lapply(aa, "[", 1)
             aa2<-t(as.data.frame(aa2))
             typ.trav<-levels(as.factor(aa2[,1]))
             aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "-")
             aa2<-lapply(aa, "[", 2)
             aa2<-str_replace(aa2, "SC", "")
             typ.subsquare<-levels(as.factor(aa2))
             count.nb.trav<-length(typ.trav)
             typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
             count.nb.al<-length(typ.al)
           })
    nnnnncol<-count.nb.al
    nnnnrow<-count.nb.trav
    theo<-nnnnncol*nnnnrow
    # ici rajotuer sécurité si trompe de square (genre D) et typ.subsquare == null ou autre chose, faisant planter lignes ci dessous
    if (!is.null(typ.subsquare)){
      typ.subsquare<-str_replace_all(typ.subsquare,"[:punct:]|[:space:]|[:math:]","")
      theo<-theo*length(typ.subsquare)
      nnnnncol<-nnnnncol*length(typ.subsquare)
    }
    
    pp<-vector("list", theo)
    list.nom.pp<-vector(length = theo)
    k=0
    for (i in 1:count.nb.trav) {
      k<-k
      for (j in 1:count.nb.al) {
        k<-k
        if (!is.null(typ.subsquare)){
          for (y in 1:length(typ.subsquare)) {
            k<-k+1
            list.nom.pp[[k]]<-paste0(typ.trav[[i]],typ.al[[j]],typ.subsquare[[y]])
          }
        } else {
          k<-k+1
          list.nom.pp[[k]]<-paste0(typ.trav[[i]],typ.al[[j]])
        }
      }
    } 
    
    orga.square.histo.freq$nnnnncol<-nnnnncol
    orga.square.histo.freq$nnnnrow<-nnnnrow
    names(pp)<-list.nom.pp
    orga.square.histo.freq$list.nom.pp<-list.nom.pp
    orga.square.histo.freq$pp<-pp
    orga.square.histo.freq$typ.trav<-typ.trav
    orga.square.histo.freq$typ.subsquare<-typ.subsquare
    orga.square.histo.freq$typ.al<-typ.al

  
  
   xmax = as.numeric(unlist(df.sub2[,"x_random"])) %>% ceiling() %>% max(na.rm = TRUE)
   xmin=as.numeric(unlist(df.sub2[,"x_random"])) %>% floor() %>% min(na.rm = TRUE)
   updateSliderInput(session,'xslider','x limits',min=xmin,max=xmax,value=c(xmin,xmax),step=stepX())
   ymax = as.numeric(unlist(df.sub2[,setYY()])) %>% ceiling() %>% max(na.rm = TRUE)
   ymin=as.numeric(unlist(df.sub2[,setYY()])) %>% floor() %>% min(na.rm = TRUE)
   updateSliderInput(session,'yslider','y limits',min=ymin,max=ymax,value=c(ymin,ymax),step=stepY())

   zmax = as.numeric(unlist(df.sub2[,"z_random"])) %>% ceiling() %>% max(na.rm = TRUE) ##add unlist as consider strangely as a list.
   zmin=as.numeric(unlist(df.sub2[,"z_random"])) %>% floor() %>% min(na.rm = TRUE)
   updateSliderInput(session,'zslider','z limits',min=zmin,max=zmax,value=c(zmin,zmax),step=stepZ())
   # updateSelectInput(session,"sety",choices = names(df$df)[c(1:ncol(df$df))],
   #                   selected = "y_random")
   # updateSelectInput(session,"setz",choices = names(df$df.data)[c(1:ncol(df$df))],
   #              selected = "z_random")
   #updateSelectInput(session,"setx",choices = names(df$df)[c(1:ncol(df$df))],
   #                  selected = "x_random")
   
  data.column$data.graph.indiv.random<-df.sub2
  randomisdone(1)
   showModal(modalDialog(
     title = "New random XYZ data",
     HTML(paste(" New random XYZ coordinates have been generated"
    ))
   ))
  
  })



##### Plot - xyz coord pie chart  ---- 
output$plot2Dbox.simple <- renderUI({
    plotOutput("sectionYplot.simple", height = height.size(), width = width.size())
  })
  
output$sectionYplot.simple <- renderPlot({
    plot(plot2D.simple.react())
    session_store$plt2D.simple<- plot2D.simple.react()
  })
  
plot2D.simple.react<-reactive({
    req(!is.null(merging.was.done()))
    req(!is.null(df.sub()))
    orthofile<-NULL
    if (input$var.ortho.simple == "yes" ){
     orthofile <- switch(input$var1.simple,
                          xy = if(!is.null(input$file2)) {stack(input$file2$datapath)},
                          yx = if(!is.null(input$file5)) {stack(input$file5$datapath)},
                          xz = if(!is.null(input$file3)) {stack(input$file3$datapath)},
                          yz = if(!is.null(input$file4)) {stack(input$file4$datapath)})
    }
    df.sub2<-df.sub() 
    myvaluesx<-unlist(myvaluesx())
    size.scale <- size.scale()

   # to correct the color for ggplot2
    #myvaluesx2<-myvaluesx[levels(as.factor(df.sub()$layer2)) %in% levels(as.factor(droplevels(df.sub2$layer2)))]
   # myvaluesx2<-myvaluesx
    list.parameter.info<-var.function(input$var1.simple)
    var<-list.parameter.info[[1]]
    var2<-list.parameter.info[[2]]      
    axis.var.name<-list.parameter.info[[3]]
    axis.var2.name<-list.parameter.info[[4]]
    Xtickmarks.size<-list.parameter.info[[5]]
    Ytickmarks.size<-list.parameter.info[[6]]
    Xminor.breaks<-list.parameter.info[[7]]
    Yminor.breaks<-list.parameter.info[[8]]
    #data.graph<-data.column$data.graph
    data.df.xy<-data.column$data.df.xy
    data.df.xy<-data.df.xy %>% 
      filter(.data[["somme"]] >= limitnbindiv())

    ## a terminer ? 
    # data_labels <- data_graph %>% 
    #   group_by(.data[[input$setID]]) %>%
    #   summarize(nb_total = nb_total2[1])
    # data_labels<-left_join(data_labels,df.sub3)

    liste.name3<-intersect(natureofobjects.list(),colnames(data.df.xy))
    myvaluesx2<-myvaluesx[levels(as.factor(df.sub()[[input$setspecies]])) %in% liste.name3]
    data.df.xy$scale.to.pie<-data.df.xy$somme/input$scale.to.pie

    p <- ggplot2::ggplot()
    if (!is.null(orthofile)){

      p<-p + ggRGB(img = orthofile,
                   r = 1,
                   g = 2,
                   b = 3,
                   maxpixels =500000,
                   ggLayer = T)
    }
    
    p<- p + scatterpie::geom_scatterpie(data=data.df.xy, aes(x=.data[[var]], y=.data[[var2]], r=scale.to.pie),  
     cols=c(liste.name3))+
     coord_fixed()
      
     #   geom_text(data = data_labels,
     #             aes(label = .data[[input$setID]], x = .data[[var]], y = .data[[var2]] + scale*sqrt(nb_total) + .0001),
     #             size =6/.pt, vjust = 0) +
     #  scale_fill_manual(values=myvaluesx)

    #   ggplot2::coord_fixed(ratio.simple())
    p<-p+ggplot2::scale_fill_manual(values=myvaluesx2)+
        xlab(paste(axis.var.name))+ylab(paste(axis.var2.name))+
        do.call(themeforfigure.choice(), list()) +
        theme(axis.title.x = element_text(size=font_size()),
              axis.title.y = element_text(size=font_size()),
              axis.text.x = element_text(size=font_tick()),
              axis.text.y = element_text(size=font_tick()),
              legend.title = element_blank())+
        theme(legend.position=legendplotlyfig())
    p<-p+scale_x_continuous(breaks=seq(floor(min(df.sub2[[var]])),max(df.sub2[[var]]),Xtickmarks.size), minor_breaks = seq(floor(min(df.sub2[[var]])),max(df.sub2[[var]]),Xminor.breaks))+
         scale_y_continuous(breaks=seq(floor(min(df.sub2[[var2]])),max(df.sub2[[var2]]),Ytickmarks.size), minor_breaks = seq(floor(min(df.sub2[[var2]])),max(df.sub2[[var2]]),Yminor.breaks))
     p   
    
}) #end plot2D.react 
  
##### Plot - xyz coord - plot.point  ----
output$plot.point <- renderUI({
  plotOutput("section.plot.point", height = height.size(), width = width.size())
})

output$section.plot.point <- renderPlot({
  plot(plot2D.point.react())
  session_store$plt2D.point<- plot2D.point.react()
})
jitter<-reactiveVal(0)
observeEvent(input$jitter, {
  jitter(input$jitter)
})



plot2D.point.react<-reactive({
  req(!is.null(merging.was.done()))
  req(!is.null(df.sub()))
  orthofile<-NULL
  if (input$var.ortho.simple == "yes" ){
    orthofile <- switch(input$var1.simple,
                        xy = if(!is.null(input$file2)) {stack(input$file2$datapath)},
                        yx = if(!is.null(input$file5)) {stack(input$file5$datapath)},
                        xz = if(!is.null(input$file3)) {stack(input$file3$datapath)},
                        yz = if(!is.null(input$file4)) {stack(input$file4$datapath)})
  }
  
  myvaluesx<-unlist(myvaluesx())
  list.parameter.info<-var.function(input$var.plot.simple)
  var<-list.parameter.info[[1]]
  var2<-list.parameter.info[[2]]      
  axis.var.name<-list.parameter.info[[3]]
  axis.var2.name<-list.parameter.info[[4]]
  Xtickmarks.size<-list.parameter.info[[5]]
  Ytickmarks.size<-list.parameter.info[[6]]
  Xminor.breaks<-list.parameter.info[[7]]
  Yminor.breaks<-list.parameter.info[[8]]

  data.df.xy<-data.column$data.graph.indiv
  setspecies<-input$setspecies
  df.sub2<-df.sub()
  liste.name3<-intersect(natureofobjects.list(),levels(as.factor(data.df.xy[[setspecies]])))
  myvaluesx2<-myvaluesx[levels(as.factor(df.sub2[[setspecies]])) %in% liste.name3]  



sq <- scales::trans_new("squared", function(x) x^2, sqrt)

data.df.xy$density <- get_density(data.df.xy[[var]], data.df.xy[[var2]],w=1, n = 100)
print("revoir la valeur W pour pondérer")
print(data.df.xy) 

ydensity <- ggplot2::ggplot(data.df.xy, aes(.data[[var]], fill=factor(.data[[inputcolor()]]))) + 
  ggplot2::geom_density(alpha=.5) + 
  ggplot2::scale_fill_manual( values = myvaluesx2)+
  do.call(themeforfigure.choice(), list()) +
  ggplot2::theme(legend.position = "none")

# Density curve of y right panel 
zdensity <- ggplot2::ggplot(data.df.xy, aes(.data[[var2]], fill=factor(.data[[inputcolor()]]))) + 
  ggplot2::geom_density(alpha=.5) + 
  scale_fill_manual( values = myvaluesx2) + 
  do.call(themeforfigure.choice(), list()) +
  ggplot2::theme(legend.position = "none")+coord_flip()
blankPlot <- ggplot2::ggplot() + 
  ggplot2::geom_blank(aes(1,1))+
  ggplot2::theme(plot.background = element_blank(), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), 
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.text.x = element_blank(), 
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank()
  )

###### revoir au dessus
  
p <- ggplot2::ggplot(data=data.df.xy)
if (!is.null(orthofile)){
  
  p<-p + ggRGB(img = orthofile,
               r = 1,
               g = 2,
               b = 3,
               maxpixels =500000,
               ggLayer = T)
}

p<-p+ggplot2::geom_point(aes(x=.data[[var]],y=.data[[var2]], 
                            size=.data[["nb_total"]], 
                             colour=factor(.data[[setspecies]]), alpha=1),
                         #size=data.df.xy[["nb_total"]],
                         #size= data.df.xy[["nb_total"]]*size.scale(),

                         position = position_jitter(width = jitter(), height = jitter()))

p<-p+ggplot2::scale_colour_manual(values=myvaluesx2)+
  ggplot2::scale_size_binned(range = c(0.1, 10), trans=sq)+
    xlab(paste(axis.var.name))+ylab(paste(axis.var2.name))+
    do.call(themeforfigure.choice(), list())+
    theme(axis.title.x = element_text(size=font_size()),
          axis.title.y = element_text(size=font_size()),
          axis.text.x = element_text(size=font_tick()),
          axis.text.y = element_text(size=font_tick()),
          legend.title = element_blank())+
    theme(legend.position=legendplotlyfig())
  p<-p+scale_x_continuous(breaks=seq(floor(min(data.df.xy[[var]])),max(data.df.xy[[var]]),Xtickmarks.size), minor_breaks = seq(floor(min(data.df.xy[[var]])),max(data.df.xy[[var]]),Xminor.breaks))+
    scale_y_continuous(breaks=seq(floor(min(data.df.xy[[var2]])),max(data.df.xy[[var2]]),Ytickmarks.size), minor_breaks = seq(floor(min(data.df.xy[[var2]])),max(data.df.xy[[var2]]),Yminor.breaks))

    
  if (input$var.density.curves== "yes") {   
    
    p <- gridExtra::grid.arrange(ydensity, blankPlot, p, zdensity, 
                                 ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
    
  } else {
    p}    

  p
}) #end plot.point


##### Plot - xyz coord - plot.point.advanced  ----
output$plot.point.adv <- renderUI({
  plotlyOutput("section.plot.point.adv", height = height.size())
})

output$section.plot.point.adv <- renderPlotly({
  plot2D.point.react.adv()
  session_store$plt2D.point.adv<- plot2D.point.react.adv()
})


plot2D.point.react.adv<-reactive({
  
  req(!is.null(merging.was.done()))
  req(!is.null(df.sub()))
  list.parameter.info<-var.function(input$var.plot.simple.adv)
  var<-list.parameter.info[[1]]
  var2<-list.parameter.info[[2]]
  axis.var.name<-list.parameter.info[[3]]
  axis.var2.name<-list.parameter.info[[4]]
  Xtickmarks.size<-list.parameter.info[[5]]
  Ytickmarks.size<-list.parameter.info[[6]]
  Xminorbreaks<-as.numeric(list.parameter.info[[7]])
  Yminorbreaks<-as.numeric(list.parameter.info[[8]])
  myvaluesx<-unlist(myvaluesx())
  size.scale <- size.scale()
  setspecies<-input$setspecies
  natureofobjects.list<-natureofobjects.list()
  
  data.df.xy<-data.column$data.graph.indiv
  liste.name3<-intersect(natureofobjects.list,levels(as.factor(data.df.xy[[setspecies]])))
  myvaluesx2<-myvaluesx[levels(as.factor(data.df.xy[[setspecies]])) %in% liste.name3]
  myvaluesx2<-myvaluesx2[natureofobjects.list %in% liste.name3]
  #size.scale<-data.df.xy[["nb_total"]]
  data.df.xy <-as.data.frame(data.df.xy)

  assign("data.df.xy",data.df.xy,envir=.GlobalEnv)
  
  data.df.xy$density <- get_density(data.df.xy[[var]], data.df.xy[[var2]],w=1, n = 100)
   print("revoir la valeur W pour pondérer")

    blankPlot <- ggplot2::ggplot() +
    ggplot2::geom_blank(aes(1,1))+
    ggplot2::theme(plot.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank()
    )

    zdensity <- ggplot2::ggplot(data.df.xy, aes(.data[[var2]], fill=factor(.data[[inputcolor()]]))) + 
      ggplot2::geom_density(alpha=.5) + 
      scale_fill_manual( values = myvaluesx2) + 
      do.call(themeforfigure.choice(), list()) +
      ggplot2::theme(legend.position = "none")+coord_flip()
    zdensity_ly<-ggplotly(zdensity)
   
     assign("zd",zdensity_ly,envir = .GlobalEnv)

  p<- plotly::plot_ly(
                      data.df.xy,
                     # height = height.size(),
              # width = width.size(),
              x = ~data.df.xy[[var]], 
               y = ~data.df.xy[[var2]])

  p<- plotly::add_trace(p, type="scatter",
                mode = 'markers', 
                color = ~data.df.xy[[setspecies]],
                colors = myvaluesx2,
              size  = ~data.df.xy[['nb_total']],  
              #sizes = c(size.scale),
              sizes=c(size.scale,50),
              #marker = list(size = ~data.df.xy[['nb_total']], opacity=0.5),
              marker = list(opacity=opacity(), sizemode = 'diameter'),
              #marker = list(size = ~data.df.xy[['nb_total']], sizeref = 10000, sizemode = 'area'),
                fill = ~'',
                #symbol = ~df.sub2$shapeX, 
                # symbols = shape.level,
                #text=data.df.xy[,setID()],   
              text=data.df.xy[['nb_total']],
                hovertemplate = paste('<b>X</b>: %{x:.4}',
                                      '<br><b>Y</b>: %{y}',
                                      '<b>%{text}</b>')
  )

    Xtval<-seq(floor(min(data.df.xy[[var]])),max(data.df.xy[[var]]),Xminorbreaks)
    Xttxt <- rep("",length(Xtval)) 
    Xttxt[seq(1,length(Xtval),Xtickmarks.size)]<-as.character(Xtval)[seq(1,length(Xtval),Xtickmarks.size)]
    Ytval<-seq(floor(min(data.df.xy[[var2]])),max(data.df.xy[[var2]]), Yminorbreaks)
    Yttxt <- rep("",length(Ytval)) 
    Yttxt[seq(1,length(Ytval),Ytickmarks.size)]<-as.character(Ytval)[seq(1,length(Ytval),Ytickmarks.size)]
    assign("pp",p,envir = .GlobalEnv)
   
  p <-  p %>% layout(
    showlegend = legendplotlyfig(),
                     scene = list( aspectmode = "manual",
                                   aspectratio=list(x=ratiox(),y=ratioy()),
                                   autosize=FALSE),
                     xaxis = list(title = paste(axis.var.name),
                                  dtick = Xtickmarks.size,
                                  tick0 = floor(min(data.df.xy[[var]])),
                                  tickvals=Xtval,
                                  ticktext=Xttxt,
                                  titlefont = list(size = font_size()), tickfont = list(size = font_tick())),
                     yaxis = list(title = paste(axis.var2.name),
                                  dtick = Ytickmarks.size,
                                  tick0 = floor(min(data.df.xy[[var2]])),
                                  tickvals=Ytval,
                                  ticktext=Yttxt,
                                  titlefont = list(size = font_size()), tickfont = list(size = font_tick())),

                     dragmode = "select")%>%
    event_register("plotly_selecting")
  
  p2<-subplot(p,zdensity_ly,nrows = 2)
  #shareY=TRUE

  p2 <-p2 %>%
    config(displaylogo = FALSE,
           modeBarButtonsToAdd = list(dl_button),
           toImageButtonOptions = list(
             format = "svg")
    )
  

  
}) #end plot.point.adv





#####Plot - xyz coord-leaflet ---- 
######Plot - xyz coord leaflet - plot2D.pie ----
# output$plot2D.pie.adv <- renderUI({
#   plotOutput("sectionpiechart.adv", height = height.size(), width = width.size())
# })
# 
# output$sectionpiechart.adv <- renderLeaflet({
#   #plot(plot2D.simple.react())
#   session_store$plt2D.simple<- plot2D.simple.react()
#   
# })


output$plot2D.pie.adv <- renderLeaflet({
   req(!is.null(merging.was.done()))
   myvaluesx<-unlist(myvaluesx())
   list.parameter.info<-var.function(input$var.pie.adv)
   var<-list.parameter.info[[1]]
   var2<-list.parameter.info[[2]]      
   axis.var.name<-list.parameter.info[[3]]
   axis.var2.name<-list.parameter.info[[4]]
   Xtickmarks.size<-list.parameter.info[[5]]
   Ytickmarks.size<-list.parameter.info[[6]]
   Xminor.breaks<-list.parameter.info[[7]]
   Yminor.breaks<-list.parameter.info[[8]]

  data.df.xy<-data.column$data.df.xy %>% 
    filter(.data[["somme"]] >= limitnbindiv())
  
  liste.name3<-intersect(natureofobjects.list(),colnames(data.df.xy))
  myvaluesx2<-myvaluesx[levels(as.factor(df.sub()[[input$setspecies]])) %in% liste.name3]
  
  data.df.xy <- na.omit(data.df.xy)
  basemap =leaflet(data.df.xy,options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple")))  %>%
    addTiles() %>%
    addSimpleGraticule(
      showOriginLabel = TRUE,
      redraw = "move",
      hidden = FALSE,
      zoomIntervals = list(
        list(start = 1, end = 3, interval = 10),
        list(start = 4, end = 9, interval = 1),
        list(start = 10, end = 17, interval = 0.1),
        list(start = 18, end = 20, interval = 0.0002)
      ),
      layerId = NULL,
      group = NULL
    )
  basemap %>% addMinicharts(
    data.df.xy[[var]], data.df.xy[[var2]],
    type = "pie",
    chartdata = dplyr::select(data.df.xy,c(liste.name3)),
    colorPalette = myvaluesx2,
    width = input$scale.to.pie.adv, height = input$scale.to.pie.adv
  )
 
}) #end plot2D.react 

######Plot - xyz coord leaflet - plot2D.histo ----

output$plot2D.histo.adv <- renderLeaflet({
  req(!is.null(merging.was.done()))
  myvaluesx<-unlist(myvaluesx())
  list.parameter.info<-var.function(input$var.2d.histo)
  var<-list.parameter.info[[1]]
  var2<-list.parameter.info[[2]]      
  axis.var.name<-list.parameter.info[[3]]
  axis.var2.name<-list.parameter.info[[4]]
  Xtickmarks.size<-list.parameter.info[[5]]
  Ytickmarks.size<-list.parameter.info[[6]]
  Xminor.breaks<-list.parameter.info[[7]]
  Yminor.breaks<-list.parameter.info[[8]]

  data.df.xy<-data.column$data.df.xy
  data.df.xy<-data.df.xy %>% 
    filter(.data[["somme"]] >= limitnbindiv())
  liste.name3<-intersect(natureofobjects.list(),colnames(data.df.xy))
  myvaluesx2<-myvaluesx[levels(as.factor(df.sub()[[input$setspecies]])) %in% liste.name3]
  
  data.df.xy <- na.omit(data.df.xy)
  basemap =leaflet(data.df.xy,options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple")))  %>%
    addTiles() %>%
    addSimpleGraticule(
      showOriginLabel = TRUE,
      redraw = "move",
      hidden = FALSE,
      zoomIntervals = list(
        list(start = 1, end = 3, interval = 10),
        list(start = 4, end = 9, interval = 1),
        list(start = 10, end = 17, interval = 0.1),
        list(start = 18, end = 20, interval = 0.0002)
      ),
      layerId = NULL,
      group = NULL
    )
  basemap %>% addMinicharts(
    data.df.xy[[var]], data.df.xy[[var2]],
    chartdata = dplyr::select(data.df.xy,c(liste.name3)),
    colorPalette = myvaluesx2,
    width = input$scale.to.hist.adv, height = input$scale.to.hist.adv
  )
  
}) #end plot2D.react 

##### Plot - xyz coord - Density plot ----  
output$plotdens <- renderUI({
  plotOutput("sectiondensityplot", height = height.size(), width = width.size())
})

output$sectiondensityplot <- renderPlot({
  df.sub4<-df.sub()
  size.scale <- size.scale()
  myvaluesx<-unlist(myvaluesx())
  
  # orthofile<-NULL
  # if (input$var.ortho2 == "yes" ){
  #   orthofile <- switch(input$var3,
  #                       xy = if(!is.null(input$file2)) {stack(input$file2$datapath)},
  #                       yx = if(!is.null(input$file5)) {stack(input$file5$datapath)},
  #                       xz = if(!is.null(input$file3)) {stack(input$file3$datapath)},
  #                       yz = if(!is.null(input$file4)) {stack(input$file4$datapath)}) }
  # 
  list.parameter.info<-var.function(input$var3)
  var<-list.parameter.info[[1]]
  var2<-list.parameter.info[[2]] 
  nameaxis<-c(list.parameter.info[[3]],list.parameter.info[[4]])
  Xtickmarks.size<-list.parameter.info[[5]]
  Ytickmarks.size<-list.parameter.info[[6]]
  Xminor.breaks<-list.parameter.info[[7]]
  Yminor.breaks<-list.parameter.info[[8]]

  
  if (is.null(input$setnb)){
    df.sub4$density <- get_density(df.sub4[[var]], df.sub4[[var2]], w=length(df.sub4[[var]]), n = 100)
  } else { 
    df.sub4$density <- get_density(df.sub4[[var]], df.sub4[[var2]], w=df.sub4[[input$setnb]])
  }
  
  if (input$var.point.size.density== "yes") {
    size.scale<-df.sub4[[input$setnb]]/input$size.dens
  }
  
  # to correct the color for ggplot2
  myvaluesx2<-myvaluesx[levels(as.factor(df$df[[inputcolor()]])) %in% levels(as.factor(df.sub4[[inputcolor()]]))]
  # Density curve of x left panel 
  ydensity <- ggplot2::ggplot(df.sub4, aes(.data[[var]], fill=factor(.data[[inputcolor()]]))) + 
    ggplot2::geom_density(alpha=.5) + 
    ggplot2::scale_fill_manual( values = myvaluesx2)+
    do.call(themeforfigure.choice(), list()) +
    ggplot2::theme(legend.position = "none")
  
  # Density curve of y right panel 
  zdensity <- ggplot2::ggplot(df.sub4, aes(.data[[var2]], fill=factor(.data[[inputcolor()]]))) + 
    ggplot2::geom_density(alpha=.5) + 
    scale_fill_manual( values = myvaluesx2) + 
    do.call(themeforfigure.choice(), list()) +
    ggplot2::theme(legend.position = "none")+coord_flip()
  blankPlot <- ggplot2::ggplot() + 
    ggplot2::geom_blank(aes(1,1))+
    ggplot2::theme(plot.background = element_blank(), 
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), 
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(), 
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank()
    )
  if (input$black.col.point == "yes") {
    df.sub4$density<-c(1)
  }
  
  # if (is.null(orthofile)){
  p<-ggplot(df.sub4,aes(.data[[var]], .data[[var2]], color = density)) + 
    ggplot2::geom_point(aes(.data[[var]], .data[[var2]], color = density), alpha=transpar(), size=size.scale) 
  if(input$fill_density=="yes") {
    p<- p+geom_density_2d_filled(alpha = 0.5)}
  
  p<- p+ggplot2::labs(x = nameaxis[1],y = nameaxis[2])+
    do.call(themeforfigure.choice(), list()) +
    ggplot2::theme(axis.title.x = element_text(size=font_size()),
                   axis.title.y = element_text(size=font_size()),
                   axis.text.x = element_text(size=font_tick()),
                   axis.text.y = element_text(size=font_tick()))+    
    ggplot2::coord_fixed(ratio.simple()) 
  # {if (input$ratio.to.coord)coord_fixed()}
  
  # } 
  # else { p <- ggplot2::ggplot()+ ggRGB(img = orthofile,
  #                              r = 1,
  #                              g = 2,
  #                              b = 3,
  #                              maxpixels =500000,
  #                              ggLayer = T) +
  #   ggplot2::geom_point(df.sub4,mapping=aes(.data[[var]], .data[[var2]], color = density),alpha=transpar(), size=df.sub4$point.size2)+
  #   ggplot2::labs(x = nameaxis[1],y = nameaxis[2])
  # }
  
  if (input$var.plotlyg.lines== "yes") {
    p<- p + ggplot2::geom_density_2d(mapping=aes(.data[[var]],.data[[var2]], color = after_stat(level)),data=df.sub4)}
  p<- p + viridis::scale_color_viridis()+
    ggplot2::guides(fill = guide_legend(title = "Level"))+
    ggplot2::theme(axis.title.x = element_text(size=font_size()),
                   axis.title.y = element_text(size=font_size()),
                   axis.text.x = element_text(size=font_tick()),
                   axis.text.y = element_text(size=font_tick()),)
  p<- p + ggplot2::scale_x_continuous(breaks=seq(floor(min(df.sub4[[var]])),max(df.sub4[[var]]),Xtickmarks.size),minor_breaks = seq(floor(min(df.sub4[[var]])),max(df.sub4[[var]]),Xminor.breaks)) + 
    ggplot2::scale_y_continuous(breaks=seq(floor(min(df.sub4[[var2]])),max(df.sub4[[var2]]),Ytickmarks.size), minor_breaks = seq(floor(min(df.sub4[[var2]])),max(df.sub4[[var2]]),Yminor.breaks))
  
  if (input$var.density.curves== "yes") {   
    
    p <- gridExtra::grid.arrange(ydensity, blankPlot, p, zdensity, 
                                 ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
    
  } else {
    p} 
  session_store$plotdensity <- p
  p
}) #end output$sectiondensityplot  

##### Plot - xyz coord- Swarn plot ---- 
output$plot.2dsw <- renderUI({
    plotOutput("sectionYplot.sw", height = height.size(), width = width.size())
  })
  
output$sectionYplot.sw <- renderPlot({
    plot(plot2D.sw.react())
    session_store$plt2D.sw<- plot2D.sw.react()
  })
plot2D.sw.react<-reactive({
    req(!is.null(merging.was.done()))
    min.size2<-minsize()
    df.sub2<-df.sub() 
    myvaluesx<-unlist(myvaluesx())
    size.scale <- size.scale()
    list.parameter.info<-var.function(input$var.2d.slice)
    var<-list.parameter.info[[1]]
    var2<-list.parameter.info[[2]]      
    axis.var.name<-list.parameter.info[[3]]
    axis.var2.name<-list.parameter.info[[4]]
    Xtickmarks.size<-list.parameter.info[[5]]
    Ytickmarks.size<-list.parameter.info[[6]]
    Xminor.breaks<-list.parameter.info[[7]]
    Yminor.breaks<-list.parameter.info[[8]]
    data.graph<-data.column$data.graph.indiv
    data.graph <- data.graph[rep(row.names(data.graph), data.graph$nb_total),]
    liste.name3<-intersect(natureofobjects.list(),levels(as.factor(data.graph[[input$setspecies]])))
    myvaluesx2<-myvaluesx[levels(as.factor(df.sub()[[input$setspecies]])) %in% liste.name3]
    temp.data<-cbind.data.frame(liste.name3,myvaluesx2)
    colnames(temp.data)[1]<-input$setspecies
    data.graph<-inner_join(data.graph,temp.data, by=input$setspecies)
    myvaluesx3<-data.graph$myvaluesx2
    data.graph<-as.data.frame(data.graph)

    p <-ggplot2::ggplot()+
      geom_beeswarm(data=data.graph, aes(x=.data[[var]],y=.data[[var2]]),
                    colour = as.factor(c(myvaluesx3)),alpha=1,priority='ascending',groupOnX=TRUE)+
      scale_color_manual(values=c(myvaluesx3))
      p<-p+scale_x_continuous(breaks=seq(floor(min(data.graph[[var]])),max(data.graph[[var]]),Xtickmarks.size), minor_breaks = seq(floor(min(data.graph[[var]])),max(data.graph[[var]]),Xminor.breaks))+
       scale_y_continuous(breaks=seq(floor(min(data.graph[[var2]])),max(data.graph[[var2]]),Ytickmarks.size), minor_breaks = seq(floor(min(data.graph[[var2]])),max(data.graph[[var2]]),Yminor.breaks))+
        xlab(paste(axis.var.name))+ylab(paste(axis.var2.name))+
        do.call(themeforfigure.choice(), list())  +
        theme(axis.title.x = element_text(size=font_size()),
              axis.title.y = element_text(size=font_size()),
              axis.text.x = element_text(size=font_tick()),
              axis.text.y = element_text(size=font_tick()),
              legend.title = element_blank())+
        theme(legend.position=legendplotlyfig())
      p  
}) #end plot2D.sw

##### Plot - splits - barplot frequency ----

# #observeEvent(c(input$go.generate.square,input$go.generate.square2), {
# observeEvent(c(input$go.generate.square), {
#   df.sub<-df.sub()
#   setID<-setID()
#   setsquare<-input$setsquare
#   setdec<-input$setdec
#   setspecies<-input$setspecies
#   data.annex<-df.sub[,c(setID,setsquare,setdec)]
#   data.annex<-dplyr::distinct(data.annex)
#   list.sq<-levels(as.factor(data.annex[[setsquare]]))
#   nbsquare<-length(list.sq)
#   typ.subsquare<-0
#   switch(formatsquare(),
#          A= { typ.trav<-levels(as.factor(data.annex[[setsquare]]))
#          count.nb.al<-length(typ.trav)
#          count.nb.trav<-1
#          typ.al<-1},
#          B= { typ.trav<-levels(as.factor(str_replace(data.annex[[setsquare]],str_extract(data.annex[[setsquare]], "\\d+"),"")))
#          count.nb.trav<-length(typ.trav)
#          typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
#          count.nb.al<-length(typ.al)
#          },
#          C= { 
#            aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "\\d+")
#            aa<-t(as.data.frame(aa))
#            typ.trav<-levels(as.factor(aa[,1]))
#            typ.subsquare<-levels(as.factor(aa[,2]))
#            count.nb.trav<-length(typ.trav)
#            typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
#            count.nb.al<-length(typ.al)
# 
#          },
#          D={
#            
#            aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "\\d+")
#            aa2<-lapply(aa, "[", 1)
#            aa2<-t(as.data.frame(aa2))
#            typ.trav<-levels(as.factor(aa2[,1]))
#            aa<-str_split(levels(as.factor(data.annex[[setsquare]])), "-")
#            aa2<-lapply(aa, "[", 2)
#            aa2<-str_replace(aa2, "SC", "")
#            typ.subsquare<-levels(as.factor(aa2))
#            count.nb.trav<-length(typ.trav)
#            typ.al<-levels(as.factor(as.numeric(str_extract(data.annex[[setsquare]], "\\d+"))))
#            count.nb.al<-length(typ.al)
#          })
#   nnnnncol<-count.nb.al
#   nnnnrow<-count.nb.trav
#   theo<-nnnnncol*nnnnrow
#   # ici rajotuer sécurité si trompe de square (genre D) et typ.subsquare == null ou autre chose, faisant planter lignes ci dessous
#   if (typ.subsquare > "0"){
#     theo<-theo*length(typ.subsquare)
#     nnnnncol<-nnnnncol*length(typ.subsquare)
#   }
# 
#   pp<-vector("list", theo)
#   list.nom.pp<-vector(length = theo)
#   k=0
#   for (i in 1:count.nb.trav) {
#     k<-k
#     for (j in 1:count.nb.al) {
#       k<-k
#       if (typ.subsquare > 0){
#         for (y in 1:length(typ.subsquare)) {
#           k<-k+1
#           list.nom.pp[[k]]<-paste0(typ.trav[[i]],typ.al[[j]],typ.subsquare[[y]])
#         }
#       } else {
#         k<-k+1
#         list.nom.pp[[k]]<-paste0(typ.trav[[i]],typ.al[[j]])
#       }
#     }
#   } 
#   
#   orga.square.histo.freq$nnnnncol<-nnnnncol
#   orga.square.histo.freq$nnnnrow<-nnnnrow
#   names(pp)<-list.nom.pp
#   orga.square.histo.freq$list.nom.pp<-list.nom.pp
#   orga.square.histo.freq$pp<-pp
#   orga.square.histo.freq$typ.trav<-typ.trav
#   orga.square.histo.freq$typ.subsquare<-typ.subsquare
#   orga.square.histo.freq$typ.al<-typ.al
#   showModal(modalDialog(
#     title = "Squaring is generate",
# 
#   ))
# })


output$plot.histo.freq <- renderUI({  
  plotOutput("sectionplot.histo.freq", height = height.size(), width = width.size())
})

output$sectionplot.histo.freq <- renderPlot({
  plot(plot2D.histo.freq())
  session_store$plt2D.histo.freq<- plot2D.histo.freq()
})




plot2D.histo.freq<-reactive({
  req(!is.null(merging.was.done()))
  df.sub<-df.sub()
  setID<-setID()
  shiny::validatevalidate(need(input$setsquare != "null" ,"Please select the square in 'Data Upload'"))
  setsquare<-input$setsquare
  shiny::validatevalidate(need(!is.null(orga.square.histo.freq$typ.al) ,"Please generate the squaring"))
  
  setdec<-input$setdec
  setspecies<-input$setspecies
  natureofobjects.list<-natureofobjects.list()
  myvaluesx<-unlist(myvaluesx())
  list.parameter.info<-var.function(input$var1.simple)
  #var<-list.parameter.info[[1]]
  #var2<-list.parameter.info[[2]]      
  axis.var.name<-list.parameter.info[[3]]
  axis.var2.name<-list.parameter.info[[4]]
  Xtickmarks.size<-list.parameter.info[[5]]
  Ytickmarks.size<-list.parameter.info[[6]]
  Xminor.breaks<-list.parameter.info[[7]]
  Yminor.breaks<-list.parameter.info[[8]]
  
  data.graph<-data.column$data.graph.indiv
 # data.graph.row <- data.graph[rep(row.names(data.graph), data.graph$nb_total),] #transform le tableau selon la colonne nb_total
  temp<-dplyr::distinct(df.sub[,c(setID,setsquare,setdec)])
  #temp<-dplyr::distinct(temp)
  data.annex<-dplyr::left_join(data.graph,temp)

  pp<-orga.square.histo.freq$pp
  nnnnncol<-orga.square.histo.freq$nnnnncol
  nnnnrow<-orga.square.histo.freq$nnnnrow
  theo<-nnnnncol*nnnnrow

  liste.name3<-dplyr::intersect(natureofobjects.list,levels(as.factor(data.graph[[setspecies]])))
  myvaluesx2<-myvaluesx[levels(as.factor(data.graph[[setspecies]])) %in% liste.name3]
  myvaluesx2<-myvaluesx2[natureofobjects.list %in% liste.name3]
  temp.data<-cbind.data.frame(liste.name3,myvaluesx2)
  colnames(temp.data)<-c(input$setspecies,"temp.col")
  mode.to.plot <-input$var.barplot
  if (mode.to.plot=="Z"){ 
    mode.to.plot <-setZZ()
  } else {
    mode.to.plot <-input$setdec
  }

 #  m.order<-max(length(factor.order.square.split$A), 
 #               length(factor.order.square.split$B),
 #               length(factor.order.square.split$C),
 #               length(factor.order.square.split$D))
 #  bb<-4
 # if(m.order>0) {
 #    tab.temp<-as.data.frame(matrix("blank",nrow=4,ncol=m.order))
 #    bi<-1
 #    if (!is.null(factor.order.square.split$A)){tab.temp[bi,1:length(factor.order.square.split$A)]<-factor.order.square.split$A
 #    bi<-bi+1} else {
 #      
 #      tab.temp<-tab.temp[-c(bi),]}
 #    tab.temp<-as.data.frame(tab.temp)
 #    if (!is.null(factor.order.square.split$B)){tab.temp[bi,1:length(factor.order.square.split$B)]<-factor.order.square.split$B
 #    bi<-bi+1} else {
 #      tab.temp<-tab.temp[-c(bi),]}
 #    tab.temp<-as.data.frame(tab.temp)
 #    if (!is.null(factor.order.square.split$C)){tab.temp[bi,1:length(factor.order.square.split$C)]<-factor.order.square.split$C
 #    bi<-bi+1} else {
 #      tab.temp<-tab.temp[-c(bi),]}
 #    tab.temp<-as.data.frame(tab.temp)
 #    if (!is.null(factor.order.square.split$D)){tab.temp[bi,1:length(factor.order.square.split$D)]<-factor.order.square.split$D
 #    bi<-bi+1} else {
 #      tab.temp<-tab.temp[-c(bi),]}
 #    tab.temp<-as.data.frame(tab.temp)
 #    bi<-bi-1
 #    tab.list<-as.vector(t(tab.temp))
 #    
 #    
 #    pp<-pp[1:length(tab.list)]
 #    names(pp)<-tab.list
 #    ##A revoir car rempli de blanc
 #    nnnnncol<-m.order
 #    nnnnrow<-bi
 #    theo<-nnnnncol*nnnnrow
 #  }

  
 for (i in 1:theo) {
    if (names(pp)[[i]]=="blank") {
      next()} else {
        data.graph2<-data.annex  %>%
          filter((.data[[setsquare]] %in% names(pp)[[i]]))
        #data.graph2[[]] <-str_sort(data.graph2[[input$setdec]], numeric = TRUE)
        if (nrow(data.graph2)==0) { next()} else {
          data.graph2<-data.graph2 %>% mutate(name = forcats::fct_reorder(as.factor(.data[[input$setdec]]), desc(str_sort(.data[[input$setdec]], numeric = TRUE)))) 
          
          data.graph3<-inner_join(data.graph2,temp.data, by=setspecies)
          
          if (!is.null(factor.order.split())){
            data.graph3[[input$setdec]]<-factor(data.graph3[[input$setdec]], levels = factor.order.split())
          }
          myvaluesx5<-myvaluesx2[liste.name3 %in% levels(as.factor(data.graph3[[setspecies]]))]
          print(data.graph3)
          
          data.graph3<-data.graph3[data.graph3["nb_total"] !=0,]
          data.graph3 <- data.graph3[rep(row.names(data.graph3),as.numeric(unlist(data.graph3["nb_total"]))),]
          
          p1 <- ggplot2::ggplot()
          p1 <- p1  + ggplot2::geom_bar(data=data.graph3, aes(y=as.factor(.data[[mode.to.plot]]), fill = factor(.data[[setspecies]])), position = "fill")+
            scale_fill_manual(values=c(myvaluesx5),
                              drop=FALSE)+
            xlab(paste(axis.var.name))+ylab(paste(axis.var2.name))+
            do.call(themeforfigure.choice(), list()) +
            theme(axis.title.x = element_text(size=font_size()),
                  axis.title.y = element_text(size=font_size()),
                  axis.text.x = element_text(size=font_tick()),
                  axis.text.y = element_text(size=font_tick()),
                  legend.title = element_blank())
          
          pp[[i]]<-p1 
          
        }
      }
  }
  p <- ggpubr::ggarrange(plotlist = pp,
                         ncol=nnnnncol, nrow=nnnnrow, align = "hv",
                         common.legend = TRUE, legend = legendplotlyfig())
  p   
  
}) #end plot2D.histo.freq 

#### Plot - splits - barplot frequency - plotly - #################### a faire ----
  output$plot.histo.freq.plotly <- renderUI({
    plotOutput("sectionplot.histo.freq.plotly", height = height.size(), width = width.size())
  })
  
  output$sectionplot.histo.freq.plotly <- renderPlotly({
    plot(plot2D.histo.freq.plotly())
    session_store$plt2D.histo.freq.plotly<- plot2D.histo.freq.plotly()
  })
  
  plot2D.histo.freq.plotly<-reactive({  
    req(!is.null(merging.was.done()))
    req(!is.null(df.sub()))
    
    myvaluesx<-unlist(myvaluesx())
    list.parameter.info<-var.function("yz")
    var<-list.parameter.info[[1]]
    var2<-list.parameter.info[[2]]      
    axis.var.name<-list.parameter.info[[3]]
    axis.var2.name<-list.parameter.info[[4]]
    Xtickmarks.size<-list.parameter.info[[5]]
    Ytickmarks.size<-list.parameter.info[[6]]
    Xminor.breaks<-list.parameter.info[[7]]
    Yminor.breaks<-list.parameter.info[[8]]
    data.df.xy<-data.column$data.df.xy
    #ll.order<-liste.passe.order()
    liste.name3<-intersect(natureofobjects.list(),colnames(data.df.xy))
    myvaluesx2<-myvaluesx[levels(as.factor(df.sub()[[input$setspecies]])) %in% liste.name3]
  
    
}) #end plot2D.histo.freq.plotly
  


###### Plot - splits - plot abundance data  ######################## a finir----
  output$plot.abondance.graph <- renderUI({  
    plotOutput("sectionplot.abondance.graph", height = height.size(), width = width.size())
  })
  
  output$sectionplot.abondance.graph <- renderPlot({
    plot(plot2D.split.abondance())
    session_store$plot2D.abondance<- plot2D.split.abondance()
  })
  
  plot2D.split.abondance<-reactive({
    req(!is.null(merging.was.done()))
    req(!is.null(df.sub()))
    df.sub<-df.sub()
    setID<-setID()
    shiny::validatevalidate(need(input$setsquare != "null" ,"Please select the square in 'Data Upload'"))
    setsquare<-input$setsquare
    shiny::validatevalidate(need(!is.null(orga.square.histo.freq$typ.al) ,"Please generate the squaring"))
    setdec<-input$setdec
    setspecies<-input$setspecies
    natureofobjects.list<-natureofobjects.list()

    list.parameter.info<-var.function(input$var1.simple)
    #var<-list.parameter.info[[1]]
    #var2<-list.parameter.info[[2]]      
    axis.var.name<-list.parameter.info[[3]]
    axis.var2.name<-list.parameter.info[[4]]
    Xtickmarks.size<-list.parameter.info[[5]]
    Ytickmarks.size<-list.parameter.info[[6]]
    Xminor.breaks<-list.parameter.info[[7]]
    Yminor.breaks<-list.parameter.info[[8]]
    
    mode.to.plot <-input$var.barplot2
    if (mode.to.plot=="Z"){ 
      mode.to.plot <-setZZ()
    } else {
      mode.to.plot <-setdec
    }
    data.graph<-data.column$data.df.xy

    if( input$var.plot.abundance=="NR per volum") {
      temp<-df.sub[,c(setID,setsquare,setdec,input$setvol)]
      temp<-dplyr::distinct(temp)
      data.annex<-dplyr::left_join(data.graph,temp)
      print(data.annex)
      data.annex$somme<-data.annex$somme/data.annex[[input$setvol]]
    }
    else {
    temp<-df.sub[,c(setID,setsquare,setdec)]
    temp<-dplyr::distinct(temp)
    data.annex<-dplyr::left_join(data.graph,temp)}
    
    pp<-orga.square.histo.freq$pp
    nnnnncol<-orga.square.histo.freq$nnnnncol
    nnnnrow<-orga.square.histo.freq$nnnnrow
    theo<-nnnnncol*nnnnrow
    
    #myvaluesx<-unlist(myvaluesx())
    # liste.name3<-dplyr::intersect(natureofobjects.list,levels(as.factor(data.graph[[setspecies]])))
    # myvaluesx2<-myvaluesx[levels(as.factor(data.graph[[setspecies]])) %in% liste.name3]
    # myvaluesx2<-myvaluesx2[natureofobjects.list %in% liste.name3]
    # temp.data<-cbind.data.frame(liste.name3,myvaluesx2)
    # colnames(temp.data)<-c(input$setspecies,"temp.col")
    
    #donc maintenant vérif somme et rajouter pondération

    
    xmax2<-data.annex$somme %>% ceiling() %>% max(na.rm = TRUE)
    xmin2<-data.annex$somme %>% floor() %>% min(na.rm = TRUE)
    
    for (i in 1:theo) {
      #assign("data.graph2",data.graph2,envir = .GlobalEnv)
      
      data.graph2<-data.annex  %>%
        filter((.data[[setsquare]] %in% names(pp)[[i]]))
      if (nrow(data.graph2)==0) { next()} else {
        data.graph2<-data.graph2 %>% mutate(name = forcats::fct_reorder(.data[[setdec]], desc(str_sort(.data[[setdec]], numeric = TRUE)))) 
        data.graph3<-data.graph2 %>%group_by(.data[[mode.to.plot]],.data[[setsquare]]) %>%
          summarize(somme = sum(somme))

        if (!is.null(factor.order.split())){
          data.graph3[[setdec]]<-factor(data.graph3[[setdec]], levels = factor.order.split())
        }
        # myvaluesx5<-myvaluesx2[liste.name3 %in% levels(as.factor(data.graph3[[setspecies]]))]

       #  p1 <- ggplot2::ggplot(data=data.graph2, aes(x=.data[["somme"]], y=as.factor(.data[[mode.to.plot]]), group=factor(.data[[setdec]])))+
       

        p1 <- ggplot2::ggplot(data=data.graph3, aes(x=.data[["somme"]], y=as.factor(.data[[mode.to.plot]])))+
         geom_line()+
          geom_point()+
          scale_x_continuous(limits = c(xmin2, xmax2)) + 
          xlab(paste(axis.var.name))+ylab(paste(axis.var2.name))+
          do.call(themeforfigure.choice(), list()) +
          theme(axis.title.x = element_text(size=font_size()),
                axis.title.y = element_text(size=font_size()),
                axis.text.x = element_text(size=font_tick()),
                axis.text.y = element_text(size=font_tick()),
                legend.title = element_blank())
        pp[[i]]<-p1 
     
      }
    }
    
     p <- ggpubr::ggarrange(plotlist = pp,
                            ncol=nnnnncol, nrow=nnnnrow, align = "hv",
                            common.legend = TRUE, legend = legendplotlyfig())
     
    p   
    
    
  })
  
  ##### Plot - splits - Square density ---- 
  output$plot.sqd <- renderUI({
    plotOutput("section.sqd", height = height.size(), width = width.size())
  })
  
  output$section.sqd <- renderPlot({
    plot(plt.sqd())
    #plt.sqd()
    session_store$plt.sqd<- plt.sqd()
  })
  
  plt.sqd<-reactive({ 
    orthofile<-NULL
    if (input$var.ortho.simple.random == "yes" ){
      orthofile <- switch(input$var1.simple,
                          xy = if(!is.null(input$file2)) {stack(input$file2$datapath)},
                          yx = if(!is.null(input$file5)) {stack(input$file5$datapath)},
                          xz = if(!is.null(input$file3)) {stack(input$file3$datapath)},
                          yz = if(!is.null(input$file4)) {stack(input$file4$datapath)})
    }
    
    
    data.df.calcul<-data.column$data.graph.indiv %>% group_by(.data[["X"]],.data[["Y"]])%>%
      summarize(nb_total = sum(!!sym("nb_total")))
    
    data.df.calcul$X<-round(data.df.calcul$X,0)
    data.df.calcul$Y<-round(data.df.calcul$Y,0)
    
    assign("data.temp",data.df.calcul, envir=.GlobalEnv)
    print(data.df.calcul)
    
    Xmmin<-min(data.df.calcul$X)
    Xmmax<-max(data.df.calcul$X)
    Ymmin<-min(data.df.calcul$Y)
    Ymmax<-max(data.df.calcul$Y)
    
    mycols <- rev(c("red4","red2","tomato2","orange","gold1","forestgreen"))
    cols <- colorRampPalette(mycols)
    
    #myvals <- c(0, 8, 9, 10, 11, 25)
    myvals <- sort(unique(data.df.calcul$nb_total))
    scaled_val <- scales::rescale(myvals, 0:1)
    
    #colnames(data.df.calcul)[1]<-"X"
    #colnames(data.df.calcul)[2]<-"Y"
    
   p<- ggplot(data.df.calcul, aes(x = setXX(), y = setYY(), fill = nb_total)) +
      geom_tile() +
      scale_fill_gradientn(colours = cols(length(mycols)), 
                           values = scaled_val) +
      theme(axis.text.y = element_text(angle = 90, hjust = 1), legend.position = "top") +
      scale_x_continuous(name = "tessssst X", breaks = seq(Xmmin, Xmmax, 1), expand = c(0, 0)) +
      scale_y_continuous(name = "YYYYYY", breaks = seq(Ymmin, Ymmax, 1), expand = c(0, 0)) +
      geom_hline(yintercept = seq(Ymmin-0.5, Ymmax+0.5)) +
      geom_vline(xintercept = seq(Xmmin-0.5, Xmmax+0.5)) +
      coord_fixed()
    
    p
    })  
 

  # ggplot(data.df.calcul, aes(x = X, y = Y, fill = nb_total)) +
  #   geom_tile() 
  #  ggplot(longData, aes(x = Consequence, y = Likelihood, fill = value)) +
  #    geom_tile() 
  #    scale_fill_gradientn(colours = cols(length(mycols)), 
  #                        values = scaled_val) 
  #   theme(axis.text.y = element_text(angle = 90, hjust = 1), legend.position = "none") +
  #   scale_x_continuous(name = "Probability", breaks = seq(1, 5, 1), expand = c(0, 0)) +
  #   scale_y_reverse(name = "Severity", breaks = seq(1, 5, 1), expand = c(0, 0)) +
  #   geom_hline(yintercept = seq(1.5, 5.5)) +
  #   geom_vline(xintercept = seq(1.5, 5.5)) +
  #   coord_fixed() 
  
  
##### Plot - random coordinate  ###################---- 
####### Plot - random coordinate - simple 2D plot ----
  output$plot2Dbox.simple.random <- renderUI({
    plotOutput("sectionYplot.simple.random", height = height.size(), width = width.size())
  })
  
  output$sectionYplot.simple.random <- renderPlot({
    plot(plot2D.simple.react.random())
    session_store$plt2D.simple.random<- plot2D.simple.react.random()
  })
plot2D.simple.react.random<-reactive({ 
    
    orthofile<-NULL
    if (input$var.ortho.simple.random == "yes" ){
      orthofile <- switch(input$var1.simple,
                          xy = if(!is.null(input$file2)) {stack(input$file2$datapath)},
                          yx = if(!is.null(input$file5)) {stack(input$file5$datapath)},
                          xz = if(!is.null(input$file3)) {stack(input$file3$datapath)},
                          yz = if(!is.null(input$file4)) {stack(input$file4$datapath)})
    }
    
    
    #df.sub2<-df.sub()   
    #data.graph<-data.column$data.graph.indiv
    #data.graph <- data.graph[rep(row.names(data.graph), data.graph$nb_total),]
    #df.sub2<-data.column$data.graph.indiv
    #df.sub2 <- df.sub2[rep(row.names(df.sub2), df.sub2$nb_total),]
    df.sub2<-data.column$data.graph.indiv.random
    
    #df.sub3<-df.sub.minpoint()
    myvaluesx<-unlist(myvaluesx())
    size.scale <- size.scale()
    setspecies<-input$setspecies
    natureofobjects.list<-natureofobjects.list()
    # to correct the color for ggplot2
   # myvaluesx2<-myvaluesx[levels(as.factor(df.sub()$layer2)) %in% levels(as.factor(droplevels(df.sub2$layer2)))]
    
    assign("df.sub2",df.sub2,envir = .GlobalEnv)
    assign("setspecies",setspecies,envir = .GlobalEnv)
    assign("size.scale",size.scale,envir = .GlobalEnv)
    assign("natureofobjects.list",natureofobjects.list,envir = .GlobalEnv)
    assign("myvaluesx",myvaluesx,envir = .GlobalEnv)
    
    if (input$setspecies!="null"){
      df.sub2 <- df.sub2[df.sub2[,input$setspecies]%in% natureofobjects.list(), ]}
    
    df.sub2<-df.sub2 %>% 
      filter(.data[[setXX()]] >= input$xslider[1], .data[[setXX()]] <= input$xslider[2]) %>% 
      filter(.data[[setYY()]] >= input$yslider[1], .data[[setYY()]] <= input$yslider[2]) %>% 
      filter(.data[[setZZ()]] >= input$zslider[1], .data[[setZZ()]] <= input$zslider[2])
    
    
    liste.name3<-dplyr::intersect(natureofobjects.list,levels(as.factor(df.sub2[[setspecies]])))
    myvaluesx2<-myvaluesx[levels(as.factor(df.sub2[[setspecies]])) %in% liste.name3]
    myvaluesx2<-myvaluesx2[natureofobjects.list %in% liste.name3]
    #temp.data<-cbind.data.frame(liste.name3,myvaluesx2)
    #colnames(temp.data)<-c(input$setspecies,"temp.col")
    #data.graph3<-inner_join(data.graph2,temp.data, by=setspecies)
    #myvaluesx5<-myvaluesx2[liste.name3 %in% levels(as.factor(data.graph3[[setspecies]]))]

    list.parameter.info<-var.function(input$var1.simple.random,random=1)
    var<-list.parameter.info[[1]]
   
    var2<-list.parameter.info[[2]]      
    axis.var.name<-list.parameter.info[[3]]
    axis.var2.name<-list.parameter.info[[4]]
    Xtickmarks.size<-list.parameter.info[[5]]
    Ytickmarks.size<-list.parameter.info[[6]]
    Xminor.breaks<-list.parameter.info[[7]]
    Yminor.breaks<-list.parameter.info[[8]]
    assign("list.parameter.info",list.parameter.info,envir = .GlobalEnv)
    
    #shapeX<-df.sub2$shapeX
    #shape.level<-levels(as.factor(shapeX))
    #point.size3<-as.factor(df.sub2$point.size2)

    
    p <- ggplot2::ggplot()
    if (!is.null(orthofile)){
      
      p<-p + ggRGB(img = orthofile,
                   r = 1,
                   g = 2,
                   b = 3,
                   maxpixels =500000,
                   ggLayer = T)
    }   
    
    p<- p + ggplot2::geom_point(data = df.sub2,
                                aes(x = .data[[var]],
                                    y = .data[[var2]],
                                    col=factor(.data[[setspecies]])),
                                #,
                                size=size.scale
                                #shape=shapeX
    )    +
      ggplot2::coord_fixed(ratio.simple())
    
    if (input$var.rand.densi == "yes") {
     p<- p+stat_density_2d(geom = "polygon",aes(data = df.sub2,x = df.sub2[[var]],
                                                     y = df.sub2[[var2]],alpha = ..level.., fill = factor(df.sub2[[setspecies]])))
    }
    
    p<-p+ggplot2::scale_color_manual(values=myvaluesx2)+
      #ggplot2::scale_shape_manual(values=shape.level)+
      ggplot2::scale_size_manual(values=c(size.scale))+
      xlab(paste(axis.var.name))+ylab(paste(axis.var2.name))+
      do.call(themeforfigure.choice(), list()) +
      theme(axis.title.x = element_text(size=font_size()),
            axis.title.y = element_text(size=font_size()),
            axis.text.x = element_text(size=font_tick()),
            axis.text.y = element_text(size=font_tick()),
            legend.title = element_blank())+
      theme(legend.position='none')
    
    
    p<-p+scale_x_continuous(breaks=seq(floor(min(df.sub2[[var]])),max(df.sub2[[var]]),Xtickmarks.size), minor_breaks = seq(floor(min(df.sub2[[var]])),max(df.sub2[[var]]),Xminor.breaks))+
      scale_y_continuous(breaks=seq(floor(min(df.sub2[[var2]])),max(df.sub2[[var2]]),Ytickmarks.size), minor_breaks = seq(floor(min(df.sub2[[var2]])),max(df.sub2[[var2]]),Yminor.breaks))
    p   
    
  }) #end plot2D.react 

####### Plot - random coordinate - advanced 2D plot ----

output$plot2Dbox.random <- renderUI({
  plotlyOutput("sectionYplot.random", height = height.size())
})

output$sectionYplot.random <- renderPlotly({
  plot2D.react.random()
  session_store$plt2D.random<- plot2D.react.random()
})
plot2D.react.random<-reactive({ 

  height.size2<-height.size()
  width.size2 <- width.size()
  
  
  
  list.parameter.info<-var.function(input$var1.random,random=1)
  var<-list.parameter.info[[1]]
  var2<-list.parameter.info[[2]]      
  axis.var.name<-list.parameter.info[[3]]
  axis.var2.name<-list.parameter.info[[4]]
  Xtickmarks.size<-list.parameter.info[[5]]
  Ytickmarks.size<-list.parameter.info[[6]]
  Xminorbreaks<-list.parameter.info[[7]]
  Yminorbreaks<-list.parameter.info[[8]]

    # minor.grid.info<-minor.grid.info.function(df.sub2,var,var2,Xminorbreaks,Xtickmarks.size,Yminorbreaks,Ytickmarks.size)
    df.sub2<-data.column$data.graph.indiv.random
    myvaluesx<-unlist(myvaluesx())
    size.scale <- size.scale()
    setspecies<-input$setspecies
    natureofobjects.list<-natureofobjects.list()
    
    if (input$setspecies!="null"){
      df.sub2 <- df.sub2[df.sub2[,input$setspecies]%in% natureofobjects.list(), ]}
    
    df.sub2<-df.sub2 %>% 
      filter(.data[[setXX()]] >= input$xslider[1], .data[[setXX()]] <= input$xslider[2]) %>% 
      filter(.data[[setYY()]] >= input$yslider[1], .data[[setYY()]] <= input$yslider[2]) %>% 
      filter(.data[[setZZ()]] >= input$zslider[1], .data[[setZZ()]] <= input$zslider[2])
    
    liste.name3<-dplyr::intersect(natureofobjects.list,levels(as.factor(df.sub2[[setspecies]])))
    myvaluesx2<-myvaluesx[levels(as.factor(df.sub2[[setspecies]])) %in% liste.name3]
    myvaluesx2<-myvaluesx2[natureofobjects.list %in% liste.name3]
    
    
    df.sub2<-df.sub2 %>% 
      filter(.data[[setXX()]] >= input$xslider[1], .data[[setXX()]] <= input$xslider[2]) %>% 
      filter(.data[[setYY()]] >= input$yslider[1], .data[[setYY()]] <= input$yslider[2]) %>% 
      filter(.data[[setZZ()]] >= input$zslider[1], .data[[setZZ()]] <= input$zslider[2])

    
      p<- plot_ly(height = height.size(),
                  width = width.size())
      p<- add_trace(p, x = ~df.sub2[[var]], y = ~df.sub2[[var2]],
                    type="scatter",
                    color = ~df.sub2[[setspecies]],
                    colors = myvaluesx2,
                    #size  = ~df.sub2$point.size2,
                    sizes = c(size.scale),
                    mode   = 'markers',
                    fill = ~'',
                    #symbol = ~df.sub2$shapeX, 
                   # symbols = shape.level,
                    text=df.sub2[,setID()],                                   
                    hovertemplate = paste('<b>X</b>: %{x:.4}',
                                          '<br><b>Y</b>: %{y}',
                                          '<b>%{text}</b>'))
    
      Xtval<-seq(floor(min(df.sub2[[var]])),max(df.sub2[[var]]),Xminorbreaks)
      Xttxt <- rep("",length(Xtval)) 
      Xttxt[seq(1,length(Xtval),Xtickmarks.size)]<-as.character(Xtval)[seq(1,length(Xtval),Xtickmarks.size)]
      
      Ytval<-seq(floor(min(df.sub2[[var2]])),max(df.sub2[[var2]]), Yminorbreaks)
      Yttxt <- rep("",length(Ytval)) 
      Yttxt[seq(1,length(Ytval),Ytickmarks.size)]<-as.character(Ytval)[seq(1,length(Ytval),Ytickmarks.size)]
      
      
      p <-  p %>% layout(showlegend = legendplotlyfig(),
                         scene = list( aspectmode = "manual",
                                       aspectratio=list(x=ratiox(),y=ratioy()),
                                       autosize=FALSE),
                         xaxis = list(title = paste(axis.var.name),
                                      dtick = Xtickmarks.size, 
                                      tick0 = floor(min(df.sub2[[var]])), 
                                      #tickmode = "linear",
                                      tickvals=Xtval,
                                      ticktext=Xttxt,
                                      titlefont = list(size = font_size()), tickfont = list(size = font_tick())),
                         yaxis = list(title = paste(axis.var2.name),
                                      dtick = Ytickmarks.size,
                                      tick0 = floor(min(df.sub2[[var2]])),
                                      #tickmode = "linear",
                                      tickvals=Ytval,
                                      ticktext=Yttxt,
                                      titlefont = list(size = font_size()), tickfont = list(size = font_tick())),
                         
                         dragmode = "select")%>%
        event_register("plotly_selecting")
      
    p <-p %>%
      config(displaylogo = FALSE,
             modeBarButtonsToAdd = list(dl_button),
             toImageButtonOptions = list(
               format = "svg")
      )
    
 # }) #end isolate
  
}) #plot2D.react



##output comparison table 
output$radiolist1=renderUI({
  list_name<-c(colnames(df$df.data))

  shinyWidgets::pickerInput(
    inputId = "Id.list1",
    label = "Select/deselect all options", 
    choices = list_name,
    options = list(
      `actions-box` = TRUE), 
    multiple = TRUE
  )
  
})
output$radiolist2=renderUI({
  list_name<-c(colnames(df$df.species))
  shinyWidgets::pickerInput(
    inputId = "Id.list2",
    label = "Select/deselect all options", 
    choices = list_name,
     options = list(
       `actions-box` = TRUE), 
    multiple = TRUE
  )
  
})

  
output$tablecomp <-  DT::renderDataTable({
     req(!is.null(merging.was.done()))
   req(!is.null(setID()))
  req(!is.null(df$df.species))
   req(!is.null(df$df.data))
   req(!is.null(input$Id.list1))
   req(!is.null(input$Id.list2))
   list.id<-as.data.frame(unique(df$df[[setID()]]))
   colnames(df$df.species)[colnames(df$df.species)==input$setID3]<-setID()
   print(input$Id.list1)

   x<-as.data.frame(df$df.data[,c(setID(),input$Id.list1)])
   x2<-as.data.frame(df$df.species[,c(setID(),input$Id.list2)])


   colnames(list.id)<-setID()
   
   table.comp<-inner_join(list.id,x, by=setID())
   table.comp<-inner_join(table.comp,x2, by=setID()) ## prob table ID not unique. A FINII-----
   
   a<-input$Id.list1[1]
   b<-input$Id.list2[1]
   table.comp <- table.comp %>% mutate(match = ifelse(table.comp[[a]] != table.comp[[b]], 1, 0))
   

   
  data.df.tot2<-as.data.frame(table.comp)
    DT::datatable(
      data= data.df.tot2,
      extensions = 'Buttons', options = list(
        list(targets = 12, visible = FALSE),
        initComplete = htmlwidgets::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
          "}")
      ),
      filter = "top") %>% formatStyle(
        'match', 'match',
        color = styleInterval(c(0), c('black', 'black')),
        backgroundColor = styleEqual(c(0, 1), c('green', 'yellow'))
      )

})#end renderDataTable



##### Microfauna treatment ----
###### Basic data ----

  output$sum.species=renderUI({
    req(!is.null(fileisupload()))
    tagList(HTML(paste("Total number of different type of objects: ",number.total.of.species())))
  })
  output$sum.species2=renderUI({
    req(!is.null(fileisupload()))
    tagList(HTML(paste("Number total of species: ",number.total.of.species())))
  })
  
  output$sum.bucket=renderUI({
    req(!is.null(fileisupload()))
    aa<-df.sub()[,c(setID())]
    tagList(HTML(paste("Number of bucket selected: ",length(unique(aa)))))
  })
  
  # output$sum.bucket2=renderUI({
  #   req(!is.null(fileisupload()))
  #   aa<-df.sub()[,c(setID())]
  #   tagList(h4(style = "color: blue;",HTML(paste("Bioclim data"))))
  # })
  output$sum.remain=renderUI({
    req(!is.null(fileisupload()))
    req(!is.null(input$setnb))
    aa<-df$df
    tagList(HTML(paste("Number total of all remains: ",sum(aa[[input$setnb]]))))
  })
  output$sum.remain2=renderUI({
    req(!is.null(fileisupload()))
    req(!is.null(input$setnb))
    aa<-df.sub()
    tagList(HTML(paste("Number of remains selected: ",sum(aa[[input$setnb]]))))
  })  

  
###### rarefaction curves ----
# output$rarefactionplotui <- renderUI({
#     plotOutput("rarefactionplot", height = height.size(), width = width.size())
# })
#   
# output$rarefactionplot <- renderPlot({
#     tab.raref_fossil<-df.species.table()
#     rownames(tab.raref_fossil)<-tab.raref_fossil[,1]
#     tab.raref_fossil.2<-tab.raref_fossil[2:ncol(tab.raref_fossil)]
#     S <- specnumber(tab.raref_fossil.2)
#     number.species.per.levels(S)
#     if (input$Id075==FALSE){
#       raremax <- min(rowSums(tab.raref_fossil.2))
#       Srare <- rarefy(tab.raref_fossil.2, raremax)
#       plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
#       rarecurve(tab.raref_fossil.2, step = 1, sample = raremax, col = "blue", cex = 0.6)
#       #session_store$plotrarecurve <- recordPlot()                                      ## record this graph version is not working 
#     } else {
#       tab.raref_fossil3<-t(tab.raref_fossil.2)
#       x<-iNEXT(tab.raref_fossil3,q=0,datatype="abundance")
#       p<-ggiNEXT(x, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE) 
#       session_store$plotrarecurve <- p
#       p
#     }
#   })
#   
####### Export table nb species per levels for rarefaction curve ----
# output$table.species.perlevels <-  DT::renderDataTable({
#     req(!is.null(number.species.per.levels()))
#     data.df.tot2<-as.data.frame(number.species.per.levels())
#     DT::datatable(
#       data= data.df.tot2, 
#       extensions = 'Buttons', options = list(
#         initComplete = htmlwidgets::JS(
#           "function(settings, json) {",
#           paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
#           "}")
#       ))
#   })#end renderDataTable
#   
# #######  save rarefaction table ----
#   output$downloadData_rarefactiondata<- downloadHandler( 
#     filename = function() {
#       paste0(Sys.Date(),"rarefaction.data.table",".csv")
#     },
#     content = function(file) {
#       write.table(as.data.frame(number.species.per.levels()), file, row.names = FALSE, sep=";",dec=".") 
#     }
#   ) 
#   
# #######  save rarefaction.curve ----
#   output$downloadData_rarefactiongraph<- downloadHandler( 
#     filename = function(){
#       paste("rarefaction.curve - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
#     content = function(file) {
#       ggsave(session_store$plotrarecurve,filename=file, device = "pdf")
#     }
#   ) 

  
###### Bioclim methods  ----
#                                               # to fix : issue with data.species and bioclim data
# BCI_LVLn_of_siteS2<-reactiveVal(NULL)
# res_lda<-reactiveVal(NULL)
# 
# output$bioclim.react <-  DT::renderDataTable({  
#   tab.raref_fossil<-df.species.table()
#   rownames(tab.raref_fossil)<-tab.raref_fossil[,1]
#   data.df.tot2<-tab.raref_fossil[2:ncol(tab.raref_fossil)]
#   data.df.tot3<-as.data.frame(t(data.df.tot2))
#   data.df.tot3<-mutate_all(data.df.tot3, function(x) as.numeric(as.character(x)))
#   list.of.site<-vector("list", ncol(data.df.tot3))
# if(ncol(data.df.tot3)>1){
#     for (i in 1:ncol(data.df.tot3)) {
#       list.of.site[[i]]<-rownames(data.df.tot3[data.df.tot3[,i]!=0,])
#    } # creation of list for several cases
# } else {
#   
#   list.of.site[[1]]<-rownames(data.df.tot3)
# }
#   BCI_LVLn_of_siteS <- Func_BCI_Calcul(list.of.site, EUL = input$var.bioclim, verif = F)
#   names(BCI_LVLn_of_siteS)<-colnames(data.df.tot3)
#   BCI_LVLn_of_siteS2<-as.data.frame(do.call(rbind, BCI_LVLn_of_siteS))
#   BCI_LVLn_of_siteS2(BCI_LVLn_of_siteS2)
#   BCI_LVLn_of_siteS[sapply(BCI_LVLn_of_siteS, is.null)] <- NULL ## to remove null element
#   res_lda<-func_LDA(BCI_LVLn_of_siteS, quantiv = TRUE)
#   res_lda<-as.data.frame(do.call(rbind, res_lda))
#   res_lda(res_lda)
# DT::datatable(
#     data= as.data.frame(BCI_LVLn_of_siteS2), 
#     extensions = 'Buttons', options = list(
#       lengthMenu = list(c(5, 15,50,100, -1), c('5', '15','50','100', 'All')),
#       pageLength = 15,
#       initComplete = htmlwidgets::JS(
#         "function(settings, json) {",
#         paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
#         "}")
#     )) 
# }) 
#   
# output$bioclim.react2 <-  DT::renderDataTable({
#   req(!is.null(res_lda()))
#   res_lda<-as.data.frame(res_lda())
#   DT::datatable(
#     data= res_lda, 
#     extensions = 'Buttons', options = list(
#       lengthMenu = list(c(5, 15,50,100, -1), c('5', '15','50','100', 'All')),
#       pageLength = 15,
#       initComplete = htmlwidgets::JS(
#         "function(settings, json) {",
#         paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
#         "}"),
#       scrollX = TRUE,
#       fixedColumns = list(leftColumns = 10)
#     ))%>%
#     formatRound(columns= c(2,4:31),digits=3)
#   
# }) 
# 
# output$bioclim.names_noused=renderUI({
#   req(!is.null(fileisupload()))
#   data.sp.used<-levels(as.factor(df.sub()[[input$setspecies]]))
#   data_allspecies <- data_species_biozone                         #### faire attention au chargement de ce jeu de donnée avec le script palber
#   taxNamesTot <- as.character(unlist(data_allspecies["Taxon"]))
#   id_noused <- which(!is.element(data.sp.used, taxNamesTot))
#   names_noused <- data.sp.used[id_noused]
#   tagList(h5(style = "color: red;",HTML(paste(names_noused))))
#   
# })

######  Ratio graphs ---- 
# output$Ratio.data.graph <- renderUI({
#   plotOutput("Ratiodatagraph", height = height.size(), width = width.size())
# })
# 
# output$Ratiodatagraph <- renderPlot({
#   plot(Ratiodatagraph.plot())
#   session_store$Ratiodatagraph.plot<- Ratiodatagraph.plot()
# })   
# 
# Ratiodatagraph.plot<-reactive({
#   df.sub<-df.sub()
#   setlevels<-input$setlevels
#   setanat<-input$setanat
#   setnb<-input$setnb
#   digcol<-input$digcol
#   nameofdigelement<-input$nameofdigelement
#   data.df.calcul<-df.sub %>% group_by(.data[[setlevels]],.data[[setanat]])%>%
#     summarize(nb_total = sum(!!sym(setnb)))
#   myFormula <- as.formula(paste0(setlevels, " ~ ",setanat))
#   data.df.calcul.2<-reshape2::dcast(data.df.calcul, myFormula , fill = 0L)
#   
#   switch(input$select.ratio,
#          "1"={
# 
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mand"))) > 0 ,"No 'mand' or 'mandible' elements found in the database"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem'or 'femur' elements found in the database"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum'or 'humerus' elements found in the database"))
#            
#            axis.var.name<-"CRA/POSTCRA%"
#            ratio<-dplyr::select(data.df.calcul.2,starts_with("mand"))/(dplyr::select(data.df.calcul.2,starts_with("mand"))+dplyr::select(data.df.calcul.2,starts_with("fem"))+dplyr::select(data.df.calcul.2,starts_with("hum")))
#            #ratio<-data.df.calcul.2$mand/(data.df.calcul.2$mand+data.df.calcul.2$fem+data.df.calcul.2$hum)
#            #somme.ratio<-data.df.calcul.2$mand+data.df.calcul.2$fem+data.df.calcul.2$hum
#            somme.ratio<-dplyr::select(data.df.calcul.2,starts_with("mand"))+dplyr::select(data.df.calcul.2,starts_with("fem"))+dplyr::select(data.df.calcul.2,starts_with("hum"))
#            axis.var.name<-"ratio CRA/POSTCRA %"
#          },
#          "2"={
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
#            
#            axis.var.name<-"AN/PO%"
#            ratio<-dplyr::select(data.df.calcul.2,starts_with("hum"))/(dplyr::select(data.df.calcul.2,starts_with("fem"))+dplyr::select(data.df.calcul.2,starts_with("hum")))
#            somme.ratio<-dplyr::select(data.df.calcul.2,starts_with("fem"))+dplyr::select(data.df.calcul.2,starts_with("hum"))
#            axis.var.name<-"ratio AN/PO %"
#          },
#          "3"={
#            data.df.calcul.gh<-df.sub %>% group_by(.data[[setlevels]],.data[[setanat]],.data[[digcol]])%>%
#              summarize(nb_total = sum(!!sym(setnb)))
#            data.df.calcul.gh.2 <- data.df.calcul.gh %>% filter(.data[[setanat]] == nameofdigelement)
#            somme.ratio<-data.df.calcul.gh.2 %>% group_by(.data[[setlevels]])%>%
#              summarize(nb_total = sum(!!sym("nb_total")))
#            myFormula <- as.formula(paste0(setlevels, " ~ ",digcol))
#            data.df.calcul.2<-reshape2::dcast(data.df.calcul.gh.2, myFormula , fill = 0L)
#            print(data.df.calcul.2)
#            
#            ## ici faire calcul dig vs pas dig. 
#            ## + rajouter dans les choix digelement: fem,hum etc
#            # et dans digcol: des noms de colonne commencant par dig
#            ratio<-data.df.calcul.2$hum/(data.df.calcul.2$hum+data.df.calcul.2$fem) # A changer
#            somme.ratio<-data.df.calcul.2$hu
#        
#            
#            axis.var.name<- paste0("Proportion of digested ", nameofdigelement)
#          },
#          "4"={
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
#            fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
#            hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
#            rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
#            uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
#            tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
#            
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mand"))) > 0 ,"No 'mand' or 'mandible' elements found in the database"))
#            mand<-dplyr::select(data.df.calcul.2,starts_with("mand"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("max"))) > 0 ,"No 'max' or 'maxillae' elements found in the database"))
#            max<-dplyr::select(data.df.calcul.2,starts_with("max"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mol"))) > 0 ,"No 'mol' or 'molars' elements found in the database"))
#            mol<-dplyr::select(data.df.calcul.2,starts_with("mol"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("i"))) > 0 ,"No 'i' or 'incisor' elements found in the database"))
#            inc<-dplyr::select(data.df.calcul.2,starts_with("mol"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtc"))) > 0 ,"No 'mtc' or 'mtc' elements found in the database"))
#            mtc<-dplyr::select(data.df.calcul.2,starts_with("mtc"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtt"))) > 0 ,"No 'mtt' or 'mtt' elements found in the database"))
#            mtt<-dplyr::select(data.df.calcul.2,starts_with("mtt"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("sca"))) > 0 ,"No 'sca' or 'scapulae' elements found in the database"))
#            sca<-dplyr::select(data.df.calcul.2,starts_with("sca"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tar"))) > 0 ,"No 'tar' or 'tarsals' elements found in the database"))
#            tar<-dplyr::select(data.df.calcul.2,starts_with("tar"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("car"))) > 0 ,"No 'car' or 'carpals' elements found in the database"))
#            car<-dplyr::select(data.df.calcul.2,starts_with("car"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pha"))) > 0 ,"No 'pha' or 'phalanges' elements found in the database"))
#            pha<-dplyr::select(data.df.calcul.2,starts_with("pha"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pat"))) > 0 ,"No 'pat' or 'patellae' elements found in the database"))
#            pat<-dplyr::select(data.df.calcul.2,starts_with("pat"))
#            
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pat"))) > 0 ,"No 'bas' or 'bassin' elements found in the database"))
#            bas<-dplyr::select(data.df.calcul.2,starts_with("bas"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("vert"))) > 0 ,"No 'vert' or 'vertebrae' elements found in the database"))
#            vert<-dplyr::select(data.df.calcul.2,starts_with("vert"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rib"))) > 0 ,"No 'rib' or 'ribs' elements found in the database"))
#            rib<-dplyr::select(data.df.calcul.2,starts_with("rib"))
#            
#            ratio<-((rad+tib+fem+hum+uln+sca+pat+mtc+mtt+car+tar+pha+bas+vert+rib)*32)/(((rad+tib+fem+hum+uln+sca+pat+mtc+mtt+car+tar+pha+bas+vert+rib)*32)+((mand+max+mol+inc)*184))
#            somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc+sca+pat+mtc+mtt+car+tar+pha+bas+vert+rib
#            axis.var.name<-"ratio PCRT/CR%"
#          },
#          "5"={
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
#            fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
#            hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
#            rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
#            uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
#            tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
#            
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mand"))) > 0 ,"No 'mand' or 'mandible' elements found in the database"))
#            mand<-dplyr::select(data.df.calcul.2,starts_with("mand"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("max"))) > 0 ,"No 'max' or 'maxillae' elements found in the database"))
#            max<-dplyr::select(data.df.calcul.2,starts_with("max"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mol"))) > 0 ,"No 'mol' or 'molars' elements found in the database"))
#            mol<-dplyr::select(data.df.calcul.2,starts_with("mol"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("i"))) > 0 ,"No 'i' or 'incisor' elements found in the database"))
#            inc<-dplyr::select(data.df.calcul.2,starts_with("mol"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtc"))) > 0 ,"No 'mtc' or 'mtc' elements found in the database"))
#            mtc<-dplyr::select(data.df.calcul.2,starts_with("mtc"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtt"))) > 0 ,"No 'mtt' or 'mtt' elements found in the database"))
#            mtt<-dplyr::select(data.df.calcul.2,starts_with("mtt"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("sca"))) > 0 ,"No 'sca' or 'scapulae' elements found in the database"))
#            sca<-dplyr::select(data.df.calcul.2,starts_with("sca"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tar"))) > 0 ,"No 'tar' or 'tarsals' elements found in the database"))
#            tar<-dplyr::select(data.df.calcul.2,starts_with("tar"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("car"))) > 0 ,"No 'car' or 'carpals' elements found in the database"))
#            car<-dplyr::select(data.df.calcul.2,starts_with("car"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pha"))) > 0 ,"No 'pha' or 'phalanges' elements found in the database"))
#            pha<-dplyr::select(data.df.calcul.2,starts_with("pha"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pat"))) > 0 ,"No 'pat' or 'patellae' elements found in the database"))
#            pat<-dplyr::select(data.df.calcul.2,starts_with("pat"))
#            
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("bas"))) > 0 ,"No 'bas' or 'bassin' elements found in the database"))
#            bas<-dplyr::select(data.df.calcul.2,starts_with("bas"))
#            
#            ratio<-((rad+tib+fem+hum+uln+sca+pat+mtc+mtt+car+tar+pha+bas)*32)/(((rad+tib+fem+hum+uln+sca+pat+mtc+mtt+car+tar+pha+bas)*32)+((mand+max+mol+inc)*114))
#             somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc+sca+pat+mtc+mtt+car+tar+pha+bas
#            axis.var.name<-"ratio PCRAP/CR%"
#          },
#          "6"={
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
#            fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
#            hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
#            rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
#            uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
#            tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
#            
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mand"))) > 0 ,"No 'mand' or 'mandible' elements found in the database"))
#            mand<-dplyr::select(data.df.calcul.2,starts_with("mand"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("max"))) > 0 ,"No 'max' or 'maxillae' elements found in the database"))
#            max<-dplyr::select(data.df.calcul.2,starts_with("max"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mol"))) > 0 ,"No 'mol' or 'molars' elements found in the database"))
#            mol<-dplyr::select(data.df.calcul.2,starts_with("mol"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("i"))) > 0 ,"No 'i' or 'incisor' elements found in the database"))
#            inc<-dplyr::select(data.df.calcul.2,starts_with("mol"))
#            
#            
#            ratio<-((rad+tib+fem+hum+uln)*32)/(((rad+tib+fem+hum+uln)*32)+((mand+max+mol+inc)*10))
#            somme.ratio<-rad+tib+fem+hum+uln+mand+max+mol+inc
#            
#            axis.var.name<-"ratio PCRLB/CR%"
#          },
#          "7"={
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
#            fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
#            hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
#            rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
#            uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
#            tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtc"))) > 0 ,"No 'mtc' or 'mtc' elements found in the database"))
#            mtc<-dplyr::select(data.df.calcul.2,starts_with("mtc"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtt"))) > 0 ,"No 'mtt' or 'mtt' elements found in the database"))
#            mtt<-dplyr::select(data.df.calcul.2,starts_with("mtt"))
# 
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tar"))) > 0 ,"No 'tar' or 'tarsals' elements found in the database"))
#            tar<-dplyr::select(data.df.calcul.2,starts_with("tar"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("car"))) > 0 ,"No 'car' or 'carpals' elements found in the database"))
#            car<-dplyr::select(data.df.calcul.2,starts_with("car"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pha"))) > 0 ,"No 'pha' or 'phalanges' elements found in the database"))
#            pha<-dplyr::select(data.df.calcul.2,starts_with("pha"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("pat"))) > 0 ,"No 'pat' or 'patellae' elements found in the database"))
#            pat<-dplyr::select(data.df.calcul.2,starts_with("pat"))
#            
#            ratio<-(mtc+mtp+tar+pha+car)*12/(((mtc+mtt+tar+pha+car)*12)+(tib+rad+uln+hum+fem+pat)*18)
#            somme.ratio<-mtc+mtp+tar+pha+car+tib+rad+uln+fem+pat
#            axis.var.name<-"ratio AUT/ZE%"
#          },
#          "8"={
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
#            rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
#            uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
#            tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
#            fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
#            hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
#      
#            ratio<-((rad+tib+uln)*4)/((tib+rad+uln)*4+((hum+fem)*6))
#            somme.ratio<-rad+tib+uln+hum+fem
#            axis.var.name<-"ratio Z/E%"
#          },
#          "9"={
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("fem"))) > 0 ,"No 'fem' or 'femur' elements found in the database"))
#            fem<-dplyr::select(data.df.calcul.2,starts_with("fem"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("hum"))) > 0 ,"No 'hum' or 'humerus' elements found in the database"))
#            hum<-dplyr::select(data.df.calcul.2,starts_with("hum"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("sca"))) > 0 ,"No 'sca' or 'scapula' elements found in the database"))
#            sca<-dplyr::select(data.df.calcul.2,starts_with("sca"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("rad"))) > 0 ,"No 'rad' or 'radius' elements found in the database"))
#            rad<-dplyr::select(data.df.calcul.2,starts_with("rad"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("uln"))) > 0 ,"No 'uln' or 'ulna' elements found in the database"))
#            uln<-dplyr::select(data.df.calcul.2,starts_with("uln"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("tib"))) > 0 ,"No 'tib' or 'tibia' elements found in the database"))
#            tib<-dplyr::select(data.df.calcul.2,starts_with("tib"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtc"))) > 0 ,"No 'mtc' or 'mtc' elements found in the database"))
#            mtc<-dplyr::select(data.df.calcul.2,starts_with("mtc"))
#            validate(need(length(dplyr::select(data.df.calcul.2,starts_with("mtt"))) > 0 ,"No 'mtt' or 'mtt' elements found in the database"))
#            mtt<-dplyr::select(data.df.calcul.2,starts_with("mtt"))
#            
#            ratio<-(hum+sca+rad+mtc)*12/(((hum+sca+rad+uln+mtc)*12)+(fem+tib+mtt)*16)
#            somme.ratio<-hum+sca+rad+uln+mtc+fem+tib+mtt
#            axis.var.name<-"AN/PO%"
#            axis.var.name<-"ratio AN/PO%"
#          }
#   )
#   
#   f_vec <-Vectorize(WilsonBinCI, vectorize.args = c("n","p"), SIMPLIFY = FALSE)
# 
#   data.df.calcul.anpo<-matrix(unlist(f_vec(c(somme.ratio),c(ratio))),ncol=2, byrow=F)
# 
#   df.ratio<-cbind.data.frame(data.df.calcul.2[,setlevels],ratio,data.df.calcul.anpo)
# 
#   colnames(df.ratio)<-c(setlevels,"ratio","lower","upper")
# 
#   if (!is.null(factor.order.level.activation())){
#   df.ratio[[setlevels]]<-factor(df.ratio[[setlevels]], levels = factor.order.level())
#   }
#   
#   print(df.ratio)
#   p <- ggplot2::ggplot(df.ratio, 
#                        aes(x = .data[["ratio"]]*100, y = .data[[setlevels]], xmin = .data[["lower"]]*100, xmax = .data[["upper"]]*100))+ 
#     scale_x_continuous(limits=c(0,100))
#   p<-p+geom_pointrange()+
#     xlab(paste(axis.var.name))+ylab(paste(setlevels))+
#     do.call(themeforfigure.choice(), list()) +
#     theme(axis.title.x = element_text(size=font_size()),
#           axis.title.y = element_text(size=font_size()),
#           axis.text.x = element_text(size=font_tick()),
#           axis.text.y = element_text(size=font_tick()),
#           legend.title = element_blank())+
#     theme(legend.position=legendplotlyfig())
#   p
#   
#   
# }) 

# output$Ratio.data.list=renderUI({
#   req(!is.null(fileisupload()))
#   selectInput("select.ratio", label = h5("Select the ratio to plot"), 
#               choices = list("CRA/POSTCRA%" = 1, "AN/PO% (1)" = 2,"AN/PO% (2)" = 9,"AUT/ZE%"=7,"Z/E%" = 8, "PCRLB/CR%"=6, "PCRAP/CR%"=5,"PCRT/CR%"=4,"Proportion digested element" = 3 ), 
#               selected = 1)
# })


##### output.contents ----  
output$contents <- renderTable({
  req(!is.null(fileisupload()))
  req(!is.null(merging.was.done()))
  df.5<-df$df[1:5,2:9]
  return(df.5)
})

output$contents.species <- renderTable({
  req(!is.null(fileisupload.species()))
  isTruthy(df.sub())
  df.5<-df$df.species
  return(df.5[1:10,])
})

##### output Table  ----
output$table <-  DT::renderDataTable(    
  DT::datatable(
    df.sub()[,-c(1:4)], extensions = 'Buttons', options = list(
      lengthMenu = list(c(5, 15,50,100, -1), c('5', '15','50','100', 'All')),
      pageLength = 15,
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
        "}")
    ))  
)#end renderDataTable

output$table.species <-  DT::renderDataTable({
  data.df.tot2<-df.species.table()
  DT::datatable(
    data= data.df.tot2, 
    extensions = 'Buttons', options = list(
      lengthMenu = list(c(5, 15,50,100, -1), c('5', '15','50','100', 'All')),
      pageLength = 15,
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
        "}")
    ))
})#end renderDataTable

output$table.dec.species <-  DT::renderDataTable({
  data.df.tot2<-data.column$data.df.xy
  DT::datatable(
    data= data.df.tot2, 
    extensions = 'Buttons', options = list(
      lengthMenu = list(c(5, 15,50,100, -1), c('5', '15','50','100', 'All')),
      pageLength = 15,
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
        "}")
    ))
})#end renderDataTable



##### output Pivot table ----
# output$liste.summary=renderUI({
#   req(!is.null(fileisupload()))
#   checkboxGroupInput("listesum", h4("Variables for summary table"),
#                      choices = names(df$df)[c(3:ncol(df$df))])
# })

Pivotdatatable<-reactive({req(input$listesum)
  df.sub<-df.sub()
  liste.sum<-c(input$listesum,input$setnb) # creation d'une liste
  table_matos<-df.sub %>% group_by(across(liste.sum)) %>% summarize(n_row=n()    )
  colnames(table_matos)<-c(unlist(liste.sum),"n_row")
  table_matos$nb_pt_tot<-table_matos[[input$setnb]]*table_matos[["n_row"]]
  table_matos<-table_matos %>% dplyr::select (-input$setnb)
  table_matos})

output$summary <- renderTable({
  Pivotdatatable()
})


#### saving data----
options(shiny.usecairo=T)
#2D plot random coordinate
output$downloadData2D.random <- downloadHandler(
  filename = function() {
    paste("plot2D - ",paste(input$file1$name)," - ", Sys.Date(), ".html", sep="")
  },
  content = function(file) {
    htmlwidgets::saveWidget(as_widget(session_store$plt2D.random), file, selfcontained = TRUE)
  }
)
output$downloadData2D.simple.random <- downloadHandler(
  filename = function(){paste("plot2D - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
  content = function(file){
    ggsave(session_store$plt2D.simple.random,filename=file, device = "pdf")
  },
)

#2d plot
  output$downloadData2D <- downloadHandler(
    filename = function() {
      paste("plot2D - ",paste(input$file1$name)," - ", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      htmlwidgets::saveWidget(as_widget(session_store$plt2D), file, selfcontained = TRUE)
    }
  )
  output$downloadData2D.simple <- downloadHandler(
    filename = function(){paste("plot2D - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
    content = function(file){
      ggsave(session_store$plt2D.simple,filename=file, device = "pdf")
    },
  )
  output$downloadData2D.sw <- downloadHandler(
    filename = function(){paste("plot2D - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
    content = function(file){
      ggsave(session_store$plt2D.sw,filename=file, device = "pdf")
    },
  )
 
  
# 2d plot density  
  output$downloadDatadensity <- downloadHandler(
    filename = function(){paste("plotDensity - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
    content = function(file){
      ggsave(session_store$plotdensity,filename=file, device = "pdf")
    },
  )
  
  output$download_plot.histo.freq<- downloadHandler( 
    filename = function(){
      paste("Fig.histo.freq - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
    content = function(file) {
      ggsave(session_store$plt2D.histo.freq,filename=file, device = "pdf")
    }
  ) 
  # save spit abondance
  output$download_plot.abundance<- downloadHandler( 
    filename = function(){
      paste("plot - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
    content = function(file) {
      ggsave(session_store$plot2D.abondance,filename=file, device = "pdf")
    }
  )  
  
  
# raw table
  output$downloadData_rawdata<- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),paste(input$file1$name),".csv",sep="")
    },
    content = function(file) {
      write.table(df$df[,3:ncol(df$df)], file, row.names = FALSE, sep=";",dec=".")
    }
  )
  
# species table
  output$downloadData_speciesdata<- downloadHandler( 
    filename = function() {
      paste0(Sys.Date(),"_species.table",".csv")
    },
    content = function(file) {
      write.table(df.species.table(), file, row.names = FALSE, sep=";",dec=".") ## to MODIF !!!
    }
  ) 
  
  # pivot table
  output$downloadData_pivotdata<- downloadHandler( 
    filename = function() {
      paste0(Sys.Date(),"_pivot.table",".csv")
    },
    content = function(file) {
      write.table(Pivotdatatable(), file, row.names = FALSE, sep=";",dec=".")
    }
  )
# save color
  output$save.col<- downloadHandler( 
    filename = function() {
      paste0(Sys.Date(),"_save.col",".csv")
    },
    content = function(file) {
      write.table(save.col.react(), file, row.names = FALSE, sep=";",dec=".")
    }
  )
  output$save.col.fit<- downloadHandler( 
    filename = function() {
      paste0(Sys.Date(),"_save.col.refit",".csv")
    },
    content = function(file) {
      write.table(save.col.react.fit(), file, row.names = FALSE, sep=";",dec=".")
    }
  )
  
  
#### Palynograph ----
  factor.order.sq<-reactiveVal(NULL)

  observeEvent(input$sq.order, {
    factor.order.sq(input$sq.order)
  })
  
  res_lda2<-reactiveVal()
  res_lda3<-reactiveVal()
  output$squaretoselect <- renderUI({
    list_name<-input$localisation
    shinyWidgets::pickerInput(
      inputId = "Id.list.square",
      label = "Select the square", 
      choices = list_name,
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE
    )
    
  })
  
output$order.sq <- renderUI({
    req(!is.null(input$Id.list.square))
    shinyjqui::orderInput(inputId = "sq.order", label = "",
                          items = input$Id.list.square, item_class = 'info'
    )
  })

output$order.X <- renderUI({
  req(!is.null(input$Id.list.square))
  sliderInput("sliderX", label = h3("Slider Range"), min = 0, 
              max = 100, value = c(0.1, nnrow2()))
  
}) 
factor.sliderX<-reactiveVal()
observeEvent(input$sliderX, {
  factor.sliderX(input$sliderX)
})


output$selectspecies <- renderUI({ 
  
  list_name<-c(unique(natureofobjects.list()))
  shinyWidgets::pickerInput(
    inputId = "Id.list.species",
    label = "Select the object to show by point", 
    choices = list_name,
    options = list( 
      `actions-box` = TRUE), 
    multiple = TRUE
  )
}) 
output$densitytoselect2 <- renderUI({ 
  
  list_name<-c("none","rodent_teeth")
  shiny::selectInput(
    inputId = "Id.list.species.dens2",
    label = "Select the object to show by density", 
    choices = list_name,
    selected = "none"
  )
}) 

output$densitytoselect <- renderUI({ 
  
  list_name<-c(unique(natureofobjects.list()))
  list_name<-c(list_name)
  shinyWidgets::pickerInput(
    inputId = "Id.list.species.dens",
    label = "Select the object to show by density", 
    choices = list_name,
    selected = "none",
    options = list( 
      `actions-box` = TRUE), 
    multiple = TRUE
  )
}) 
list.dens.input<-reactiveVal(0)
observeEvent(input$Id.list.species.dens, {
  list.dens.input(length(input$Id.list.species.dens))
})
observeEvent(input$trackObjDens, {
  if (input$trackObjDens==1){
  list.dens.input(0)
    }
  
})


output$Palynograph.plot.output.simple <- renderUI({
  plotOutput("Palynograph.plot.output", height = height.size(), width = width.size())
})
datatest <- reactiveVal()
datatest3 <- reactiveVal()

observeEvent(input$tablepalyno2_cell_edit, {
  info <- input$tablepalyno2_cell_edit
  isolate({
  data <- datatest()
  true_row <- data[info$row + 1, "ID_table"] 
  
  row_index <- which(data$ID_table == true_row)
  if (length(row_index) == 1) {
    col_name <- names(data)[info$col +1 ]
    data[row_index, col_name] <- info$value
  }
  
   datatest(data)
  res_lda2(data)
  })
})


observeEvent(input$tablepalyno3_cell_edit, {
  info <- input$tablepalyno3_cell_edit
  isolate({
    data <- datatest3()
    true_row <- data[info$row + 1, "ID_table"] 
    
    row_index <- which(data$ID_table == true_row)
    if (length(row_index) == 1) {
      col_name <- names(data)[info$col +1 ]
      data[row_index, col_name] <- info$value
    }
    
    datatest3(data)
    res_lda3(data)
  })
})

output$tablepalyno2 <-  DT::renderDataTable({
  req(datatest())
  dat <- datatest()
  req(nrow(dat) > 0)

  DT::datatable(
    data = dat,
    editable = list(
      target = "cell",
      disable = list(columns = c(0,1))  # désactive édition sur ID et square
    ),
    #editable = TRUE,
     rownames = FALSE,  ##bug marche pas 
     selection = "none",  
     callback = callback2,  
    # extensions = 'Buttons',
    options = list(
      pageLength = 10,

      columnDefs = list(
        list(targets = c(2, 3), className = "editable"),
         list(targets = c(0), visible = FALSE)
      )
    )
  )   %>%
    DT::formatStyle(
      columns = "color",  
      target = "cell",
      backgroundColor = DT::styleEqual(
        unique(dat$color),
        unique(dat$color)
      )
    )
}, server = FALSE)
          

output$tablepalyno3 <-  DT::renderDataTable({
  req(!is.null(input$Id.list.square))
  req(datatest3())
  dat <- datatest3()
  req(nrow(dat) > 0)
  
  DT::datatable(
    data = dat,
    editable = list(
      target = "cell",
      disable = list(columns = c(0,1))  # désactive édition sur ID et square
    ),
    #editable = TRUE,
    rownames = FALSE,  ##bug marche pas 
    selection = "none",  
    callback = callback3,  
    # extensions = 'Buttons',
    options = list(
      pageLength = 10,
      
      columnDefs = list(
        list(targets = c(2, 3), className = "editable"),
        list(targets = c(0), visible = FALSE)
      )
    )
  )   %>%
    DT::formatStyle(
      columns = "color",  
      target = "cell",
      backgroundColor = DT::styleEqual(
        unique(dat$color),
        unique(dat$color)
      )
    )
}, server = FALSE)



observeEvent(c(input$Id.list.species,input$Id.list.square), {
  req(!is.null(Palynodatatable()))
   datatest(Palynodatatable())
  res_lda2(datatest())
})

observeEvent(c(input$Id.list.species.dens), {
  req(!is.null(Palynodatatable3()))
  datatest3(Palynodatatable3())
  res_lda3(datatest3())
})


Palynodatatable<-reactive({
  req(!is.null(input$Id.list.species))
  species<-input$Id.list.species
  shape<-rep("square",length(species))
  color<-rep("red",length(species))
  
  res_lda<-data.frame( ID_table = seq_along(species),name_species=species,shape=shape,color=color,
                       stringsAsFactors = FALSE)
  return(res_lda)
  
})
Palynodatatable3<-reactive({
  req(!is.null(input$Id.list.species.dens))
  
  species<-input$Id.list.species.dens
  shape<-rep("dens",length(species))
  color<-rep("blue",length(species))
  
  res_lda3<-data.frame( ID_table = seq_along(species),name_species=species,shape=shape,color=color,
                       stringsAsFactors = FALSE)
  return(res_lda3)
  
})

nnrow<-reactiveVal(1)
nnrow2<-reactiveVal(1)

  Palynograph.plot<-reactive({
    print("aaazzzaaaa")
 
    req(!is.null(input$Id.list.square))
    req(!is.null(input$Id.list.species))
    req(!is.null(res_lda2()))
    res_lda<-res_lda2()
    res_lda<-res_lda[,-c(1)]
    list.parameter.info<-var.function("yz")
    var<-list.parameter.info[[1]]
    var2<-list.parameter.info[[2]]      
    axis.var.name<-list.parameter.info[[3]]
    axis.var2.name<-list.parameter.info[[4]]
    Xtickmarks.size<-list.parameter.info[[5]]
    Ytickmarks.size<-list.parameter.info[[6]]
    Xminor.breaks<-list.parameter.info[[7]]
    Yminor.breaks<-list.parameter.info[[8]]
    
    res_ldaX<-data.column$data.graph.indiv.square
    res_ldaX<-subset(res_ldaX, subset= name_square %in% input$Id.list.square)

    res_ldaX2<-inner_join(res_ldaX,res_lda)
    shiny::validate(need(nrow(res_ldaX2)>0, "There is no such species in the dataset selected. Try with another species."))
    
    
    assign("df1",res_lda,envir=globalenv())
    assign("df2",res_ldaX,envir=globalenv())
    assign("df3",data.column$data.graph.indiv.square,envir=globalenv())
    assign("df4",res_ldaX2,envir=globalenv())
   
    df.sub.g<-res_ldaX2

    assign("df.sub.g",df.sub.g,envir=globalenv())

    shiny::validate(need(nrow(df.sub.g)>0, "There is no such species in the dataset selected. Try with another species."))
    
   ### density part ##a finir ici pour inclure density. ---- 
    
    res_lda_dens<-data.column$data.graph.indiv.square2
    res_lda_dens<-subset(res_lda_dens, subset= name_square %in% input$Id.list.square)
    res_lda3<-res_lda3()
   densityorno<-0
    assign("res_lda3",res_lda3,envir=globalenv())
    if (input$trackObjDens ==1 && input$Id.list.species.dens2!="none" ){
        densityorno<-1
        res_lda_dens<-subset(res_lda_dens, subset= name_anat %in% c("m1sup","mol",
                                                                    "m2sup","incisor",
                                                                    "m3sup","m2inf",
                                                                    "m3inf","mand"
                                                                    
                                                                    ))
        res_lda_dens<-subset(res_lda_dens, subset=name_taxa %in% c("rodentia"))
        
  
        assign("res_lda_dens",res_lda_dens,envir=globalenv()) 
      }

    if (input$trackObjDens ==2 & list.dens.input()!=0 ){
        densityorno<-2
        # res_lda_dens <- df.sub.g %>% 
        res_lda_dens <- res_ldaX %>%          #test
          left_join(res_lda3 %>% dplyr::select(name_species,shape_new = shape, color_new = color),
                    by = "name_species") %>%
                   mutate(
            shape = shape_new,
            color = color_new,
            name_taxa=name_species
          ) %>%
          dplyr::select(-shape_new, -color_new)
        

        res_lda_dens<-subset(res_lda_dens, subset=name_species %in% as.vector(res_lda3[[2]]))
        
        res_lda_dens <- res_lda_dens %>%
          tidyr::uncount(weights = nb_total)

       
        
        }

      
      
    ###
    
    Lcombi <- lapply(df.sub.g$nb_total, function(a){1:a} )
    names(Lcombi)<-df.sub.g$name_species
    liste.names<-df.sub.g$ID
    fddddd<-cbind.data.frame(df.sub.g$ID,df.sub.g$nb_total)
    taa<-as.data.frame(matrix(nrow = sum(df.sub.g$nb_total),ncol=ncol(df.sub.g)))
    colnames(taa)<- colnames(df.sub.g)
    
    
    p<-1
    for (i in 1:length(Lcombi)){
      k  <-length(Lcombi[[i]])+p
      k2<-length(Lcombi[[i]])
      
      for(kk in 1:k2){
        taa[kk+p,]<-df.sub.g[i,]
      }
      p<-k
      
    }
    taa<-taa[-c(1),]
    
    colnames(fddddd)<-c("V1","V2")
  
    aa<-NULL
    ab<-NULL
    un<-unique(liste.names)
    for (i in 1:unique(length(liste.names))){
      ab<-subset(fddddd, subset=V1 %in% un[i])
      
      v3<-cumsum(as.numeric(ab$V2)+1)
      ab$v3<- v3
      aa<-rbind.data.frame(aa,ab)  
    }
    fddddd<-aa

    vX<-cumsum(as.numeric(fddddd$V2)+1)
    
    fddddd$v4<-fddddd$v3-as.numeric(fddddd$V2)
    
    temp<-cbind.data.frame(df.sub.g,fddddd,vX)
    temp$line<-temp$vX-temp$nb_total
    colnames(temp)<-c("ID","name_species","name_square","X","Y","Z","nb_total","shape","color","ID2","nb_total2","cumsum","dep.per.ID","end.per.ID","line")
    taa2<-matrix(nrow=nrow(taa),ncol=ncol(temp))
    taa2<-as.data.frame(taa2)
    colnames(taa2)<-colnames(temp)
    for (i in 1:nrow(temp)){
      val.dep<-temp[i,"line"]
      nb.val<-temp[i,"nb_total2"]
      x<-temp[i,"dep.per.ID"]
      for (ii in 1:nb.val){
        aa <-val.dep+ii-1
        
        taa2[c(aa),]<-temp[i,]
        taa2[c(aa),"dep.per.ID"]<-x
        x<-x+1
      }
    }

    data.swarn<-taa2
    data.swarn<-na.omit(data.swarn)
    assign("df6",data.swarn,envir=globalenv())
    
 ####

    if (!is.null(factor.order.sq())){
      print((unique(data.swarn[["name_square"]])))
      if(length(unique(data.swarn[["name_square"]]))==length(factor.order.sq())){
      data.swarn[["name_square"]]<-factor(data.swarn[["name_square"]], levels = factor.order.sq()) ### bug si rajoute un carré apres définition factor.order.sq. a améliorer ----
    }
    }
    data.swarn$name_square <- factor(data.swarn$name_square,exclude=NULL)
    
    
    lvl<-levels(as.factor(data.swarn$name_square))

   list.data.frame<-list()
   list.data.frame.dens<-list()
     for (i in 1:length(lvl)){
    list.data.frame[[i]] <-base::subset(data.swarn,name_square==lvl[[i]])
    list.data.frame.dens[[i]]<-base::subset(res_lda_dens,name_square==lvl[[i]])
    names(list.data.frame)[i]<-lvl[[i]]
    names(list.data.frame.dens)[i]<-lvl[[i]]
    name_square2<-lvl[[i]]
    
    if (nrow(list.data.frame[[i]])>nnrow()){
      nnrow(nrow(list.data.frame[[i]]))
      p <- ggplot(list.data.frame[[i]], aes(y = Z, fill = name_species)) +
        geom_density()
      dens_data <- ggplot_build(p)$data[[1]]
      nnrow2(max(range(dens_data$x, na.rm = TRUE)))
    }
     }
  
   
####    
    axis.var2.name<-"Z (mm)"
    size.scale <- size.scale()
    colorscale<-as.vector(res_lda[[3]])
    lvl.especes<-as.vector(res_lda[[1]])
    shape.especes<-as.vector(res_lda[[2]])
    shape.especes<-gsub("triangle",24,shape.especes)
    shape.especes<-gsub("square",22,shape.especes)
    shape.especes<-gsub("circle",21,shape.especes)
    shape.especes<-gsub("diamond",23,shape.especes)
    shape.especes<-as.numeric(shape.especes)
    
  
    combined<-lapply(
      names(list.data.frame),
      function(key2){
        colorscale2<-colorscale
        lvl.especes2<-lvl.especes
        shape.especes2<-shape.especes
        data2<-list.data.frame[names(list.data.frame)==key2]
        name_square22<-names(list.data.frame)[key2]
        
        res_lda_dens_updated<-list.data.frame.dens[names(list.data.frame)==key2]
        print(res_lda_dens_updated)
        assign("res_lda_dens_updated2",res_lda_dens_updated,envir=globalenv())
        assign("list.data.frame.dens",list.data.frame.dens,envir=globalenv())
        res_lda_dens_updated<-res_lda_dens_updated[[1]]
        lvl.especes3<-as.vector(res_lda3[[2]])
        colorscale3<-as.vector(res_lda3[[4]])

      ####  

      x.ax<-names(data2)
      data2<-data2[[1]]
  
      if(nrow(data2)==0){ 
          next()} else{
            
            if(length(setdiff(res_lda$name_species, data2$name_species))>0){
              indice<-which(res_lda$name_species %in% setdiff(res_lda$name_species, data2$name_species))
              colorscale2<-colorscale2[-c(indice)]
              lvl.especes2<-lvl.especes2[-c(indice)]
              shape.especes2<-shape.especes2[-c(indice)]
            }

    size.scale<-rep(size.scale,nrow(data2))
    
## pour density
    lim.X<-nnrow2() 
    if (!is.null(factor.sliderX())){
      lim.X<-factor.sliderX()
    }
    assign("res_lda_dens_updated",res_lda_dens_updated,envir = globalenv())
    assign("data2",data2,envir = globalenv())
    assign("lvl.especes2",lvl.especes2,envir = globalenv())
    assign("colorscale2",colorscale2,envir = globalenv())
    assign("nnrow",nnrow(),envir = globalenv())
    assign("lvl.especes3",lvl.especes3,envir = globalenv())
    assign("colorscale3",colorscale3,envir = globalenv())
    
     p <-ggplot2::ggplot()
    if (densityorno==1){
      # if (input$trackObjDens==2){

        
    p<-p+ ggplot2::geom_density(data=res_lda_dens_updated,mapping=aes(y=.data[["Z"]], 
                                                               fill=.data[["name_taxa"]], alpha=.5))+ 
                                  # scale_fill_manual(
                                  #   breaks= lvl.especes3,
                                  #                   values=colorscale3)+
      scale_fill_manual(values=c('lightblue'))+
                                 scale_x_reverse() +ggnewscale::new_scale_fill() 
    
    }
    
    if (densityorno==2){
        p<-p+ ggplot2::geom_density(data=res_lda_dens_updated,mapping=aes(y=.data[["Z"]], 
                                                                        fill=.data[["name_species"]]))+ 
        scale_fill_manual(
          breaks= lvl.especes3,
                          values=colorscale3)+
        # scale_fill_manual(alpha(values=c('lightblue'),0.75)+
                            scale_x_reverse() +ggnewscale::new_scale_fill() 
                          
    }   
      
    
    p<-p  +ggplot2::geom_point(data=data2,aes(x=dep.per.ID,y=Z, fill=factor(name_species),
                     size=factor(name_species),
                     shape=factor(name_species)
                                     )
                 )+ggplot2::scale_fill_manual(breaks= lvl.especes2,
                                              values=colorscale2)    +  scale_x_reverse() 
      
 
    p<-p+
       ggplot2::scale_size_manual(values=c(size.scale))+
       ggplot2::scale_shape_manual(values=shape.especes2)+
       xlab(paste(x.ax))+
       do.call(themeforfigure.choice(), list()) +
      theme(axis.title.x = element_text(size=font_size()),
            axis.text.x = element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y = element_text(size=font_tick()),
            legend.title = element_blank())

    p<-p+scale_y_continuous(limits=c(floor(min(data.swarn[["Z"]])),ceiling(max(data.swarn[["Z"]]))), 
                         breaks=seq(floor(min(data.swarn[["Z"]])),max(data.swarn[["Z"]]),Ytickmarks.size), 
                         minor_breaks = seq(floor(min(data.swarn[["Z"]])),max(data.swarn[["Z"]]),Yminor.breaks))+
      scale_x_continuous(limits=c(0,lim.X))

    # ggsave(filename = paste0("plots/", key, ".jpg"))

    p
          }
  })#end of lapply  
   
     # wrap_plots(combined) & scale_y_continuous(breaks=seq(floor(min(data.swarn[["Z"]])),max(data.swarn[["Z"]]),Ytickmarks.size), minor_breaks = seq(floor(min(data.swarn[["Z"]])),max(data.swarn[["Z"]]),Yminor.breaks))

    figure <- ggarrange(plotlist=combined,
                        ncol = length(names(list.data.frame)), nrow = 1, 
                        common.legend = TRUE,legend = input$optioninfosfigplotly,
                        align="v")
    figure

    
  }) # end 
  
  output$Palynograph.plot.output <- renderPlot({
    req(!is.null(input$Id.list.square))
    req(!is.null(input$Id.list.species))
    req(!is.null(res_lda2()))
    plot(Palynograph.plot())
    session_store$Palynograph.plot<- Palynograph.plot()
  })
  
  
  
  
#### button example of Paleolithique site -- to finish at the end ----
  observeEvent(input$button_example, {
    updateTabsetPanel(session, "mainpanel",
                      selected = "tab1")
    path <- paste0(tempdir(), "/data.df.ferrassie.csv")
    write.csv2(data.df.xy2, path)
    input_file_xyzspecies.name("data.df.ferrassie.csv")
    input_file_xyzspecies.datapath(path)
    getdata.launch.xyzspecies(1)
  })

#### Import data_species_biozone et bioclimatic_spectra_and_climate data -- to finish at the end ----
# data_species_biozone<-reactiveVal(NULL)
# bioclimatic_spectra_and_climate<-reactiveVal(NULL)
# observeEvent(merging.was.done(), {
#   file.fit <- SEAHORS.MICRO::data_species_biozone
#   data_species_biozone(file.fit)
#   file.fit <- SEAHORS.MICRO::bioclimatic_spectra_and_climate
#   bioclimatic_spectra_and_climate(file.fit)
#   print(bioclimatic_spectra_and_climate())
# })
### save palynograph
  
  output$downloadpalynograph <- downloadHandler(
    filename = function(){paste("plotDensity - ",paste(input$file1$name)," - ", Sys.Date(), '.pdf', sep = '')},
    content = function(file){
      ggsave(session_store$Palynograph.plot,filename=file, device = "pdf")
    },
  )

} # end server

