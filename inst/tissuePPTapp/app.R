library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(openxlsx)
library(gdata)
library(ggplot2)
library(DT)
library(data.table)
library(patchwork)
library(ggsci)
library(ggrepel)
library(pheatmap)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(stringr)
library(scales)
library(psych)
library(ggpubr)
colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))[-1]
#
ui<-renderUI(
  fluidPage(
    style="min-width:1400px;",
    title="tissuePPT",
    shinyjs::useShinyjs(),
    uiOutput("liulabui"),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
        tags$script(type="text/javascript", src = "busy.js"),
        #tags$style(type="text/css","#methodids{margin-left:2%;margin-right:2%}"),
        tags$style(type="text/css", "
                   #loadmessage {
                   position: fixed;
                   top: 0px;
                   left: 0px;
                   width: 100%;
                   height:100%;
                   padding: 250px 0px 5px 0px;
                   text-align: center;
                   font-weight: bold;
                   font-size: 100px;
                   color: #000000;
                   background-color: #D6D9E4;
                   opacity:0.6;
                   z-index: 105;
                   }
                   "),
        tags$script('
                    var dimension = [0, 0];
                    $(document).on("shiny:connected", function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    $(window).resize(function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    '),
        tags$style(type="text/css", "
                   #tooltip {
                   position: absolute;
                   border: 1px solid #333;
                   background: #fff;
                   padding: 1px;
                   color: #333;
                   display: block;
                   width:300px;
                   z-index:5;
                   }
                   "),#F5F5DC
        tags$style(type="text/css", "
                   #tooltip2 {
                   position: absolute;
                   border: 1px solid #333;
                   background: #fff;
                   padding: 1px;
                   color: #333;
                   display: block;
                   width:300px;
                   z-index:5;
                   }
                   "),
        tags$style(type="text/css", "
                   #tooltip3 {
                   position: absolute;
                   border: 1px solid #333;
                   background: #fff;
                   padding: 1px;
                   color: #333;
                   display: block;
                   width:300px;
                   z-index:5;
                   }
                   "),
        tags$style(type="text/css", "
                   #tooltip4 {
                   position: absolute;
                   border: 1px solid #333;
                   background: #fff;
                   padding: 1px;
                   color: #333;
                   display: block;
                   width:300px;
                   z-index:5;
                   }
                   "),
        tags$style(type="text/css", "
                   #tooltip5 {
                   position: absolute;
                   border: 1px solid #333;
                   background: #fff;
                   padding: 1px;
                   color: #333;
                   display: block;
                   width:300px;
                   z-index:5;
                   }
                   "),
        tags$style(HTML("
    .radio {
      margin-bottom: 21px; /* Increase the space between the radio button choices */
    }
  "))
      )
    ),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      id="maintab",
      tabPanel(
        "Home",
        uiOutput("welcomeui"),
        icon = icon("home")
      ),
      navbarMenu(
        "Analyses",
        tabPanel(
          "Heat-circle (HC) plot across tissues",
          value = "module1panel",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              h3(
                "Heat-circle (HC) plot across tissues",
                tags$span(
                  id = 'span1',
                  `data-toggle` = "tooltip",
                  title = '
                HC plot supports a synchronized overview of protein abundance (PA, in terms of iBAQ values derived from DIA-MS) and protein lifetime (PT, determined by pulsed SILAC labeling) for individual proteins or protein sets.
                ',
                  tags$span(class = "glyphicon glyphicon-question-sign")
                )
              ),
              tags$hr(style="border-color: grey;"),
              radioButtons(
                "loaddatatype",
                label = "",
                choices = list("1. Paste"=2,"2. Upload" = 1,"3. Example data"=3),
                selected = 2,
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.loaddatatype==1",
                fileInput('heatmapfile1', h5('Please import your data:'),accept=c('.csv/.txt'))
              ),
              conditionalPanel(
                condition = "input.loaddatatype==2",
                textAreaInput("ID_zhantie",label = h5("Please Paste gene names/UniProt IDs here："),value="",height ="100px")
              ),
              conditionalPanel(
                condition = "input.loaddatatype==3",
                downloadButton("loaddatadownload1","Download example data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              ),
              hr(),
              #selectInput("heatmapdatatypex",h5("1.1. Select data type:"),choices = c("mRNA","Protein","Phospho")),
              #checkboxInput('RelativeiBAQif', '4. Log2(ppm) or not?', FALSE),
              #radioButtons(
              #  "RelativeiBAQif",
              #  label = NULL,
              #  choices = list("4. PA: Log2(riBAQ in ppm)"=1,
              #                 "5. PA: Log2(iBAQ)" = 2,
              #                 "6. Log2(MS-intensity)"=3)
              #),
              radioButtons(
                "RelativeiBAQif",
                label = NULL,
                choiceNames = list(
                  HTML("4. PA: Log<sub>2</sub>(riBAQ in ppm)"),
                  HTML("5. PA: Log<sub>2</sub>(iBAQ)"),
                  HTML("6. Log<sub>2</sub>(MS-intensity)")
                ),
                choiceValues = list(1, 2, 3)
              ),
              bsTooltip("RelativeiBAQif",'ppm means the relative contribution of iBAQ values to the total protein amount.',
                        placement = "bottom",options = list(container = "body")),
              checkboxInput('Proteinspecificif', '7. PT rank percentage in each tissue?', FALSE),#rank normalization
              conditionalPanel(
                condition = "input.Proteinspecificif==true",
                checkboxInput('Proteinrowscaleif', '7.1. Row scaled across tissues?', FALSE)
              ),
              checkboxInput('heatmapheightif','8. Change figure height?', FALSE),
              conditionalPanel(
                condition = "input.heatmapheightif==true",
                numericInput("heatmapheightx",h5("8.1. Figure height:"),550)
              ),
              #downloadButton("silacprodatadl","Download table")
              div(
                style="margin-top:20px;",
                downloadButton("heatmapfiguredl","Download figure")
              )
            ),
            mainPanel(
              width = 9,
              radioButtons(
                "heatmaprestype",
                label = "",
                choices = list("Uploaded gene names/Uniprot IDs" = 1,"Expression data"=2,"HC plot"=3),
                selected = 1,
                inline = TRUE
              ),
              hr(),
              conditionalPanel(
                condition = "input.heatmaprestype==1",
                dataTableOutput("uploadgenestb")
              ),
              conditionalPanel(
                condition = "input.heatmaprestype==2",
                conditionalPanel(
                  condition = "input.RelativeiBAQif==1",
                  h4("A. Protein relative iBAQ (ppm) table:")
                ),
                conditionalPanel(
                  condition = "input.RelativeiBAQif==2",
                  h4("A. Protein iBAQ table:")
                ),
                conditionalPanel(
                  condition = "input.RelativeiBAQif==3",
                  h4("A. Protein MS-intensity table:")
                ),
                downloadButton("heatmapibaqtabledl","Download table"),
                dataTableOutput("heatmapibaqtable"),
                conditionalPanel(
                  condition = "input.Proteinspecificif==true",
                  conditionalPanel(
                    condition = "input.Proteinrowscaleif==false",
                    h4("B. Protein lifetime rank percentage table:")
                  ),
                  conditionalPanel(
                    condition = "input.Proteinrowscaleif==true",
                    h4("B. Row scaled protein lifetime rank percentage table:")
                  )
                ),
                conditionalPanel(
                  condition = "input.Proteinspecificif==false",
                  h4("B. Protein lifetime table:")
                ),
                downloadButton("heatmaplifetabledl","Download table"),
                dataTableOutput("heatmaplifetable")
              ),
              conditionalPanel(
                condition = "input.heatmaprestype==3",
                plotOutput("heatmapplot",width="100%")
              )
            )
          )
        ),
        tabPanel(
          "Heatmap analysis for protein sets",
          value = "module2panel",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              h3(
                "Heatmap analysis for protein sets",
                tags$span(
                  id = 'span2',
                  `data-toggle` = "tooltip2",
                  title = '
                Heatmap analysis supports extraction and visualization of protein abundance (DIA result), protein lifetime, P-site abundance (DIA result), and P-site lifetime for a given protein set.
                ',
                  tags$span(class = "glyphicon glyphicon-question-sign")
                )
              ),
              tags$hr(style="border-color: grey;"),
              radioButtons(
                "loaddatatypex2",
                label = "",
                choices = list("1. Paste"=2,"2. Upload" = 1,"3. Example data"=3),
                selected = 2,
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.loaddatatypex2==1",
                fileInput('heatmapfile1x2', h5('Please import your data:'),accept=c('.csv/.txt'))
              ),
              conditionalPanel(
                condition = "input.loaddatatypex2==2",
                textAreaInput("ID_zhantiex2",label = h5("Please Paste gene names/UniProt IDs here："),value="",height ="100px")
              ),
              conditionalPanel(
                condition = "input.loaddatatypex2==3",
                downloadButton("loaddatadownload1x2","Download example data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              ),
              hr(),
              #selectInput("heatmapdatatypex",h5("1.1. Select data type:"),choices = c("mRNA","Protein","Phospho")),
              numericInput("heatmapheightxx2",h5("Figure Height:"),1200),
              #downloadButton("silacprodatadl","Download table")
              downloadButton("heatmapfigurex2dl","Download figure")
            ),
            mainPanel(
              width = 9,
              radioButtons(
                "heatmaprestypex2",
                label = "",
                choices = list("Uploaded gene names/Uniprot IDs" = 1,"Expression data"=2,"Heatmap"=3),
                selected = 1,
                inline = TRUE
              ),
              hr(),
              conditionalPanel(
                condition = "input.heatmaprestypex2==1",
                dataTableOutput("uploadgenestbx2")
              ),
              conditionalPanel(
                condition = "input.heatmaprestypex2==2",
                h4("A. Protein abundance table:"),
                downloadButton("heatmapibaqtablex2dl","Download table"),
                dataTableOutput("heatmapibaqtablex2"),
                h4("B. Protein lifetime table:"),
                downloadButton("heatmaplifetablex2dl","Download table"),
                dataTableOutput("heatmaplifetablex2"),
                h4("C. Phospho abundance table:"),
                downloadButton("phosheatmapibaqtablex2dl","Download table"),
                dataTableOutput("phosheatmapibaqtablex2"),
                h4("D. Phospho lifetime table:"),
                downloadButton("phosheatmaplifetablex2dl","Download table"),
                dataTableOutput("phosheatmaplifetablex2")
              ),
              conditionalPanel(
                condition = "input.heatmaprestypex2==3",
                plotOutput("heatmapplotx2")
              )
            )
          )
        ),
        tabPanel(
          "Protein-specific barplots",
          value = "module3panel",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              h3(
                "Protein-specific barplots",
                tags$span(
                  id = 'span3',
                  `data-toggle` = "tooltip3",
                  title = '
                Protein-specific barplot supports visualization of protein abundance, protein lifetime, P-site abundance, and P-site lifetime for a given protein.
                ',
                  tags$span(class = "glyphicon glyphicon-question-sign")
                )
              ),
              tags$hr(style="border-color: grey;"),
              selectInput("abunbarplottype",h5("1. Select data type:"),
                          choices = c("Protein abundance"=1,"Protein lifetime"=2,"Phospho abundance"=3,"Phospho lifetime"=4)),
              numericInput("abunbarheightx",h5("2. Figure Height:"),900),
              hr(),
              downloadButton("figuredl","Download figure"),
              downloadButton("abunbarplotdatadl","Download data")
            ),
            mainPanel(
              width = 9,
              fluidRow(
                column(
                  width = 7,
                  div(
                    style="text-align:center;margin-left:250px;margin-top:30px;",
                    textInput("proinputids","Please enter the target Mouse gene name or gene ID",value="",width = '600px', placeholder = 'e.g. Snca or O55042')
                  )
                ),
                column(
                  width = 5,
                  actionButton("proinputids_btn","Search",width='100px',icon("paper-plane"),
                               style="margin-top:55px;text-align:left;margin-right:0px;color: #fff; background-color: #0072B2; border-color: #0072B2;")
                )
              ),
              hr(),
              plotOutput("proinputidsplot")
            )
          )
        ),
        tabPanel(
          "Correlation analysis: Individual protein",
          value = "module4panel",
          sidebarLayout(
            sidebarPanel(
              width = 2,
              h3(
                "Correlation analysis: Individual protein",
                tags$span(
                  id = 'span4',
                  `data-toggle` = "tooltip4",
                  title = '
                Correlation analysis supports discovery of relationship and dependency between protein abundance, protein lifetime, P-site abundance, and P-site lifetime for a given protein.
                ',
                  tags$span(class = "glyphicon glyphicon-question-sign")
                )
              ),
              tags$hr(style="border-color: grey;"),
              #checkboxInput("corlog2if","2.1. Log2 or not?",TRUE),
              selectInput("abunbarplottypexaxis",h5("1. Select data type for x axis:"),
                          choices = c("Protein abundance"=1,"Protein lifetime"=2,"Phospho abundance"=3,"Phospho lifetime"=4)),
              selectInput("abunbarplottypeyaxis",h5("2. Select data type for y axis:"),
                          choices = c("Protein lifetime"=1,"Phospho abundance"=2,"Phospho lifetime"=3,"Protein abundance"=4)),
              textInput("corpointcol",h5("3. Point color:"),value = "#4575B4"),
              textInput("corlinecol",h5("4. Line color:"),value = "#74ADD1"),
              numericInput("corheightx",h5("5. Each sub figure height:"),350)#,
              #numericInput("corwidthx",h5("1.5. Figure Width:"),1000)#,
              #hr(),
              #downloadButton("phosdatadl","Download table")
              #downloadButton("corfiguredl","Download figure")
            ),
            mainPanel(
              width = 10,
              fluidRow(
                column(
                  width = 7,
                  div(
                    style="text-align:center;margin-left:250px;margin-top:30px;",
                    textInput("proinputidscor","Please enter the target Mouse gene name or UniProt ID",value="",width = '600px', placeholder = 'e.g. Snca or O55042')
                  )
                ),
                column(
                  width = 5,
                  actionButton("proinputidscor_btn","Search",width='100px',icon("paper-plane"),
                               style="margin-top:55px;text-align:left;margin-right:0px;color: #fff; background-color: #0072B2; border-color: #0072B2;")
                )
              ),
              #hr(),
              downloadButton("corfiguredl","Download figure"),
              plotOutput("corplotx")
            )
          )
        ),
        tabPanel(
          "Correlation analysis: Protein sets",
          value = "module5panel",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              h3(
                "Correlation analysis: Protein sets",
                tags$span(
                  id = 'span5',
                  `data-toggle` = "tooltip5",
                  title = '
                Correlation analysis supports discovery of relationship and dependency between protein abundance and protein lifetime for a given protein set or two protein sets.
                ',
                  tags$span(class = "glyphicon glyphicon-question-sign")
                )
              ),
              tags$hr(style="border-color: grey;"),
              radioButtons("prosettype",label = "",choices = list("One protein list" = 1,"Two protein lists"=2),selected = 1,inline = TRUE),
              #tags$hr(style="border-color: grey;"),
              conditionalPanel(
                condition = "input.prosettype==1",
                radioButtons(
                  "loadprosettype1",
                  label = "",
                  choices = list("1. Paste"=2,"2. Upload" = 1,"3. Example data"=3),
                  selected = 2,
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.loadprosettype1==1",
                  fileInput('loadprosetcsv', h5('Please import your data:'),accept=c('.csv/.txt'))
                ),
                conditionalPanel(
                  condition = "input.loadprosettype1==2",
                  textAreaInput("loadprosetID_zhantie",label = h5("Please Paste gene names/UniProt IDs here："),value="",height ="100px")
                ),
                conditionalPanel(
                  condition = "input.loadprosettype1==3",
                  downloadButton("loadproset1examdl","Download example data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
                ),
                hr(),
                selectInput("loadprosetxaxis",h5("4. Select data type for x axis:"),
                            choices = c("Protein set average abundance"=1,"Protein set average lifetime"=2,"Proteome average abundance"=3,"Proteome average lifetime"=4)),
                selectInput("loadprosetyaxis",h5("5. Select data type for y axis:"),
                            choices = c("Proteome average abundance"=1,"Proteome average lifetime"=2,"Protein set average abundance"=3,"Protein set average lifetime"=4)),
                checkboxInput("oneprosetxylabelif","6. Change X axis and/or Y axis label or not?",value = FALSE),
                conditionalPanel(
                  condition = "input.oneprosetxylabelif==true",
                  textInput("oneprosetxlabel",h5("6.1. X axis label:"),value = "Protein set average"),
                  textInput("oneprosetylabel",h5("6.2. Y axis label:"),value = "Proteome average")
                ),
                textInput("loadprosetcorpointcol",h5("7. Point color:"),value = "darkred"),
                textInput("loadprosetcorlinecol",h5("8. Line color:"),value = "#74ADD1"),
                numericInput("loadprosetcorheight",h5("9. Each sub figure height:"),700)
              ),
              conditionalPanel(
                condition = "input.prosettype==2",
                radioButtons(
                  "loadprosettype2",
                  label = "",
                  choices = list("1. Paste"=2,"2. Upload" = 1,"3. Example data"=3),
                  selected = 2,
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.loadprosettype2==1",
                  fileInput('loadprosetcsv1', h5('Please import protein list 1:'),accept=c('.csv/.txt')),
                  fileInput('loadprosetcsv2', h5('Please import protein list 2:'),accept=c('.csv/.txt'))
                ),
                conditionalPanel(
                  condition = "input.loadprosettype2==2",
                  textAreaInput("loadprosetID_zhantie1",label = h5("Please Paste protein list 1："),value="",height ="100px"),
                  textAreaInput("loadprosetID_zhantie2",label = h5("Please Paste protein list 2："),value="",height ="100px")
                ),
                conditionalPanel(
                  condition = "input.loadprosettype2==3",
                  downloadButton("loadproset1examdl1","Download example 1",style="color: #fff; background-color: #6495ED; border-color: #6495ED"),
                  downloadButton("loadproset1examdl2","Download example 2",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
                ),
                hr(),
                selectInput("loadprosetxaxisx",h5("4. Select data type for x axis:"),
                            choices = c("Protein set 1 average abundance"=1,"Protein set 1 average lifetime"=2,"Protein set 2 average abundance"=3,"Protein set 2 average lifetime"=4)),
                selectInput("loadprosetyaxisx",h5("5. Select data type for y axis:"),
                            choices = c("Protein set 2 average abundance"=1,"Protein set 2 average lifetime"=2,"Protein set 1 average abundance"=3,"Protein set 1 average lifetime"=4)),
                checkboxInput("prosetxylabelif","6. Change X axis and/or Y axis label or not?",value = FALSE),
                conditionalPanel(
                  condition = "input.prosetxylabelif==true",
                  textInput("prosetxlabel",h5("6.1. X axis label:"),value = "Proteosome"),
                  textInput("prosetylabel",h5("6.2. Y axis label:"),value = "Lysosome")
                ),
                textInput("loadprosetcorpointcolx",h5("7. Point color:"),value = "darkred"),
                textInput("loadprosetcorlinecolx",h5("8. Line color:"),value = "#74ADD1"),
                numericInput("loadprosetcorheightx",h5("9. Each sub figure height:"),700)
              )
            ),
            mainPanel(
              width = 9,
              conditionalPanel(
                condition = "input.prosettype==1",
                radioButtons(
                  "prosetrestype",
                  label = "",
                  choices = list("Uploaded one protein list" = 1,"Expression data"=2,"Correlation plot"=3),
                  selected = 1,
                  inline = TRUE
                ),
                hr(),
                conditionalPanel(
                  condition = "input.prosetrestype==1",
                  dataTableOutput("uploadprosetlist")
                ),
                conditionalPanel(
                  condition = "input.prosetrestype==2",
                  h4("Protein list table:"),
                  downloadButton("uploadprosetabundfdl","Download table"),
                  dataTableOutput("uploadprosetabundf")
                ),
                conditionalPanel(
                  condition = "input.prosetrestype==3",
                  downloadButton("prosetcorplotdl","Download figure"),
                  plotOutput("prosetcorplot")
                )
              ),
              conditionalPanel(
                condition = "input.prosettype==2",
                radioButtons(
                  "prosetrestypex",
                  label = "",
                  choices = list("Uploaded two protein lists" = 1,"Expression data"=2,"Correlation plot"=3),
                  selected = 1,
                  inline = TRUE
                ),
                hr(),
                conditionalPanel(
                  condition = "input.prosetrestypex==1",
                  h4("Protein list 1:"),
                  dataTableOutput("uploadprosetlist1"),
                  h4("Protein list 2:"),
                  dataTableOutput("uploadprosetlist2")
                ),
                conditionalPanel(
                  condition = "input.prosetrestypex==2",
                  h4("Protein list 1 table:"),
                  downloadButton("uploadprosetabundf1dl","Download table"),
                  dataTableOutput("uploadprosetabundf1"),
                  h4("Protein list 2 table:"),
                  downloadButton("uploadprosetabundf2dl","Download table"),
                  dataTableOutput("uploadprosetabundf2")
                ),
                conditionalPanel(
                  condition = "input.prosetrestypex==3",
                  downloadButton("prosetcorplotxdl","Download figure"),
                  plotOutput("prosetcorplotx")
                )
              )
            )
          )
        )
        #tabPanel(
        #  "Heatmap",
        #  value = "module3panel",
        #  sidebarLayout(
        #    sidebarPanel(
        #      width = 3,
        #      h3(
        #        "Heatmap",
        #        tags$span(
        #          id = 'span3',
        #          `data-toggle` = "tooltip3",
        #          title = '
        #        In this part, users can show the genes that they are interested in using a Heatmap.
        #        ',
        #          tags$span(class = "glyphicon glyphicon-question-sign")
        #        )
        #      ),
        #      tags$hr(style="border-color: grey;"),
        #      radioButtons(
        #        "loaddatatype",
        #        label = "",
        #        choices = list("Upload" = 1,"Example data"=2),
        #        selected = 1,
        #        inline = TRUE
        #      ),
        #      conditionalPanel(
        #        condition = "input.loaddatatype==1",
        #        fileInput('heatmapfile1', h5('Import your data:'),accept=c('.csv'))
        #      ),
        #      conditionalPanel(
        #        condition = "input.loaddatatype==2",
        #        downloadButton("loaddatadownload1","Download example data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
        #      ),
        #      hr(),
        #      selectInput("heatmapdatatypex",h5("3.1. Select data type:"),choices = c("mRNA","Protein","Phospho")),
        #      numericInput("heatmapheightx",h5("3.2. Figure Height:"),800),
        #      #downloadButton("silacprodatadl","Download table")
        #      downloadButton("heatmapfiguredl","Download figure")
        #    ),
        #    mainPanel(
        #      width = 9,
        #      radioButtons(
        #        "heatmaprestype",
        #        label = "",
        #        choices = list("Uploaded genes/IDs" = 1,"Expression data"=2,"Heatmap"=3),
        #        selected = 1,
        #        inline = TRUE
        #      ),
        #      hr(),
        #      conditionalPanel(
        #        condition = "input.heatmaprestype==1",
        #        dataTableOutput("uploadgenestb")
        #      ),
        #      conditionalPanel(
        #        condition = "input.heatmaprestype==2",
        #        downloadButton("heatmapdatadl","Download table"),
        #        dataTableOutput("heatmaptable")
        #      ),
        #      conditionalPanel(
        #        condition = "input.heatmaprestype==3",
        #        plotOutput("heatmapplot")
        #      )
        #    )
        #  )
        #)
      ),
      tabPanel(
        "Manual",#About the project
        uiOutput("aboutprojectui")
      ),
      tabPanel(
        "About us",
        uiOutput("aboutusui")
      ),
      tabPanel(
        "Contact",
        uiOutput("contactui")
      )
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #liulabui
  output$liulabui<-renderUI({
    screenwidth2<-input$dimension[1]
    if(is.null(screenwidth2)){
      return(NULL)
    }else{
      if(screenwidth2<=1024){
        imgwidth2<-100
      }
      else if(screenwidth2>1024 & screenwidth2<=1440){
        imgwidth2<-100
      }
      else{
        imgwidth2<-100
      }
    }
    div(
      style='margin-bottom:-36px;margin-top:10px;margin-left:18.5%',
      img(src='liulablogo.png',style=paste0("width:",imgwidth2,"px;"))
    )
  })
  #welcomeui
  #welcomeui
  output$welcomeui <- renderUI({
    screenwidth <- input$dimension[1]
    imgwidth <- ifelse(is.null(screenwidth), 510, 
                       ifelse(screenwidth <= 1024, 510, 
                              ifelse(screenwidth <= 1440, 510, 510)))
    
    fluidPage(
      div(
        style = "background: linear-gradient(to bottom, #fdfcfb, #e2eafc); color: #333; padding: 40px 0; text-align: center; border-bottom: 2px solid #ddd;",
        HTML("<h1 style='font-size: 3.5em; font-weight: bold; margin-bottom: 20px;'>An Atlas of Proteome and Phosphoproteome Turnover</h1>"),
        HTML("<h1 style='font-size: 3.5em; font-weight: bold; margin-bottom: 30px;'>Across Mouse Tissues and Brain Regions</h1>"),
        HTML("<p style='font-size: 1.2em; max-width: 800px; margin: auto; line-height: 1.8;'>This web service offers easy access to data of mass spectrometry-based proteome and phosphoproteome turnover across mouse tissues and brain regions. Explore bioinformatics tools to elucidate how proteome and phosphoproteome lifetimes vary across tissues.</p>"),
        div(
          style = "margin-top: 30px;",
          img(src = 'tissuePPTsy.png', style = paste0("width:", 600, "px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);"))
        )
      ),
      # Features Section
      div(
        style = "padding: 60px 20px; background: #f8f9fa;",
        div(
          style = "text-align: center; margin-bottom: 50px;",
          HTML("<h2 style='font-size: 2.8em; font-weight: bold; color: #045082;'>Explore Our Interactive Tools</h2>"),
          HTML("<p style='font-size: 1.2em; color: #6c757d;'>Discover the features and tools available for analyzing proteome and phosphoproteome data.</p>")
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "text-align: center; margin: 0 auto; max-width: 700px;",
              div(
                style = "background: white; padding: 30px; border-radius: 15px; box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1); text-align: center; margin-bottom: 30px; transition: transform 0.3s;",
                onmouseover = "$(this).css('transform', 'scale(1.05)');",
                onmouseout = "$(this).css('transform', 'scale(1)');",
                img(src = 'bubbleexample.png', height = "200", style = "border-radius: 0px; margin-bottom: 20px;"),
                HTML("<h4 style='font-size: 1.8em; font-weight: bold; color: #BB0021FF;'>Heat-circle (HC) Plot</h4>"),
                HTML("<p style='font-size: 1.1em; color: #4f4f4f;'>HC plot provides an overview of protein abundance (in terms of iBAQ values) and protein lifetime for individual proteins or protein sets.</p>"),
                actionButton("button_module1", "Explore", style = "color: white; background-color: #BB0021FF; border: none; padding: 10px 20px; font-size: 1em; border-radius: 5px; margin-top: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);")
              )
            )
          )
        ),
        fluidRow(
          column(
            3,
            div(
              style = "background: white; padding: 30px; border-radius: 15px; box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1); text-align: center; margin-bottom: 30px; transition: transform 0.3s;",
              onmouseover = "$(this).css('transform', 'scale(1.05)');",
              onmouseout = "$(this).css('transform', 'scale(1)');",
              img(src = 'heatmapexample.png', height = "180", style = "border-radius: 0px; margin-bottom: 20px;"),
              HTML("<h4 style='font-size: 1.6em; font-weight: bold; color: #045082;'>Heatmap Analysis</h4>"),
              HTML("<p style='font-size: 1em; color: #6c757d;'>Visualize protein abundance, lifetime, P-site abundance, and P-site lifetime for specific protein sets.</p>"),
              actionButton("button_module2", "Explore", style = "color: white; background-color: #045082; border: none; padding: 10px 20px; font-size: 1em; border-radius: 5px; margin-top: 10px;")
            )
          ),
          column(
            3,
            div(
              style = "background: white; padding: 30px; border-radius: 15px; box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1); text-align: center; margin-bottom: 30px; transition: transform 0.3s;",
              onmouseover = "$(this).css('transform', 'scale(1.05)');",
              onmouseout = "$(this).css('transform', 'scale(1)');",
              img(src = 'barplotexample.png', height = "180", style = "border-radius: 0px; margin-bottom: 20px;"),
              HTML("<h4 style='font-size: 1.6em; font-weight: bold; color: #045082;'>Protein-Specific Barplots</h4>"),
              HTML("<p style='font-size: 1em; color: #6c757d;'>Generate barplots to visualize protein abundance, lifetime, P-site abundance, and P-site lifetime for specific proteins.</p>"),
              actionButton("button_module3", "Explore", style = "color: white; background-color: #045082; border: none; padding: 10px 20px; font-size: 1em; border-radius: 5px; margin-top: 10px;")
            )
          ),
          column(
            3,
            div(
              style = "background: white; padding: 30px; border-radius: 15px; box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1); text-align: center; margin-bottom: 30px; transition: transform 0.3s;",
              onmouseover = "$(this).css('transform', 'scale(1.05)');",
              onmouseout = "$(this).css('transform', 'scale(1)');",
              img(src = 'correlationexample.png', height = "180", style = "border-radius: 0px; margin-bottom: 20px;"),
              HTML("<h4 style='font-size: 1.6em; font-weight: bold; color: #045082;'>Correlation Analysis: Proteins</h4>"),
              HTML("<p style='font-size: 1em; color: #6c757d;'>Discover relationships between protein abundance, lifetime, P-site abundance, and P-site lifetime for specific proteins.</p>"),
              actionButton("button_module4", "Explore", style = "color: white; background-color: #045082; border: none; padding: 10px 20px; font-size: 1em; border-radius: 5px; margin-top: 10px;")
            )
          ),
          column(
            3,
            div(
              style = "background: white; padding: 30px; border-radius: 15px; box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1); text-align: center; margin-bottom: 30px; transition: transform 0.3s;",
              onmouseover = "$(this).css('transform', 'scale(1.05)');",
              onmouseout = "$(this).css('transform', 'scale(1)');",
              img(src = 'correlationexample2.png', height = "180", style = "border-radius: 0px; margin-bottom: 20px;"),
              HTML("<h4 style='font-size: 1.6em; font-weight: bold; color: #045082;'>Correlation Analysis: Protein Sets</h4>"),
              HTML("<p style='font-size: 1em; color: #6c757d;'>Explore relationships between protein abundance and lifetime for user-defined protein sets.</p>"),
              actionButton("button_module5", "Explore", style = "color: white; background-color: #045082; border: none; padding: 10px 20px; font-size: 1em; border-radius: 5px; margin-top: 10px;")
            )
          )
        )
      ),
      # Footer
      div(
        style = "background: #343a40; color: white; padding: 20px 0; text-align: center;",
        HTML("<p style='font-size: 1em; margin: 0;'>© 2024 <a href='https://www.yslproteomics.org/' target='_blank' style='color: #0072B2; text-decoration: none;'>Yansheng Liu's Group</a>. All Rights Reserved.</p>")
      )
    )
  })
  
  #aboutprojectui
  # aboutprojectui
  output$aboutprojectui <- renderUI({
    fluidPage(
      div(
        style = "background: linear-gradient(to bottom, #f8f9fa, #eaeaea); padding: 20px 0;",
        div(
          style = "margin-bottom: 30px; margin-left: 20%;margin-top: -10px; ",
          HTML("<h1 style='font-size: 2.7em; font-weight: bold; color: #045082;'>User Manual</h1>")
        ),
        div(
          style = "margin-bottom: 30px; text-align: center;",
          HTML("<h1 style='font-size: 2.1em; font-weight: bold; color: #045082;'>About the Project</h1>")
        ),
        div(
          style = "max-width: 800px; margin: auto;",
          HTML("<p style='font-size: 1.1em; line-height: 1.8; color: #6c757d;'>Understanding molecular diversity between different mammalian tissues has been a central objective of modern biology. 
        This project maps both abundance and lifetime for 11,000 proteins and 40,000 phosphosites across eight tissues and multiple brain regions in mice, 
        offering a unique resource for exploring proteome and phosphoproteome turnover across tissues.</p>")
        )
      ),
      # Content Section
      div(
        style = "padding: 60px 20px; background: #ffffff;",
        div(
          style = "max-width: 900px; margin: auto;",
          div(
            style = "margin-bottom: 50px; text-align: center;margin-top: -40px;",
            HTML("<h2 style='font-size: 2.1em; font-weight: bold; color: #045082;'>How to use Tissue-PPT?</h2>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.8; color: #6c757d;'>Tissue-PPT is a proteome and phosphoproteome turnover atlas that provides precise measurements based on Tandem mass tag (TMT) and data-independent acquisition mass spectrometry (DIA-MS) coupled with isotopic amino acid labeling. It is designed to help researchers understand tissue phenotypic and functional diversity at the molecular level. Please click 'Details' button below to check the detailed inroduction of the relative module.</p>")
          ),
          
          # Features Section
          div(
            style = "display: flex; flex-direction: column; gap: 30px;",
            div(
              style = "display: flex; flex-wrap: wrap; justify-content: space-between; gap: 30px;",
              div(
                style = "flex: 1; min-width: 280px; background: #f8f9fa; padding: 20px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
                HTML("<h3 style='font-size: 1.4em; font-weight: bold; color: #045082;'>Heat-circle (HC) Plot</h3>"),
                HTML("<p style='font-size: 1em; line-height: 1.6; color: #6c757d;'>The Heat-circle (HC) plot provides a synchronized overview of protein abundance and lifetime across tissues. Upload your data or use example data to create interactive HC plots.</p>"),
                actionButton("introbtn_hcplot","Details",icon("file-alt"),
                             style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
              ),
              div(
                style = "flex: 1; min-width: 280px; background: #f8f9fa; padding: 20px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
                HTML("<h3 style='font-size: 1.4em; font-weight: bold; color: #045082;'>Heatmap Analysis</h3>"),
                HTML("<p style='font-size: 1em; line-height: 1.6; color: #6c757d;'>Visualize protein abundance, lifetime, phospho abundance, and phospho lifetime across tissues with heatmaps. This module provides a comprehensive view of data trends.</p>"),
                actionButton("introbtn_heatmap","Details",icon("file-alt"),
                             style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
              )
            ),
            
            # Lower Part: Three Features
            div(
              style = "display: flex; flex-wrap: wrap; justify-content: space-between; gap: 30px;",
              div(
                style = "flex: 1; min-width: 280px; background: #f8f9fa; padding: 20px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
                HTML("<h3 style='font-size: 1.4em; font-weight: bold; color: #045082;'>Protein-Specific Barplots</h3>"),
                HTML("<p style='font-size: 1em; line-height: 1.6; color: #6c757d;'>Generate barplots to visualize protein abundance, lifetime, phospho abundance, and phospho lifetime for specific proteins. Search by protein name or ID for detailed visualizations.</p>"),
                actionButton("introbtn_PSBarplot","Details",icon("file-alt"),
                             style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
              ),
              div(
                style = "flex: 1; min-width: 280px; background: #f8f9fa; padding: 20px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
                HTML("<h3 style='font-size: 1.4em; font-weight: bold; color: #045082;'>Correlation Analysis: Individual Proteins</h3>"),
                HTML("<p style='font-size: 1em; line-height: 1.6; color: #6c757d;'>Explore relationships between protein abundance and lifetime for individual proteins. Generate scatter plots with regression lines and correlation coefficients.</p>"),
                actionButton("introbtn_CorIndividual","Details",icon("file-alt"),
                             style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
              ),
              div(
                style = "flex: 1; min-width: 280px; background: #f8f9fa; padding: 20px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
                HTML("<h3 style='font-size: 1.4em; font-weight: bold; color: #045082;'>Correlation Analysis: Protein Sets</h3>"),
                HTML("<p style='font-size: 1em; line-height: 1.6; color: #6c757d;'>Analyze relationships between protein set averages and proteome averages or between two protein sets. Visualize scatter plots with regression lines and correlation coefficients.</p>"),
                actionButton("introbtn_CorSets","Details",icon("file-alt"),
                             style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
              )
            )
          )
          
        )
      ),
      
      # Conclusion Section
      div(
        style = "padding: 40px 20px; background: #f8f9fa; text-align: center;",
        div(
          style = "max-width: 800px; margin: auto;",
          HTML("<h2 style='font-size: 2.1em; font-weight: bold; color: #045082;'>Why Tissue-PPT Matters?</h2>"),
          HTML("<p style='font-size: 1.1em; line-height: 1.8; color: #6c757d;'>Tissue-PPT offers novel opportunities for exploring the in vivo stability and dynamics of proteins and their functional phosphorylation events, enabling researchers to uncover mechanisms underlying tissue-specific functions and phenotypes.</p>")
        )
      )#,
      
      # Footer Section
      #div(
      #  style = "background: #343a40; color: white; padding: 20px 0; text-align: center;",
      #  HTML("<p style='font-size: 1em; margin: 0;'>© 2024 <a href='https://www.yslproteomics.org/' target='_blank' style='color: #0072B2; text-decoration: none;'>Yansheng Liu's Group</a>. All Rights Reserved.</p>")
      #)
    )
  })
  ##1
  observeEvent(input$introbtn_hcplot, {
    showModal(modalDialog(
      title = NULL,
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",
      div(
        style = "padding: 20px; font-family: Arial, sans-serif;",
        # Hero Section with Title and Subtitle
        div(
          style = "background: linear-gradient(to right, #6a11cb, #2575fc); color: white; padding: 20px; border-radius: 10px; text-align: center;",
          HTML("<h2 style='font-size: 2.5em; font-weight: bold;'>Heat-Circle (HC) Plot</h2>"),
          HTML("<p style='font-size: 1.2em; margin-top: 10px;'>Visualize protein abundance and lifetime across tissues in a synchronized manner.</p>")
        ),
        
        # Content Section with Subsections and Images
        div(
          style = "margin-top: 20px;",
          # Section 1: Overview
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>What is HC-Plot?</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>HC-plot provides a synchronized overview of protein abundance (PA, in terms of iBAQ values, relative iBAQ values or riBAQ, and MS-intensities) and protein lifetime (PT or protein T50 determined by pulsed SILAC labeling) for individual proteins or protein sets. This allows researchers to explore relationships between protein abundance and lifetime across tissues.</p>")
          ),
          # Section 3: How to Use HC-Plot
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>How to Use HC-Plot?</h3>"),
            #HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Input data:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 10px;",
              img(src = "HCplot1.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Parameters:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Paste. Users can paste the gene names/UniProt IDs directly here. We found this option very useful for direct inspection of PA and PT values for given genes/proteins."),
              tags$li("Upload. This means users can upload the gene names/UniProt IDs in a .csv file."),
              tags$li("Example data. Here shows an example data for users."),
              tags$li("PA: Log2(riBAQ in ppm). riBAQ mean relative iBAQ. ppm means the relative contribution of iBAQ values to the total protein amount. If users select this parameter, this tool will transform the iBAQ values (log2) into ppm (log2). The values where Log2(riBAQ in ppm) < -1 or Log2(riBAQ in ppm) > 9 are defined as outliers."),
              tags$li("PA: Log2(iBAQ). If users select this parameter, this tool will match the iBAQ values (log2) for the input gene names/UniProt IDs."),
              tags$li("Log2(MS-intensity). If users select this parameter, this tool will match the protein intensity (log2) for the input gene names/UniProt IDs."),
              tags$li(
                tags$span("PT rank percentage in each tissue? If true, this tool will calculate the rank of the protein lifetime value and normalize every rank to the maximum rank in terms of rank percentage (0-100%) for each tissue."),
                tags$ul(
                  tags$li(tags$span("7.1. Row scaled across tissues? If true in 7. above, users will see this parameter, which means this tool will additionally scale the rank percentage value across all tissues for the current gene/protein list (Z score across tissues)."))
                )
              ),
              tags$li(
                tags$span("Change figure height? If enabled, users can manually adjust the figure height using the parameter A.8.1 (described below). If disabled, the tool will automatically adjust the figure height based on the number of input gene names or UniProt IDs."),
                tags$ul(
                  tags$li(tags$span("8.1. Figure Height. This parameter adjusts the height of the plot."))
                )
              )
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>B. Results:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Uploaded gene names/Uniprot IDs. Here shows the input gene names/Uniprot IDs. If users input nothing, it shows 'NO data here. Please paste the genes/IDs, or load the example data to check first.'"),
              tags$li("Expression data. Here shows the matched results of the input gene names/Uniprot IDs, including protein iBAQ values and protein lifetime values. Please note that if users select parameters 4, 5, and/or 6 mentioned above, the corresponding results will be displayed based on the selected parameters."),
              tags$li("HC plot. Here shows the HC plot based on the matched results.")
            )
          ),
          # Section 4: Example Output
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>Example Output</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Users can choose the '3. Example data' in the left parameter panel. Shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot2.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Then users click 'Expression data', this tool will match relative iBAQ values in ppm (log2) and lifetime values (log2) for the input genes based on the default parameters, shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot3.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users click the “Download table” button, the corresponding table will be downloaded to their local device.</p>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>The HC-plot based on these values is shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot4.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users click the “Download figure” button, the corresponding figure will be downloaded to their local device.</p>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users choose '5. PA: Log2 (iBAQ)', this tool will match iBAQ values (log2) and lifetime values (log2) for the input genes, shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot5.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>The HC-plot based on these values is shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot6.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users choose '6. Log2(MS-intensity)', this tool will match protein intensities (log2) and lifetime values (log2) for the input genes, shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot7.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>The HC-plot based on these values is shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot8.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users choose '4. PA: Log2(riBAQ in ppm)' and '7. PT rank percentage in each tissue?', this tool will match relative iBAQ values in ppm (log2) and then calculate rank percentage of each protein lifetime value for each tissue, shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot9.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>The HC-plot based on these values is shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot10.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users choose '7.1. Row scaled across tissues?', this tool will scale the rank percentage of each protein lifetime value across all tissues for the current gene/protein list, shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot11.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>The HC-plot based on these values is shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "HCplot12.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            )
          )
        )
      )
    ))
  })
  
  ##2
  observeEvent(input$introbtn_heatmap,{
    showModal(modalDialog(
      title = NULL,
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",
      div(
        style = "padding: 20px; font-family: Arial, sans-serif;",
        # Hero Section with Title and Subtitle
        div(
          style = "background: linear-gradient(to right, #6a11cb, #2575fc); color: white; padding: 20px; border-radius: 10px; text-align: center;",
          HTML("<h2 style='font-size: 2.5em; font-weight: bold;'>Heatmap Analysis</h2>"),
          HTML("<p style='font-size: 1.2em; margin-top: 10px;'>Visualize protein abundance, lifetime, phospho abundance, and phospho lifetime across tissues with heatmaps.</p>")
        ),
        # Content Section with Subsections and Images
        div(
          style = "margin-top: 20px;",
          # Section 1: Overview
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>What is Heatmap Analysis?</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Visualize protein abundance, lifetime, phospho abundance, and phospho lifetime across tissues with heatmaps. This module provides a rapid and comprehensive view of data trends for protein sets.</p>")
          ),
          # Section 3: How to Use HC-Plot
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>How to Use Heatmap Analysis?</h3>"),
            #HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Input data:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 10px;",
              img(src = "Heatmap1.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Parameters:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Paste. This means users can paste the gene names/UniProt IDs directly here."),
              tags$li("Upload. This means users can upload the gene names/UniProt IDs in a .csv file."),
              tags$li("Example data. Here shows an example data for users."),
              tags$li("Figure Height. This parameter adjust the height of the plot.")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>B. Results:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Uploaded gene names/Uniprot IDs. Here shows the input gene names/Uniprot IDs. If users input nothing, it shows 'NO data here. Please paste the genes/IDs, or load the example data to check first.'"),
              tags$li("Expression data. Here shows the matched results of the input gene names/Uniprot IDs, including protein abundance (MS-intensities derived from BoxCarmax-DIA data), protein lifetime, phospho abundance, and phospho lifetime."),
              tags$li("Heatmap. Here shows the Heatmap based on the matched results.")
            )
          ),
          # Section 4: Example Output
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>Example Output</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Users can choose the '3. Example data' in the left parameter panel. Shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "Heatmap2.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>When users click on 'Expression data', the tool retrieves and matches four types of log2-transformed values for the input genes: Protein abundance, Protein lifetime, Phospho abundance, and Phospho lifetime. The matched data is displayed as shown below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "Heatmap3.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users click the “Download table” button, the corresponding table will be downloaded to their local device.</p>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>The Heatmap based on these values is shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "Heatmap4.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users click the “Download figure” button, the corresponding figure will be downloaded to their local device.</p>")
          )
        )
      )
    ))
  })
  ##3
  observeEvent(input$introbtn_PSBarplot,{
    showModal(modalDialog(
      title = NULL,
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",
      div(
        style = "padding: 20px; font-family: Arial, sans-serif;",
        # Hero Section with Title and Subtitle
        div(
          style = "background: linear-gradient(to right, #6a11cb, #2575fc); color: white; padding: 20px; border-radius: 10px; text-align: center;",
          HTML("<h2 style='font-size: 2.5em; font-weight: bold;'>Protein-Specific Barplots</h2>"),
          HTML("<p style='font-size: 1.2em; margin-top: 10px;'>Generate barplots to visualize protein abundance, lifetime, P-site abundance, and P-site lifetime for specific proteins.</p>")
        ),
        # Content Section with Subsections and Images
        div(
          style = "margin-top: 20px;",
          # Section 1: Overview
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>What is Protein-Specific Barplots?</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Protein-Specific Barplots are visual tools designed to display the distribution of protein-related data, such as protein abundance, lifetime, phosphosite abundance, or phosphosite lifetime, across different tissues for a specific protein. These barplots allow researchers to analyze and compare how a particular protein's characteristics vary across multiple tissues. By entering a protein's name or ID, users can generate detailed plots that summarize the protein's behavior, making it easier to identify patterns, trends, or anomalies in the data.</p>")
          ),
          # Section 3: How to Use HC-Plot
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>How to Use Protein-Specific Barplots?</h3>"),
            #HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Input data:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 10px;",
              img(src = "PSbarplot1.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Parameters:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Select data type. There are four types of datasets, including Protein abundance (MS-intensities derived from BoxCarmax-DIA data), Protein lifetime, Phosphosite abundance, and Phosphosite lifetime. Users should choose one of them."),
              tags$li("Figure Height. This parameter adjust the height of the plot.")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>B. Results:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Please enter the target Mouse gene name or gene ID. Here users should type in a mouse gene name or UniProt ID that they want to check, for example, Snca or O55042. Case does not matter.")
            )
          ),
          # Section 4: Example Output
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>Example Output</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Users can type in a mouse gene name or UniProt ID that they want to check, for example, Snca. Then click the 'Search' button and the results are shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "PSbarplot2.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users click the “Download figure” button, the corresponding figure will be downloaded to their local device.</p>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users click the “Download data” button, the corresponding data used for the barplot will be downloaded to their local device.</p>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>When users select 'Protein lifetime', the results are displayed as shown below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "PSbarplot3.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>When users select 'Phospho abundance', the results are displayed as shown below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "PSbarplot4.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>When users select 'Phospho lifetime', the results are displayed as shown below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "PSbarplot5.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            )
          )
        )
      )
    ))
  })
  ##4
  observeEvent(input$introbtn_CorIndividual,{
    showModal(modalDialog(
      title = NULL,
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",
      div(
        style = "padding: 20px; font-family: Arial, sans-serif;",
        # Hero Section with Title and Subtitle
        div(
          style = "background: linear-gradient(to right, #6a11cb, #2575fc); color: white; padding: 20px; border-radius: 10px; text-align: center;",
          HTML("<h2 style='font-size: 2.5em; font-weight: bold;'>Correlation Analysis: Individual Proteins</h2>"),
          HTML("<p style='font-size: 1.2em; margin-top: 10px;'>Explore relationships between protein abundance and lifetime for individual proteins. Generate scatter plots with regression lines and correlation coefficients.</p>")
        ),
        # Content Section with Subsections and Images
        div(
          style = "margin-top: 20px;",
          # Section 1: Overview
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>What is Correlation Analysis: Individual Proteins?</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Correlation Analysis: Individual Protein refers to a method for examining the relationship between a specific protein's abundance, lifetime, phospho abundance, or phospho lifetime across different tissues. By default, both Pearson and Spearman correlations are calculated for users. This analysis helps researchers understand how the behavior of an individual protein aligns with other variables or datasets. It provides valuable insights into tissue-specific dynamics, functional roles, or potential regulatory mechanisms associated with the protein, enabling a deeper understanding of its biological significance.</p>")
          ),
          # Section 3: How to Use HC-Plot
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>How to Use Correlation Analysis: Individual Proteins?</h3>"),
            #HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Input data:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 10px;",
              img(src = "CorIndividual1.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Parameters:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Select data type for x axis. There are four types of datasets, including Protein abundance, Protein lifetime, Phosphosite abundance, and Phosphosite lifetime. Users should choose one of them for x axis."),
              tags$li("Select data type for y axis. Similar to above, but for y axis."),
              tags$li("Point color. Users can type in a color name to change the point color in the correlation plot."),
              tags$li("Line color. Users can type in a color name to change the line color in the correlation plot."),
              tags$li("Each sub figure height. This parameter adjust the height of each subgraph.")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>B. Results:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Please enter the target Mouse gene name or gene ID. Here users should type in a mouse gene name or UniProt ID that they want to check, for example, Snca or O55042. Case does not matter.")
            )
          ),
          # Section 4: Example Output
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>Example Output</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Users can type in a mouse gene name or UniProt ID that they want to check, for example, Snca. Then click the 'Search' button and the results are shown as below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "CorIndividual2.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'><i>r</i> represents the Pearson correlation coefficient, while <i>p</i> denotes the p-value obtained from the Pearson correlation test performed using the cor.test function in R. Similarly, <i>rho</i> represents the Spearman correlation coefficient, and <i>p.rho</i> denotes the p-value obtained from the Spearman correlation test performed using the cor.test function in R.</p>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>When users select 'Protein abundance' for x axis and 'Phospho lifetime' for y axis, the results are displayed as shown below:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "CorIndividual3.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            )
          )
        )
      )
    ))
  })
  ##5
  observeEvent(input$introbtn_CorSets,{
    showModal(modalDialog(
      title = NULL,
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",
      div(
        style = "padding: 20px; font-family: Arial, sans-serif;",
        # Hero Section with Title and Subtitle
        div(
          style = "background: linear-gradient(to right, #6a11cb, #2575fc); color: white; padding: 20px; border-radius: 10px; text-align: center;",
          HTML("<h2 style='font-size: 2.5em; font-weight: bold;'>Correlation Analysis: Protein Sets</h2>"),
          HTML("<p style='font-size: 1.2em; margin-top: 10px;'>Explore relationships between protein abundance and lifetime for user-defined protein sets.</p>")
        ),
        # Content Section with Subsections and Images
        div(
          style = "margin-top: 20px;",
          # Section 1: Overview
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>What is Correlation Analysis: Protein Sets?</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Correlation Analysis: Protein Set supports discovery of relationship and dependency between protein abundance and protein lifetime for a given protein set or two protein sets or the averaged protein levels. By default, both Pearson and Spearman correlations are calculated for users.</p>")
          ),
          # Section 3: How to Use HC-Plot
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>How to Use Correlation Analysis: Individual Proteins?</h3>"),
            #HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Input data:</p>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>I. One protein list. If users choose 'One protein list' here, they can input a single list of proteins, and this tool will perform correlation analysis between the proteins in the provided list and the entire proteome dataset integrated within the tool.</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 10px;",
              img(src = "CorSets1.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Parameters:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Paste. This means users can paste the gene names/UniProt IDs directly here."),
              tags$li("Upload. This means users can upload the gene names/UniProt IDs in a .csv file."),
              tags$li("Example data. Here shows an example data for users."),
              tags$li("Select data type for x axis. There are four types of datasets, including Protein set average abundance (the average abundance of all proteins within the input protein set across different tissues), 
                      Protein set average lifetime (the average lifetime of all proteins within the input protein set across different tissues), 
                      Proteome average abundance (the average abundance of all proteins within the whole integrated proteome dataset across different tissues),
                      Proteome average lifetime (the average lifetime of all proteins within the whole integrated proteome dataset across different tissues). Users should choose one of them for x axis."),
              tags$li("Select data type for y axis. Similar to above, but for y axis."),
              tags$li("Change X axis and/or Y axis label or not? If ture, users can define the X/Y axis label by themselves."),
              tags$li("Point color. Users can type in a color name to change the point color in the correlation plot."),
              tags$li("Line color. Users can type in a color name to change the line color in the correlation plot."),
              tags$li("Each sub figure height. This parameter adjust the height of each subgraph.")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>B. Results:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Uploaded one protein list. Here shows the input one list of proteins. If users input nothing, it shows 'NO data here. Please paste the genes/IDs, or load the example data to check first.'"),
              tags$li("Expression data. Here shows the matched results of the input gene names/Uniprot IDs."),
              tags$li("Correlation plot. Here shows the Correlation plot based on the matched results.")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;margin-top: 20px;'>II. Two protein lists. Users can input two lists of proteins, and this tool will perform correlation analysis between the two lists of proteins.</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 10px;",
              img(src = "CorSets2.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>A. Parameters:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Paste. This means users can paste the gene names/UniProt IDs directly here. Please note, users should input two lists of proteins here."),
              tags$li("Upload. This means users can upload the gene names/UniProt IDs in a .csv file. Please note, users should upload two lists of proteins here."),
              tags$li("Example data. Here shows two example data for users."),
              tags$li("Other parameters are similar to above.")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>B. Results:</p>"),
            tags$ol(
              style = "font-size: 1.1em; line-height: 1.8; color: #555;",
              tags$li("Uploaded two protein lists . Here shows the input two lists of proteins. If users input nothing, it shows 'NO data here. Please paste the genes/IDs, or load the example data to check first.'"),
              tags$li("Expression data. Here shows the matched results of the two lists of proteins."),
              tags$li("Correlation plot. Here shows the Correlation plot based on the matched results.")
            )
          ),
          # Section 4: Example Output
          div(
            style = "margin-bottom: 30px;",
            HTML("<h3 style='font-size: 1.8em; font-weight: bold; color: #333;'>Example Output</h3>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>If users choose 'One protein list', and click '3. Example data', the 'Uploaded one protein list' will be shown as below, which means the example dataset contains 17 proteins:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "CorSets3.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Then users can click 'Expression data', below shows the matched results for the input 17 proteins.</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "CorSets4.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>When users click 'Correlation plot', here shows the correlation plot as below. Users can choose the parameter “6. Change X axis and/or Y axis label or not?” to rename the protein lists.</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "CorSets5.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'><i>r</i> represents the Pearson correlation coefficient, while <i>p</i> denotes the p-value obtained from the Pearson correlation test performed using the cor.test function in R. Similarly, <i>rho</i> represents the Spearman correlation coefficient, and <i>p.rho</i> denotes the p-value obtained from the Spearman correlation test performed using the cor.test function in R.</p>"),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;margin-top: 20px;'>If users choose 'Two protein lists', and click '3. Example data', the 'Uploaded two protein lists' will be shown as below, there are two example datasets here:</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "CorSets6.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>Then users can click 'Expression data', below shows the matched results for the two lists of proteins.</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "CorSets7.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'>When users click 'Correlation plot', here shows the correlation plot as below. Users can choose the parameter “6. Change X axis and/or Y axis label or not?” to rename the protein lists.</p>"),
            div(
              style = "text-align: center; margin-top: 10px;margin-bottom: 20px;",
              img(src = "CorSets8.png", style = "width: 100%; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
            ),
            HTML("<p style='font-size: 1.1em; line-height: 1.6; color: #555;'><i>r</i> represents the Pearson correlation coefficient, while <i>p</i> denotes the p-value obtained from the Pearson correlation test performed using the cor.test function in R. Similarly, <i>rho</i> represents the Spearman correlation coefficient, and <i>p.rho</i> denotes the p-value obtained from the Spearman correlation test performed using the cor.test function in R.</p>")
          )
        )
      )
    ))
  })
  ##
  #aboutusui
  output$aboutusui <- renderUI({
    fluidRow(
      div(
        style = "margin-top: 0px; background: #eaeaea; height: 150px; padding: 20px 0;",
        div(
          style = "display: flex; flex-direction: column; margin-left: 20%;margin-top: 35px;",
          div(
            style = "font-size: 260%; font-weight: bold; color: #045082;",
            HTML("About us <br />")
          )
        )
      ),
      div(
        style = "display: flex; flex-wrap: wrap; justify-content: center; align-items: center; margin: 50px auto; max-width: 1200px; gap: 0px; padding: 20px;",
        div(
          style = "flex: 1; min-width: 300px; max-width: 500px; text-align: left; font-size: 120%; color: #045082;margin-top: -50px;margin-left: 3%;",
          HTML(
            "The research of the Liu Lab at Yale focuses on analyzing protein turnover
          and post-translational modifications to understand cancer aneuploidy,
          cellular signaling transduction, and biodiversity. The Liu lab is further
          dedicated to advancing multiplexed data-independent acquisition mass
          spectrometry and recently harnessing MALDI imaging mass spectrometry
          techniques in clinical applications.<br />
          For more information, please visit: <a href='https://www.yslproteomics.org' target='_blank'>https://www.yslproteomics.org</a>."
          )
        ),
        div(
          style = "flex: 1; min-width: 300px; max-width: 500px; display: flex; justify-content: center;margin-top: -30px;margin-right: 10%;",
          img(src = 'groupmember.jpg', style = "width: 100%; max-width: 400px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
        )
      ),
      column(
        12,
        div(
          id = "footerx",
          style = "margin-top: 100px;"
        )
      )
    )
  })
  
  #contactui
  output$contactui<-renderUI({
    fluidRow(
      div(
        style="margin-top:0px;background:#eaeaea;height:150px",
        div(
          style="padding-top:40px;margin-left:25%;font-size:260%;font-weight:bold;color:#045082",
          HTML("Contact <br />")
        )#,
        #div(
        #  style="margin-top:-0px;margin-left:53%;",
        #  img(src='Liupicture.png')
        #)#
      ),
      div(
        style="margin-top:50px;margin-left:40%;font-size:150%;font-weight:bold;color:#045082",
        HTML("- Lab Head:<br />")
      ),
      div(
        style="margin-top:20px;margin-left:40%;font-size:120%;font-weight:bold;color:#045082",
        HTML("Dr. Yansheng Liu")
      ),
      div(
        style="margin-top:3px;margin-left:40%;font-size:120%;color:#045082",
        HTML("Associate Professor<br />Department of Pharmacology<br />
        Department of Biomedical lnformatics & Data Science<br />
        Yale University School of Medicine<br />")
      ),
      div(
        style="margin-top:20px;margin-left:40%;font-size:120%;font-weight:bold;color:#045082",
        HTML("Institute Address:")
      ),
      div(
        style="margin-top:3px;margin-left:40%;font-size:120%;color:#045082",
        HTML("Cancer Biology Institute<br />
        ABC 371C, 840 West Campus Drive,<br />
        West Haven, CT 06516<br />Tel: +1 (203)737-3853<br />
             <b>Email:</b> yansheng.liu@yale.edu<br />")
      ),
      div(
        style="margin-top:50px;margin-left:40%;font-size:150%;font-weight:bold;color:#045082",
        HTML("- Web technical support:<br />")
      ),
      div(
        style="margin-top:10px;margin-left:40%;font-size:120%;color:#045082",
        HTML("Dr. Shisheng Wang")
      ),
      div(
        style="margin-top:10px;margin-left:40%;font-size:120%;color:#045082",
        HTML("<b>Email:</b> shishengwk@gmail.com")
      ),
      column(
        12,
        div(
          id="footerx",
          style="margin-top: 100px;"
        )
      )
    )
  })
  #Abundance distribution
  observeEvent(input$button_module1, {
    updateTabsetPanel(session, "maintab",
                      selected = "module1panel")
  })
  observeEvent(input$button_module2, {
    updateTabsetPanel(session, "maintab",
                      selected = "module2panel")
  })
  observeEvent(input$button_module3, {
    updateTabsetPanel(session, "maintab",
                      selected = "module3panel")
  })
  observeEvent(input$button_module4, {
    updateTabsetPanel(session, "maintab",
                      selected = "module4panel")
  })
  observeEvent(input$button_module5, {
    updateTabsetPanel(session, "maintab",
                      selected = "module5panel")
  })
  ##pro data
  proibaqout<-reactive({
    proibaqdf<-read.delim("final_protein_iBAQ_log2.txt",header = TRUE,
                      na.strings = c(""," ","NA","N.A.","NaN","n.a.", "Filtered"))
    proibaqdf<-subset(proibaqdf,select=str_subset(colnames(proibaqdf),"Hippo",negate = T))
    proibaqdf<-subset(proibaqdf,select=str_subset(colnames(proibaqdf),"Plasma",negate = T))
    proibaqdf$Proteins<-unlist(lapply(proibaqdf$Proteins,function(x){
      strsplit(x,";")[[1]][1]
    }))
    proibaqdf$Genes<-unlist(lapply(proibaqdf$Genes,function(x){
      strsplit(x,";")[[1]][1]
    }))
    data_with_missing_count <- proibaqdf %>%
      mutate(missing_count = rowSums(is.na(proibaqdf[, -c(1,2)])))
    resultx <- data_with_missing_count %>%
      group_by(Genes) %>%
      filter(missing_count == min(missing_count)) %>%
      ungroup() %>%
      select(-missing_count)%>%
      as.data.frame()
    #proibaqdf<<-resultx
    #if(input$RelativeiBAQif==2){
    #  ibaqdf1<-proibaqdf#heatmaptableout2x$proibaqoutx3
    #  ibaqdf2<-2^ibaqdf1[,-c(1,2)]
    #  ibaqdf3<-sweep(ibaqdf2,2,apply(ibaqdf2, 2, sum,na.rm=T),FUN = "/")*10^6
    #  ibaqdf3<-log2(ibaqdf3)
    #  ibaqdf4<-cbind(ibaqdf1[,c(1,2)],ibaqdf3)
    #}else{
    #  ibaqdf4<-proibaqdf#heatmaptableout2x$proibaqoutx3
    #}
    resultx#proibaqdf#ibaqdf4#
  })
  prorelativeibaqout<-reactive({
    proibaqdf<-read.delim("final_protein_iBAQ_log2.txt",header = TRUE,
                          na.strings = c(""," ","NA","N.A.","NaN","n.a.", "Filtered"))
    proibaqdf<-subset(proibaqdf,select=str_subset(colnames(proibaqdf),"Hippo",negate = T))
    proibaqdf<-subset(proibaqdf,select=str_subset(colnames(proibaqdf),"Plasma",negate = T))
    proibaqdf$Proteins<-unlist(lapply(proibaqdf$Proteins,function(x){
      strsplit(x,";")[[1]][1]
    }))
    proibaqdf$Genes<-unlist(lapply(proibaqdf$Genes,function(x){
      strsplit(x,";")[[1]][1]
    }))
    #proibaqdf<<-proibaqdf
    ibaqdf1<-proibaqdf#heatmaptableout2x$proibaqoutx3
    ibaqdf2<-2^ibaqdf1[,-c(1,2)]
    ibaqdf3<-sweep(ibaqdf2,2,apply(ibaqdf2, 2, sum,na.rm=T),FUN = "/")*10^6
    ibaqdf3<-log2(ibaqdf3)
    ibaqdf4<-cbind(ibaqdf1[,c(1,2)],ibaqdf3)
    data_with_missing_count <- ibaqdf4 %>%
      mutate(missing_count = rowSums(is.na(ibaqdf4[, -c(1,2)])))
    resultx <- data_with_missing_count %>%
      group_by(Genes) %>%
      filter(missing_count == min(missing_count)) %>%
      ungroup() %>%
      select(-missing_count)%>%
      as.data.frame()
    resultx#ibaqdf4#proibaqdf
  })
  proibaqintensityout<-reactive({
    prodatadf<-read.csv("final_protein_abundance_log2.txt",sep = "\t",stringsAsFactors = FALSE,
                        na.strings = c(""," ","NA","N.A.","NaN","n.a.", "Filtered"))
    prodatadf<-subset(prodatadf,select=str_subset(colnames(prodatadf),"Hippo",negate = T))
    prodatadf<-subset(prodatadf,select=str_subset(colnames(prodatadf),"Plasma",negate = T))
    prodatadf$Proteins<-unlist(lapply(prodatadf$Proteins,function(x){
      strsplit(x,";")[[1]][1]
    }))
    prodatadf$Genes<-unlist(lapply(prodatadf$Genes,function(x){
      strsplit(x,";")[[1]][1]
    }))
    prodatadf<-prodatadf[apply(prodatadf,1,function(x){sum(is.na(x))})<11,]
    data_with_missing_count <- prodatadf %>%
      mutate(missing_count = rowSums(is.na(prodatadf[, -c(1,2)])))
    resultx <- data_with_missing_count %>%
      group_by(Genes) %>%
      filter(missing_count == min(missing_count)) %>%
      ungroup() %>%
      select(-missing_count)%>%
      as.data.frame()
    resultx#prodatadf
  })
  proT50dataibaqout<-reactive({
    proT50datadf<-read.csv("final_protein_T50.txt",sep = "\t",stringsAsFactors = FALSE,
                           na.strings = c(""," ","NA","N.A.","NaN","n.a.", "Filtered"))
    proT50datadf<-subset(proT50datadf,select=str_subset(colnames(proT50datadf),"Hippo",negate = T))
    proT50datadf<-subset(proT50datadf,select=str_subset(colnames(proT50datadf),"Plasma",negate = T))
    proT50datadf<-proT50datadf[apply(proT50datadf,1,function(x){sum(is.na(x))})<11,]
    proT50datadf[,-c(1,2)]<-log2(proT50datadf[,-c(1,2)])
    data_with_missing_count <- proT50datadf %>%
      mutate(missing_count = rowSums(is.na(proT50datadf[, -c(1,2)])))
    resultx <- data_with_missing_count %>%
      group_by(Genes) %>%
      filter(missing_count == min(missing_count)) %>%
      ungroup() %>%
      select(-missing_count)%>%
      as.data.frame()
    if(input$Proteinspecificif){
      ibaqdf4x<-resultx[,-c(1,2)]
      ibaqdf5<-apply(ibaqdf4x,2,rank,na.last="keep")
      ibaqdf6<-sweep(ibaqdf5,2,apply(ibaqdf5, 2, max,na.rm=T),FUN = "/")
      if(input$Proteinrowscaleif){
        ibaqdf6x<-t(scale(t(ibaqdf6)))
        ibaqdf6x1<-t(apply(ibaqdf6x,1,function(x){
          scales::rescale(x,to=c(-1,1))
        }))
        ibaqdf7<-cbind(resultx[,c(1,2)],ibaqdf6x1)
      }else{
        ibaqdf7<-cbind(resultx[,c(1,2)],ibaqdf6)
      }
    }else{
      ibaqdf7<-resultx
    }
    ibaqdf7#proT50datadf
  })
  prodataout<-reactive({
    prodatadf<-read.csv("final_protein_abundance_log2.txt",sep = "\t",stringsAsFactors = FALSE,
                        na.strings = c(""," ","NA","N.A.","NaN","n.a.", "Filtered"))
    prodatadf<-subset(prodatadf,select=str_subset(colnames(prodatadf),"Hippo",negate = T))
    prodatadf<-subset(prodatadf,select=str_subset(colnames(prodatadf),"Plasma",negate = T))
    prodatadf$Proteins<-unlist(lapply(prodatadf$Proteins,function(x){
      strsplit(x,";")[[1]][1]
    }))
    prodatadf$Genes<-unlist(lapply(prodatadf$Genes,function(x){
      strsplit(x,";")[[1]][1]
    }))
    data_with_missing_count <- prodatadf %>%
      mutate(missing_count = rowSums(is.na(prodatadf[, -c(1,2)])))
    resultx <- data_with_missing_count %>%
      group_by(Genes) %>%
      filter(missing_count == min(missing_count)) %>%
      ungroup() %>%
      select(-missing_count)%>%
      as.data.frame()
    prodatadf<-resultx
    prodatadf$Proteins<-paste0(";",prodatadf$Proteins,";")
    prodatadf$Genes<-paste0(";",prodatadf$Genes,";")
    prodatadf<-prodatadf[apply(prodatadf,1,function(x){sum(is.na(x))})<11,]
    prodatadf<-prodatadf[!duplicated(prodatadf$Proteins),]
    prodatadf<-prodatadf[!duplicated(prodatadf$Genes),]
    prodatadf
  })
  proT50dataout<-reactive({
    proT50datadf<-read.csv("final_protein_T50.txt",sep = "\t",stringsAsFactors = FALSE,
                           na.strings = c(""," ","NA","N.A.","NaN","n.a.", "Filtered"))
    proT50datadf<-subset(proT50datadf,select=str_subset(colnames(proT50datadf),"Hippo",negate = T))
    proT50datadf<-subset(proT50datadf,select=str_subset(colnames(proT50datadf),"Plasma",negate = T))
    proT50datadf<-proT50datadf[apply(proT50datadf,1,function(x){sum(is.na(x))})<11,]
    proT50datadf[,-c(1,2)]<-log2(proT50datadf[,-c(1,2)])
    data_with_missing_count <- proT50datadf %>%
      mutate(missing_count = rowSums(is.na(proT50datadf[, -c(1,2)])))
    resultx <- data_with_missing_count %>%
      group_by(Genes) %>%
      filter(missing_count == min(missing_count)) %>%
      ungroup() %>%
      select(-missing_count)%>%
      as.data.frame()
    proT50datadf<-resultx
    proT50datadfxx<-round(proT50datadf[,-c(1,2)],5)
    proT50.lowindex<-which(proT50datadfxx < -1, arr.ind = TRUE)
    proT50.aboveindex<-which(proT50datadfxx >10, arr.ind = TRUE)
    proT50datadfxx[proT50.lowindex]<- -1.1
    proT50datadfxx[proT50.aboveindex]<- 10.1
    proT50datadfxx1<-cbind(proT50datadf[,c(1,2)],proT50datadfxx)
    proT50datadfxx1<-proT50datadfxx1[!duplicated(proT50datadfxx1$Proteins),]
    proT50datadfxx1<-proT50datadfxx1[!duplicated(proT50datadfxx1$Genes),]
    proT50datadfxx1#proT50datadf
  })
  phosdataout<-reactive({
    phosdatadf<-read.csv("final_phosphorylation_abundance_log2.txt",sep = "\t",stringsAsFactors = FALSE,
                         na.strings = c(""," ","NA","N.A.","NaN","n.a.", "Filtered"))
    phosdatadf<-subset(phosdatadf,select=str_subset(colnames(phosdatadf),"Hippo",negate = T))
    phosdatadf<-subset(phosdatadf,select=str_subset(colnames(phosdatadf),"Plasma",negate = T))
    phosdatadf$Proteins<-unlist(lapply(phosdatadf$Proteins,function(x){
      x1<-gsub("^;+","",x)
      strsplit(x1,";")[[1]][1]
    }))
    phosdatadf$Genes<-unlist(lapply(phosdatadf$Genes,function(x){
      x1<-gsub("^;+","",x)
      strsplit(x1,";")[[1]][1]
    }))
    phosdatadf$PTMsite<-unlist(lapply(phosdatadf$PTMsite,function(x){
      x1<-gsub("^;+","",x)
      strsplit(x1,";")[[1]][1]
    }))
    phosdatadf$Proteins<-paste0(";",phosdatadf$Proteins,";")
    phosdatadf$Genes<-paste0(";",phosdatadf$Genes,";")
    phosdatadf<-phosdatadf[apply(phosdatadf,1,function(x){sum(is.na(x))})<11,]
    phosdatadf
  })
  phosT50dataout<-reactive({
    phosT50datadf<-read.csv("final_phosphorylation_T50.txt",sep = "\t",stringsAsFactors = FALSE,
                            na.strings = c(""," ","NA","N.A.","NaN","n.a.", "Filtered"))
    phosT50datadf<-subset(phosT50datadf,select=str_subset(colnames(phosT50datadf),"Hippo",negate = T))
    phosT50datadf<-subset(phosT50datadf,select=str_subset(colnames(phosT50datadf),"Plasma",negate = T))
    phosT50datadf<-phosT50datadf[apply(phosT50datadf,1,function(x){sum(is.na(x))})<11,]
    phosT50datadf[,-c(1:4)]<-log2(phosT50datadf[,-c(1:4)])
    phosT50datadfxx<-round(phosT50datadf[,-c(1:4)],5)
    phosT50.aboveindex<-which(phosT50datadfxx >10, arr.ind = TRUE)
    phosT50datadfxx[phosT50.aboveindex]<-10.1
    phosT50datadfxx1<-cbind(phosT50datadf[,c(1:4)],phosT50datadfxx)
    phosT50datadfxx1#phosT50datadf
  })
  abunbarheightx<-reactive({
    input$abunbarheightx
  })
  abunbarwidthx<-reactive({
    input$abunbarheightx/0.8
  })
  phosheightx<-reactive({
    input$phosheightx
  })
  phoswidthx<-reactive({
    input$phosheightx/0.618
  })
  ###Heatmap
  examplegeneout<-reactive({
    dataread<-read.csv("examplegenes.csv",stringsAsFactors = F,check.names = F)
    dataread
  })
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("ExampleData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplegeneout(),file,row.names = FALSE)
    }
  )
  uploadgenesout<-reactive({
    if(input$loaddatatype==1){
      files1 <<- input$heatmapfile1
      if (is.null(files1)){
        dataread<-data.frame(Description="NO data here. Please upload the genes/IDs, or load the example data to check first.")
      }else{
        dataread<-read.csv(files1$datapath,stringsAsFactors = F)
      }
    }
    if(input$loaddatatype==2){
      zhantieidstr<<-strsplit(input$ID_zhantie,"\n")[[1]]
      zhantieidstr<-zhantieidstr[zhantieidstr!=""]
      #dataread<-data.frame(Input_ID=zhantieidstr[[1]])
      if(length(zhantieidstr)==0){
        dataread<-data.frame(Description="NO data here. Please paste the genes/IDs, or load the example data to check first.")
      }else{
        dataread<-data.frame(Input_ID=zhantieidstr)
      }
    }
    dataread<-data.frame(Inputs=unique(dataread[[1]]))
    dataread
  })
  output$uploadgenestb<-renderDataTable({
    loadtt<<-input$loaddatatype
    #aax<<-examplegeneout()
    if(input$loaddatatype==1 | input$loaddatatype==2){
      datatable(uploadgenesout(), options = list(pageLength = 10))
    }else{
      datatable(examplegeneout(), options = list(pageLength = 10))
    }
  })
  heatmaptableout1<-reactive({
    proibaqoutx<<-proibaqout()
    proibaqoutx<-proibaqoutx[!duplicated(proibaqoutx$Proteins),]
    proibaqoutx<-proibaqoutx[!duplicated(proibaqoutx$Genes),]
    colnames(proibaqoutx)[-c(1,2)]<-paste0("Pro_",colnames(proibaqoutx)[-c(1,2)])
    proT50dataoutx<<-proT50dataout()
    proT50dataoutx<-proT50dataoutx[!duplicated(proT50dataoutx$Proteins),]
    proT50dataoutx<-proT50dataoutx[!duplicated(proT50dataoutx$Genes),]
    colnames(proT50dataoutx)[-c(1,2)]<-paste0("ProT50_",colnames(proT50dataoutx)[-c(1,2)])
    #phosdatadfx<<-phosdataout()
    if(input$loaddatatype==1 | input$loaddatatype==2){
      datadf<-uploadgenesout()
    }else{
      datadf<-examplegeneout()
    }
    datadf<<-datadf
    #proibaqT50df<-base::merge(proibaqoutx,proT50dataoutx,by="Proteins",sort=F,all = T)
    #proibaqT50df
    list(proibaqoutx=proibaqoutx,proT50dataoutx=proT50dataoutx)
  })
  heatmaptableout2<-reactive({
    #proibaqoutx<<-proibaqout()
    if(input$RelativeiBAQif==1){
      proibaqoutx<<-prorelativeibaqout()
    }else if(input$RelativeiBAQif==2){
      proibaqoutx<<-proibaqout()
    }else{
      proibaqoutx<<-proibaqintensityout()
    }
    proibaqoutx<-proibaqoutx[!duplicated(proibaqoutx$Proteins),]
    proibaqoutx<-proibaqoutx[!duplicated(proibaqoutx$Genes),]
    colnames(proibaqoutx)[-c(1,2)]<-paste0("Pro_",colnames(proibaqoutx)[-c(1,2)])
    proT50dataoutx<<-proT50dataibaqout()#proT50dataout()
    proT50dataoutx<-proT50dataoutx[!duplicated(proT50dataoutx$Proteins),]
    proT50dataoutx<-proT50dataoutx[!duplicated(proT50dataoutx$Genes),]
    colnames(proT50dataoutx)[-c(1,2)]<-paste0("ProT50_",colnames(proT50dataoutx)[-c(1,2)])
    if(input$loaddatatype==1 | input$loaddatatype==2){
      datadf<-uploadgenesout()
    }else{
      datadf<-examplegeneout()
    }
    datadf<<-datadf
    proibaqoutx1<-proibaqoutx[toupper(proibaqoutx[[1]])%in%toupper(unique(datadf[[1]])),]
    proibaqoutx2<-proibaqoutx[toupper(proibaqoutx[[2]])%in%toupper(unique(datadf[[1]])),]
    proibaqoutx3<-rbind(proibaqoutx1,proibaqoutx2)
    proT50dataoutx1<-proT50dataoutx[toupper(proT50dataoutx[[1]])%in%toupper(unique(datadf[[1]])),]
    proT50dataoutx2<-proT50dataoutx[toupper(proT50dataoutx[[2]])%in%toupper(unique(datadf[[1]])),]
    proT50dataoutx3<-rbind(proT50dataoutx1,proT50dataoutx2)
    list(proibaqoutx3=proibaqoutx3,proT50dataoutx3=proT50dataoutx3)
  })
  heatmaptableout<-reactive({
    if(input$RelativeiBAQif==1){
      proibaqoutx<<-prorelativeibaqout()
    }else if(input$RelativeiBAQif==2){
      proibaqoutx<<-proibaqout()
    }else{
      proibaqoutx<<-proibaqintensityout()
    }
    proibaqoutx<-proibaqoutx[!duplicated(proibaqoutx$Proteins),]
    proibaqoutx<-proibaqoutx[!duplicated(proibaqoutx$Genes),]
    colnames(proibaqoutx)[-c(1,2)]<-paste0("Pro_",colnames(proibaqoutx)[-c(1,2)])
    proT50dataoutx<<-proT50dataibaqout()#proT50dataout()
    proT50dataoutx<-proT50dataoutx[!duplicated(proT50dataoutx$Proteins),]
    proT50dataoutx<-proT50dataoutx[!duplicated(proT50dataoutx$Genes),]
    colnames(proT50dataoutx)[-c(1,2)]<-paste0("ProT50_",colnames(proT50dataoutx)[-c(1,2)])
    #phosdatadfx<<-phosdataout()
    if(input$loaddatatype==1 | input$loaddatatype==2){
      datadf<-uploadgenesout()
    }else{
      datadf<-examplegeneout()
    }
    proibaqT50df<-proibaqT50df1<-base::merge(proibaqoutx,proT50dataoutx,by="Genes",sort=F,all = T)
    proibaqT50df2<-proibaqT50df1[toupper(proibaqT50df1[[1]])%in%toupper(unique(datadf[[1]])),]
    proibaqT50df3<-proibaqT50df1[toupper(proibaqT50df1[[2]])%in%toupper(unique(datadf[[1]])),]
    proibaqT50df4<<-rbind(proibaqT50df2,proibaqT50df3)
    if(nrow(proibaqT50df4)==0){
      datareadx<-data.frame(Description="NO data here, which means there is no mapping data between the uploaded data and our database.")
    }else{
      datareadx<-proibaqT50df4[,-c(1,2,17)]
      rownames(datareadx)<-proibaqT50df4[,1]
      #datareadx1<-proibaqT50df4[,-c(1,2,17)]
      #datareadx1$Names<-apply(proibaqT50df4[,c(2,17)],1,function(x){
      #  na.omit(x)[1]
      #})
      #datareadx<-datareadx1%>%group_by(Names)%>%
      #  summarise(across(everything(), ~mean(.x,na.rm=T)),
      #            .groups = 'drop')  %>% column_to_rownames(var="Names")%>% 
      #  as.data.frame()
      #rownames(datareadx)
    }
    datareadx
  })
  output$heatmapibaqtable<-renderDataTable({
    heatmaptableout2x<<-heatmaptableout2()
    datatable(heatmaptableout2x$proibaqoutx3)
  })
  output$heatmapibaqtabledl<-downloadHandler(
    filename = function(){paste("Protein.iBAQ.data_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(heatmaptableout2x$proibaqoutx3,file,row.names = F)
    }
  )
  output$heatmaplifetable<-renderDataTable({
    heatmaptableout2x<<-heatmaptableout2()
    datatable(heatmaptableout2x$proT50dataoutx3)
  })
  output$heatmaplifetabledl<-downloadHandler(
    filename = function(){paste("Protein.T50.data_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(heatmaptableout2x$proT50dataoutx3,file,row.names = F)
    }
  )
  heatheightx<-reactive({
    if(input$heatmapheightif){
      input$heatmapheightx
    }else{
      if(input$loaddatatype==1 | input$loaddatatype==2){
        datadf<-uploadgenesout()
      }else{
        datadf<-examplegeneout()
      }
      if(nrow(datadf)<=8){
        450
      }else{
        nrow(datadf)*48
      }
    }
    #if(input$Proteinspecificif){
    #  input$heatmapheightx#*4
    #}else{
    #  input$heatmapheightx
    #}
    #bubbledf<<-heatmaptableout()
    #if(ncol(bubbledf)==1){
    #  768
    #}else{
    #  if(nrow(bubbledf)<=5){
    #    480
    #  }else{
    #    nrow(bubbledf)*90
    #  }
    #}
  })
  heatwidthx<-reactive({
    #input$heatmapheightx*1.3
    if(input$Proteinspecificif){
      864#950
    }else{
      864
    }
  })
  output$heatmapplot<-renderPlot({
    #proibaqT50df1x<<-heatmaptableout1()[,-c(1,2,17)]
    if(input$loaddatatype==1 | input$loaddatatype==2){
      datadf<-uploadgenesout()
    }else{
      datadf<-examplegeneout()
    }
    bubbledf<<-heatmaptableout()
    if(ncol(bubbledf)==1){
      ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Protein iBAQ data!')),size=10, vjust = -10) +
        xlab(NULL)
    }
    #else if(ncol(bubbledf)==14){
    #  if(nrow(bubbledf)==1){
    #    bubbledfx<-reshape2::melt(bubbledf)
    #    ggplot(bubbledfx, aes(x=variable, y=value)) +
    #      geom_bar(stat="identity", position=position_dodge(),fill="#4DBBD5FF",
    #               alpha=0.8,show.legend=F)+
    #      labs(x = "Tissues", y = "Intensity (Log2)")+
    #      theme_bw()+
    #      theme(plot.title = element_text(size=21),
    #            #strip.text = element_text(size=18),
    #            axis.title = element_text(size=17),
    #            axis.text = element_text(size=15),
    #            legend.title=element_text(size=15),
    #            legend.text=element_text(size=14),
    #            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    #  }else{
    #    pheatmap(bubbledf, scale = "row", cluster_rows = TRUE,cluster_cols = FALSE,
    #             color = colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(100),
    #             #color = colorRampPalette(c("green", "black", "red"))(100),
    #             border_color=NA,fontsize=13)
    #  }
    #}
    else{
      bubbledf$Names<-rownames(bubbledf)
      a<-reshape2::melt(bubbledf)
      a$tissue<-a$variable
      a$tissue<-gsub("Pro_","",a$tissue)
      a$tissue<-gsub("ProT50_","",a$tissue)
      a$Group<-a$variable
      a$Group<-gsub("Pro_.*","Abundance",a$Group)
      a$Group<-gsub("ProT50_.*","Lifetime",a$Group)
      colnames(a)<-gsub("Names","Term",colnames(a))
      a<-reshape2::dcast(a[,c(1,3,4,5)],Term+tissue~Group)
      a$tissue<-gsub("Frontal_Cortex","F.C.",a$tissue)
      a$tissue<-gsub("Cerebellum","Ce.",a$tissue)
      a$tissue<-gsub("Entorhinal_Cortex","E.C.",a$tissue)
      a$tissue<-gsub("Amygdala","Am.",a$tissue)
      a$tissue<-gsub("Olfactory_Bulb","O.B.",a$tissue)
      a$tissue<-gsub("Substantia_Nigra","S.N.",a$tissue)
      a$tissue<-gsub("Thalamus","Th.",a$tissue)
      a$tissue<-gsub("Striatum","St.",a$tissue)
      a$tissue<-factor(a$tissue,levels = c("F.C.","Ce.","E.C.","Am.","O.B.","S.N.","Th.","St.",
                                           "Heart","Kidney","Lung","Spleen","Liver","Gut"))
      quantilex1<-a
      uploadnames<-rownames(bubbledf)
      uploadnames1<-unlist(lapply(datadf[[1]],function(x){
        grep(paste0("\\b",x,"\\b"),uploadnames,ignore.case = T,value = T)
      }))
      quantilex1$Term<-factor(quantilex1$Term,levels=rev(uploadnames1))
      if(input$RelativeiBAQif==1){
        #quantilex1 <- quantilex1 %>%
        #  mutate(Abundance_Group = case_when(
        #    Abundance < 0.7 ~ "<0.7",
        #    Abundance > 550 ~ ">550",
        #    TRUE ~ "0.7-550"
        #  ))
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(5,18))
        #quantilex1 <- quantilex1 %>%
        #  mutate(Abundance_Group = case_when(
        #    Abundance < 6 ~ "<6",
        #    Abundance > 16 ~ ">16",
        #    TRUE ~ "6-16"
        #  ))
        quantilex1 <- quantilex1 %>%
          mutate(Abundance_Group = case_when(
            Abundance < -1 ~ "<-1",
            Abundance > 9 ~ ">9",
            TRUE ~ "-1-9"
          ))
        if(sum(quantilex1$Abundance< -1,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance< -1]<- -1.3
        }
        if(sum(quantilex1$Abundance> 9,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance> 9]<- 9.3
        }
        quantilex1<<-quantilex1
        quantilex1$Abundance_Group<-factor(quantilex1$Abundance_Group,levels = c("<-1","-1-9",">9"))
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(4,18))
        #quantilex1$Lifetime<-rescale(quantilex1$Lifetime,to=c(-1,5))
        if(input$Proteinspecificif){
          if(input$Proteinrowscaleif){
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '')+
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   name = 'Row scaled rank percentage',
                                   na.value = "grey") +
              #scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
              #                     name = 'Z score',#Log2 T50
              #                     na.value = "grey") +
              scale_size_continuous(limits = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(2, 12), 
                                    name = expression(Log[2] ~ "(riBAQ in ppm)"),#'Log2(riBAQ in ppm)',#Relative iBAQ
                                    breaks = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)), 
                                               -1,1,3,5,7,9,#6, 8, 10, 12, 14, 16, 
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#5,7
                                    labels = c("< -1","-1","1", "3", "5", "7", "9","> 9")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
              guides(size = guide_legend(override.aes = list(shape = c(24,21,21,21,21,21,21,23),
                                                             labels = c("< -1","-1","1", "3", "5", "7", "9","> 9"))), 
                     shape = "none") +
              scale_shape_manual(values = c("<-1" = 24, "-1-9" = 21, ">9" = 23)) +
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }else{
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '')+
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   midpoint = 0.5,limits = c(0, 1),name = 'Rank percentage (%)',#Log2 T50
                                   na.value = "grey",breaks = c(0,0.25,0.5,0.75,1),
                                   labels = c("0%","25%","50%","75%","100%"))+#"100%","75","50%","25%","0%"
              #scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
              #                     name = 'Z score',#Log2 T50
              #                     na.value = "grey") +
              scale_size_continuous(limits = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(2, 12), 
                                    name = expression(Log[2] ~ "(riBAQ in ppm)"),#'Log2(riBAQ in ppm)',#Relative iBAQ
                                    breaks = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)), 
                                               -1,1,3,5,7,9,#6, 8, 10, 12, 14, 16, 
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#5,7
                                    labels = c("< -1","-1","1", "3", "5", "7", "9","> 9")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
              guides(size = guide_legend(override.aes = list(shape = c(24,21,21,21,21,21,21,23),
                                                             labels = c("< -1","-1","1", "3", "5", "7", "9","> 9"))), 
                     shape = "none") +
              scale_shape_manual(values = c("<-1" = 24, "-1-9" = 21, ">9" = 23)) +
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }
        }else{
          if(sum(quantilex1$Lifetime< -1,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime< -1]<- -1
          }
          if(sum(quantilex1$Lifetime> 5,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime> 5]<- 5
          }
          ggplot(data = quantilex1, aes(tissue,Term,text = Term)) +#reorder(Term, -Abundance)
            geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
            labs(x = '', y = '') +
            scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                 midpoint = 2,limits = c(-1, 5),name = expression(Log[2] ~ "(T50 days)"),#'Log2 T50 (days)',#
                                 na.value = "grey")+
            scale_size_continuous(limits = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)),
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#min(quantilex1$Abundance), max(quantilex1$Abundance)
                                  range = c(2, 12), 
                                  name = expression(Log[2] ~ "(riBAQ in ppm)"),#'Log2(riBAQ in ppm)',#Relative iBAQ
                                  breaks = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)), 
                                             -1,1,3,5,7,9,#6, 8, 10, 12, 14, 16, 
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#5,7
                                  labels = c("< -1","-1","1", "3", "5", "7", "9","> 9")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
            guides(size = guide_legend(override.aes = list(shape = c(24,21,21,21,21,21,21,23),
                                                           labels = c("< -1","-1","1", "3", "5", "7", "9","> 9"))), 
                   shape = "none") +
            scale_shape_manual(values = c("<-1" = 24, "-1-9" = 21, ">9" = 23)) +
            theme_classic() +
            theme(strip.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                  axis.ticks = element_line(linewidth = 1),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                  axis.text.y = element_text(size=15))
        }
      }else if(input$RelativeiBAQif==2){
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(5,17))
        #quantilex1 <- quantilex1 %>%
        #  mutate(Abundance_Group = case_when(
        #    Abundance < 6 ~ "<6",#24,#
        #    Abundance > 16 ~ ">16",#23,#
        #    TRUE ~ "6-16"#21#
        #  ))
        quantilex1 <- quantilex1 %>%
          mutate(Abundance_Group = case_when(
            Abundance < 7 ~ "<7",#24,#
            Abundance > 19 ~ ">19",#23,#
            TRUE ~ "7-19"#21#
          ))
        if(sum(quantilex1$Abundance< 7,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance< 7]<- 6
        }
        if(sum(quantilex1$Abundance> 19,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance> 19]<- 20
        }
        quantilex1<<-quantilex1
        quantilex1$Abundance_Group<-factor(quantilex1$Abundance_Group,levels = c("<7","7-19",">19"))
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(4,18))
        #quantilex1$Lifetime<-rescale(quantilex1$Lifetime,to=c(-1,5))
        if(input$Proteinspecificif){
          if(input$Proteinrowscaleif){
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '') +
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   name = 'Row scaled rank percentage',
                                   na.value = "grey")+
              scale_size_continuous(limits = c(5.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(0.1, 12),
                                    name = expression(Log[2] ~ "(iBAQ)"),#'Log2(iBAQ)',
                                    breaks = c(6.1,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                               7,9,11,13,15,17,19,#8, 10, 12, 14, 16,18,
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))),
                                    labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19")) +#"<8", "8", "10", "12", "14", "16", "18", ">18"
              guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21, 21,21, 21, 23),
                                                             labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19"))), 
                     shape = "none") +
              scale_shape_manual(values = c("<7" = 24, "7-19" = 21, ">19" = 23)) +
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }else{
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '') +
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   midpoint = 0.5,limits = c(0, 1),name = 'Rank percentage (%)',#Log2 T50
                                   na.value = "grey",breaks = c(0,0.25,0.5,0.75,1),
                                   labels = c("0%","25%","50%","75%","100%"))+
              scale_size_continuous(limits = c(5.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(0.1, 12),
                                    name = expression(Log[2] ~ "(iBAQ)"),#'Log2(iBAQ)',
                                    breaks = c(6.1,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                               7,9,11,13,15,17,19,#8, 10, 12, 14, 16,18,
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))),
                                    labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19")) +#"<8", "8", "10", "12", "14", "16", "18", ">18"
              guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21, 21,21, 21, 23),
                                                             labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19"))), 
                     shape = "none") +
              scale_shape_manual(values = c("<7" = 24, "7-19" = 21, ">19" = 23)) +
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }
        }else{
          if(sum(quantilex1$Lifetime< -1,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime< -1]<- -1
          }
          if(sum(quantilex1$Lifetime> 5,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime> 5]<- 5
          }
          ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
            geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
            labs(x = '', y = '') +
            scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                 midpoint = 2,limits = c(-1, 5),name = expression(Log[2] ~ "(T50 days)"),#'Log2 T50 (days)',#
                                 na.value = "grey") +
            scale_size_continuous(limits = c(5.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                  range = c(0.1, 12),
                                  name = expression(Log[2] ~ "(iBAQ)"),#'Log2(iBAQ)',
                                  breaks = c(6.1,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                             7,9,11,13,15,17,19,#8, 10, 12, 14, 16,18,
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))),
                                  labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19")) +#"<8", "8", "10", "12", "14", "16", "18", ">18"
            guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21, 21,21, 21, 23),
                                                           labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19"))), 
                   shape = "none") +
            scale_shape_manual(values = c("<7" = 24, "7-19" = 21, ">19" = 23)) +
            theme_classic() +
            theme(strip.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                  axis.ticks = element_line(linewidth = 1),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                  axis.text.y = element_text(size=15))
        }
      }else{
        quantilex1 <- quantilex1 %>%
          mutate(Abundance_Group = case_when(
            Abundance < 10 ~ "<10",#24,#
            Abundance > 20 ~ ">20",#23,#
            TRUE ~ "10-20"#21#
          ))
        if(sum(quantilex1$Abundance< 10,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance< 10]<- 9
        }
        if(sum(quantilex1$Abundance> 20,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance> 20]<- 21
        }
        quantilex1<<-quantilex1
        quantilex1$Abundance_Group<-factor(quantilex1$Abundance_Group,levels = c("<10","10-20",">20"))
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(4,18))
        #quantilex1$Lifetime<-rescale(quantilex1$Lifetime,to=c(-1,5))
        if(input$Proteinspecificif){
          if(input$Proteinrowscaleif){
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '') +
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   name = 'Row scaled rank percentage',
                                   na.value = "grey")+
              scale_size_continuous(limits = c(8.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(0, 12), 
                                    name = expression(Log[2] ~ "(MS-intensity)"),#'Log2(MS-intensity)',
                                    breaks = c(9.2,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)), 
                                               10,12,14,16,18,20,#6, 8, 10, 12, 14, 16, 
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))),
                                    labels = c("<10","10", "12", "14", "16", "18","20", ">20")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
              guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21,21,21, 23),
                                                             labels = c("<10","10", "12", "14", "16", "18","20", ">20"))), 
                     shape = "none")+
              scale_shape_manual(values = c("<10" = 24, "10-20" = 21, ">20" = 23))+
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }else{
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '') +
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   midpoint = 0.5,limits = c(0, 1),name = 'Rank percentage (%)',#Log2 T50
                                   na.value = "grey",breaks = c(0,0.25,0.5,0.75,1),
                                   labels = c("0%","25%","50%","75%","100%"))+
              scale_size_continuous(limits = c(8.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(0, 12), 
                                    name = expression(Log[2] ~ "(MS-intensity)"),#'Log2(MS-intensity)',
                                    breaks = c(9.2,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)), 
                                               10,12,14,16,18,20,#6, 8, 10, 12, 14, 16, 
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))),
                                    labels = c("<10","10", "12", "14", "16", "18","20", ">20")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
              guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21,21,21, 23),
                                                             labels = c("<10","10", "12", "14", "16", "18","20", ">20"))), 
                     shape = "none")+
              scale_shape_manual(values = c("<10" = 24, "10-20" = 21, ">20" = 23))+
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }
        }else{
          if(sum(quantilex1$Lifetime< -1,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime< -1]<- -1
          }
          if(sum(quantilex1$Lifetime> 5,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime> 5]<- 5
          }
          ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
            geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
            labs(x = '', y = '') +
            scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                 midpoint = 2,limits = c(-1, 5),name = expression(Log[2] ~ "(T50 days)"),#'Log2 T50 (days)',#
                                 na.value = "grey") +
            scale_size_continuous(limits = c(8.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)),
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                  range = c(0, 12), 
                                  name = expression(Log[2] ~ "(MS-intensity)"),#'Log2(MS-intensity)',
                                  breaks = c(9.2,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)), 
                                             10,12,14,16,18,20,#6, 8, 10, 12, 14, 16, 
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))),
                                  labels = c("<10","10", "12", "14", "16", "18","20", ">20")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
            guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21,21,21, 23),
                                                           labels = c("<10","10", "12", "14", "16", "18","20", ">20"))), 
                   shape = "none")+
            scale_shape_manual(values = c("<10" = 24, "10-20" = 21, ">20" = 23)) +
            theme_classic() +
            theme(strip.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                  axis.ticks = element_line(linewidth = 1),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                  axis.text.y = element_text(size=15))
        }
      }
    }
  },height=heatheightx,width = heatwidthx)
  heatmapplotout<-reactive({
    #proibaqT50df1x<<-heatmaptableout1()[,-c(1,2,17)]
    if(input$loaddatatype==1 | input$loaddatatype==2){
      datadf<-uploadgenesout()
    }else{
      datadf<-examplegeneout()
    }
    bubbledf<<-heatmaptableout()
    if(ncol(bubbledf)==1){
      ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Protein iBAQ data!')),size=10, vjust = -10) +
        xlab(NULL)
    }
    #else if(ncol(bubbledf)==14){
    #  if(nrow(bubbledf)==1){
    #    bubbledfx<-reshape2::melt(bubbledf)
    #    ggplot(bubbledfx, aes(x=variable, y=value)) +
    #      geom_bar(stat="identity", position=position_dodge(),fill="#4DBBD5FF",
    #               alpha=0.8,show.legend=F)+
    #      labs(x = "Tissues", y = "Intensity (Log2)")+
    #      theme_bw()+
    #      theme(plot.title = element_text(size=21),
    #            #strip.text = element_text(size=18),
    #            axis.title = element_text(size=17),
    #            axis.text = element_text(size=15),
    #            legend.title=element_text(size=15),
    #            legend.text=element_text(size=14),
    #            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    #  }else{
    #    pheatmap(bubbledf, scale = "row", cluster_rows = TRUE,cluster_cols = FALSE,
    #             color = colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(100),
    #             #color = colorRampPalette(c("green", "black", "red"))(100),
    #             border_color=NA,fontsize=13)
    #  }
    #}
    else{
      bubbledf$Names<-rownames(bubbledf)
      a<-reshape2::melt(bubbledf)
      a$tissue<-a$variable
      a$tissue<-gsub("Pro_","",a$tissue)
      a$tissue<-gsub("ProT50_","",a$tissue)
      a$Group<-a$variable
      a$Group<-gsub("Pro_.*","Abundance",a$Group)
      a$Group<-gsub("ProT50_.*","Lifetime",a$Group)
      colnames(a)<-gsub("Names","Term",colnames(a))
      a<-reshape2::dcast(a[,c(1,3,4,5)],Term+tissue~Group)
      a$tissue<-gsub("Frontal_Cortex","F.C.",a$tissue)
      a$tissue<-gsub("Cerebellum","Ce.",a$tissue)
      a$tissue<-gsub("Entorhinal_Cortex","E.C.",a$tissue)
      a$tissue<-gsub("Amygdala","Am.",a$tissue)
      a$tissue<-gsub("Olfactory_Bulb","O.B.",a$tissue)
      a$tissue<-gsub("Substantia_Nigra","S.N.",a$tissue)
      a$tissue<-gsub("Thalamus","Th.",a$tissue)
      a$tissue<-gsub("Striatum","St.",a$tissue)
      a$tissue<-factor(a$tissue,levels = c("F.C.","Ce.","E.C.","Am.","O.B.","S.N.","Th.","St.",
                                           "Heart","Kidney","Lung","Spleen","Liver","Gut"))
      quantilex1<-a
      uploadnames<-rownames(bubbledf)
      uploadnames1<-unlist(lapply(datadf[[1]],function(x){
        grep(paste0("\\b",x,"\\b"),uploadnames,ignore.case = T,value = T)
      }))
      quantilex1$Term<-factor(quantilex1$Term,levels=rev(uploadnames1))
      if(input$RelativeiBAQif==1){
        #quantilex1 <- quantilex1 %>%
        #  mutate(Abundance_Group = case_when(
        #    Abundance < 0.7 ~ "<0.7",
        #    Abundance > 550 ~ ">550",
        #    TRUE ~ "0.7-550"
        #  ))
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(5,18))
        #quantilex1 <- quantilex1 %>%
        #  mutate(Abundance_Group = case_when(
        #    Abundance < 6 ~ "<6",
        #    Abundance > 16 ~ ">16",
        #    TRUE ~ "6-16"
        #  ))
        quantilex1 <- quantilex1 %>%
          mutate(Abundance_Group = case_when(
            Abundance < -1 ~ "<-1",
            Abundance > 9 ~ ">9",
            TRUE ~ "-1-9"
          ))
        if(sum(quantilex1$Abundance< -1,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance< -1]<- -1.3
        }
        if(sum(quantilex1$Abundance> 9,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance> 9]<- 9.3
        }
        quantilex1<<-quantilex1
        quantilex1$Abundance_Group<-factor(quantilex1$Abundance_Group,levels = c("<-1","-1-9",">9"))
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(4,18))
        #quantilex1$Lifetime<-rescale(quantilex1$Lifetime,to=c(-1,5))
        if(input$Proteinspecificif){
          if(input$Proteinrowscaleif){
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '')+
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   name = 'Row scaled rank percentage',
                                   na.value = "grey") +
              #scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
              #                     name = 'Z score',#Log2 T50
              #                     na.value = "grey") +
              scale_size_continuous(limits = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(2, 12), 
                                    name = expression(Log[2] ~ "(riBAQ in ppm)"),#'Log2(riBAQ in ppm)',#Relative iBAQ
                                    breaks = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)), 
                                               -1,1,3,5,7,9,#6, 8, 10, 12, 14, 16, 
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#5,7
                                    labels = c("< -1","-1","1", "3", "5", "7", "9","> 9")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
              guides(size = guide_legend(override.aes = list(shape = c(24,21,21,21,21,21,21,23),
                                                             labels = c("< -1","-1","1", "3", "5", "7", "9","> 9"))), 
                     shape = "none") +
              scale_shape_manual(values = c("<-1" = 24, "-1-9" = 21, ">9" = 23)) +
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }else{
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '')+
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   midpoint = 0.5,limits = c(0, 1),name = 'Rank percentage (%)',#Log2 T50
                                   na.value = "grey",breaks = c(0,0.25,0.5,0.75,1),
                                   labels = c("0%","25%","50%","75%","100%"))+#"100%","75","50%","25%","0%"
              #scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
              #                     name = 'Z score',#Log2 T50
              #                     na.value = "grey") +
              scale_size_continuous(limits = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(2, 12), 
                                    name = expression(Log[2] ~ "(riBAQ in ppm)"),#'Log2(riBAQ in ppm)',#Relative iBAQ
                                    breaks = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)), 
                                               -1,1,3,5,7,9,#6, 8, 10, 12, 14, 16, 
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#5,7
                                    labels = c("< -1","-1","1", "3", "5", "7", "9","> 9")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
              guides(size = guide_legend(override.aes = list(shape = c(24,21,21,21,21,21,21,23),
                                                             labels = c("< -1","-1","1", "3", "5", "7", "9","> 9"))), 
                     shape = "none") +
              scale_shape_manual(values = c("<-1" = 24, "-1-9" = 21, ">9" = 23)) +
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }
          #pplots <- lapply(levels(quantilex1$tissue), function(tissue_type) {
          #  aaax<-quantilex1[quantilex1$tissue == tissue_type, ]
          #  ggplot(data = aaax, aes(tissue, Term, text = Term)) +
          #    geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
          #    
          #    labs(x = '', y = '') +
          #    scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
          #                         midpoint = median(aaax$Lifetime),name = 'Log2 T50 (days)',#
          #                         na.value = "grey") +
          #    scale_size_continuous(limits = c(5,17), #min(quantilex1$Abundance), max(quantilex1$Abundance)
          #                          range = c(3, 16), 
          #                          name = 'Log2(ppm)',
          #                          breaks = c(5, 6, 8, 10, 12, 14, 16, 17),
          #                          labels = c("<6", "6", "8", "10", "12", "14", "16", ">16"),guide="none") +
          #    scale_shape_manual(values = c("<6" = 24, "6-16" = 21, ">16" = 23),guide="none") +
          #    theme_classic() +
          #    theme(strip.background = element_blank(),
          #          panel.grid = element_blank(),
          #          axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
          #          axis.ticks = element_line(linewidth = 1),
          #          axis.text.x = element_text(size=15),#angle = 0, vjust = 1, hjust = 1,
          #          axis.text.y = element_text(size=15))
          #})
          #ibaqlegend<-ggplot(data = quantilex1[quantilex1$tissue == levels(quantilex1$tissue)[1], ], aes(tissue, reorder(Term, -Abundance), text = Term)) +
          #  geom_point(aes(size = Abundance, shape = Abundance_Group), color = "black") +
          #  labs(x = '', y = '') +
          #  scale_size_continuous(limits = c(5,17), #min(quantilex1$Abundance), max(quantilex1$Abundance)
          #                        range = c(3, 16), 
          #                        name = 'Log2(ppm)',
          #                        breaks = c(5, 6, 8, 10, 12, 14, 16, 17),
          #                        labels = c("<6", "6", "8", "10", "12", "14", "16", ">16")) +
          #  guides(size = guide_legend(override.aes = list(shape = c(24,21,21,21,21,21,21,23),
          #                                                 labels = c("<6", "6", "8", "10", "12", "14", "16", ">16")),ncol = 4),shape = "none") +
          #  theme_classic() +
          #  theme(strip.background = element_blank(),
          #        panel.grid = element_blank(),
          #        axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
          #        axis.ticks = element_line(linewidth = 1),
          #        axis.text.x = element_text(size=15),#angle = 0, vjust = 1, hjust = 1,
          #        axis.text.y = element_text(size=15))
          #pplots[[15]]<-as_ggplot(get_legend(ibaqlegend))
          #combined_plot <- wrap_plots(pplots, ncol = 3)
          #combined_plot
        }else{
          if(sum(quantilex1$Lifetime< -1,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime< -1]<- -1
          }
          if(sum(quantilex1$Lifetime> 5,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime> 5]<- 5
          }
          ggplot(data = quantilex1, aes(tissue,Term,text = Term)) +#reorder(Term, -Abundance)
            geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
            labs(x = '', y = '') +
            scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                 midpoint = 2,limits = c(-1, 5),name = expression(Log[2] ~ "(T50 days)"),#'Log2 T50 (days)',#
                                 na.value = "grey")+
            scale_size_continuous(limits = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)),
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#min(quantilex1$Abundance), max(quantilex1$Abundance)
                                  range = c(2, 12), 
                                  name = expression(Log[2] ~ "(riBAQ in ppm)"),#'Log2(riBAQ in ppm)',#Relative iBAQ
                                  breaks = c(ifelse(min(quantilex1$Abundance,na.rm = T)>-1,-1.3,min(quantilex1$Abundance,na.rm = T)), 
                                             -1,1,3,5,7,9,#6, 8, 10, 12, 14, 16, 
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<9,9.3,max(quantilex1$Abundance,na.rm = T))),#5,7
                                  labels = c("< -1","-1","1", "3", "5", "7", "9","> 9")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
            guides(size = guide_legend(override.aes = list(shape = c(24,21,21,21,21,21,21,23),
                                                           labels = c("< -1","-1","1", "3", "5", "7", "9","> 9"))), 
                   shape = "none") +
            scale_shape_manual(values = c("<-1" = 24, "-1-9" = 21, ">9" = 23)) +
            theme_classic() +
            theme(strip.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                  axis.ticks = element_line(linewidth = 1),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                  axis.text.y = element_text(size=15))
        }
      }else if(input$RelativeiBAQif==2){
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(5,17))
        #quantilex1 <- quantilex1 %>%
        #  mutate(Abundance_Group = case_when(
        #    Abundance < 6 ~ "<6",#24,#
        #    Abundance > 16 ~ ">16",#23,#
        #    TRUE ~ "6-16"#21#
        #  ))
        quantilex1 <- quantilex1 %>%
          mutate(Abundance_Group = case_when(
            Abundance < 7 ~ "<7",#24,#
            Abundance > 19 ~ ">19",#23,#
            TRUE ~ "7-19"#21#
          ))
        if(sum(quantilex1$Abundance< 7,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance< 7]<- 6
        }
        if(sum(quantilex1$Abundance> 19,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance> 19]<- 20
        }
        quantilex1<<-quantilex1
        quantilex1$Abundance_Group<-factor(quantilex1$Abundance_Group,levels = c("<7","7-19",">19"))
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(4,18))
        #quantilex1$Lifetime<-rescale(quantilex1$Lifetime,to=c(-1,5))
        if(input$Proteinspecificif){
          if(input$Proteinrowscaleif){
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '') +
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   name = 'Row scaled rank percentage',
                                   na.value = "grey")+
              scale_size_continuous(limits = c(5.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(0.1, 12),
                                    name = expression(Log[2] ~ "(iBAQ)"),#'Log2(iBAQ)',
                                    breaks = c(6.1,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                               7,9,11,13,15,17,19,#8, 10, 12, 14, 16,18,
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))),
                                    labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19")) +#"<8", "8", "10", "12", "14", "16", "18", ">18"
              guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21, 21,21, 21, 23),
                                                             labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19"))), 
                     shape = "none") +
              scale_shape_manual(values = c("<7" = 24, "7-19" = 21, ">19" = 23)) +
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }else{
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '') +
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   midpoint = 0.5,limits = c(0, 1),name = 'Rank percentage (%)',#Log2 T50
                                   na.value = "grey",breaks = c(0,0.25,0.5,0.75,1),
                                   labels = c("0%","25%","50%","75%","100%"))+
              scale_size_continuous(limits = c(5.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(0.1, 12),
                                    name = expression(Log[2] ~ "(iBAQ)"),#'Log2(iBAQ)',
                                    breaks = c(6.1,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                               7,9,11,13,15,17,19,#8, 10, 12, 14, 16,18,
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))),
                                    labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19")) +#"<8", "8", "10", "12", "14", "16", "18", ">18"
              guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21, 21,21, 21, 23),
                                                             labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19"))), 
                     shape = "none") +
              scale_shape_manual(values = c("<7" = 24, "7-19" = 21, ">19" = 23)) +
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }
        }else{
          if(sum(quantilex1$Lifetime< -1,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime< -1]<- -1
          }
          if(sum(quantilex1$Lifetime> 5,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime> 5]<- 5
          }
          ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
            geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
            labs(x = '', y = '') +
            scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                 midpoint = 2,limits = c(-1, 5),name = expression(Log[2] ~ "(T50 days)"),#'Log2 T50 (days)',#
                                 na.value = "grey") +
            scale_size_continuous(limits = c(5.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                  range = c(0.1, 12),
                                  name = expression(Log[2] ~ "(iBAQ)"),#'Log2(iBAQ)',
                                  breaks = c(6.1,#ifelse(min(quantilex1$Abundance,na.rm = T)>7,5,min(quantilex1$Abundance,na.rm = T)),
                                             7,9,11,13,15,17,19,#8, 10, 12, 14, 16,18,
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<19,20,max(quantilex1$Abundance,na.rm = T))),
                                  labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19")) +#"<8", "8", "10", "12", "14", "16", "18", ">18"
            guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21, 21,21, 21, 23),
                                                           labels = c("<7", "7", "9", "11", "13", "15", "17","19", ">19"))), 
                   shape = "none") +
            scale_shape_manual(values = c("<7" = 24, "7-19" = 21, ">19" = 23)) +
            theme_classic() +
            theme(strip.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                  axis.ticks = element_line(linewidth = 1),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                  axis.text.y = element_text(size=15))
        }
      }else{
        quantilex1 <- quantilex1 %>%
          mutate(Abundance_Group = case_when(
            Abundance < 10 ~ "<10",#24,#
            Abundance > 20 ~ ">20",#23,#
            TRUE ~ "10-20"#21#
          ))
        if(sum(quantilex1$Abundance< 10,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance< 10]<- 9
        }
        if(sum(quantilex1$Abundance> 20,na.rm = T)>0){
          quantilex1$Abundance[quantilex1$Abundance> 20]<- 21
        }
        quantilex1<<-quantilex1
        quantilex1$Abundance_Group<-factor(quantilex1$Abundance_Group,levels = c("<10","10-20",">20"))
        #quantilex1$Abundance<-scales::rescale(quantilex1$Abundance,to=c(4,18))
        #quantilex1$Lifetime<-rescale(quantilex1$Lifetime,to=c(-1,5))
        if(input$Proteinspecificif){
          if(input$Proteinrowscaleif){
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '') +
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   name = 'Row scaled rank percentage',
                                   na.value = "grey")+
              scale_size_continuous(limits = c(8.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(0, 12), 
                                    name = expression(Log[2] ~ "(MS-intensity)"),#'Log2(MS-intensity)',
                                    breaks = c(9.2,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)), 
                                               10,12,14,16,18,20,#6, 8, 10, 12, 14, 16, 
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))),
                                    labels = c("<10","10", "12", "14", "16", "18","20", ">20")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
              guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21,21,21, 23),
                                                             labels = c("<10","10", "12", "14", "16", "18","20", ">20"))), 
                     shape = "none")+
              scale_shape_manual(values = c("<10" = 24, "10-20" = 21, ">20" = 23))+
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }else{
            ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
              geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
              labs(x = '', y = '') +
              scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                   midpoint = 0.5,limits = c(0, 1),name = 'Rank percentage (%)',#Log2 T50
                                   na.value = "grey",breaks = c(0,0.25,0.5,0.75,1),
                                   labels = c("0%","25%","50%","75%","100%"))+
              scale_size_continuous(limits = c(8.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)),
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                    range = c(0, 12), 
                                    name = expression(Log[2] ~ "(MS-intensity)"),#'Log2(MS-intensity)',
                                    breaks = c(9.2,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)), 
                                               10,12,14,16,18,20,#6, 8, 10, 12, 14, 16, 
                                               ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))),
                                    labels = c("<10","10", "12", "14", "16", "18","20", ">20")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
              guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21,21,21, 23),
                                                             labels = c("<10","10", "12", "14", "16", "18","20", ">20"))), 
                     shape = "none")+
              scale_shape_manual(values = c("<10" = 24, "10-20" = 21, ">20" = 23))+
              theme_classic() +
              theme(strip.background = element_blank(),
                    panel.grid = element_blank(),
                    axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                    axis.ticks = element_line(linewidth = 1),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                    axis.text.y = element_text(size=15))
          }
        }else{
          if(sum(quantilex1$Lifetime< -1,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime< -1]<- -1
          }
          if(sum(quantilex1$Lifetime> 5,na.rm = T)>0){
            quantilex1$Lifetime[quantilex1$Lifetime> 5]<- 5
          }
          ggplot(data = quantilex1, aes(tissue, Term, text = Term)) +#reorder(Term, -Abundance)
            geom_point(aes(size = Abundance, fill = Lifetime, shape = Abundance_Group), color = "black") +
            labs(x = '', y = '') +
            scale_fill_gradient2(low = '#DD2624',mid = 'white',high = '#206FB0',
                                 midpoint = 2,limits = c(-1, 5),name = expression(Log[2] ~ "(T50 days)"),#'Log2 T50 (days)',#
                                 na.value = "grey") +
            scale_size_continuous(limits = c(8.8,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)),
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))), #min(quantilex1$Abundance), max(quantilex1$Abundance)
                                  range = c(0, 12), 
                                  name = expression(Log[2] ~ "(MS-intensity)"),#'Log2(MS-intensity)',
                                  breaks = c(9.2,#ifelse(min(quantilex1$Abundance,na.rm = T)>10,8,min(quantilex1$Abundance,na.rm = T)), 
                                             10,12,14,16,18,20,#6, 8, 10, 12, 14, 16, 
                                             ifelse(max(quantilex1$Abundance,na.rm = T)<20,21,max(quantilex1$Abundance,na.rm = T))),
                                  labels = c("<10","10", "12", "14", "16", "18","20", ">20")) +#"<6", "6", "8", "10", "12", "14", "16", ">16"
            guides(size = guide_legend(override.aes = list(shape = c(24, 21, 21, 21, 21,21,21, 23),
                                                           labels = c("<10","10", "12", "14", "16", "18","20", ">20"))), 
                   shape = "none")+
            scale_shape_manual(values = c("<10" = 24, "10-20" = 21, ">20" = 23)) +
            theme_classic() +
            theme(strip.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title.x = element_text(family = 'sans', face = 'bold', size = 15),
                  axis.ticks = element_line(linewidth = 1),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=15),
                  axis.text.y = element_text(size=15))
        }
      }
    }
  })
  output$heatmapfiguredl<-downloadHandler(
    filename = function(){paste("HC.Plot",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width = heatwidthx()/80,height = heatheightx()/75)
      print(heatmapplotout())
      dev.off()
    }
  )
  ##Heatmap
  uploadgenesx2out<-reactive({
    if(input$loaddatatypex2==1){
      files1 <<- input$heatmapfile1x2
      if (is.null(files1)){
        dataread<-data.frame(Description="NO data here. Please upload the genes/IDs, or load the example data to check first.")
      }else{
        dataread<-read.csv(files1$datapath,stringsAsFactors = F)
      }
    }
    if(input$loaddatatypex2==2){
      zhantieidstr<<-strsplit(input$ID_zhantiex2,"\n")[[1]]
      zhantieidstr<-zhantieidstr[zhantieidstr!=""]
      #dataread<-data.frame(Input_ID=zhantieidstr[[1]])
      if(length(zhantieidstr)==0){
        dataread<-data.frame(Description="NO data here. Please paste the genes/IDs, or load the example data to check first.")
      }else{
        dataread<-data.frame(Input_ID=zhantieidstr)
      }
    }
    dataread<-data.frame(Inputs=unique(dataread[[1]]))
    dataread
  })
  output$uploadgenestbx2<-renderDataTable({
    loadtt<<-input$loaddatatypex2
    #aax<<-examplegeneout()
    if(input$loaddatatypex2==1 | input$loaddatatypex2==2){
      datatable(uploadgenesx2out(), options = list(pageLength = 10))
    }else{
      datatable(examplegeneout(), options = list(pageLength = 10))
    }
  })
  output$loaddatadownload1x2<-downloadHandler(
    filename = function(){paste("ExampleData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplegeneout(),file,row.names = FALSE)
    }
  )
  heatmapdatax2out<-reactive({
    prodatadfx<<-prodataout()
    prodatadfx<-prodatadfx[!duplicated(prodatadfx$Proteins),]
    prodatadfx<-prodatadfx[!duplicated(prodatadfx$Genes),]
    proT50datadfx<<-proT50dataout()
    proT50datadfx<-proT50datadfx[!duplicated(proT50datadfx$Proteins),]
    proT50datadfx<-proT50datadfx[!duplicated(proT50datadfx$Genes),]
    phosdatadfx<<-phosdataout()
    phosT50datadfx<<-phosT50dataout()
    if(input$loaddatatypex2==1 | input$loaddatatypex2==2){
      datadf<-uploadgenesx2out()
    }else{
      datadf<-examplegeneout()
    }
    datadf<<-datadf
    proabunoutx1<-prodatadfx[unlist(lapply(toupper(unique(datadf[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,prodatadfx[[1]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]
    #proabunoutx2<-prodatadfx[toupper(prodatadfx[[2]])%in%toupper(unique(datadf[[1]])),]
    proabunoutx2<-prodatadfx[unlist(lapply(toupper(unique(datadf[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,prodatadfx[[2]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]
    proabunoutx3<-rbind(proabunoutx1,proabunoutx2)
    proabunoutx3[[1]]<-gsub("^;|;$","",proabunoutx3[[1]])
    proabunoutx3[[2]]<-gsub("^;|;$","",proabunoutx3[[2]])
    proT50dataoutx1<-proT50datadfx[toupper(proT50datadfx[[1]])%in%toupper(unique(datadf[[1]])),]
    proT50dataoutx2<-proT50datadfx[toupper(proT50datadfx[[2]])%in%toupper(unique(datadf[[1]])),]
    proT50dataoutx3<-rbind(proT50dataoutx1,proT50dataoutx2)
    phosdatadfx1<-phosdatadfx[unlist(lapply(toupper(unique(datadf[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,phosdatadfx[[1]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]#phosdatadfx[toupper(phosdatadfx[[1]])%in%toupper(unique(datadf[[1]])),]
    phosdatadfx2<-phosdatadfx[unlist(lapply(toupper(unique(datadf[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,phosdatadfx[[2]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]#phosdatadfx[toupper(phosdatadfx[[2]])%in%toupper(unique(datadf[[1]])),]
    phosdatadfx3<-rbind(phosdatadfx1,phosdatadfx2)
    phosdatadfx3[[1]]<-gsub("^;|;$","",phosdatadfx3[[1]])
    phosdatadfx3[[2]]<-gsub("^;|;$","",phosdatadfx3[[2]])
    phosT50datadfx1<-phosT50datadfx[toupper(phosT50datadfx[[1]])%in%toupper(unique(datadf[[1]])),]
    phosT50datadfx2<-phosT50datadfx[toupper(phosT50datadfx[[2]])%in%toupper(unique(datadf[[1]])),]
    phosT50datadfx3<-rbind(phosT50datadfx1,phosT50datadfx2)
    list(proabunoutx3=proabunoutx3,proT50dataoutx3=proT50dataoutx3,
         phosdatadfx3=phosdatadfx3,phosT50datadfx3=phosT50datadfx3)
  })
  output$heatmapibaqtablex2<-renderDataTable({
    aaxx2<<-heatmapdatax2out()
    datatable(aaxx2$proabunoutx3)
  })
  output$heatmapibaqtablex2dl<-downloadHandler(
    filename = function(){paste("Protein.Abundance.data_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(aaxx2$proabunoutx3,file,row.names = F)
    }
  )
  ##2
  output$heatmaplifetablex2<-renderDataTable({
    aaxx2<<-heatmapdatax2out()
    datatable(aaxx2$proT50dataoutx3)
  })
  output$heatmaplifetablex2dl<-downloadHandler(
    filename = function(){paste("Protein.T50.data_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(aaxx2$proT50dataoutx3,file,row.names = F)
    }
  )
  ##3
  output$phosheatmapibaqtablex2<-renderDataTable({
    aaxx2<<-heatmapdatax2out()
    datatable(aaxx2$phosdatadfx3)
  })
  output$phosheatmapibaqtablex2dl<-downloadHandler(
    filename = function(){paste("Phospho.Abundance.data_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(aaxx2$phosdatadfx3,file,row.names = F)
    }
  )
  ##4
  output$phosheatmaplifetablex2<-renderDataTable({
    aaxx2<<-heatmapdatax2out()
    datatable(aaxx2$phosT50datadfx3)
  })
  output$phosheatmaplifetablex2dl<-downloadHandler(
    filename = function(){paste("Phospho.T50.data_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(aaxx2$phosT50datadfx3,file,row.names = F)
    }
  )
  heatheightxx2<-reactive({
    input$heatmapheightxx2
  })
  heatwidthxx2<-reactive({
    input$heatmapheightxx2*0.9
  })
  output$heatmapplotx2<-renderPlot({
    library(ggplotify)
    prodatadfx<<-prodataout()
    #stats::quantile(prodatadfx[,-c(1,2)],probs=c(0.025,0.975),na.rm=T)
    proT50datadfx<<-proT50dataout()
    phosdatadfx<<-phosdataout()
    phosT50datadfx<<-phosT50dataout()
    aaxx2<<-heatmapdatax2out()
    proabunoutx3<-aaxx2$proabunoutx3
    if(nrow(proabunoutx3)==0){
      ppi1<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Protein abundance data!')),size=10) +
        xlab(NULL)
    }else{
      proabunoutx4<-proabunoutx3[,-1]%>%group_by(Genes)%>%
        summarise(across(everything(), ~mean(.x,na.rm=T)),
                  .groups = 'drop')  %>% column_to_rownames(var="Genes")%>% 
        as.data.frame()
      #proabunoutx4<-proabunoutx3[,-c(1,2)]
      #rownames(proabunoutx4)<-proabunoutx3[,2]
      breaksList <- seq(9, 18, by = 0.1)
      pp1<-tryCatch({
        pheatmap(proabunoutx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = T,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#F39B7FFF","white","#00A087FF"))(length(breaksList)))
      },
      error=function(e){
        pheatmap(proabunoutx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = F,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#F39B7FFF","white","#00A087FF"))(length(breaksList)))
        #ggplot() +
        #  theme_void() +
        #  geom_text(aes(0,0,label=paste0('Nothing here!')),size=10) +
        #  xlab(NULL)
      })
      ppi1<-as.ggplot(pp1)+ggtitle("Protein abundace")
    }
    proT50dataoutx3<-aaxx2$proT50dataoutx3
    if(nrow(proT50dataoutx3)==0){
      ppi2<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Protein lifetime data!')),size=10) +
        xlab(NULL)
    }else{
      #proT50dataoutx4<-proT50dataoutx3[,-c(1,2)]
      #rownames(proT50dataoutx4)<-proT50dataoutx3[,2]
      proT50dataoutx4<-proT50dataoutx3[,-1]%>%group_by(Genes)%>%
        summarise(across(everything(), ~mean(.x,na.rm=T)),
                  .groups = 'drop')  %>% column_to_rownames(var="Genes")%>% 
        as.data.frame()
      breaksList <- seq(-1, 5, by = 0.1)
      pp2<-tryCatch({
        pheatmap(proT50dataoutx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = T,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#DD2624","white","#206FB0"))(length(breaksList)))
      },
      error=function(e){
        pheatmap(proT50dataoutx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = F,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#DD2624","white","#206FB0"))(length(breaksList)))
        #ggplot() +
        #  theme_void() +
        #  geom_text(aes(0,0,label=paste0('Nothing here!')),size=10) +
        #  xlab(NULL)
      })
      ppi2<-as.ggplot(pp2)+ggtitle("Protein T50")
    }
    phosdatadfx3<-aaxx2$phosdatadfx3
    if(nrow(phosdatadfx3)==0){
      ppi3<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Phospho abundance data!')),size=10) +
        xlab(NULL)
    }else{
      phosdatadfx4<-phosdatadfx3[,-c(1:4)]
      phosdatadfx4$Names<-paste0(phosdatadfx3[,2],"_",phosdatadfx3[,4])
      #rownames(phosdatadfx4)<-paste0(phosdatadfx3[,2],"_",phosdatadfx3[,4])
      phosdatadfx4<-phosdatadfx4%>%group_by(Names)%>%
        summarise(across(everything(), ~mean(.x,na.rm=T)),
                  .groups = 'drop')  %>% column_to_rownames(var="Names")%>% 
        as.data.frame()
      breaksList <- seq(8, 18, by = 0.1)
      pp3<-tryCatch({
        pheatmap(phosdatadfx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = T,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#A73030FF","white","#996600FF"))(length(breaksList)))
      },
      error=function(e){
        pheatmap(phosdatadfx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = F,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#A73030FF","white","#996600FF"))(length(breaksList)))
        #ggplot() +
        #  theme_void() +
        #  geom_text(aes(0,0,label=paste0('Nothing here!')),size=10) +
        #  xlab(NULL)
      })
      ppi3<-as.ggplot(pp3)+ggtitle("Phospho abundance")
    }
    phosT50datadfx3<-aaxx2$phosT50datadfx3
    if(nrow(phosT50datadfx3)==0){
      ppi4<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Phospho T50 data!')),size=10) +
        xlab(NULL)
    }else{
      phosT50datadfx4<-phosT50datadfx3[,-c(1:4)]
      #rownames(phosT50datadfx4)<-paste0(phosT50datadfx3[,2],"_",phosT50datadfx3[,4])
      phosT50datadfx4$Names<-paste0(phosT50datadfx3[,2],"_",phosT50datadfx3[,4])
      #rownames(phosdatadfx4)<-paste0(phosdatadfx3[,2],"_",phosdatadfx3[,4])
      phosT50datadfx4<-phosT50datadfx4%>%group_by(Names)%>%
        summarise(across(everything(), ~mean(.x,na.rm=T)),
                  .groups = 'drop')  %>% column_to_rownames(var="Names")%>% 
        as.data.frame()
      #phosT50datadfx4[is.na(phosT50datadfx4)]<-min(phosT50datadfx4,na.rm = T)
      breaksList <- seq(0, 85, by = 1)
      pp4<-tryCatch({
        pheatmap(phosT50datadfx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = T,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#A20056FF","white","#FF9900FF"))(length(breaksList)))
      },
      error=function(e){
        pheatmap(phosT50datadfx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = F,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#A20056FF","white","#FF9900FF"))(length(breaksList)))
        #ggplot() +
        #  theme_void() +
        #  geom_text(aes(0,0,label=paste0('Nothing here!')),size=10) +
        #  xlab(NULL)
      })
      ppi4<-as.ggplot(pp4)+ggtitle("Phospho T50")
    }
    (ppi1+ppi2)/(ppi3+ppi4)
  },height=heatheightxx2,width = heatwidthxx2)
  heatmapplotx2out<-reactive({
    library(ggplotify)
    prodatadfx<<-prodataout()
    #stats::quantile(prodatadfx[,-c(1,2)],probs=c(0.025,0.975),na.rm=T)
    proT50datadfx<<-proT50dataout()
    phosdatadfx<<-phosdataout()
    phosT50datadfx<<-phosT50dataout()
    aaxx2<<-heatmapdatax2out()
    proabunoutx3<-aaxx2$proabunoutx3
    if(nrow(proabunoutx3)==0){
      ppi1<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Protein abundance data!')),size=10) +
        xlab(NULL)
    }else{
      proabunoutx4<-proabunoutx3[,-1]%>%group_by(Genes)%>%
        summarise(across(everything(), ~mean(.x,na.rm=T)),
                  .groups = 'drop')  %>% column_to_rownames(var="Genes")%>% 
        as.data.frame()
      #proabunoutx4<-proabunoutx3[,-c(1,2)]
      #rownames(proabunoutx4)<-proabunoutx3[,2]
      breaksList <- seq(9, 18, by = 0.1)
      pp1<-tryCatch({
        pheatmap(proabunoutx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = T,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#F39B7FFF","white","#00A087FF"))(length(breaksList)))
      },
      error=function(e){
        pheatmap(proabunoutx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = F,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#F39B7FFF","white","#00A087FF"))(length(breaksList)))
        #ggplot() +
        #  theme_void() +
        #  geom_text(aes(0,0,label=paste0('Nothing here!')),size=10) +
        #  xlab(NULL)
      })
      ppi1<-as.ggplot(pp1)+ggtitle("Protein abundace")
    }
    proT50dataoutx3<-aaxx2$proT50dataoutx3
    if(nrow(proT50dataoutx3)==0){
      ppi2<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Protein lifetime data!')),size=10) +
        xlab(NULL)
    }else{
      #proT50dataoutx4<-proT50dataoutx3[,-c(1,2)]
      #rownames(proT50dataoutx4)<-proT50dataoutx3[,2]
      proT50dataoutx4<-proT50dataoutx3[,-1]%>%group_by(Genes)%>%
        summarise(across(everything(), ~mean(.x,na.rm=T)),
                  .groups = 'drop')  %>% column_to_rownames(var="Genes")%>% 
        as.data.frame()
      breaksList <- seq(-1, 5, by = 0.1)
      pp2<-tryCatch({
        pheatmap(proT50dataoutx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = T,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#DD2624","white","#206FB0"))(length(breaksList)))
      },
      error=function(e){
        pheatmap(proT50dataoutx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = F,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#DD2624","white","#206FB0"))(length(breaksList)))
        #ggplot() +
        #  theme_void() +
        #  geom_text(aes(0,0,label=paste0('Nothing here!')),size=10) +
        #  xlab(NULL)
      })
      ppi2<-as.ggplot(pp2)+ggtitle("Protein T50")
    }
    phosdatadfx3<-aaxx2$phosdatadfx3
    if(nrow(phosdatadfx3)==0){
      ppi3<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Phospho abundance data!')),size=10) +
        xlab(NULL)
    }else{
      phosdatadfx4<-phosdatadfx3[,-c(1:4)]
      phosdatadfx4$Names<-paste0(phosdatadfx3[,2],"_",phosdatadfx3[,4])
      #rownames(phosdatadfx4)<-paste0(phosdatadfx3[,2],"_",phosdatadfx3[,4])
      phosdatadfx4<-phosdatadfx4%>%group_by(Names)%>%
        summarise(across(everything(), ~mean(.x,na.rm=T)),
                  .groups = 'drop')  %>% column_to_rownames(var="Names")%>% 
        as.data.frame()
      breaksList <- seq(8, 18, by = 0.1)
      pp3<-tryCatch({
        pheatmap(phosdatadfx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = T,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#A73030FF","white","#996600FF"))(length(breaksList)))
      },
      error=function(e){
        pheatmap(phosdatadfx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = F,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#A73030FF","white","#996600FF"))(length(breaksList)))
        #ggplot() +
        #  theme_void() +
        #  geom_text(aes(0,0,label=paste0('Nothing here!')),size=10) +
        #  xlab(NULL)
      })
      ppi3<-as.ggplot(pp3)+ggtitle("Phospho abundance")
    }
    phosT50datadfx3<-aaxx2$phosT50datadfx3
    if(nrow(phosT50datadfx3)==0){
      ppi4<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label=paste0('Nothing found in the Phospho T50 data!')),size=10) +
        xlab(NULL)
    }else{
      phosT50datadfx4<-phosT50datadfx3[,-c(1:4)]
      #rownames(phosT50datadfx4)<-paste0(phosT50datadfx3[,2],"_",phosT50datadfx3[,4])
      phosT50datadfx4$Names<-paste0(phosT50datadfx3[,2],"_",phosT50datadfx3[,4])
      #rownames(phosdatadfx4)<-paste0(phosdatadfx3[,2],"_",phosdatadfx3[,4])
      phosT50datadfx4<-phosT50datadfx4%>%group_by(Names)%>%
        summarise(across(everything(), ~mean(.x,na.rm=T)),
                  .groups = 'drop')  %>% column_to_rownames(var="Names")%>% 
        as.data.frame()
      #phosT50datadfx4[is.na(phosT50datadfx4)]<-min(phosT50datadfx4,na.rm = T)
      breaksList <- seq(0, 85, by = 1)
      pp4<-tryCatch({
        pheatmap(phosT50datadfx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = T,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#A20056FF","white","#FF9900FF"))(length(breaksList)))
      },
      error=function(e){
        pheatmap(phosT50datadfx4,scale="none",clustering_distance_rows="euclidean",
                 show_rownames = T, show_colnames = T,
                 cluster_rows = F,cluster_cols = F,#breaks = breaksList,
                 color = colorRampPalette(c("#A20056FF","white","#FF9900FF"))(length(breaksList)))
        #ggplot() +
        #  theme_void() +
        #  geom_text(aes(0,0,label=paste0('Nothing here!')),size=10) +
        #  xlab(NULL)
      })
      ppi4<-as.ggplot(pp4)+ggtitle("Phospho T50")
    }
    (ppi1+ppi2)/(ppi3+ppi4)
  })
  output$heatmapfigurex2dl<-downloadHandler(
    filename = function(){paste("Heatmap.Plot",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width = heatwidthxx2()/100,height = heatheightxx2()/100)
      print(heatmapplotx2out())
      dev.off()
    }
  )
  ##
  humantomouseout<-reactive({
    humantomousex<-read.csv("humantomouse.mart_export.txt")
    humantomousex[[1]]<-paste0(";",humantomousex[[1]],";")
    humantomousex[[2]]<-paste0(";",humantomousex[[2]],";")
    humantomousex
  })
  observeEvent(input$proinputids_btn,{
    output$proinputidsplot<-renderPlot({
      humantomousex<<-humantomouseout()
      prodatadfx<<-prodataout()
      proT50datadfx<<-proT50dataout()
      phosdatadfx<<-phosdataout()
      phosT50datadfx<<-phosT50dataout()
      proinputidsx<<-isolate(input$proinputids)
      if(proinputidsx==""){
        #stop("Please type in a proper gene name or gene id above!")
        shiny::showNotification("Please type in a proper gene name or gene id above!",
                                duration=15,type = "error")
      }else{
        proinputidsxx<<-paste0(";",proinputidsx,";")
        if(input$abunbarplottype==1){
          proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
          proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
          if(length(proidpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
            }else{
              proidpipei2<-NULL
            }
          }
          idpipeiall1<-c(proidpipei1,proidpipei2)
          if(length(idpipeiall1)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            ppx<-ggplot() +
              theme_void() +
              geom_text(aes(0,5,label=paste0('Nothing found for ',proinputidsx,' in the Protein abundance data!')),size=10, vjust = -13) +
              xlab(NULL)
            #shiny::showNotification("No results found here!",
            #                        duration=15,type = "warning")
          }else{
            prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
            prodatadfx2<-prodatadfx1[,-c(1:2)]
            rownames(prodatadfx2)<-proinputidsx
            prodatadfx3<-reshape2::melt(prodatadfx2)
            prodatadfx3$Names<-rownames(prodatadfx2)#,group=1
            ppx<-ggplot(prodatadfx3, aes(x=variable, y=value,fill=Names)) +
              geom_bar(stat="identity", position=position_dodge(),alpha=0.8,show.legend=F)+
              scale_fill_manual(values = colpalettes[1:length(unique(prodatadfx3$Names))])+
              labs(x = "Tissues", y = "Log2(MS-intensity)", title = paste0("Distribution of Protein: ",proinputidsx))+
              theme_bw()+
              theme(plot.title = element_text(size=21),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=17),
                    axis.text = element_text(size=15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=14),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          }
        }else if(input$abunbarplottype==2){
          proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
          proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                               paste0(";",proT50datadfx[[2]],";"),
                               ignore.case = TRUE)
          if(length(proT50idpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
            }else{
              proT50idpipei2<-NULL
            }
          }
          idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
          if(length(idpipeiall2)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            ppx<-ggplot() +
              theme_void() +
              geom_text(aes(0,5,label=paste0('Nothing found for ',proinputidsx,' in the Protein lifetime data!')),size=10, vjust = -13) +
              xlab(NULL)
          }else{
            proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
            proT50datadfx2<-proT50datadfx1[,-c(1:2)]
            rownames(proT50datadfx2)<-proinputidsx
            proT50datadfx3<-reshape2::melt(proT50datadfx2)
            proT50datadfx3$Names<-rownames(proT50datadfx2)
            ppx<-ggplot(proT50datadfx3, aes(x=variable, y=value,fill=Names)) +
              geom_bar(stat="identity", position=position_dodge(),alpha=0.8,show.legend=F)+
              scale_fill_manual(values = colpalettes[1:length(unique(proT50datadfx3$Names))])+
              labs(x = "Tissues", y = "T50 (Log2)", title = paste0("T50 distribution of Protein: ",proinputidsx)) +
              theme_bw()+
              theme(plot.title = element_text(size=21),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=17),
                    axis.text = element_text(size=15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=14),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
            
          }
        }else if(input$abunbarplottype==3){
          phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
          phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
          if(length(phosidpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
            }else{
              phosidpipei2<-NULL
            }
          }
          idpipeiall3<-c(phosidpipei1,phosidpipei2)
          if(length(idpipeiall3)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            ppx<-ggplot() +
              theme_void() +
              geom_text(aes(0,5,label=paste0('Nothing found for ',proinputidsx,' in the Phospho abundance data!')),size=10, vjust = -13) +
              xlab(NULL)
          }else{
            phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
            phosdatadfx2<-phosdatadfx1[,-c(1:4)]
            phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
            rownames(phosdatadfx2)<-phositeid
            phosdatadfx3<-reshape2::melt(phosdatadfx2)
            phosdatadfx3$Names<-rownames(phosdatadfx2)
            ppx<-ggplot(phosdatadfx3, aes(x=variable, y=value,fill=Names)) +
              geom_bar(stat="identity", position=position_dodge(),alpha=0.8,show.legend=F)+
              facet_wrap(~Names,ncol = 3)+
              scale_fill_manual(values = colpalettes[1:length(unique(phosdatadfx3$Names))])+
              labs(x = "Tissues", y = "Log2(MS-intensity)", title = paste0("Distribution of PhosphoProtein: ",proinputidsx)) +
              theme_bw()+
              theme(plot.title = element_text(size=21),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=17),
                    axis.text = element_text(size=15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=14),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
          }
        }else{
          phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
          #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
          phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                               paste0(";",phosT50datadfx[[2]],";"),
                               ignore.case = TRUE)
          if(length(phosT50idpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
            }else{
              phosT50idpipei2<-NULL
            }
          }
          idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
          if(length(idpipeiall4)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            ppx<-ggplot() +
              theme_void() +
              geom_text(aes(0,5,label=paste0('Nothing found for ',proinputidsx,' in the Phospho lifetime data!')),size=10, vjust = -13) +
              xlab(NULL)
          }else{
            phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
            phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
            phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
            rownames(phosT50datadfx2)<-phositeid
            phosT50datadfx3<-reshape2::melt(phosT50datadfx2)
            phosT50datadfx3$Names<-rownames(phosT50datadfx2)
            ppx<-ggplot(phosT50datadfx3, aes(x=variable, y=value,fill=Names)) +
              geom_bar(stat="identity", position=position_dodge(),alpha=0.8,show.legend=F)+
              facet_wrap(~Names,ncol = 3)+
              scale_fill_manual(values = colpalettes[1:length(unique(phosT50datadfx3$Names))])+
              labs(x = "Tissues", y = "Log2(MS-intensity)", title = paste0("Distribution of PhosphoProtein: ",proinputidsx)) +
              theme_bw()+
              theme(plot.title = element_text(size=21),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=17),
                    axis.text = element_text(size=15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=14),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
          }
        }
        ppx
      }
      
    },height=abunbarheightx,width = abunbarwidthx)
    proinputidsplotout<-reactive({
      humantomousex<<-humantomouseout()
      prodatadfx<<-prodataout()
      proT50datadfx<<-proT50dataout()
      phosdatadfx<<-phosdataout()
      phosT50datadfx<<-phosT50dataout()
      proinputidsx<<-isolate(input$proinputids)
      if(proinputidsx==""){
        #stop("Please type in a proper gene name or gene id above!")
        shiny::showNotification("Please type in a proper gene name or gene id above!",
                                duration=15,type = "error")
      }else{
        proinputidsxx<<-paste0(";",proinputidsx,";")
        if(input$abunbarplottype==1){
          proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
          proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
          if(length(proidpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
            }else{
              proidpipei2<-NULL
            }
          }
          idpipeiall1<-c(proidpipei1,proidpipei2)
          if(length(idpipeiall1)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            ppx<-ggplot() +
              theme_void() +
              geom_text(aes(0,5,label=paste0('Nothing found for ',proinputidsx,' in the Protein abundance data!')),size=10, vjust = -13) +
              xlab(NULL)
            #shiny::showNotification("No results found here!",
            #                        duration=15,type = "warning")
          }else{
            prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
            prodatadfx2<-prodatadfx1[,-c(1:2)]
            rownames(prodatadfx2)<-proinputidsx
            prodatadfx3<-reshape2::melt(prodatadfx2)
            prodatadfx3$Names<-rownames(prodatadfx2)#,group=1
            ppx<-ggplot(prodatadfx3, aes(x=variable, y=value,fill=Names)) +
              geom_bar(stat="identity", position=position_dodge(),alpha=0.8,show.legend=F)+
              scale_fill_manual(values = colpalettes[1:length(unique(prodatadfx3$Names))])+
              labs(x = "Tissues", y = "Log2(MS-intensity)", title = paste0("Distribution of Protein: ",proinputidsx))+
              theme_bw()+
              theme(plot.title = element_text(size=21),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=17),
                    axis.text = element_text(size=15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=14),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          }
        }else if(input$abunbarplottype==2){
          proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
          proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                               paste0(";",proT50datadfx[[2]],";"),
                               ignore.case = TRUE)
          if(length(proT50idpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
            }else{
              proT50idpipei2<-NULL
            }
          }
          idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
          if(length(idpipeiall2)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            ppx<-ggplot() +
              theme_void() +
              geom_text(aes(0,5,label=paste0('Nothing found for ',proinputidsx,' in the Protein lifetime data!')),size=10, vjust = -13) +
              xlab(NULL)
          }else{
            proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
            proT50datadfx2<-proT50datadfx1[,-c(1:2)]
            rownames(proT50datadfx2)<-proinputidsx
            proT50datadfx3<-reshape2::melt(proT50datadfx2)
            proT50datadfx3$Names<-rownames(proT50datadfx2)
            ppx<-ggplot(proT50datadfx3, aes(x=variable, y=value,fill=Names)) +
              geom_bar(stat="identity", position=position_dodge(),alpha=0.8,show.legend=F)+
              scale_fill_manual(values = colpalettes[1:length(unique(proT50datadfx3$Names))])+
              labs(x = "Tissues", y = "T50 (Log2)", title = paste0("T50 distribution of Protein: ",proinputidsx)) +
              theme_bw()+
              theme(plot.title = element_text(size=21),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=17),
                    axis.text = element_text(size=15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=14),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
            
          }
        }else if(input$abunbarplottype==3){
          phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
          phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
          if(length(phosidpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
            }else{
              phosidpipei2<-NULL
            }
          }
          idpipeiall3<-c(phosidpipei1,phosidpipei2)
          if(length(idpipeiall3)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            ppx<-ggplot() +
              theme_void() +
              geom_text(aes(0,5,label=paste0('Nothing found for ',proinputidsx,' in the Phospho abundance data!')),size=10, vjust = -13) +
              xlab(NULL)
          }else{
            phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
            phosdatadfx2<-phosdatadfx1[,-c(1:4)]
            phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
            rownames(phosdatadfx2)<-phositeid
            phosdatadfx3<-reshape2::melt(phosdatadfx2)
            phosdatadfx3$Names<-rownames(phosdatadfx2)
            ppx<-ggplot(phosdatadfx3, aes(x=variable, y=value,fill=Names)) +
              geom_bar(stat="identity", position=position_dodge(),alpha=0.8,show.legend=F)+
              facet_wrap(~Names,ncol = 3)+
              scale_fill_manual(values = colpalettes[1:length(unique(phosdatadfx3$Names))])+
              labs(x = "Tissues", y = "Log2(MS-intensity)", title = paste0("Distribution of PhosphoProtein: ",proinputidsx)) +
              theme_bw()+
              theme(plot.title = element_text(size=21),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=17),
                    axis.text = element_text(size=15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=14),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
          }
        }else{
          phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
          #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
          phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                                paste0(";",phosT50datadfx[[2]],";"),
                                ignore.case = TRUE)
          if(length(phosT50idpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
            }else{
              phosT50idpipei2<-NULL
            }
          }
          idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
          if(length(idpipeiall4)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            ppx<-ggplot() +
              theme_void() +
              geom_text(aes(0,5,label=paste0('Nothing found for ',proinputidsx,' in the Phospho lifetime data!')),size=10, vjust = -13) +
              xlab(NULL)
          }else{
            phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
            phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
            phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
            rownames(phosT50datadfx2)<-phositeid
            phosT50datadfx3<-reshape2::melt(phosT50datadfx2)
            phosT50datadfx3$Names<-rownames(phosT50datadfx2)
            ppx<-ggplot(phosT50datadfx3, aes(x=variable, y=value,fill=Names)) +
              geom_bar(stat="identity", position=position_dodge(),alpha=0.8,show.legend=F)+
              facet_wrap(~Names,ncol = 3)+
              scale_fill_manual(values = colpalettes[1:length(unique(phosT50datadfx3$Names))])+
              labs(x = "Tissues", y = "Log2(MS-intensity)", title = paste0("Distribution of PhosphoProtein: ",proinputidsx)) +
              theme_bw()+
              theme(plot.title = element_text(size=21),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=17),
                    axis.text = element_text(size=15),
                    legend.title=element_text(size=15),
                    legend.text=element_text(size=14),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
          }
        }
        ppx
      }
      
    })
    output$figuredl<-downloadHandler(
      filename = function(){paste("Abundance.barplot.",usertimenum,".pdf",sep="")},
      content = function(file){
        pdf(file,width = abunbarwidthx()/100,height = abunbarheightx()/100)
        print(proinputidsplotout())
        dev.off()
      }
    )
    proinputidsplotdataout<-reactive({
      humantomousex<<-humantomouseout()
      prodatadfx<<-prodataout()
      proT50datadfx<<-proT50dataout()
      phosdatadfx<<-phosdataout()
      phosT50datadfx<<-phosT50dataout()
      proinputidsx<<-isolate(input$proinputids)
      if(proinputidsx==""){
        #stop("Please type in a proper gene name or gene id above!")
        shiny::showNotification("Please type in a proper gene name or gene id above!",
                                duration=15,type = "error")
      }else{
        proinputidsxx<<-paste0(";",proinputidsx,";")
        if(input$abunbarplottype==1){
          proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
          proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
          if(length(proidpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
            }else{
              proidpipei2<-NULL
            }
          }
          idpipeiall1<-c(proidpipei1,proidpipei2)
          if(length(idpipeiall1)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            bardataout<-data.frame(Description='Nothing found for ',proinputidsx,' in the Protein abundance data!')
            #shiny::showNotification("No results found here!",
            #                        duration=15,type = "warning")
          }else{
            prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
            prodatadfx2<-prodatadfx1[,-c(1:2)]
            rownames(prodatadfx2)<-proinputidsx
            prodatadfx3<-reshape2::melt(prodatadfx2)
            prodatadfx3$Names<-rownames(prodatadfx2)#,group=1
            bardataout<-prodatadfx3
          }
        }else if(input$abunbarplottype==2){
          proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
          proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                               paste0(";",proT50datadfx[[2]],";"),
                               ignore.case = TRUE)
          if(length(proT50idpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
            }else{
              proT50idpipei2<-NULL
            }
          }
          idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
          if(length(idpipeiall2)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            bardataout<-data.frame(Description='Nothing found for ',proinputidsx,' in the Protein lifetime data!')
          }else{
            proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
            proT50datadfx2<-proT50datadfx1[,-c(1:2)]
            rownames(proT50datadfx2)<-proinputidsx
            proT50datadfx3<-reshape2::melt(proT50datadfx2)
            proT50datadfx3$Names<-rownames(proT50datadfx2)
            bardataout<-proT50datadfx3
          }
        }else if(input$abunbarplottype==3){
          phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
          phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
          if(length(phosidpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
            }else{
              phosidpipei2<-NULL
            }
          }
          idpipeiall3<-c(phosidpipei1,phosidpipei2)
          if(length(idpipeiall3)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            bardataout<-data.frame(Description='Nothing found for ',proinputidsx,' in the Phospho abundance data!')
          }else{
            phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
            phosdatadfx2<-phosdatadfx1[,-c(1:4)]
            phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
            rownames(phosdatadfx2)<-phositeid
            phosdatadfx3<-reshape2::melt(phosdatadfx2)
            phosdatadfx3$Names<-rownames(phosdatadfx2)
            bardataout<-phosdatadfx3
          }
        }else{
          phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
          #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
          phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                                paste0(";",phosT50datadfx[[2]],";"),
                                ignore.case = TRUE)
          if(length(phosT50idpipei2)==0){
            humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
            humantomousex2<-humantomousex[[2]][humantomousex1]
            if(length(humantomousex2)>0){
              phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
            }else{
              phosT50idpipei2<-NULL
            }
          }
          idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
          if(length(idpipeiall4)==0){
            #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
            #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
            #                        duration=15,type = "error")
            bardataout<-data.frame(Description='Nothing found for ',proinputidsx,' in the Phospho lifetime data!')
          }else{
            phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
            phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
            phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
            rownames(phosT50datadfx2)<-phositeid
            phosT50datadfx3<-reshape2::melt(phosT50datadfx2)
            phosT50datadfx3$Names<-rownames(phosT50datadfx2)
            bardataout<-phosT50datadfx3
          }
        }
        bardataout
      }
    })
    output$abunbarplotdatadl<-downloadHandler(
      filename = function(){paste("Abundance.barplot.data_",usertimenum,".csv",sep="")},
      content = function(file){
        write.csv(proinputidsplotdataout(),file,row.names = F)
      }
    )
  })
  observeEvent(input$proinputidscor_btn,{
    ##cor plot
    corplotnum<-reactive({
      humantomousex<<-humantomouseout()
      prodatadfx<<-prodataout()
      proT50datadfx<<-proT50dataout()
      phosdatadfx<<-phosdataout()
      phosT50datadfx<<-phosT50dataout()
      proinputidsx<<-isolate(input$proinputidscor)
      if(proinputidsx==""){
        shiny::showNotification("Please type in a proper gene name or gene id above!",
                                duration=15,type = "error")
      }
      proinputidsxx<<-paste0(";",proinputidsx,";")
      if(input$abunbarplottypexaxis==1){
        proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
        proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
        if(length(proidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
          }else{
            proidpipei2<-NULL
          }
        }
        idpipeiall1<-c(proidpipei1,proidpipei2)
        if(length(idpipeiall1)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein abundance data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
          prodatadfx2<-prodatadfx1[,-c(1:2)]
          rownames(prodatadfx2)<-proinputidsx
          axis.x<-prodatadfx2
        }
      }else if(input$abunbarplottypexaxis==2){
        proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
        proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                             paste0(";",proT50datadfx[[2]],";"),
                             ignore.case = TRUE)
        if(length(proT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            proT50idpipei2<-NULL
          }
        }
        idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
        if(length(idpipeiall2)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein lifetime data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
          proT50datadfx2<-proT50datadfx1[,-c(1:2)]
          rownames(proT50datadfx2)<-proinputidsx
          axis.x<-proT50datadfx2
        }
      }else if(input$abunbarplottypexaxis==3){
        phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
        phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
        if(length(phosidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
          }else{
            phosidpipei2<-NULL
          }
        }
        idpipeiall3<-c(phosidpipei1,phosidpipei2)
        if(length(idpipeiall3)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho abundance data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
          phosdatadfx2<-phosdatadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
          rownames(phosdatadfx2)<-phositeid
          axis.x<-phosdatadfx2
        }
      }else{
        phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
        #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
        phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                              paste0(";",phosT50datadfx[[2]],";"),
                              ignore.case = TRUE)
        if(length(phosT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            phosT50idpipei2<-NULL
          }
        }
        idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
        if(length(idpipeiall4)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho lifetime data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
          phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
          rownames(phosT50datadfx2)<-phositeid
          axis.x<-phosT50datadfx2
        }
      }
      ##
      if(input$abunbarplottypeyaxis==2){
        phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
        phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
        if(length(phosidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
          }else{
            phosidpipei2<-NULL
          }
        }
        idpipeiall3<-c(phosidpipei1,phosidpipei2)
        if(length(idpipeiall3)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho abundance data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
          phosdatadfx2<-phosdatadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
          phosdatadfx2$phositeid<-phositeid
          phosdatadfx2<-phosdatadfx2 %>% 
            group_by(phositeid) %>%
            summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))%>% 
            tibble::column_to_rownames(var="phositeid")
          #rownames(phosdatadfx2)<-phositeid
          axis.y<-phosdatadfx2
        }
      }else if(input$abunbarplottypeyaxis==3){
        phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
        #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
        phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                              paste0(";",phosT50datadfx[[2]],";"),
                              ignore.case = TRUE)
        if(length(phosT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            phosT50idpipei2<-NULL
          }
        }
        idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
        if(length(idpipeiall4)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho lifetime data!",
          #                        duration=15,type = "error")
          axis.y<-data.frame()
        }else{
          phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
          phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
          phosT50datadfx2$phositeid<-phositeid
          phosT50datadfx2<-phosT50datadfx2 %>% 
            group_by(phositeid) %>%
            summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))%>% 
            tibble::column_to_rownames(var="phositeid")
          #rownames(phosT50datadfx2)<-phositeid
          axis.y<-phosT50datadfx2
        }
      }else if(input$abunbarplottypeyaxis==4){
        proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
        proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
        if(length(proidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
          }else{
            proidpipei2<-NULL
          }
        }
        idpipeiall1<-c(proidpipei1,proidpipei2)
        if(length(idpipeiall1)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein abundance data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
          prodatadfx2<-prodatadfx1[,-c(1:2)]
          rownames(prodatadfx2)<-proinputidsx
          axis.y<-prodatadfx2
        }
      }else{
        proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
        #proT50idpipei2<-grep(proinputidsx,proT50datadfx[[2]],ignore.case = TRUE)
        proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                             paste0(";",proT50datadfx[[2]],";"),
                             ignore.case = TRUE)
        if(length(proT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            proT50idpipei2<-NULL
          }
        }
        idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
        if(length(idpipeiall2)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein lifetime data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
          proT50datadfx2<-proT50datadfx1[,-c(1:2)]
          rownames(proT50datadfx2)<-proinputidsx
          axis.y<-proT50datadfx2
        }
      }
      ##
      #axis.x<<-axis.x
      #axis.y<<-axis.y
      if(nrow(axis.x)>0 & nrow(axis.y)>0){
        corplotnumx<-nrow(axis.x)*nrow(axis.y)
      }else{
        corplotnumx<-1
      }
      corplotnumx
    })
    corheightx<-reactive({
      corplotnumx<-corplotnum()
      input$corheightx*ceiling(corplotnumx/3)
    })
    corwidthx<-reactive({
      #input$corwidthx#corheightx*2
      input$corheightx*3
    })
    output$corplotx<-renderPlot({
      humantomousex<<-humantomouseout()
      prodatadfx<<-prodataout()
      proT50datadfx<<-proT50dataout()
      phosdatadfx<<-phosdataout()
      phosT50datadfx<<-phosT50dataout()
      proinputidsx<<-isolate(input$proinputidscor)
      if(proinputidsx==""){
        shiny::showNotification("Please type in a proper gene name or gene id above!",
                                duration=15,type = "error")
      }
      proinputidsxx<<-paste0(";",proinputidsx,";")
      if(input$abunbarplottypexaxis==1){
        proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
        proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
        if(length(proidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
          }else{
            proidpipei2<-NULL
          }
        }
        idpipeiall1<-c(proidpipei1,proidpipei2)
        if(length(idpipeiall1)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein abundance data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
          prodatadfx2<-prodatadfx1[,-c(1:2)]
          rownames(prodatadfx2)<-proinputidsx
          axis.x<-prodatadfx2
        }
      }else if(input$abunbarplottypexaxis==2){
        proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
        proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                             paste0(";",proT50datadfx[[2]],";"),
                             ignore.case = TRUE)
        if(length(proT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            proT50idpipei2<-NULL
          }
        }
        idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
        if(length(idpipeiall2)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein lifetime data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
          proT50datadfx2<-proT50datadfx1[,-c(1:2)]
          rownames(proT50datadfx2)<-proinputidsx
          axis.x<-proT50datadfx2
        }
      }else if(input$abunbarplottypexaxis==3){
        phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
        phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
        if(length(phosidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
          }else{
            phosidpipei2<-NULL
          }
        }
        idpipeiall3<-c(phosidpipei1,phosidpipei2)
        if(length(idpipeiall3)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho abundance data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
          phosdatadfx2<-phosdatadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
          rownames(phosdatadfx2)<-phositeid
          axis.x<-phosdatadfx2
        }
      }else{
        phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
        #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
        phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                              paste0(";",phosT50datadfx[[2]],";"),
                              ignore.case = TRUE)
        if(length(phosT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            phosT50idpipei2<-NULL
          }
        }
        idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
        if(length(idpipeiall4)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho lifetime data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
          phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
          rownames(phosT50datadfx2)<-phositeid
          axis.x<-phosT50datadfx2
        }
      }
      ##
      if(input$abunbarplottypeyaxis==2){
        phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
        phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
        if(length(phosidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
          }else{
            phosidpipei2<-NULL
          }
        }
        idpipeiall3<-c(phosidpipei1,phosidpipei2)
        if(length(idpipeiall3)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho abundance data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
          phosdatadfx2<-phosdatadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
          phosdatadfx2$phositeid<-phositeid
          phosdatadfx2<-phosdatadfx2 %>% 
            group_by(phositeid) %>%
            summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))%>% 
            tibble::column_to_rownames(var="phositeid")
          #rownames(phosdatadfx2)<-phositeid
          axis.y<-phosdatadfx2
        }
      }else if(input$abunbarplottypeyaxis==3){
        phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
        #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
        phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                              paste0(";",phosT50datadfx[[2]],";"),
                              ignore.case = TRUE)
        if(length(phosT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            phosT50idpipei2<-NULL
          }
        }
        idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
        if(length(idpipeiall4)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho lifetime data!",
          #                        duration=15,type = "error")
          axis.y<-data.frame()
        }else{
          phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
          phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
          phosT50datadfx2$phositeid<-phositeid
          phosT50datadfx2<-phosT50datadfx2 %>% 
            group_by(phositeid) %>%
            summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))%>% 
            tibble::column_to_rownames(var="phositeid")
          #rownames(phosT50datadfx2)<-phositeid
          axis.y<-phosT50datadfx2
        }
      }else if(input$abunbarplottypeyaxis==4){
        proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
        proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
        if(length(proidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
          }else{
            proidpipei2<-NULL
          }
        }
        idpipeiall1<-c(proidpipei1,proidpipei2)
        if(length(idpipeiall1)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein abundance data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
          prodatadfx2<-prodatadfx1[,-c(1:2)]
          rownames(prodatadfx2)<-proinputidsx
          axis.y<-prodatadfx2
        }
      }else{
        proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
        #proT50idpipei2<-grep(proinputidsx,proT50datadfx[[2]],ignore.case = TRUE)
        proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                             paste0(";",proT50datadfx[[2]],";"),
                             ignore.case = TRUE)
        if(length(proT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            proT50idpipei2<-NULL
          }
        }
        idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
        if(length(idpipeiall2)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein lifetime data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
          proT50datadfx2<-proT50datadfx1[,-c(1:2)]
          rownames(proT50datadfx2)<-proinputidsx
          axis.y<-proT50datadfx2
        }
      }
      ##
      axis.x<<-axis.x
      axis.y<<-axis.y
      if(nrow(axis.x)>0 & nrow(axis.y)>0){
        corpointcolx<<-input$corpointcol
        corlinecolx<<-input$corlinecol
        lm_eqn <- function(df){
          m <- lm(b ~ a, df);
          eq <- substitute(~~italic("r")~"="~r*","~~italic("p")~"="~p,
                           list(a = format(coef(m)[1], digits = 3),
                                b = format(coef(m)[2], digits = 3),
                                r = format(r, digits = 3),
                                p = format(p, digits=3)))
          as.character(as.expression(eq));
        }
        lm_eqn.rho <- function(df){
          eq <- substitute(~~italic("rho")~"="~rho*","~~italic("p.rho")~"="~p.rho,
                           list(rho = format(rho, digits = 3),
                                p.rho = format(p.rho, digits=3)))
          as.character(as.expression(eq));
        }
        corproppxlist<-list()
        k<-1
        for(i in 1:nrow(axis.x)){
          for(j in 1:nrow(axis.y)){
            prodatadfx3<-reshape2::melt(axis.x[i,])
            prodatadfx3$Names<-rownames(axis.x[i,])
            prodatadfx3$a<-prodatadfx3$value
            genedatadfx3<-reshape2::melt(axis.y[j,])
            genedatadfx3$Names<-rownames(axis.y[j,])
            prodatadfx3$b<-genedatadfx3$value
            df<-prodatadfx3[,c(4,5)]
            out <- cor.test(df$a,df$b) ; r <- out$estimate ; p <- out$p.value
            out.rho <- cor.test(df$a,df$b,method="spearman") ; rho <- out.rho$estimate ; p.rho <- out.rho$p.value
            lm_eqnx<-lm_eqn(df)
            lm_eqnx1<-gsub("c\\(cor = ","",lm_eqnx)
            lm_eqnx1<-gsub("\\) \\* "," \\* ",lm_eqnx1)
            lm_eqnx.rho<-lm_eqn.rho(df)
            lm_eqnx.rho1<-gsub("c\\(rho = ","",lm_eqnx.rho)
            lm_eqnx.rho1<-gsub("\\) \\* "," \\* ",lm_eqnx.rho1)
            df$label_names<-prodatadfx3$variable
            df1<-df[complete.cases(df),]
            corproppxlist[[k]]<-ggplot(df1, aes(a, b)) +
              geom_point(shape = 15, size = 5, color =corpointcolx, show.legend = FALSE, alpha = .8 ) +
              geom_smooth(method=lm,se=F,show.legend=F, color =corlinecolx) +
              geom_text(x = min(df1$a,na.rm = T) + 0.4*(max(df1$a,na.rm = T)-min(df1$a,na.rm = T)),
                        y = min(df1$b,na.rm = T)+min(0.5,0.05*(max(df1$b,na.rm = T)-min(df1$b,na.rm = T))), 
                        label = lm_eqnx1,parse = TRUE,show.legend=F,color="black",size = 4) +
              geom_text(x = min(df1$a,na.rm = T) + 0.4*(max(df1$a,na.rm = T)-min(df1$a,na.rm = T)),
                        y = min(df1$b,na.rm = T), label = lm_eqnx.rho1,
                        parse = TRUE,show.legend=F,color="black",size = 4) +
              geom_text_repel(data=df1,aes(label=label_names),size=4,color =corpointcolx) +
              labs(x = rownames(axis.x[i,]), y = rownames(axis.y[j,]))+#, title = paste0("Correlation plot of ",proinputidsx)
              theme_bw()+
              theme(plot.title = element_text(size=16),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=15),
                    axis.text = element_text(size=13),
                    legend.position="none",
                    legend.title=element_text(size=13),
                    legend.text=element_text(size=11),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            k<-k+1
          }
        }
        gridExtra::grid.arrange(grobs=corproppxlist,ncol=3)
      }else{
        corproppx<-ggplot(data.frame(x=1,y=1),aes(x=x, y=y, label = paste0("No data searched for ",proinputidsx," in the data!")))+
          geom_text(size=10)+#,hjust=1, vjust=-5
          theme_void()
        corproppx
      }
    },height=corheightx,width = corwidthx)
    corplotxout<-reactive({
      humantomousex<<-humantomouseout()
      prodatadfx<<-prodataout()
      proT50datadfx<<-proT50dataout()
      phosdatadfx<<-phosdataout()
      phosT50datadfx<<-phosT50dataout()
      proinputidsx<<-isolate(input$proinputidscor)
      if(proinputidsx==""){
        shiny::showNotification("Please type in a proper gene name or gene id above!",
                                duration=15,type = "error")
      }
      proinputidsxx<<-paste0(";",proinputidsx,";")
      if(input$abunbarplottypexaxis==1){
        proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
        proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
        if(length(proidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
          }else{
            proidpipei2<-NULL
          }
        }
        idpipeiall1<-c(proidpipei1,proidpipei2)
        if(length(idpipeiall1)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein abundance data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
          prodatadfx2<-prodatadfx1[,-c(1:2)]
          rownames(prodatadfx2)<-proinputidsx
          axis.x<-prodatadfx2
        }
      }else if(input$abunbarplottypexaxis==2){
        proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
        proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                             paste0(";",proT50datadfx[[2]],";"),
                             ignore.case = TRUE)
        if(length(proT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            proT50idpipei2<-NULL
          }
        }
        idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
        if(length(idpipeiall2)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein lifetime data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
          proT50datadfx2<-proT50datadfx1[,-c(1:2)]
          rownames(proT50datadfx2)<-proinputidsx
          axis.x<-proT50datadfx2
        }
      }else if(input$abunbarplottypexaxis==3){
        phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
        phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
        if(length(phosidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
          }else{
            phosidpipei2<-NULL
          }
        }
        idpipeiall3<-c(phosidpipei1,phosidpipei2)
        if(length(idpipeiall3)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho abundance data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
          phosdatadfx2<-phosdatadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
          rownames(phosdatadfx2)<-phositeid
          axis.x<-phosdatadfx2
        }
      }else{
        phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
        #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
        phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                              paste0(";",phosT50datadfx[[2]],";"),
                              ignore.case = TRUE)
        if(length(phosT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            phosT50idpipei2<-NULL
          }
        }
        idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
        if(length(idpipeiall4)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho lifetime data!",
          #                        duration=15,type = "warning")
          axis.x<-data.frame()
        }else{
          phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
          phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
          rownames(phosT50datadfx2)<-phositeid
          axis.x<-phosT50datadfx2
        }
      }
      ##
      if(input$abunbarplottypeyaxis==2){
        phosidpipei1<-grep(proinputidsxx,phosdatadfx[[1]],ignore.case = TRUE)
        phosidpipei2<-grep(proinputidsxx,phosdatadfx[[2]],ignore.case = TRUE)
        if(length(phosidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosidpipei2<-grep(humantomousex2,phosdatadfx[[2]],ignore.case = TRUE)
          }else{
            phosidpipei2<-NULL
          }
        }
        idpipeiall3<-c(phosidpipei1,phosidpipei2)
        if(length(idpipeiall3)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho abundance data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          phosdatadfx1<-phosdatadfx[c(phosidpipei1,phosidpipei2),]
          phosdatadfx2<-phosdatadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosdatadfx[c(phosidpipei1,phosidpipei2),4])
          phosdatadfx2$phositeid<-phositeid
          phosdatadfx2<-phosdatadfx2 %>% 
            group_by(phositeid) %>%
            summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))%>% 
            tibble::column_to_rownames(var="phositeid")
          #rownames(phosdatadfx2)<-phositeid
          axis.y<-phosdatadfx2
        }
      }else if(input$abunbarplottypeyaxis==3){
        phosT50idpipei1<-grep(proinputidsx,phosT50datadfx[[1]],ignore.case = TRUE)
        #phosT50idpipei2<-grep(proinputidsx,phosT50datadfx[[2]],ignore.case = TRUE)
        phosT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                              paste0(";",phosT50datadfx[[2]],";"),
                              ignore.case = TRUE)
        if(length(phosT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            phosT50idpipei2<-grep(humantomousex2,paste0(";",phosT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            phosT50idpipei2<-NULL
          }
        }
        idpipeiall4<-c(phosT50idpipei1,phosT50idpipei2)
        if(length(idpipeiall4)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Phospho lifetime data!",
          #                        duration=15,type = "error")
          axis.y<-data.frame()
        }else{
          phosT50datadfx1<-phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),]
          phosT50datadfx2<-phosT50datadfx1[,-c(1:4)]
          phositeid<-paste0(proinputidsx," ",phosT50datadfx[c(phosT50idpipei1,phosT50idpipei2),4])
          phosT50datadfx2$phositeid<-phositeid
          phosT50datadfx2<-phosT50datadfx2 %>% 
            group_by(phositeid) %>%
            summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))%>% 
            tibble::column_to_rownames(var="phositeid")
          #rownames(phosT50datadfx2)<-phositeid
          axis.y<-phosT50datadfx2
        }
      }else if(input$abunbarplottypeyaxis==4){
        proidpipei1<-grep(proinputidsxx,prodatadfx[[1]],ignore.case = TRUE)
        proidpipei2<-grep(proinputidsxx,prodatadfx[[2]],ignore.case = TRUE)
        if(length(proidpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proidpipei2<-grep(humantomousex2,prodatadfx[[2]],ignore.case = TRUE)
          }else{
            proidpipei2<-NULL
          }
        }
        idpipeiall1<-c(proidpipei1,proidpipei2)
        if(length(idpipeiall1)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein abundance data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          prodatadfx1<-prodatadfx[c(proidpipei1,proidpipei2),]
          prodatadfx2<-prodatadfx1[,-c(1:2)]
          rownames(prodatadfx2)<-proinputidsx
          axis.y<-prodatadfx2
        }
      }else{
        proT50idpipei1<-grep(proinputidsx,proT50datadfx[[1]],ignore.case = TRUE)
        #proT50idpipei2<-grep(proinputidsx,proT50datadfx[[2]],ignore.case = TRUE)
        proT50idpipei2<-grep(paste0(";",proinputidsx,";"),
                             paste0(";",proT50datadfx[[2]],";"),
                             ignore.case = TRUE)
        if(length(proT50idpipei2)==0){
          humantomousex1<-grep(proinputidsxx,humantomousex[[1]],ignore.case = TRUE)
          humantomousex2<-humantomousex[[2]][humantomousex1]
          if(length(humantomousex2)>0){
            proT50idpipei2<-grep(humantomousex2,paste0(";",proT50datadfx[[2]],";"),ignore.case = TRUE)
          }else{
            proT50idpipei2<-NULL
          }
        }
        idpipeiall2<-c(proT50idpipei1,proT50idpipei2)
        if(length(idpipeiall2)==0){
          #stop(paste0("No find data for ",proinputidsx,". Please try another one!"))
          #shiny::showNotification("An error has occurred. Check if the data is avaiable for the type you selected or contact the app author for clarification.",
          #                        duration=15,type = "error")
          #shiny::showNotification("Nothing found in the Protein lifetime data!",
          #                        duration=15,type = "warning")
          axis.y<-data.frame()
        }else{
          proT50datadfx1<-proT50datadfx[c(proT50idpipei1,proT50idpipei2),]
          proT50datadfx2<-proT50datadfx1[,-c(1:2)]
          rownames(proT50datadfx2)<-proinputidsx
          axis.y<-proT50datadfx2
        }
      }
      ##
      axis.x<<-axis.x
      axis.y<<-axis.y
      if(nrow(axis.x)>0 & nrow(axis.y)>0){
        corpointcolx<<-input$corpointcol
        corlinecolx<<-input$corlinecol
        lm_eqn <- function(df){
          m <- lm(b ~ a, df);
          eq <- substitute(~~italic("r")~"="~r*","~~italic("p")~"="~p,
                           list(a = format(coef(m)[1], digits = 3),
                                b = format(coef(m)[2], digits = 3),
                                r = format(r, digits = 3),
                                p = format(p, digits=3)))
          as.character(as.expression(eq));
        }
        lm_eqn.rho <- function(df){
          eq <- substitute(~~italic("rho")~"="~rho*","~~italic("p.rho")~"="~p.rho,
                           list(rho = format(rho, digits = 3),
                                p.rho = format(p.rho, digits=3)))
          as.character(as.expression(eq));
        }
        corproppxlist<-list()
        k<-1
        for(i in 1:nrow(axis.x)){
          for(j in 1:nrow(axis.y)){
            prodatadfx3<-reshape2::melt(axis.x[i,])
            prodatadfx3$Names<-rownames(axis.x[i,])
            prodatadfx3$a<-prodatadfx3$value
            genedatadfx3<-reshape2::melt(axis.y[j,])
            genedatadfx3$Names<-rownames(axis.y[j,])
            prodatadfx3$b<-genedatadfx3$value
            df<-prodatadfx3[,c(4,5)]
            out <- cor.test(df$a,df$b) ; r <- out$estimate ; p <- out$p.value
            out.rho <- cor.test(df$a,df$b,method="spearman") ; rho <- out.rho$estimate ; p.rho <- out.rho$p.value
            lm_eqnx<-lm_eqn(df)
            lm_eqnx1<-gsub("c\\(cor = ","",lm_eqnx)
            lm_eqnx1<-gsub("\\) \\* "," \\* ",lm_eqnx1)
            lm_eqnx.rho<-lm_eqn.rho(df)
            lm_eqnx.rho1<-gsub("c\\(rho = ","",lm_eqnx.rho)
            lm_eqnx.rho1<-gsub("\\) \\* "," \\* ",lm_eqnx.rho1)
            df$label_names<-prodatadfx3$variable
            df1<-df[complete.cases(df),]
            corproppxlist[[k]]<-ggplot(df1, aes(a, b)) +
              geom_point(shape = 15, size = 5, color =corpointcolx, show.legend = FALSE, alpha = .8 ) +
              geom_smooth(method=lm,se=F,show.legend=F, color =corlinecolx) +
              geom_text(x = min(df1$a,na.rm = T) + 0.4*(max(df1$a,na.rm = T)-min(df1$a,na.rm = T)),
                        y = min(df1$b,na.rm = T)+min(0.5,0.05*(max(df1$b,na.rm = T)-min(df1$b,na.rm = T))), 
                        label = lm_eqnx1,parse = TRUE,show.legend=F,color="black",size = 4) +
              geom_text(x = min(df1$a,na.rm = T) + 0.4*(max(df1$a,na.rm = T)-min(df1$a,na.rm = T)),
                        y = min(df1$b,na.rm = T), label = lm_eqnx.rho1,
                        parse = TRUE,show.legend=F,color="black",size = 4) +
              geom_text_repel(data=df1,aes(label=label_names),size=4,color =corpointcolx) +
              labs(x = rownames(axis.x[i,]), y = rownames(axis.y[j,]))+#, title = paste0("Correlation plot of ",proinputidsx)
              theme_bw()+
              theme(plot.title = element_text(size=16),
                    #strip.text = element_text(size=18),
                    axis.title = element_text(size=15),
                    axis.text = element_text(size=13),
                    legend.position="none",
                    legend.title=element_text(size=13),
                    legend.text=element_text(size=11),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            k<-k+1
          }
        }
        gridExtra::grid.arrange(grobs=corproppxlist,ncol=3)
      }else{
        corproppx<-ggplot(data.frame(x=1,y=1),aes(x=x, y=y, label = paste0("No data searched for ",proinputidsx," in the data!")))+
          geom_text(size=10)+#,hjust=1, vjust=-5
          theme_void()
        corproppx
      }
    })
    output$corfiguredl<-downloadHandler(
      filename = function(){paste("Correlation.",usertimenum,".pdf",sep="")},
      content = function(file){
        pdf(file,width = corwidthx()/100,height = corheightx()/100)
        print(corplotxout())
        dev.off()
      }
    )
  })
  ##
  exampleproset1out<-reactive({
    dataread<-data.frame(IDs=c("Psmd7", "Psmd12", "Psmd6", "Psmd1", "Psmc3", "Psmc2", 
                               "Psmd11", "Psmb2", "Psmb4", "Psma6", "Psmb6", "Psmb1", 
                               "Psmb3", "Psma4", "Psma1", "Psma3", "Psma5"))
    dataread
  })
  exampleproset2out<-reactive({
    dataread<-data.frame(IDs=c("Man2b1", "Ctsf", "Pla2g15", "Rab9a", "Dpp7", "Vps16", 
                               "Mtor", "Arsa", "Pon2", "Lamp1", "Fnbp1", "Capn1", 
                               "Tpp1", "Lamtor5", "Pot1", "Asah1", "Hexb", "Capn2", 
                               "Psap", "Usp5", "Got1", "Ywhaz"))
    dataread
  })
  output$loadproset1examdl<-downloadHandler(
    filename = function(){paste("ExampleProteinList1_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(exampleproset1out(),file,row.names = FALSE)
    }
  )
  output$loadproset1examdl1<-downloadHandler(
    filename = function(){paste("ExampleProteinList1_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(exampleproset1out(),file,row.names = FALSE)
    }
  )
  output$loadproset1examdl2<-downloadHandler(
    filename = function(){paste("ExampleProteinList2_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(exampleproset2out(),file,row.names = FALSE)
    }
  )
  uploadprosetout<-reactive({
    if(input$loadprosettype1==1){
      files1 <<- input$loadprosetcsv
      if (is.null(files1)){
        dataread<-data.frame(Description="NO data here. Please upload the genes/IDs, or load the example data to check first.")
      }else{
        dataread<-read.csv(files1$datapath,stringsAsFactors = F)
      }
    }
    if(input$loadprosettype1==2){
      zhantieidstr<<-strsplit(input$loadprosetID_zhantie,"\n")[[1]]
      zhantieidstr<-zhantieidstr[zhantieidstr!=""]
      #dataread<-data.frame(Input_ID=zhantieidstr[[1]])
      if(length(zhantieidstr)==0){
        dataread<-data.frame(Description="NO data here. Please paste the genes/IDs, or load the example data to check first.")
      }else{
        dataread<-data.frame(Input_ID=zhantieidstr)
      }
    }
    dataread<-data.frame(Inputs=unique(dataread[[1]]))
    dataread
  })
  uploadprosetxout<-reactive({
    if(input$loadprosettype2==1){
      files1 <<- input$loadprosetcsv1
      if (is.null(files1)){
        dataread1<-data.frame(Description="NO data here. Please upload the genes/IDs, or load the example data to check first.")
      }else{
        dataread1<-read.csv(files1$datapath,stringsAsFactors = F)
      }
      files2 <<- input$loadprosetcsv2
      if (is.null(files2)){
        dataread2<-data.frame(Description="NO data here. Please upload the genes/IDs, or load the example data to check first.")
      }else{
        dataread2<-read.csv(files1$datapath,stringsAsFactors = F)
      }
    }
    if(input$loadprosettype2==2){
      zhantieidstr1<<-strsplit(input$loadprosetID_zhantie1,"\n")
      if(length(zhantieidstr1[[1]])==0){
        dataread1<-data.frame(Description="NO data here. Please paste the genes/IDs, or load the example data to check first.")
      }else{
        dataread1<-data.frame(Input_ID=zhantieidstr1[[1]])
      }
      zhantieidstr2<<-strsplit(input$loadprosetID_zhantie2,"\n")
      if(length(zhantieidstr2[[1]])==0){
        dataread2<-data.frame(Description="NO data here. Please paste the genes/IDs, or load the example data to check first.")
      }else{
        dataread2<-data.frame(Input_ID=zhantieidstr2[[1]])
      }
    }
    list(dataread1=dataread1,dataread2=dataread2)
  })
  output$uploadprosetlist<-renderDataTable({
    loadtt<<-input$loadprosettype1
    #aax<<-exampleproset1out()
    if(input$loadprosettype1==1 | input$loadprosettype1==2){
      datatable(uploadprosetout(), options = list(pageLength = 10))
    }else{
      datatable(exampleproset1out(), options = list(pageLength = 10))
    }
  })
  output$uploadprosetlist1<-renderDataTable({
    if(input$loadprosettype2==1 | input$loadprosettype2==2){
      datatable(uploadprosetxout()$dataread1, options = list(pageLength = 10))
    }else{
      datatable(exampleproset1out(), options = list(pageLength = 10))
    }
  })
  output$uploadprosetlist2<-renderDataTable({
    if(input$loadprosettype2==1 | input$loadprosettype2==2){
      datatable(uploadprosetxout()$dataread2, options = list(pageLength = 10))
    }else{
      datatable(exampleproset2out(), options = list(pageLength = 10))
    }
  })
  uploadprosetlistout<-reactive({
    prodatadfx<<-prodataout()
    prodatadfx<-prodatadfx[!duplicated(prodatadfx$Proteins),]
    colnames(prodatadfx)[-c(1,2)]<-paste0("Pro_",colnames(prodatadfx)[-c(1,2)])
    proT50dataoutx<<-proT50dataout()
    proT50dataoutx<-proT50dataoutx[!duplicated(proT50dataoutx$Proteins),]
    colnames(proT50dataoutx)[-c(1,2)]<-paste0("ProT50_",colnames(proT50dataoutx)[-c(1,2)])
    if(input$loadprosettype1==1 | input$loadprosettype1==2){
      datadf<-uploadprosetout()
    }else{
      datadf<-exampleproset1out()
    }
    datadf<<-datadf
    proabunoutx1<-prodatadfx[unlist(lapply(toupper(unique(datadf[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,prodatadfx[[1]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]
    proabunoutx2<-prodatadfx[unlist(lapply(toupper(unique(datadf[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,prodatadfx[[2]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]
    proabunoutx3<-rbind(proabunoutx1,proabunoutx2)
    proabunoutx3[[1]]<-gsub("^;|;$","",proabunoutx3[[1]])
    proabunoutx3[[2]]<-gsub("^;|;$","",proabunoutx3[[2]])
    proT50dataoutx1<-proT50dataoutx[toupper(proT50dataoutx[[1]])%in%toupper(unique(datadf[[1]])),]
    proT50dataoutx2<-proT50dataoutx[toupper(proT50dataoutx[[2]])%in%toupper(unique(datadf[[1]])),]
    proT50dataoutx3<-rbind(proT50dataoutx1,proT50dataoutx2)
    list(proabunoutx3=proabunoutx3,proT50dataoutx3=proT50dataoutx3)
  })
  uploadprosetlistxout<-reactive({
    prodatadfx<<-prodataout()
    prodatadfx<-prodatadfx[!duplicated(prodatadfx$Proteins),]
    colnames(prodatadfx)[-c(1,2)]<-paste0("Pro_",colnames(prodatadfx)[-c(1,2)])
    proT50dataoutx<<-proT50dataout()
    proT50dataoutx<-proT50dataoutx[!duplicated(proT50dataoutx$Proteins),]
    colnames(proT50dataoutx)[-c(1,2)]<-paste0("ProT50_",colnames(proT50dataoutx)[-c(1,2)])
    if(input$loadprosettype2==1 | input$loadprosettype2==2){
      datadf1<-uploadprosetxout()$dataread1
      datadf2<-uploadprosetxout()$dataread2
    }else{
      datadf1<-exampleproset1out()
      datadf2<-exampleproset2out()
    }
    datadf1<<-datadf1
    datadf2<<-datadf2
    proabunoutx1<-prodatadfx[unlist(lapply(toupper(unique(datadf1[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,prodatadfx[[1]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]
    proabunoutx2<-prodatadfx[unlist(lapply(toupper(unique(datadf1[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,prodatadfx[[2]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]
    proabunoutx3<-rbind(proabunoutx1,proabunoutx2)
    proabunoutx3[[1]]<-gsub("^;|;$","",proabunoutx3[[1]])
    proabunoutx3[[2]]<-gsub("^;|;$","",proabunoutx3[[2]])
    ##
    proabunoutx1x<-prodatadfx[unlist(lapply(toupper(unique(datadf2[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,prodatadfx[[1]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]
    proabunoutx2x<-prodatadfx[unlist(lapply(toupper(unique(datadf2[[1]])),function(x){
      x1<-paste0(";",x,";")
      x2<-grep(x1,prodatadfx[[2]],ignore.case = TRUE)
      if(is.na(x2[1])){
        FALSE
      }else{
        x2[1]
      }
    })),]
    proabunoutx3x<-rbind(proabunoutx1x,proabunoutx2x)
    proabunoutx3x[[1]]<-gsub("^;|;$","",proabunoutx3x[[1]])
    proabunoutx3x[[2]]<-gsub("^;|;$","",proabunoutx3x[[2]])
    ##2
    proT50dataoutx1<-proT50dataoutx[toupper(proT50dataoutx[[1]])%in%toupper(unique(datadf1[[1]])),]
    proT50dataoutx2<-proT50dataoutx[toupper(proT50dataoutx[[2]])%in%toupper(unique(datadf1[[1]])),]
    proT50dataoutx3<-rbind(proT50dataoutx1,proT50dataoutx2)
    ##2
    proT50dataoutx1x<-proT50dataoutx[toupper(proT50dataoutx[[1]])%in%toupper(unique(datadf2[[1]])),]
    proT50dataoutx2x<-proT50dataoutx[toupper(proT50dataoutx[[2]])%in%toupper(unique(datadf2[[1]])),]
    proT50dataoutx3x<-rbind(proT50dataoutx1x,proT50dataoutx2x)
    list(proabunoutx3=proabunoutx3,proT50dataoutx3=proT50dataoutx3,
         proabunoutx3x=proabunoutx3x,proT50dataoutx3x=proT50dataoutx3x)
  })
  output$uploadprosetabundf<-renderDataTable({
    prodatadfx<<-prodataout()
    proT50dataoutx<<-proT50dataout()
    prodatadfx1<-prodatadfx[,-c(1,2)]
    proT50dataoutx1<-proT50dataoutx[,-c(1,2)]
    prodatadfx2<-t(as.matrix(apply(prodatadfx1,2,geometric.mean)))
    proT50dataoutx2<-t(as.matrix(apply(proT50dataoutx1,2,geometric.mean)))
    if(input$loadprosetxaxis==1){
      dataout<-uploadprosetlistout()$proabunoutx3
    }else if(input$loadprosetxaxis==2){
      dataout<-uploadprosetlistout()$proT50dataoutx3
    }else if(input$loadprosetxaxis==3){
      dataout<-prodatadfx2
    }else{
      dataout<-proT50dataoutx2
    }
    datatable(dataout, options = list(pageLength = 10))
  })
  output$uploadprosetabundfdl<-downloadHandler(
    filename = function(){paste("Protein.List1.Expression_",usertimenum,".csv",sep="")},
    content = function(file){
      prodatadfx<<-prodataout()
      proT50dataoutx<<-proT50dataout()
      prodatadfx1<-prodatadfx[,-c(1,2)]
      proT50dataoutx1<-proT50dataoutx[,-c(1,2)]
      prodatadfx2<-t(as.matrix(apply(prodatadfx1,2,geometric.mean)))
      proT50dataoutx2<-t(as.matrix(apply(proT50dataoutx1,2,geometric.mean)))
      if(input$loadprosetxaxis==1){
        dataout<-uploadprosetlistout()$proabunoutx3
      }else if(input$loadprosetxaxis==2){
        dataout<-uploadprosetlistout()$proT50dataoutx3
      }else if(input$loadprosetxaxis==3){
        dataout<-prodatadfx2
      }else{
        dataout<-proT50dataoutx2
      }
      write.csv(dataout,file,row.names = FALSE)
    }
  )
  ##2
  output$uploadprosetabundf1<-renderDataTable({
    if(input$loadprosetxaxisx==1){
      dataout<-uploadprosetlistxout()$proabunoutx3
    }
    else if(input$loadprosetxaxisx==2){
      dataout<-uploadprosetlistxout()$proT50dataoutx3
    }
    else if(input$loadprosetxaxisx==3){
      dataout<-uploadprosetlistxout()$proabunoutx3x
    }
    else{
      dataout<-uploadprosetlistxout()$proT50dataoutx3x
    }
    datatable(dataout, options = list(pageLength = 10))
  })
  output$uploadprosetabundf1dl<-downloadHandler(
    filename = function(){paste("Protein.List1.Expression_",usertimenum,".csv",sep="")},
    content = function(file){
      if(input$loadprosetxaxisx==1){
        dataout<-uploadprosetlistxout()$proabunoutx3
      }
      else if(input$loadprosetxaxisx==2){
        dataout<-uploadprosetlistxout()$proT50dataoutx3
      }
      else if(input$loadprosetxaxisx==3){
        dataout<-uploadprosetlistxout()$proabunoutx3x
      }
      else{
        dataout<-uploadprosetlistxout()$proT50dataoutx3x
      }
      write.csv(dataout,file,row.names = FALSE)
    }
  )
  output$uploadprosetabundf2<-renderDataTable({
    if(input$loadprosetyaxisx==1){
      dataout<-uploadprosetlistxout()$proabunoutx3x
    }
    else if(input$loadprosetyaxisx==2){
      dataout<-uploadprosetlistxout()$proT50dataoutx3x
    }
    else if(input$loadprosetyaxisx==3){
      dataout<-uploadprosetlistxout()$proabunoutx3
    }
    else{
      dataout<-uploadprosetlistxout()$proT50dataoutx3
    }
    datatable(dataout, options = list(pageLength = 10))
  })
  output$uploadprosetabundf2dl<-downloadHandler(
    filename = function(){paste("Protein.List2.Expression_",usertimenum,".csv",sep="")},
    content = function(file){
      if(input$loadprosetyaxisx==1){
        dataout<-uploadprosetlistxout()$proabunoutx3x
      }
      else if(input$loadprosetyaxisx==2){
        dataout<-uploadprosetlistxout()$proT50dataoutx3x
      }
      else if(input$loadprosetyaxisx==3){
        dataout<-uploadprosetlistxout()$proabunoutx3
      }
      else{
        dataout<-uploadprosetlistxout()$proT50dataoutx3
      }
      write.csv(dataout,file,row.names = FALSE)
    }
  )
  ##
  loadprosetcorheight<-reactive({
    input$loadprosetcorheight
  })
  loadprosetcorheightx<-reactive({
    input$loadprosetcorheightx
  })
  output$prosetcorplot<-renderPlot({
    prodatadfx<<-prodataout()
    proT50dataoutx<<-proT50dataout()
    prodatadfx1<-prodatadfx[,-c(1,2)]
    proT50dataoutx1<-proT50dataoutx[,-c(1,2)]
    prodatadfx2<-t(as.matrix(apply(prodatadfx1,2,geometric.mean)))
    proT50dataoutx2<-t(as.matrix(apply(proT50dataoutx1,2,geometric.mean)))
    if(input$loadprosetxaxis==1){
      dataoutx<-uploadprosetlistout()$proabunoutx3
      dataoutx1<<-dataoutx[,-c(1,2)]
      if(input$oneprosetxylabelif){
        ylabx<-input$oneprosetxlabel
      }else{
        ylabx<-"Protein set average abundance"
      }
    }
    else if(input$loadprosetxaxis==2){
      dataoutx<-uploadprosetlistout()$proT50dataoutx3
      dataoutx1<<-dataoutx[,-c(1,2)]
      if(input$oneprosetxylabelif){
        ylabx<-input$oneprosetxlabel
      }else{
        ylabx<-"Protein set average lifetime"
      }
    }
    else if(input$loadprosetxaxis==3){
      dataoutx1<-prodatadfx2
      if(input$oneprosetxylabelif){
        ylabx<-input$oneprosetylabel
      }else{
        ylabx<-"Proteome average abundance"
      }
    }
    else{
      dataoutx1<-proT50dataoutx2
      if(input$oneprosetxylabelif){
        ylabx<-input$oneprosetylabel
      }else{
        ylabx<-"Proteome average lifetime"
      }
    }
    
    if(input$loadprosetyaxis==1){
      dataouty<-prodatadfx2
      if(input$oneprosetxylabelif){
        ylabx1<-input$oneprosetylabel
      }else{
        ylabx1<-"Proteome average abundance"
      }
    }
    else if(input$loadprosetyaxis==2){
      dataouty<-proT50dataoutx2
      if(input$oneprosetxylabelif){
        ylabx1<-input$oneprosetylabel
      }else{
        ylabx1<-"Proteome average lifetime"
      }
    }
    else if(input$loadprosetyaxis==3){
      dataoutx<-uploadprosetlistout()$proabunoutx3
      dataouty<<-dataoutx[,-c(1,2)]
      if(input$oneprosetxylabelif){
        ylabx1<-input$oneprosetxlabel
      }else{
        ylabx1<-"Protein set average abundance"
      }
    }
    else{
      dataoutx<-uploadprosetlistout()$proT50dataoutx3
      dataouty<<-dataoutx[,-c(1,2)]
      if(input$oneprosetxylabelif){
        ylabx1<-input$oneprosetxlabel
      }else{
        ylabx1<-"Protein set average lifetime"
      }
    }
    ##
    dataoutx1<<-dataoutx1
    dataouty<<-dataouty
    prodatadfx1<<-prodatadfx1
    if(nrow(dataoutx1)>0){
      colnames(dataoutx1)<-colnames(prodatadfx1)
      colnames(dataouty)<-colnames(prodatadfx1)
      dataoutx2<-apply(dataoutx1,2,geometric.mean)
      dataouty1<-apply(dataouty,2,geometric.mean)
      df<-data.frame(a=dataoutx2,b=dataouty1)
      out <- cor.test(df$a,df$b) ; r <- out$estimate ; p <- out$p.value
      out.rho <- cor.test(df$a,df$b,method="spearman") ; rho <- out.rho$estimate ; p.rho <- out.rho$p.value
      lm_eqn <- function(df){
        m <- lm(b ~ a, df);
        eq <- substitute(~~italic("r")~"="~r*","~~italic("p")~"="~p,
                         list(a = format(coef(m)[1], digits = 3),
                              b = format(coef(m)[2], digits = 3),
                              r = format(r, digits = 3),
                              p = format(p, digits=3)))
        as.character(as.expression(eq));
      }
      lm_eqn.rho <- function(df){
        eq <- substitute(~~italic("rho")~"="~rho*","~~italic("p.rho")~"="~p.rho,
                         list(rho = format(rho, digits = 3),
                              p.rho = format(p.rho, digits=3)))
        as.character(as.expression(eq));
      }
      lm_eqnx<-lm_eqn(df)
      lm_eqnx1<-gsub("c\\(cor = ","",lm_eqnx)
      lm_eqnx1<-gsub("\\) \\* "," \\* ",lm_eqnx1)
      lm_eqnx.rho<-lm_eqn.rho(df)
      lm_eqnx.rho1<-gsub("c\\(rho = ","",lm_eqnx.rho)
      lm_eqnx.rho1<-gsub("\\) \\* "," \\* ",lm_eqnx.rho1)
      df$label_names<-names(dataouty1)
      corpointcolx<<-input$loadprosetcorpointcol
      corlinecolx<<-input$loadprosetcorlinecol
      corproppx<-ggplot(df, aes(a, b)) +
        geom_point(shape = 16, size = 7, color =corpointcolx, show.legend = FALSE, alpha = .8 ) +
        geom_smooth(method=lm,se=F,show.legend=F, color =corlinecolx) +
        geom_text(x = min(df$a,na.rm = T) + 0.5*(max(df$a,na.rm = T)-min(df$a,na.rm = T)), 
                  y = min(df$b,na.rm = T)+min(0.5,0.05*(max(df$b,na.rm = T)-min(df$b,na.rm = T))), 
                  label = lm_eqnx1,
                  parse = TRUE,show.legend=F,color="black",size = 7) +
        geom_text(x = min(df$a,na.rm = T) + 0.5*(max(df$a,na.rm = T)-min(df$a,na.rm = T)), 
                  y = min(df$b,na.rm = T), label = lm_eqnx.rho1,
                  parse = TRUE,show.legend=F,color="black",size = 7) +
        geom_text_repel(data=df,aes(label=label_names),size=6,color =corpointcolx) +
        labs(x = ylabx, y = ylabx1)+
        theme_bw()+
        theme(plot.title = element_text(size=21),
              #strip.text = element_text(size=18),
              axis.title = element_text(size=17),
              axis.text = element_text(size=15),
              legend.position="none",
              legend.title=element_text(size=15),
              legend.text=element_text(size=14),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    }else{
      corproppx<-ggplot(data.frame(x=1,y=1),aes(x=x, y=y, label = paste0("No data searched in the data")))+
        geom_text(size=10)+
        theme_void()
    }
    corproppx
  },height=loadprosetcorheight,width=loadprosetcorheight)
  prosetcorplotout<-reactive({
    prodatadfx<<-prodataout()
    proT50dataoutx<<-proT50dataout()
    prodatadfx1<-prodatadfx[,-c(1,2)]
    proT50dataoutx1<-proT50dataoutx[,-c(1,2)]
    prodatadfx2<-t(as.matrix(apply(prodatadfx1,2,geometric.mean)))
    proT50dataoutx2<-t(as.matrix(apply(proT50dataoutx1,2,geometric.mean)))
    if(input$loadprosetxaxis==1){
      dataoutx<-uploadprosetlistout()$proabunoutx3
      dataoutx1<<-dataoutx[,-c(1,2)]
      if(input$oneprosetxylabelif){
        ylabx<-input$oneprosetxlabel
      }else{
        ylabx<-"Protein set average abundance"
      }
    }
    else if(input$loadprosetxaxis==2){
      dataoutx<-uploadprosetlistout()$proT50dataoutx3
      dataoutx1<<-dataoutx[,-c(1,2)]
      if(input$oneprosetxylabelif){
        ylabx<-input$oneprosetxlabel
      }else{
        ylabx<-"Protein set average lifetime"
      }
    }
    else if(input$loadprosetxaxis==3){
      dataoutx1<-prodatadfx2
      if(input$oneprosetxylabelif){
        ylabx<-input$oneprosetylabel
      }else{
        ylabx<-"Proteome average abundance"
      }
    }
    else{
      dataoutx1<-proT50dataoutx2
      if(input$oneprosetxylabelif){
        ylabx<-input$oneprosetylabel
      }else{
        ylabx<-"Proteome average lifetime"
      }
    }
    
    if(input$loadprosetyaxis==1){
      dataouty<-prodatadfx2
      if(input$oneprosetxylabelif){
        ylabx1<-input$oneprosetylabel
      }else{
        ylabx1<-"Proteome average abundance"
      }
    }
    else if(input$loadprosetyaxis==2){
      dataouty<-proT50dataoutx2
      if(input$oneprosetxylabelif){
        ylabx1<-input$oneprosetylabel
      }else{
        ylabx1<-"Proteome average lifetime"
      }
    }
    else if(input$loadprosetyaxis==3){
      dataoutx<-uploadprosetlistout()$proabunoutx3
      dataouty<<-dataoutx[,-c(1,2)]
      if(input$oneprosetxylabelif){
        ylabx1<-input$oneprosetxlabel
      }else{
        ylabx1<-"Protein set average abundance"
      }
    }
    else{
      dataoutx<-uploadprosetlistout()$proT50dataoutx3
      dataouty<<-dataoutx[,-c(1,2)]
      if(input$oneprosetxylabelif){
        ylabx1<-input$oneprosetxlabel
      }else{
        ylabx1<-"Protein set average lifetime"
      }
    }
    ##
    #dataoutx1<<-dataoutx1
    #dataouty<<-dataouty
    if(nrow(dataoutx1)>0){
      colnames(dataoutx1)<-colnames(prodatadfx1)
      colnames(dataouty)<-colnames(prodatadfx1)
      dataoutx2<-apply(dataoutx1,2,geometric.mean)
      dataouty1<-apply(dataouty,2,geometric.mean)
      df<-data.frame(a=dataoutx2,b=dataouty1)
      out <- cor.test(df$a,df$b) ; r <- out$estimate ; p <- out$p.value
      out.rho <- cor.test(df$a,df$b,method="spearman") ; rho <- out.rho$estimate ; p.rho <- out.rho$p.value
      lm_eqn <- function(df){
        m <- lm(b ~ a, df);
        eq <- substitute(~~italic("r")~"="~r*","~~italic("p")~"="~p,
                         list(a = format(coef(m)[1], digits = 3),
                              b = format(coef(m)[2], digits = 3),
                              r = format(r, digits = 3),
                              p = format(p, digits=3)))
        as.character(as.expression(eq));
      }
      lm_eqn.rho <- function(df){
        eq <- substitute(~~italic("rho")~"="~rho*","~~italic("p.rho")~"="~p.rho,
                         list(rho = format(rho, digits = 3),
                              p.rho = format(p.rho, digits=3)))
        as.character(as.expression(eq));
      }
      lm_eqnx<-lm_eqn(df)
      lm_eqnx1<-gsub("c\\(cor = ","",lm_eqnx)
      lm_eqnx1<-gsub("\\) \\* "," \\* ",lm_eqnx1)
      lm_eqnx.rho<-lm_eqn.rho(df)
      lm_eqnx.rho1<-gsub("c\\(rho = ","",lm_eqnx.rho)
      lm_eqnx.rho1<-gsub("\\) \\* "," \\* ",lm_eqnx.rho1)
      df$label_names<-names(dataouty1)
      corpointcolx<<-input$loadprosetcorpointcol
      corlinecolx<<-input$loadprosetcorlinecol
      corproppx<-ggplot(df, aes(a, b)) +
        geom_point(shape = 16, size = 7, color =corpointcolx, show.legend = FALSE, alpha = .8 ) +
        geom_smooth(method=lm,se=F,show.legend=F, color =corlinecolx) +
        geom_text(x = min(df$a,na.rm = T) + 0.5*(max(df$a,na.rm = T)-min(df$a,na.rm = T)), 
                  y = min(df$b,na.rm = T)+min(0.5,0.05*(max(df$b,na.rm = T)-min(df$b,na.rm = T))), 
                  label = lm_eqnx1,
                  parse = TRUE,show.legend=F,color="black",size = 7) +
        geom_text(x = min(df$a,na.rm = T) + 0.5*(max(df$a,na.rm = T)-min(df$a,na.rm = T)), 
                  y = min(df$b,na.rm = T), label = lm_eqnx.rho1,
                  parse = TRUE,show.legend=F,color="black",size = 7) +
        geom_text_repel(data=df,aes(label=label_names),size=6,color =corpointcolx) +
        labs(x = ylabx, y = ylabx1)+
        theme_bw()+
        theme(plot.title = element_text(size=21),
              #strip.text = element_text(size=18),
              axis.title = element_text(size=17),
              axis.text = element_text(size=15),
              legend.position="none",
              legend.title=element_text(size=15),
              legend.text=element_text(size=14),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    }else{
      corproppx<-ggplot(data.frame(x=1,y=1),aes(x=x, y=y, label = paste0("No data searched in the data")))+
        geom_text(size=10)+
        theme_void()
    }
    corproppx
  })
  output$prosetcorplotdl<-downloadHandler(
    filename = function(){paste("ProteinSet.Proteome.CorPlot",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width = loadprosetcorheight()/100,height = loadprosetcorheight()/100)
      print(prosetcorplotout())
      dev.off()
    }
  )
  ##2
  output$prosetcorplotx<-renderPlot({
    prodatadfx<<-prodataout()
    proT50dataoutx<<-proT50dataout()
    prodatadfx1<-prodatadfx[,-c(1,2)]
    proT50dataoutx1<-proT50dataoutx[,-c(1,2)]
    prodatadfx2<-apply(prodatadfx1,2,geometric.mean)
    proT50dataoutx2<-apply(proT50dataoutx1,2,geometric.mean)
    if(input$loadprosetxaxisx==1){
      dataoutx1<-uploadprosetlistxout()$proabunoutx3
      if(input$prosetxylabelif){
        ylabx1<-input$prosetxlabel
      }else{
        ylabx1<-"Protein set 1 average abundance"
      }
    }
    else if(input$loadprosetxaxisx==2){
      dataoutx1<-uploadprosetlistxout()$proT50dataoutx3
      if(input$prosetxylabelif){
        ylabx1<-input$prosetxlabel
      }else{
        ylabx1<-"Protein set 1 average lifetime"
      }
    }
    else if(input$loadprosetxaxisx==3){
      dataoutx1<-uploadprosetlistxout()$proabunoutx3x
      if(input$prosetxylabelif){
        ylabx1<-input$prosetxlabel
      }else{
        ylabx1<-"Protein set 2 average abundance"
      }
    }
    else{
      dataoutx1<-uploadprosetlistxout()$proT50dataoutx3x
      if(input$prosetxylabelif){
        ylabx1<-input$prosetxlabel
      }else{
        ylabx1<-"Protein set 2 average lifetime"
      }
    }
    dataoutx11<<-dataoutx1[,-c(1,2)]
    if(input$loadprosetyaxisx==1){
      dataoutx2<-uploadprosetlistxout()$proabunoutx3x
      if(input$prosetxylabelif){
        ylabx2<-input$prosetylabel
      }else{
        ylabx2<-"Protein set 2 average abundance"
      }
    }
    else if(input$loadprosetyaxisx==2){
      dataoutx2<-uploadprosetlistxout()$proT50dataoutx3x
      if(input$prosetxylabelif){
        ylabx2<-input$prosetylabel
      }else{
        ylabx2<-"Protein set 2 average lifetime"
      }
    }
    else if(input$loadprosetyaxisx==3){
      dataoutx2<-uploadprosetlistxout()$proabunoutx3
      if(input$prosetxylabelif){
        ylabx2<-input$prosetylabel
      }else{
        ylabx2<-"Protein set 1 average abundance"
      }
    }
    else{
      dataoutx2<-uploadprosetlistxout()$proT50dataoutx3
      if(input$prosetxylabelif){
        ylabx2<-input$prosetylabel
      }else{
        ylabx2<-"Protein set 1 average lifetime"
      }
    }
    dataoutx22<<-dataoutx2[,-c(1,2)]
    ##
    if(nrow(dataoutx11)>0 & nrow(dataoutx2)>0){
      dataoutx1x<-apply(dataoutx11,2,geometric.mean)
      dataoutx2x<-apply(dataoutx22,2,geometric.mean)
      df<-data.frame(a=dataoutx1x,b=dataoutx2x)
      out <- cor.test(df$a,df$b) ; r <- out$estimate ; p <- out$p.value
      out.rho <- cor.test(df$a,df$b,method="spearman") ; rho <- out.rho$estimate ; p.rho <- out.rho$p.value
      lm_eqn <- function(df){
        m <- lm(b ~ a, df);
        eq <- substitute(~~italic("r")~"="~r*","~~italic("p")~"="~p,
                         list(a = format(coef(m)[1], digits = 3),
                              b = format(coef(m)[2], digits = 3),
                              r = format(r, digits = 3),
                              p = format(p, digits=3)))
        as.character(as.expression(eq));
      }
      lm_eqn.rho <- function(df){
        eq <- substitute(~~italic("rho")~"="~rho*","~~italic("p.rho")~"="~p.rho,
                         list(rho = format(rho, digits = 3),
                              p.rho = format(p.rho, digits=3)))
        as.character(as.expression(eq));
      }
      lm_eqnx<-lm_eqn(df)
      lm_eqnx1<-gsub("c\\(cor = ","",lm_eqnx)
      lm_eqnx1<-gsub("\\) \\* "," \\* ",lm_eqnx1)
      lm_eqnx.rho<-lm_eqn.rho(df)
      lm_eqnx.rho1<-gsub("c\\(rho = ","",lm_eqnx.rho)
      lm_eqnx.rho1<-gsub("\\) \\* "," \\* ",lm_eqnx.rho1)
      df$label_names<-names(proT50dataoutx2)
      corpointcolx<<-input$loadprosetcorpointcolx
      corlinecolx<<-input$loadprosetcorlinecolx
      corproppx<-ggplot(df, aes(a, b)) +
        geom_point(shape = 16, size = 7, color =corpointcolx, show.legend = FALSE, alpha = .8 ) +
        geom_smooth(method=lm,se=F,show.legend=F, color =corlinecolx) +
        geom_text(x = min(df$a,na.rm = T) + 0.5*(max(df$a,na.rm = T)-min(df$a,na.rm = T)), 
                  y = min(df$b,na.rm = T)+min(0.5,0.05*(max(df$b,na.rm = T)-min(df$b,na.rm = T))), label = lm_eqnx1,
                  parse = TRUE,show.legend=F,color="black",size = 7) +
        geom_text(x = min(df$a,na.rm = T) + 0.5*(max(df$a,na.rm = T)-min(df$a,na.rm = T)), 
                  y = min(df$b,na.rm = T), label = lm_eqnx.rho1,
                  parse = TRUE,show.legend=F,color="black",size = 7) +
        geom_text_repel(data=df,aes(label=label_names),size=6,color =corpointcolx) +
        labs(x = ylabx1, y = ylabx2)+
        theme_bw()+
        theme(plot.title = element_text(size=21),
              #strip.text = element_text(size=18),
              axis.title = element_text(size=17),
              axis.text = element_text(size=15),
              legend.position="none",
              legend.title=element_text(size=15),
              legend.text=element_text(size=14),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    }else{
      corproppx<-ggplot(data.frame(x=1,y=1),aes(x=x, y=y, label = paste0("No data searched in the data")))+
        geom_text(size=10)+
        theme_void()
    }
    corproppx
  },height=loadprosetcorheightx,width=loadprosetcorheightx)
  prosetcorplotxout<-reactive({
    prodatadfx<<-prodataout()
    proT50dataoutx<<-proT50dataout()
    prodatadfx1<-prodatadfx[,-c(1,2)]
    proT50dataoutx1<-proT50dataoutx[,-c(1,2)]
    prodatadfx2<-apply(prodatadfx1,2,geometric.mean)
    proT50dataoutx2<-apply(proT50dataoutx1,2,geometric.mean)
    if(input$loadprosetxaxisx==1){
      dataoutx1<-uploadprosetlistxout()$proabunoutx3
      if(input$prosetxylabelif){
        ylabx1<-input$prosetxlabel
      }else{
        ylabx1<-"Protein set 1 average abundance"
      }
    }
    else if(input$loadprosetxaxisx==2){
      dataoutx1<-uploadprosetlistxout()$proT50dataoutx3
      if(input$prosetxylabelif){
        ylabx1<-input$prosetxlabel
      }else{
        ylabx1<-"Protein set 1 average lifetime"
      }
    }
    else if(input$loadprosetxaxisx==3){
      dataoutx1<-uploadprosetlistxout()$proabunoutx3x
      if(input$prosetxylabelif){
        ylabx1<-input$prosetxlabel
      }else{
        ylabx1<-"Protein set 2 average abundance"
      }
    }
    else{
      dataoutx1<-uploadprosetlistxout()$proT50dataoutx3x
      if(input$prosetxylabelif){
        ylabx1<-input$prosetxlabel
      }else{
        ylabx1<-"Protein set 2 average lifetime"
      }
    }
    dataoutx11<<-dataoutx1[,-c(1,2)]
    if(input$loadprosetyaxisx==1){
      dataoutx2<-uploadprosetlistxout()$proabunoutx3x
      if(input$prosetxylabelif){
        ylabx2<-input$prosetylabel
      }else{
        ylabx2<-"Protein set 2 average abundance"
      }
    }
    else if(input$loadprosetyaxisx==2){
      dataoutx2<-uploadprosetlistxout()$proT50dataoutx3x
      if(input$prosetxylabelif){
        ylabx2<-input$prosetylabel
      }else{
        ylabx2<-"Protein set 2 average lifetime"
      }
    }
    else if(input$loadprosetyaxisx==3){
      dataoutx2<-uploadprosetlistxout()$proabunoutx3
      if(input$prosetxylabelif){
        ylabx2<-input$prosetylabel
      }else{
        ylabx2<-"Protein set 1 average abundance"
      }
    }
    else{
      dataoutx2<-uploadprosetlistxout()$proT50dataoutx3
      if(input$prosetxylabelif){
        ylabx2<-input$prosetylabel
      }else{
        ylabx2<-"Protein set 1 average lifetime"
      }
    }
    dataoutx22<<-dataoutx2[,-c(1,2)]
    ##
    if(nrow(dataoutx11)>0 & nrow(dataoutx2)>0){
      dataoutx1x<-apply(dataoutx11,2,geometric.mean)
      dataoutx2x<-apply(dataoutx22,2,geometric.mean)
      df<-data.frame(a=dataoutx1x,b=dataoutx2x)
      out <- cor.test(df$a,df$b) ; r <- out$estimate ; p <- out$p.value
      out.rho <- cor.test(df$a,df$b,method="spearman") ; rho <- out.rho$estimate ; p.rho <- out.rho$p.value
      lm_eqn <- function(df){
        m <- lm(b ~ a, df);
        eq <- substitute(~~italic("r")~"="~r*","~~italic("p")~"="~p,
                         list(a = format(coef(m)[1], digits = 3),
                              b = format(coef(m)[2], digits = 3),
                              r = format(r, digits = 3),
                              p = format(p, digits=3)))
        as.character(as.expression(eq));
      }
      lm_eqn.rho <- function(df){
        eq <- substitute(~~italic("rho")~"="~rho*","~~italic("p.rho")~"="~p.rho,
                         list(rho = format(rho, digits = 3),
                              p.rho = format(p.rho, digits=3)))
        as.character(as.expression(eq));
      }
      lm_eqnx<-lm_eqn(df)
      lm_eqnx1<-gsub("c\\(cor = ","",lm_eqnx)
      lm_eqnx1<-gsub("\\) \\* "," \\* ",lm_eqnx1)
      lm_eqnx.rho<-lm_eqn.rho(df)
      lm_eqnx.rho1<-gsub("c\\(rho = ","",lm_eqnx.rho)
      lm_eqnx.rho1<-gsub("\\) \\* "," \\* ",lm_eqnx.rho1)
      df$label_names<-names(proT50dataoutx2)
      corpointcolx<<-input$loadprosetcorpointcolx
      corlinecolx<<-input$loadprosetcorlinecolx
      corproppx<-ggplot(df, aes(a, b)) +
        geom_point(shape = 16, size = 7, color =corpointcolx, show.legend = FALSE, alpha = .8 ) +
        geom_smooth(method=lm,se=F,show.legend=F, color =corlinecolx) +
        geom_text(x = min(df$a,na.rm = T) + 0.5*(max(df$a,na.rm = T)-min(df$a,na.rm = T)), 
                  y = min(df$b,na.rm = T)+min(0.5,0.05*(max(df$b,na.rm = T)-min(df$b,na.rm = T))), label = lm_eqnx1,
                  parse = TRUE,show.legend=F,color="black",size = 7) +
        geom_text(x = min(df$a,na.rm = T) + 0.5*(max(df$a,na.rm = T)-min(df$a,na.rm = T)), 
                  y = min(df$b,na.rm = T), label = lm_eqnx.rho1,
                  parse = TRUE,show.legend=F,color="black",size = 7) +
        geom_text_repel(data=df,aes(label=label_names),size=6,color =corpointcolx) +
        labs(x = ylabx1, y = ylabx2)+
        theme_bw()+
        theme(plot.title = element_text(size=21),
              #strip.text = element_text(size=18),
              axis.title = element_text(size=17),
              axis.text = element_text(size=15),
              legend.position="none",
              legend.title=element_text(size=15),
              legend.text=element_text(size=14),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    }else{
      corproppx<-ggplot(data.frame(x=1,y=1),aes(x=x, y=y, label = paste0("No data searched in the data")))+
        geom_text(size=10)+
        theme_void()
    }
    corproppx
  })
  output$prosetcorplotxdl<-downloadHandler(
    filename = function(){paste("ProteinSet1.ProteinSet2.CorPlot",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width = loadprosetcorheightx()/100,height = loadprosetcorheightx()/100)
      print(prosetcorplotxout())
      dev.off()
    }
  )
  
  
})

shinyApp(ui = ui, server = server)
