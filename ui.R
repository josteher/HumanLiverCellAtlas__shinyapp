library(shiny)

genenames <- readRDS("./Data/shiny__GENES__Human_Liver_Atlas_Aizarani_et_al_Nature_2019.Rds")
shinyUI(navbarPage("A Human Liver Cell Atlas reveals Heterogeneity and Epithelial Progenitors", position="fixed-top",
                   
                   
                   ###################################
                   ###################################
                   ###################################
                   # CAR & PaS cells (Wt)
                   
                   
                   tabPanel("Healthy Human Liver",
                            br(),
                            br(),
                            br(),
                            a(href="https://imprint.ie-freiburg.mpg.de/imprint/imprint.php?ref=12", "Imprint"),
                            br(),
                            a(href="https://imprint.ie-freiburg.mpg.de/privacy/privacy.php?ref=12", "Privacy Notice"),
                            br(),
                    
                            p("Shiny app contact:", style = "font-size:18px", align="left",
                              a(href  = "mailto:herman@ie-freiburg.mpg.de", "Josip S. Herman",
                                style = "font-size:18px")),
                            br(),

                            
                            fluidRow(
                              column(6,
                                  h1("A Human Liver Cell Atlas reveals Heterogeneity and Epithelial Progenitors"),
                                  a(href  = "https://doi.org/10.1038/s41586-019-1373-2", "https://doi.org/10.1038/s41586-019-1373-2",
                                    style = "font-size:18px"),
                                  p(
                                    a(href  = "mailto:aizarani@ie-freiburg.mpg.de", "Nadim Aizarani",
                                                                                 style = "font-size:20px"),
                                    style = "font-size:20px",
                                    ", Antonio Saviano,  Sagar,  Laurent Mailly,  Sarah Durand, Josip S. Herman, Patrick Pessaux,",
                                     a(href  = "mailto:thomas.baumert@unistra.fr", "Thomas F. Baumert",
                                       style = "font-size:20px"),
                                      "," ,
                                      a(href  = "mailto:gruen@ie-freiburg.mpg.de", "Dominic Grün",
                                      style = "font-size:20px")
                                    )
                              ),
                              
                              column(3,
                                     shiny::imageOutput("mpi_ie_logo", 
                                                        height = "100%",
                                                        width = "100%")
                              ),

                              column(3,
                                     shiny::imageOutput("inserm_logo",
                                                        height = "100%",
                                                        width = "100%")
                              )
                            ),

                            
                            h2("Identified clusters after k-medoids clustering and sorted samples"),
                            h5("(Loading of data takes app. 30s)"),
                            
                            
                            fluidRow(
                              column(6,
                                     plotOutput("plotmap",       
                                                width = "100%",
                                                height = "720px")),
                              column(6,
                                     plotOutput("plotsymbolsmap",
                                                width = "100%",
                                                height = "720px"))
                            ),


                            br(),
                            h2("Compare one cluster against all the other clusters"),

                            fluidRow(
                              column(2,
                                     numericInput("cdiff_cl",
                                                  value=1,
                                                  label = "Choose a cluster",
                                                  min = 1,
                                                  max = 1000  )),
                              column(5,
                                     plotOutput("plotclust",
                                                width = "100%",
                                                height = "500px"),
                                     offset = 2)

                            ),

                            h5("mean.ncl = mean expression of gene not in cluster"),
                            h5("mean.cl  = mean expression of gene in cluster"),
                            h5("fc  = fold change compared to all other clusters"),
                            h5("pv = p-value"),
                            h5("padj = adjusted p-value"),
                            br(),

                            fluidRow(
                              column(2,
                                     numericInput("cdiff_pval",
                                                  value = 0.05,
                                                  "Set p-value threshhold"),
                                     numericInput("cdiff_padj",
                                                  value = 1,
                                                  "Set adjusted p-value threshhold"))
                            ),
                            fluidRow(
                              column(6,
                                     DT::dataTableOutput("upgenes")),
                              column(6,
                                     DT::dataTableOutput("downgenes"))
                            ),

                            br(),
                            br(),
                            br(),
                            
                            # h2("Plot gene expression in tSNE-Map"),
                            # fluidRow(
                            #   column(2,
                            #          selectInput("ptexp_gene",
                            #                      label = "Enter a genesymbol",
                            #                      genenames,
                            #                      selected = "ALB"))
                            #   ),
                            # 
                            # fluidRow(
                            #   column(6,
                            #          plotOutput("plotexptsne",
                            #                     width = "100%",
                            #                     height = "500px")),
                            #   column(6,
                            #          plotOutput("plotexptsne_logsc",
                            #                     width = "100%",
                            #                     height = "500px"))
                            # ),
                            # 
                            # br(),
                            # br(),
                            # br(),
                            
                            # h2("Violinplots of gene expression for all clusters"),
                            # fluidRow(
                            #   column(2,
                            #          selectInput("gene_v1",
                            #                      label = "Enter a genesymbol",
                            #                      genenames,
                            #                      selected = "TACSTD2"))
                            # ),

                            # fluidRow(
                            #   column(12,
                            #          plotOutput("plotviolin",
                            #                     width = "100%",
                            #                     height = "200px"))
                            #   ),
                            # 
                            # br(),
                            # br(),
                            # br(),
                            
                            
                            h2("Plot gene expression in tSNE-Map"),
                            fluidRow(
                              column(2,
                                     selectInput("ptexp_gene",
                                                 label = "Enter a genesymbol",
                                                 genenames,
                                                 selected = "ALB"))
                            ),
                            
                            fluidRow(
                              column(6,
                                     plotOutput("plotexptsne",
                                                width = "100%",
                                                height = "500px")),
                              column(6,
                                     plotOutput("plotexptsne_logsc",
                                                width = "100%",
                                                height = "500px"))
                            ),
                            
                            br(),
                            br(),
                            br(),
                            
                            


                            h2("Compare gene expression of two clusters"),

                            fluidRow(column(2,
                                            selectInput("cluster1",
                                                        label = "Choose cluster A",
                                                        as.character(1:39),
                                                        selected = "4"),
                                            selectInput("cluster2",
                                                        label = "Choose cluster B",
                                                        as.character(1:39),
                                                        selected = "7"))
                            ),

                            fluidRow(
                              column(6,
                                     plotOutput("plotclust_cl1",
                                                width = "100%",
                                                height = "600px")),
                              column(6,
                                     plotOutput("plotclust_cl2",
                                                width = "100%",
                                                height = "600px"))
                            ),


                            fluidRow(
                              column(2,
                                     numericInput("c2c_pval",
                                                  value = 0.05,
                                                  "Set p-value threshhold"),
                                     numericInput("padj",
                                                  value = 0.05,
                                                  "Set adjusted p-value threshhold"))
                            ),
                            fluidRow(
                              column(6,
                                     DT::dataTableOutput("diffexpnb_2cl_1")),
                              column(6,
                                     DT::dataTableOutput("diffexpnb_2cl_2"))
                            ),


                            br(),
                            br(),
                            br(),

                            fluidRow(
                              column(2,
                                     numericInput("top_genes",
                                                  value = 100,
                                                  "Choose number of top genes to display"))
                            ),



                            br(),
                            h2("MA Plot for cluster comparison"),
                            fluidRow(
                              column(12,
                                     plotOutput("maplot",
                                                width = "100%",
                                                height = "1000px"))
                            )#,
                            
                            
                            
                            
                            
                            # fluidRow(
                            #   column(2,
                            #          selectInput("maplot_iactiv_cp2",
                            #                        label = "Choose MA plot mode",
                            #                        c("interactive", "non-interactive"),
                            #                        selected = "non-interactive"))
                            # ),



                            # br(),
                            # h2("Interactive MA Plot"),
                            # fluidRow(
                            #   column(12,
                            #          plotlyOutput("maplot_2",
                            #                       width = "100%",
                            #                       height = "1000px"))
                            # ),
                            # br(),
                            # br(),
                            # br()
                   ),
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   tabPanel("Gene expression plots",
                            
                            br(),
                            br(),
                            
                            
                            h2("Plot gene expression in tSNE-Map"),
                            fluidRow(
                              column(2,
                                     selectInput("ptexp_gene_2",
                                                 label = "Enter a genesymbol",
                                                 genenames,
                                                 selected = "ALB"))
                            ),
                            
                            fluidRow(
                              column(6,
                                     plotOutput("plotexptsne_2",
                                                width = "100%",
                                                height = "500px")),
                              column(6,
                                     plotOutput("plotexptsne_logsc_2",
                                                width = "100%",
                                                height = "500px"))
                            ),
                            
                            
                            br(),
                            br(),

                            
                            h2("Violinplots of gene expression for all clusters"),
                            
                            br(),
                            
                            fluidRow(
                              column(12,
                                     plotOutput("tsne_facet_exp",
                                                width = "100%",
                                                height = "800px"))
                            ),
                            
                            
                            
                            
                            
                            
                            br(),
                            
                            fluidRow(
                              column(12,
                                     selectizeInput("genes_fac_1",
                                                    label = "Enter a genesymbol",
                                                    genenames,
                                                    multiple = T,
                                                    selected = c("TACSTD2", "ALB", "CFTR", "PECAM1") ))
                            ),
                            
                            
                            
                            
                            
                            
                            
                            br(),
                            
                            fluidRow(
                              column(12,
                                     selectizeInput("clusters_fac_1",
                                                    label = "Enter cluster numbers",
                                                    1:39,
                                                    multiple = T,
                                                    selected = 1:39 ))
                            ),
                            
                            
                            
                            
                            
                            
                            
                            br(),
                            
                            
                            fluidRow(
                              column(12,
                                     plotOutput("plotviolin",
                                                width = "100%",
                                                height = "1000px"))
                            )
                            
                            
                            
                            


                            
                            )
))