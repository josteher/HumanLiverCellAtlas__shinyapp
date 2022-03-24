### shiny Server

library(shiny)
library(DT)
library(RColorBrewer)
library(data.table)
library(RaceID)
library(ggrepel)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)



# Modified version of plotsymbolsmap taking a data.frame with tsne coordinates, types => plotmap_df
plotsymbolsmap_mod <- function (object, types=object$type, subset = NULL, samples_col = NULL, cex = 0.25, 
                               fr = FALSE, leg = TRUE, map = TRUE, leg.pos="topleft", leg.cex=0.75, axes=F) 
                          {
                            if (is.null(subset)) 
                              subset <- unique(types)
                            h <- sort(unique(types)) %in% subset
                            if (!is.null(subset)) {
                              fp <- rep(FALSE, length(types))
                              fp[types %in% subset] <- TRUE
                            }
                            if (is.null(samples_col)) {
                              samples_col <- rainbow(length(unique(types[fp])))
                            }
                            else {
                              samples_col <- samples_col[h]
                            }
                            # if (fr | dim(object@tsne)[1] == 0) 
                            #   d <- object@fr
                            # else 
                            d <- object[c("X", "Y")]#object@tsne
                            if (map) {
                              plot(d, xlab = "", ylab = "", axes = axes, cex = cex, 
                                   pch = 20, col = "grey")
                              for (i in 1:length(unique(types[fp]))) {
                                f <- types == sort(unique(types[fp]))[i]
                                points(d[f, 1], d[f, 2], col = samples_col[i], pch = 20, 
                                       cex = cex)
                              }
                            }
                            else {
                              plot(d, xlab = "", ylab = "", axes = axes, cex = 0, 
                                   pch = 20, col = "grey", xlim = c(min(d[, 1]), max(d[, 
                                                                                       1])), ylim = c(min(d[, 2]), max(d[, 2])))
                            }
                            if (leg) 
                              legend(leg.pos, legend = sort(unique(types[fp])), col = samples_col, 
                                     pch = 20, cex = leg.cex, bty = "n")
                          }





cdiff      <- readRDS("./Data/shiny__cdiff__Human_Liver_Atlas_Aizarani_et_al_Nature_2019.Rds")
plotmap_df <- readRDS("./Data/shiny__plotmap_df__Human_Liver_Atlas_Aizarani_et_al_Nature_2019.Rds")
sc_counts  <- readRDS("./Data/shiny__sc_counts__Human_Liver_Atlas_Aizarani_et_al_Nature_2019.Rds")
sc_fcol    <- readRDS("./Data/shiny__sc_fcol__Human_Liver_Atlas_Aizarani_et_al_Nature_2019.Rds")
names(sc_fcol) <- 1:length(sc_fcol)
sc_cpart   <- readRDS("./Data/shiny__sc_cpart__Human_Liver_Atlas_Aizarani_et_al_Nature_2019.Rds")
sc_ndata   <- as.tibble(fread("./Data/shiny__sc_ndata__pairwise_comparisons_of_all_clusters_Human_Liver_Atlas_Aizarani_et_al_Nature_2019.csv"))
pairwise_dir <- "./Data/pairwise_comparisons/"
mpi_logo     <- "./logos/MPIIE_en_logo_text.png"
inserm_logo  <- "./logos/Inserm_UniStra.png"






# Define server logic
shinyServer(function(input, output) {
  
  
  # autoInvalidate <- reactiveTimer(3000)
  
  
  # Diplay Logos of 
  
  output$mpi_ie_logo <- renderImage({
    list(src = mpi_logo,
         contentType = 'image/png',
         width = "100%",
         height = "100%")
  }, deleteFile = FALSE)

  output$inserm_logo <- renderImage({
    list(src = inserm_logo,
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         align = "right")
  }, deleteFile = FALSE)


  
  ###
  ### Human Liver tSNE Map
  ###
  
  output$plotmap <- renderPlot({
    # plotmap(sc, cex=1.5)

    ggplot(plotmap_df, aes(X, Y, color=factor(cluster) )) +
      geom_point(alpha=0.5) +
      # plot and label medoids
      geom_point(data = plotmap_df %>% filter(is_medoid == T ),
                 aes(fill=cluster),
                 color="black",
                 size=4,
                 pch=21,
                 alpha=0.8,
                 stroke=0.8) +
      geom_label_repel(data = plotmap_df %>% filter(is_medoid == T ),
                       aes(label=cluster, color=cluster),
                       fill="#fafafa",
                       size=8,
                       nudge_x = -0.05,
                       label.padding = unit(0.15, "lines"),
                       label.r = unit(0.25, "lines"),
                       label.size = 0.0,
                       alpha=0.9,
                       fontface= "bold.italic") +
      # geom_text_repel() +
      # use colors from RaceID base plotting
      scale_colour_manual(name = "cluster",
                          values = sc_fcol) +
      scale_fill_manual(name = "cluster",
                        values = sc_fcol) +
      # coord_fixed(ratio = 1/1.7) +
      theme_void() +
      theme(legend.position = "none" )

    
    # plot(1:10)

  })
  
  output$plotsymbolsmap <- renderPlot({
 
    set.seed(12345)
    plotsymbolsmap_mod(plotmap_df, cex=0.6, leg.cex = 1.2, samples_col = c("#0000FFFF","#AA00FFFF","#00AAFFFF","#FF00AAFF",
                                                           "#AAFF00FF","#FF0000FF","#00FF00FF","#FFAA00FF","#00FFAAFF") 
                      )

  })


  ########################################################
  ### cdiff output
  ###
  ### Plot a particular cluster
  output$plotclust <- renderPlot({
    # plotmap_m(sc,
    #           my_part = input$cdiff_cl,
    #           cex = 1.5)

    ggplot(plotmap_df, aes(X, Y )) +
      geom_point(alpha=0.6, color="grey") +
      # plot and label medoids
      geom_point(data = plotmap_df %>% filter(cluster %in% input$cdiff_cl ),
                 aes(color = factor(cluster)),
                 alpha=0.6) +
      geom_point(data = plotmap_df %>% filter(is_medoid == T, cluster %in% input$cdiff_cl ),
                 aes(fill=cluster),
                 color="black",
                 size=4,
                 pch=21,
                 alpha=0.8,
                 stroke=0.8) +
      geom_label_repel(data = plotmap_df %>% filter(is_medoid == T, cluster %in% input$cdiff_cl   ),
                       aes(label=cluster, color=cluster),
                       fill="#fafafa",
                       size=7,
                       nudge_x = -0.05,
                       label.padding = unit(0.15, "lines"),
                       label.r = unit(0.25, "lines"),
                       label.size = 0.0,
                       alpha=0.85,
                       fontface= "bold.italic") +
      # use colors from RaceID base plotting
      scale_colour_manual(name = "cluster",
                          values = sc_fcol) +
      scale_fill_manual(name = "cluster",
                        values = sc_fcol) +
      theme_void() +
      theme(legend.position = "none" )

  })

  # Make cdiff list and user input reactive
  r_cdiff  <- reactive(cdiff)
  cd_p_val <- reactive(input$cdiff_pval)
  cd_p_adj <- reactive(input$cdiff_padj)


  # Show two data tables of cdiff output for one cluster
  output$upgenes   <- renderDataTable( apply( r_cdiff()[[input$cdiff_cl]][r_cdiff()[[input$cdiff_cl]]$pv <= cd_p_val() & r_cdiff()[[input$cdiff_cl ]]$padj <= cd_p_adj(),],
                                                  2,
                                                  function(x){round(x,digits = 8)} ) )
  output$downgenes <- renderDataTable( apply( r_cdiff()[[input$cdiff_cl ]][r_cdiff()[[input$cdiff_cl ]]$pv <= cd_p_val() & r_cdiff()[[input$cdiff_cl ]]$padj <= cd_p_adj(),],
                                                  2,
                                                  function(x){round(x,digits = 8)} ) )

  
  ########################################################
  # plotexpmap
  ###
  ### Plotexpmap Gene expression in tSNE Map
  my_gene <- reactive(input$ptexp_gene)
  gene_df <- reactive(sc_ndata %>%
                        filter(gene %in% my_gene() ) %>%
                        gather(key = "cellid", value="scaled_expr", colnames(sc_ndata)[!(colnames(sc_ndata) %in% "gene")] ) %>%
                        left_join(plotmap_df, by="cellid") %>%
                        left_join( tibble(cellid=names(sc_counts), count=sc_counts), by="cellid" ) %>%
                        # transform scaled expression into transcript counts
                        mutate(expr = scaled_expr * min(count) + 0.1) %>%
                        arrange(gene, expr)
                      )
             

  output$plotexptsne       <- renderPlot(
                                    # plotexpmap( sc,input$ptexp_gene , logsc=F)
                                    gene_df() %>% 
                                      ggplot(aes(X, Y, color = expr )) +
                                      geom_point() + 
                                      scale_color_gradientn(colors = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100) ) +
                                      ggtitle(unique( my_gene() )) +
                                      theme_void() +
                                      # coord_fixed(ratio=1) +
                                      theme(plot.title = element_text(face = "bold.italic", hjust=0.5, size = 40))

                                    )

  output$plotexptsne_logsc <- renderPlot(
                                    # plotexpmap( sc,input$ptexp_gene , logsc=T, n = paste(input$ptexp_gene ,"log2"))
                                    gene_df() %>% 
                                      ggplot(aes(X, Y, color = log2(expr) )) +
                                      geom_point() + 
                                      scale_color_gradientn(colors = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100) ) +
                                      ggtitle(unique( my_gene() )) +
                                      theme_void() +
                                      # coord_fixed(ratio=1) +
                                      theme(plot.title = element_text(face = "bold.italic", hjust=0.5, size = 40))

                                    )
  
  
  
  
  
  # Second plotexmap plot
  my_gene_2 <- reactive(input$ptexp_gene_2)
  gene_df_2 <- reactive(sc_ndata %>%
                        filter(gene %in% my_gene_2() ) %>%
                        gather(key = "cellid", value="scaled_expr", colnames(sc_ndata)[!(colnames(sc_ndata) %in% "gene")] ) %>%
                        left_join(plotmap_df, by="cellid") %>%
                        left_join( tibble(cellid=names(sc_counts), count=sc_counts), by="cellid" ) %>%
                        # transform scaled expression into transcript counts
                        mutate(expr = scaled_expr * min(count) + 0.1) %>%
                        arrange(gene, expr)
  )
  
  
  output$plotexptsne_2       <- renderPlot(
    # plotexpmap( sc,input$ptexp_gene , logsc=F)
    gene_df_2() %>% 
      ggplot(aes(X, Y, color = expr )) +
      geom_point() + 
      scale_color_gradientn(colors = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100) ) +
      ggtitle(unique( my_gene_2() )) +
      theme_void() +
      # coord_fixed(ratio=1) +
      theme(plot.title = element_text(face = "bold.italic", hjust=0.5, size = 40))
    
  )
  
  output$plotexptsne_logsc_2 <- renderPlot(
    # plotexpmap( sc,input$ptexp_gene , logsc=T, n = paste(input$ptexp_gene ,"log2"))
    gene_df_2() %>% 
      ggplot(aes(X, Y, color = log2(expr) )) +
      geom_point() + 
      scale_color_gradientn(colors = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100) ) +
      ggtitle(unique( my_gene_2() )) +
      theme_void() +
      # coord_fixed(ratio=1) +
      theme(plot.title = element_text(face = "bold.italic", hjust=0.5, size = 40))
    
  )
  


  
  
  
  
  
  ########################################################
  ### Plot expression in all clusters for 1 gene in violin Plots
  
  my_genes <- reactive(input$genes_fac_1)
  genes_df <- reactive(sc_ndata %>%
                        filter(gene %in% my_genes() ) %>%
                        gather(key = "cellid", value="scaled_expr", colnames(sc_ndata)[!(colnames(sc_ndata) %in% "gene")] ) %>%
                        left_join(plotmap_df, by="cellid") %>%
                        left_join( tibble(cellid=names(sc_counts), count=sc_counts), by="cellid" ) %>%
                        # transform scaled expression into transcript counts
                        mutate(expr = scaled_expr * min(count) + 0.1) %>%
                        group_by(gene) %>%
                        arrange(gene, expr)
              )
    
  output$tsne_facet_exp       <- renderPlot({
                                 
                                 # autoInvalidate()

                                 genes_df() %>% 
                                   ggplot(aes(X, Y, color = log2(expr) )) +
                                   geom_point(size=0.8) + 
                                   scale_color_gradientn(colors = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100) ) +
                                   theme_void() +
                                   facet_wrap(facets = ~gene, ncol = 5 ) +
                                   coord_fixed(ratio = 1) +
                                   theme(strip.text.x = element_text(size = 15, face = "bold.italic", colour = "black", angle = 0))
                                  
                                })

    gene_violinplot <- reactive(
                                 
                              
                                genes_df() %>%
                                filter(cluster %in% input$clusters_fac_1 ) %>%
                                ggplot(aes(x=cluster,y=log2(expr) )) +
                                geom_point(aes(fill=cluster, color=cluster), position = "jitter", size =0.5, alpha=0.8) +
                                geom_violin(aes(fill=cluster), lwd=0.2) +
                                geom_boxplot(width=0.1, outlier.alpha = 0, fill="darkgrey",  color="black", lwd=0.2 ) +
                                # scale_y_continuous(name="Count", limits=c(0, 20000), 
                                #                    breaks=seq(0, 30000, 2500)) +
                                scale_colour_manual(name = "cluster",
                                                    values = sc_fcol) +
                                scale_fill_manual(name = "cluster",
                                                  values = sc_fcol) + 
                                facet_grid(rows = vars(gene), scales="free_y" ) +
                                theme_bw(base_size = 18) +
                                theme(strip.text.y = element_text(size = 15, face = "bold.italic", colour = "black", angle = 0),
                                      strip.background = element_rect(linetype = "blank", fill = "white" ),
                                      legend.position = "none" )

                                )



    output$plotviolin  <- renderPlot({
      
                                    # autoInvalidate()
                                    gene_violinplot()
      
                          })






  


  ########################################################
  ### Compare two clusters
  output$plotclust_cl1 <- renderPlot(
                             # plotmap_m( sc, my_part = as.numeric(input$cluster1) , cex = 1.5)

                                ggplot(plotmap_df, aes(X, Y )) +
                                  geom_point(alpha=0.6, color="grey") +
                                  # plot and label medoids
                                  geom_point(data = plotmap_df %>% filter(cluster %in% as.numeric(input$cluster1) ),
                                             aes(color = factor(cluster)),
                                             alpha=0.6) +
                                  geom_point(data = plotmap_df %>% filter(is_medoid == T, cluster %in% as.numeric(input$cluster1) ),
                                             aes(fill=cluster),
                                             color="black",
                                             size=4,
                                             pch=21,
                                             alpha=0.8,
                                             stroke=0.8) +
                                  geom_label_repel(data = plotmap_df %>% filter(is_medoid == T, cluster %in% as.numeric(input$cluster1)  ),
                                                   aes(label=cluster, color=cluster),
                                                   fill="#fafafa",
                                                   size=7,
                                                   nudge_x = -0.05,
                                                   label.padding = unit(0.15, "lines"),
                                                   label.r = unit(0.25, "lines"),
                                                   label.size = 0.0,
                                                   alpha=0.85,
                                                   fontface= "bold.italic") +
                                  # use colors from RaceID base plotting
                                  scale_colour_manual(name = "cluster",
                                                      values = sc_fcol) +
                                  scale_fill_manual(name = "cluster",
                                                    values = sc_fcol) +
                                  theme_void() +
                                  theme(legend.position = "none" )
                             )
  output$plotclust_cl2 <- renderPlot(
                              # plotmap_m( sc, my_part = as.numeric(input$cluster2) , cex = 1.5)


                              ggplot(plotmap_df, aes(X, Y )) +
                                geom_point(alpha=0.6, color="grey") +
                                # plot and label medoids
                                geom_point(data = plotmap_df %>% filter(cluster %in% as.numeric(input$cluster2) ),
                                           aes(color = factor(cluster)),
                                           alpha=0.6) +
                                geom_point(data = plotmap_df %>% filter(is_medoid == T, cluster %in% as.numeric(input$cluster2) ),
                                           aes(fill=cluster),
                                           color="black",
                                           size=4,
                                           pch=21,
                                           alpha=0.8,
                                           stroke=0.8) +
                                geom_label_repel(data = plotmap_df %>% filter(is_medoid == T, cluster %in% as.numeric(input$cluster2)  ),
                                                 aes(label=cluster, color=cluster),
                                                 fill="#fafafa",
                                                 size=7,
                                                 nudge_x = -0.05,
                                                 label.padding = unit(0.15, "lines"),
                                                 label.r = unit(0.25, "lines"),
                                                 label.size = 0.0,
                                                 alpha=0.85,
                                                 fontface= "bold.italic") +
                                # use colors from RaceID base plotting
                                scale_colour_manual(name = "cluster",
                                                    values = sc_fcol) +
                                scale_fill_manual(name = "cluster",
                                                  values = sc_fcol) +
                                theme_void() +
                                theme(legend.position = "none" )

                              )


  # Two clusters to compare
  cl1  <- reactive(names( sc_cpart[ sc_cpart %in% as.numeric(input$cluster1) ]))
  cl2  <- reactive(names( sc_cpart[ sc_cpart %in% as.numeric(input$cluster2) ]))

  # RaceID function to compare the two clusters

  #working
  # diffexp  <- reactive(fread(paste( "/data/gruen/herman/Aizarani_et_al_Nature_2019_shiny_app_data/pairwise_comparisons/",input$cluster1, ".v.", input$cluster2, "__pairwise_comparison.csv", sep="") ))
  # diffexp  <- reactive(fread(paste( "./Data/pairwise_comparisons/",input$cluster1, ".v.", input$cluster2, "__pairwise_comparison.csv", sep="") ))
  diffexp  <- reactive(fread(paste( pairwise_dir ,input$cluster1, ".v.", input$cluster2, "__pairwise_comparison.csv", sep="") ))
  p_val    <- reactive( input$c2c_pval )
  p_adj    <- reactive( input$padj )
  #

  #working
  # # Show two data tables of differentially expressed genes
  output$diffexpnb_2cl_1   <- renderDataTable( diffexp() %>%
                                                 filter(pval <= p_val(), padj <= p_adj() ) %>%
                                                 mutate_if(is.numeric, round, digits=8)
                                               )
    # apply( diffexp()[diffexp()$pval <= p_val() & diffexp()$padj <= p_adj(),],
    #        2,
    #        function(x){round(x[-1],digits = 8)}))

  output$diffexpnb_2cl_2   <- renderDataTable( diffexp() %>%
                                                 filter(pval <= p_val(), padj <= p_adj() ) %>%
                                                 mutate_if(is.numeric, round, digits=8) )
    # apply( diffexp()[diffexp()$pval <= p_val() & diffexp()$padj <= p_adj(),],
    #        2,
    #        function(x){round(x[-1],digits = 8)}))






  # # Get input for top genes
  top_g_ma  <- reactive(input$top_genes )



  # Create MA plot using ggplot from data contating all pairwise comparisons
  gg_maplot <- reactive(
                diffexp() %>%
                  mutate(up_thr   = foldChange < 1,
                         down_thr = foldChange > 1) %>%
                  mutate(significance = case_when( (padj < p_adj() & up_thr)   ~ "sig_upin_A",
                                                   (padj < p_adj() & down_thr) ~ "sig_downin_A",
                                                   TRUE ~ "NS")
                  ) %>%
                  ggplot( aes(y = log2(baseMeanB) - log2(baseMeanA), x = log2(baseMeanA + baseMeanB), color = significance) ) +
                  geom_point() +
                  geom_hline( yintercept = 0, color= "black", linetype = "dashed") +
                  geom_label_repel( aes( label = gene),
                                    data     = . %>% filter(padj < p_adj() ) %>% arrange(desc(abs(log2FoldChange))) %>% head( top_g_ma() ),
                                    size     = 3,
                                    alpha    = 0.8,
                                    col      = "black",
                                    fontface = "bold.italic") +
                  scale_color_manual( values = c("grey","#B31B21", "#1465AC") ) +
                  scale_y_continuous(breaks=seq(-200,200, 0.5)) +
                  scale_x_continuous(breaks=seq(-200,200, 1)) +
                  theme_classic()
               )



  output$maplot   <- renderPlot({
    gg_maplot()
  })


# 
#   output$maplot_2   <- renderPlotly({
#       ggplotly(gg_maplot() +
#                  geom_point(alpha=0.5)+
#                  geom_text(aes(label=gene),color="black", data = . %>% filter(padj < p_adj() ), size=2.5, alpha=0.8) +
#                  theme(legend.title=element_blank()) +
#                  xlab("Log2 mean expression") +
#                  ylab("Log2 fold change") )
#   })
  
})