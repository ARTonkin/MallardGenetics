setwd("D:/Datasci/duckproj")

fulldat = read.csv("Combined_Data.csv")



library(party)
library(ggparty)
library(caret)

#### GRAPHING FUNCTIONS ####
# The following code is the creation of the functions needed to graph the party datatype in a visually attractive way.

# Graphing functions for colour

geno1fun = function(x){
ggparty(x) +
    geom_edge() +
    
    geom_node_label(aes(label = paste0("n = ", nodesize)),
                    ids = "terminal", nudge_y = 0.017) +
    geom_edge_label() +
    geom_node_splitvar() +
    # pass list to gglist containing all ggplot components we want to plot for each
    # (default: terminal) node
    geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = Genotype.1),
                                          position = position_fill()),
                                 theme_bw(),
                                 theme(panel.grid = element_blank(),
                                       panel.border = element_blank(),
                                       axis.title.x=element_blank(),
                                       axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       axis.title.y=element_blank(),
                                       axis.text.y=element_blank(),
                                       axis.ticks.y=element_blank())),
                   shared_axis_labels = F)
}

geno2fun = function(x){
    ggparty(x) +
        geom_edge() +
        
        geom_node_label(aes(label = paste0("n = ", nodesize)),
                        ids = "terminal", nudge_y = 0.017) +
        geom_edge_label() +
        geom_node_splitvar() +
        # pass list to gglist containing all ggplot components we want to plot for each
        # (default: terminal) node
        geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = Genotype.2),
                                              position = position_fill()),
                                     theme_bw(),
                                     theme(panel.grid = element_blank(),
                                           panel.border = element_blank(),
                                           axis.title.x=element_blank(),
                                           axis.text.x=element_blank(),
                                           axis.ticks.x=element_blank(),
                                           axis.title.y=element_blank(),
                                           axis.text.y=element_blank(),
                                           axis.ticks.y=element_blank())),
                       shared_axis_labels = F)
}

phenofun = function(x){
    ggparty(x) +
        geom_edge() +
        
        geom_node_label(aes(label = paste0("n = ", nodesize)),
                        ids = "terminal", nudge_y = 0.017) +
        geom_edge_label() +
        geom_node_splitvar() +
        # pass list to gglist containing all ggplot components we want to plot for each
        # (default: terminal) node
        geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = Phenotype),
                                              position = position_fill()),
                                     theme_bw(),
                                     theme(panel.grid = element_blank(),
                                           panel.border = element_blank(),
                                           axis.title.x=element_blank(),
                                           axis.text.x=element_blank(),
                                           axis.ticks.x=element_blank(),
                                           axis.title.y=element_blank(),
                                           axis.text.y=element_blank(),
                                           axis.ticks.y=element_blank())),
                       shared_axis_labels = F)
}




# Graphing functions for greyscale


geno1fun = function(x){
    tab <- sapply(1:length(x), function(id) {
        y <- data_party(x[id])
        y <- y[["(response)"]]
        table(y)
    })
    proptab = round(tab / rep(colSums(tab), each=nrow(tab)), 3)
    print(proptab)
    ggparty(x, terminal_space = .1) +
        geom_node_plot(gglist = list(theme_void())) +
        geom_edge() +
        geom_node_label(aes(
            label = paste0("GENOTYPE | G = Grey Duck | H = Hybrid | M = Mallard")),
                        ids = 1, nudge_y = -1, line_gpar = list(list(size = 8))) +
        geom_node_label(
            line_list = list(
                aes(label = splitvar),
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("H", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M", sprintf("%.3f", proptab["MALL",id])))
            ),
            line_gpar = list(
                list(size = 11),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "inner"
        ) +
        geom_node_label(
            line_list = list(
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("H", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M", sprintf("%.3f", proptab["MALL",id])))
            ),
            line_gpar = list(
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "terminal"
        ) +
        geom_edge_label()
}


geno2fun = function(x){
    tab <- sapply(1:length(x), function(id) {
        y <- data_party(x[id])
        y <- y[["(response)"]]
        table(y)
    })
    proptab = round(tab / rep(colSums(tab), each=nrow(tab)), 3)
    print(proptab)
    ggparty(x, terminal_space = .1) +
        geom_node_plot(gglist = list(theme_void())) +
        geom_edge() +
        geom_node_label(aes(label = paste0("GENOTYPE | G = Grey Duck | GH = Grey Duck Backcross | H = Hybrid | M = Mallard | MH = Mallard Backcross")),
                        ids = 1, nudge_y = -1, line_gpar = list(list(size = 8))) +
        geom_node_label(
            line_list = list(
                aes(label = splitvar),
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G ", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("GH", sprintf("%.3f", proptab["Grey Hybrid",id]))),
                aes(label = paste("H ", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M ", sprintf("%.3f", proptab["MALL",id]))),
                aes(label = paste("MH", sprintf("%.3f", proptab["Mall Hybrid",id])))
            ),
            line_gpar = list(
                list(size = 11),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "inner"
        ) +
        geom_node_label(
            line_list = list(
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G ", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("GH", sprintf("%.3f", proptab["Grey Hybrid",id]))),
                aes(label = paste("H ", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M ", sprintf("%.3f", proptab["MALL",id]))),
                aes(label = paste("MH", sprintf("%.3f", proptab["Mall Hybrid",id])))
            ),
            line_gpar = list(
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "terminal"
        ) +
        geom_edge_label()
}


phenofun = function(x){
    tab <- sapply(1:length(x), function(id) {
        y <- data_party(x[id])
        y <- y[["(response)"]]
        table(y)
    })
    proptab = round(tab / rep(colSums(tab), each=nrow(tab)), 3)
    print(proptab)
    ggparty(x, terminal_space = .1) +
        geom_node_plot(gglist = list(theme_void())) +
        geom_edge() +
        geom_node_label(aes(label = paste0("PHENOTYPE | G = Grey Duck | H = Hybrid | M = Mallard")),
                        ids = 1, nudge_y = -1, line_gpar = list(list(size = 8))) +
        geom_node_label(
            line_list = list(
                aes(label = splitvar),
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("H", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M", sprintf("%.3f", proptab["MALL",id])))
            ),
            line_gpar = list(
                list(size = 11),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "inner"
        ) +
        geom_node_label(
            line_list = list(
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("H", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M", sprintf("%.3f", proptab["MALL",id])))
            ),
            line_gpar = list(
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "terminal"
        ) +
        geom_edge_label()
}


# Functions for missing GREY duck pops


geno1fun2 = function(x){
    tab <- sapply(1:length(x), function(id) {
        y <- data_party(x[id])
        y <- y[["(response)"]]
        table(y)
    })
    proptab = round(tab / rep(colSums(tab), each=nrow(tab)), 3)
    proptab = rbind(t(data.frame(GREY = rep(0, each=ncol(proptab)))), proptab)
    print(proptab)
    ggparty(x, terminal_space = .1) +
        geom_node_plot(gglist = list(theme_void())) +
        geom_edge() +
        geom_node_label(aes(label = paste0("GENOTYPE | G = Grey Duck | H = Hybrid | M = Mallard")),
                        ids = 1, nudge_y = -1, line_gpar = list(list(size = 8))) +
        geom_node_label(
            line_list = list(
                aes(label = splitvar),
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("H", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M", sprintf("%.3f", proptab["MALL",id])))
            ),
            line_gpar = list(
                list(size = 11),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "inner"
        ) +
        geom_node_label(
            line_list = list(
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("H", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M", sprintf("%.3f", proptab["MALL",id])))
            ),
            line_gpar = list(
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "terminal"
        ) +
        geom_edge_label()
}


geno2fun2 = function(x){
    tab <- sapply(1:length(x), function(id) {
        y <- data_party(x[id])
        y <- y[["(response)"]]
        table(y)
    })
    proptab = round(tab / rep(colSums(tab), each=nrow(tab)), 3)
    proptab = rbind(t(data.frame("GREY" = rep(0, each=ncol(proptab)))), proptab)
    print(proptab)
    ggparty(x, terminal_space = .1) +
        geom_node_plot(gglist = list(theme_void())) +
        geom_edge() +
        geom_node_label(aes(label = paste0("GENOTYPE | G = Grey Duck | GH = Grey Duck Backcross | H = Hybrid | M = Mallard | MH = Mallard Backcross")),
                        ids = 1, nudge_y = -1, line_gpar = list(list(size = 8))) +
        geom_node_label(
            line_list = list(
                aes(label = splitvar),
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G ", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("GH", sprintf("%.3f", proptab["Grey Hybrid",id]))),
                aes(label = paste("H ", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M ", sprintf("%.3f", proptab["MALL",id]))),
                aes(label = paste("MH", sprintf("%.3f", proptab["Mall Hybrid",id])))
            ),
            line_gpar = list(
                list(size = 11),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "inner"
        ) +
        geom_node_label(
            line_list = list(
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G ", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("GH", sprintf("%.3f", proptab["Grey Hybrid",id]))),
                aes(label = paste("H ", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M ", sprintf("%.3f", proptab["MALL",id]))),
                aes(label = paste("MH", sprintf("%.3f", proptab["Mall Hybrid",id])))
            ),
            line_gpar = list(
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "terminal"
        ) +
        geom_edge_label()
}

# Format for large tree


geno2fun3 = function(x){
    tab <- sapply(1:length(x), function(id) {
        y <- data_party(x[id])
        y <- y[["(response)"]]
        table(y)
    })
    proptab = round(tab / rep(colSums(tab), each=nrow(tab)), 3)
    print(proptab)
    ggparty(x, terminal_space = .1,
            layout = data.frame(id = c(1,2,3,6,9,11,13,14),
                                x = c(.2,.2,.1,.3,.45,.7,.95,.8), y = c(1,.7,.35,.35,.9,.75,.6,.35))) +
        geom_node_plot(gglist = list(theme_void())) +
        geom_edge() +
        geom_node_label(aes(label = paste0("GENOTYPE | G = Grey Duck | GH = Grey Duck Backcross | H = Hybrid | M = Mallard | MH = Mallard Backcross")),
                        ids = 1, nudge_y = -1, nudge_x = .3, line_gpar = list(list(size = 8))) +
        geom_node_label(
            line_list = list(
                aes(label = splitvar),
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G ", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("GH", sprintf("%.3f", proptab["Grey Hybrid",id]))),
                aes(label = paste("H ", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M ", sprintf("%.3f", proptab["MALL",id]))),
                aes(label = paste("MH", sprintf("%.3f", proptab["Mall Hybrid",id])))
            ),
            line_gpar = list(
                list(size = 11),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "inner"
        ) +
        geom_node_label(
            line_list = list(
                aes(label = paste("n =", nodesize)),
                aes(label = paste("G ", sprintf("%.3f", proptab["GREY",id]))),
                aes(label = paste("GH", sprintf("%.3f", proptab["Grey Hybrid",id]))),
                aes(label = paste("H ", sprintf("%.3f", proptab["HYBRID",id]))),
                aes(label = paste("M ", sprintf("%.3f", proptab["MALL",id]))),
                aes(label = paste("MH", sprintf("%.3f", proptab["Mall Hybrid",id])))
            ),
            line_gpar = list(
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono"),
                list(size = 10, family = "mono")
            ),
            ids = "terminal"
        ) +
        geom_edge_label(size=3)
}

geno2fun3(fit)


#### PERFORMANCE ####
# The following code is used to measure the performance of the decision trees.

performfunc = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Species)^2), nrow=nlevels(df$Species))
    print(endtab)
    i = 0
    
    while(i < runs){
        selection = sample(nrow(df), 5)
        test = df[selection,]
        train = df[-selection,]
        #print(selection)
        #print(test)
        
        perftree = ctree(Species~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        conftab = table(treepred, test$Species)
        
        endtab = endtab + conftab
        i = i+1
    }
    
    #print(endtab)
    confmat = confusionMatrix(endtab)
    print(confmat)
    
}

performfunc(iris, 100)


performfuncG1 = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Genotype.1)^2), nrow=nlevels(df$Genotype.1))
    #`print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Genotype.1)), nrow=3)
    
    while(i < runs){
        selection = sample(nrow(df), 5)
        test = df[selection,]
        train = df[-selection,]
        #print(selection)
        #print(test)
        
        perftree = ctree(Genotype.1~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        conftab = table(treepred, test$Genotype.1)
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/runs)
    print(pval/runs)
    print(perf/runs)
    
}

performfuncG1(geno1dat, 10)



performfuncG2 = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Genotype.2)^2), nrow=nlevels(df$Genotype.2))
    #print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Genotype.2)), nrow=5)
    
    while(i < runs){
        selection = sample(nrow(df), 5)
        test = df[selection,]
        train = df[-selection,]
        #print(selection)
        #print(test)
        
        perftree = ctree(Genotype.2~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        conftab = table(treepred, test$Genotype.2)
        
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/runs)
    print(pval/runs)
    print(perf/runs)
    
}

performfuncG2(geno2dat, 10)



performfuncP = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Phenotype)^2), nrow=nlevels(df$Phenotype))
    #print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Phenotype)), nrow=3)
    
    while(i < runs){
        selection = sample(nrow(df), 5)
        test = df[selection,]
        train = df[-selection,]
        #print(selection)
        #print(test)
        
        perftree = ctree(Phenotype~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        conftab = table(treepred, test$Phenotype)
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/runs)
    print(pval/runs)
    print(perf/runs)
    
}

performfuncP(phenodat, 1000)


## Performance for missing greys


performfuncG1_2 = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Genotype.1)^2), nrow=nlevels(df$Genotype.1))
    #print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Genotype.1)), nrow=3)
    
    #df$Genotype.1 <- factor(df$Genotype.1, levels = c("GREY", levels(df$Genotype.1)))
    #print(table(df$Genotype.1))
    
    while(i < runs){
        selection = sample(nrow(df), 5)
        test = df[selection,]
        train = df[-selection,]
        #print(selection)
        
        perftree = ctree(Genotype.1~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        levels(treepred) = c("HYBRID", "MALL", "GREY")
        #print(table(treepred))
        conftab = table(treepred, test$Genotype.1)
        conftab = conftab[c(3, 1, 2),]
        #print(conftab)
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/runs)
    print(pval/runs)
    print(perf/runs)
    
}

performfuncG1_2(geno1southdat, 1000)


performfuncG2_2 = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Genotype.2)^2), nrow=nlevels(df$Genotype.2))
    #print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Genotype.2)), nrow=5)
    
    while(i < runs){
        selection = sample(nrow(df), 5)
        test = df[selection,]
        train = df[-selection,]
        #print(selection)
        #print(test)
        
        perftree = ctree(Genotype.2~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        levels(treepred) = c("Grey Hybrid", "HYBRID", "MALL", "Mall Hybrid", "GREY")
        #print(table(treepred))
        conftab = table(treepred, test$Genotype.2)
        conftab = conftab[c(5, 1:4),]
        #print(conftab)
        
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/runs)
    print(pval/runs)
    print(perf/runs)
    
}

performfuncG2_2(geno2southdat, 1000)


### PERFORMANCE VIA CV


performfuncG1 = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Genotype.1)^2), nrow=nlevels(df$Genotype.1))
    #`print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Genotype.1)), nrow=3)
    weight = weightfuncG1(x=df)
    
    for(k in 1:20){
        dat = df[sample(1:nrow(df)),]
        dat$fold = c(rep(1:10, nrow(dat) %/% 10), if(nrow(dat) %% 10 != 0) 1:(nrow(dat)%%10))
    
    for(j in 1:10){
        #selection = sample(nrow(df), 5)
        test = df[dat$fold==j,]
        train = df[dat$fold!=j,]
        #print(head(train))
        w = weight[dat$fold!=j]
        #print(selection)
        #print(test)
        
        perftree = ctree(Genotype.1~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        conftab = table(treepred, test$Genotype.1)
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
    }
        
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/200)
    print(pval/200)
    print(perf/200)
    
}


performfuncG2 = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Genotype.2)^2), nrow=nlevels(df$Genotype.2))
    #print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Genotype.2)), nrow=5)
    weight = weightfuncG2(x=df)
    
    for(k in 1:20){
        dat = df[sample(1:nrow(df)),]
        dat$fold = c(rep(1:10, nrow(dat) %/% 10), if(nrow(dat) %% 10 != 0) 1:(nrow(dat)%%10))
        
        for(j in 1:10){
            #selection = sample(nrow(df), 5)
            test = df[dat$fold==j,]
            train = df[dat$fold!=j,]
            #print(head(train))
            w = weight[dat$fold!=j]
            #print(selection)
            #print(test)
            
            perftree = ctree(Genotype.2~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        conftab = table(treepred, test$Genotype.2)
        
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
        }
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/nrow(df))
    print(pval/nrow(df))
    print(perf/nrow(df))
    
}


performfuncP = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Phenotype)^2), nrow=nlevels(df$Phenotype))
    #print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Phenotype)), nrow=3)
    weight = weightfuncP(x=df)
    
    for(k in 1:20){
        dat = df[sample(1:nrow(df)),]
        dat$fold = c(rep(1:10, nrow(dat) %/% 10), if(nrow(dat) %% 10 != 0) 1:(nrow(dat)%%10))
        
        for(j in 1:10){
            #selection = sample(nrow(df), 5)
            test = df[dat$fold==j,]
            train = df[dat$fold!=j,]
            #print(head(train))
            w = weight[dat$fold!=j]
            #print(selection)
            #print(test)
            
            perftree = ctree(Phenotype~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        conftab = table(treepred, test$Phenotype)
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
        }
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/nrow(df))
    print(pval/nrow(df))
    print(perf/nrow(df))
    
}


performfuncG1_2 = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Genotype.1)^2), nrow=nlevels(df$Genotype.1))
    #print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Genotype.1)), nrow=3)
    weight = weightfuncG1(x=df)
    
    for(k in 1:20){
        dat = df[sample(1:nrow(df)),]
        dat$fold = c(rep(1:10, nrow(dat) %/% 10), if(nrow(dat) %% 10 != 0) 1:(nrow(dat)%%10))
        
        for(j in 1:10){
            #selection = sample(nrow(df), 5)
            test = df[dat$fold==j,]
            train = df[dat$fold!=j,]
            #print(head(train))
            w = weight[dat$fold!=j]
            #print(selection)
            #print(test)
            
            perftree = ctree(Genotype.1~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        levels(treepred) = c("HYBRID", "MALL", "GREY")
        #print(table(treepred))
        conftab = table(treepred, test$Genotype.1)
        conftab = conftab[c(3, 1, 2),]
        #print(conftab)
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
        }
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/nrow(df))
    print(pval/nrow(df))
    print(perf/nrow(df))
    
}


performfuncG2_2 = function(df, runs=1000){
    
    endtab = matrix(rep(0, nlevels(df$Genotype.2)^2), nrow=nlevels(df$Genotype.2))
    #print(endtab)
    i = 0
    acc = 0
    pval = 0
    perf = matrix(rep(0, 3*nlevels(df$Genotype.2)), nrow=5)
    weight = weightfuncG2(x=df)
    
    for(k in 1:20){
        dat = df[sample(1:nrow(df)),]
        dat$fold = c(rep(1:10, nrow(dat) %/% 10), if(nrow(dat) %% 10 != 0) 1:(nrow(dat)%%10))
        
        for(j in 1:10){
            #selection = sample(nrow(df), 5)
            test = df[dat$fold==j,]
            train = df[dat$fold!=j,]
            #print(head(train))
            w = weight[dat$fold!=j]
            #print(selection)
            #print(test)
            
            perftree = ctree(Genotype.2~., data=train)
        #print(perftree)
        
        treepred = predict(perftree, test)
        levels(treepred) = c("Grey Hybrid", "HYBRID", "MALL", "Mall Hybrid", "GREY")
        #print(table(treepred))
        conftab = table(treepred, test$Genotype.2)
        conftab = conftab[c(5, 1:4),]
        #print(conftab)
        
        
        endtab = endtab + conftab
        
        confmat = confusionMatrix(conftab)
        acc = acc + confmat$overall["Accuracy"]
        pval = pval + confmat$overall["AccuracyPValue"]
        inter = confmat$byClass[,c(1,2,7)]
        perf = perf + replace(inter, is.na(inter), 0)
        #print(acc)
        #print(pval)
        #print(perf)
        
        i = i+1
        }
    }
    
    #print(endtab)
    #confmat = confusionMatrix(endtab)
    #print(confmat)
    
    print(acc/nrow(df))
    print(pval/nrow(df))
    print(perf/nrow(df))
    
}



### WEIGHTING FUNCTIONS

weightfuncG1 = function(x){
    
    weight = ifelse(x$Genotype.1 == "GREY", nrow(x)/(nlevels(x$Genotype.1)*sum(x$Genotype.1=="GREY")),
                    ifelse(x$Genotype.1 == "HYBRID", nrow(x)/(nlevels(x$Genotype.1)*sum(x$Genotype.1=="HYBRID")),
                           nrow(x)/(nlevels(x$Genotype.1)*sum(x$Genotype.1=="MALL"))))
    #print(weightg1[1:30])
    return(weight)
}

weightfuncG2 = function(x){
    
    weight = ifelse(x$Genotype.2 == "GREY", nrow(x)/(nlevels(x$Genotype.2)*sum(x$Genotype.2=="GREY")),
                    ifelse(x$Genotype.2 == "HYBRID", nrow(x)/(nlevels(x$Genotype.2)*sum(x$Genotype.2=="HYBRID")),
                           ifelse(x$Genotype.2 == "MALL", nrow(x)/(nlevels(x$Genotype.2)*sum(x$Genotype.2=="MALL")),
                                  ifelse(x$Genotype.2 == "Mall Hybrid", nrow(x)/(nlevels(x$Genotype.2)*sum(x$Genotype.2=="Mall Hybrid")),
                                         nrow(x)/(nlevels(x$Genotype.2)*sum(x$Genotype.2=="Grey Hybrid"))))))
    #print(weightg1[1:30])
    return(weight)
}

weightfuncP = function(x){
    
    weight = ifelse(x$Phenotype == "GREY", nrow(x)/(nlevels(x$Phenotype)*sum(x$Phenotype=="GREY")),
                    ifelse(x$Phenotype == "HYBRID", nrow(x)/(nlevels(x$Phenotype)*sum(x$Phenotype=="HYBRID")),
                           nrow(x)/(nlevels(x$Phenotype)*sum(x$Phenotype=="MALL"))))
    #print(weightg1[1:30])
    return(weight)
}



#### Creating Trees ####
# The client had specific requests as to a variety of different combinations of variables that they wanted included.
# Each of the following sections is devoted to these combinations.


##################
## 1. Full Data ##
##################

# Creating datasets specific to each classification tree, randomising order

geno1dat = fulldat[sample(1:nrow(fulldat)), c("Genotype.1", "Sex", "Culmen", "Right.Wing")]
geno2dat = fulldat[sample(1:nrow(fulldat)), c("Genotype.2", "Sex", "Culmen", "Right.Wing")]
phenodat = fulldat[sample(1:nrow(fulldat)), c("Phenotype", "Sex", "Culmen", "Right.Wing")]



# Making classification trees 

weight = ifelse(geno1dat$Genotype.1 == "GREY", nrow(geno1dat)/(nlevels(geno1dat$Genotype.1)*sum(geno1dat$Genotype.1=="GREY")),
       ifelse(geno1dat$Genotype.1 == "HYBRID", nrow(geno1dat)/(nlevels(geno1dat$Genotype.1)*sum(geno1dat$Genotype.1=="HYBRID")),
              nrow(geno1dat)/(nlevels(geno1dat$Genotype.1)*sum(geno1dat$Genotype.1=="MALL"))))

fit <- ctree(Genotype.1~., data=geno1dat, weights = weightfuncG1(x=geno1dat))
geno1fun(fit)

fit <- ctree(Genotype.2~., data=geno2dat, weights = weightfuncG2(x=geno2dat))
geno2fun(fit)

fit <- ctree(Phenotype~., data=phenodat, weights = weightfuncP(x=phenodat))
phenofun(fit)


performfuncG1(geno1dat, 1000)

performfuncG2(geno2dat, 1000)

performfuncP(phenodat, 1000)



############################
## 1.5 Full Data + ISLAND ##
############################


# Creating datasets specific to each classification tree, randomising order

geno1dat = fulldat[sample(1:nrow(fulldat)), c("Genotype.1", "Sex", "Culmen", "Right.Wing", "ISLAND")]
geno2dat = fulldat[sample(1:nrow(fulldat)), c("Genotype.2", "Sex", "Culmen", "Right.Wing", "ISLAND")]
phenodat = fulldat[sample(1:nrow(fulldat)), c("Phenotype", "Sex", "Culmen", "Right.Wing", "ISLAND")]

# Making classification trees 

fit <- ctree(Genotype.1~., data=geno1dat)
geno1fun(fit)

fit <- ctree(Genotype.2~., data=geno2dat)
geno2fun(fit)

fit <- ctree(Phenotype~., data=phenodat)
phenofun(fit)


performfuncG1(geno1dat, 1000)

performfuncG2(geno2dat, 1000)

performfuncP(phenodat, 1000)

############################
## 2. Separated by Island ##
############################


# Creating datasets for each classification tree, splitting by North and South Island

northdat = fulldat[fulldat$ISLAND == "NORTH",]
southdat = fulldat[fulldat$ISLAND == "SOUTH",]

geno1northdat = northdat[sample(1:nrow(northdat)), c("Genotype.1", "Sex", "Culmen", "Right.Wing")]
geno2northdat = northdat[sample(1:nrow(northdat)), c("Genotype.2", "Sex", "Culmen", "Right.Wing")]
phenonorthdat = northdat[sample(1:nrow(northdat)), c("Phenotype", "Sex", "Culmen", "Right.Wing")]

geno1southdat = southdat[sample(1:nrow(southdat)), c("Genotype.1", "Sex", "Culmen", "Right.Wing")]
geno2southdat = southdat[sample(1:nrow(southdat)), c("Genotype.2", "Sex", "Culmen", "Right.Wing")]
phenosouthdat = southdat[sample(1:nrow(southdat)), c("Phenotype", "Sex", "Culmen", "Right.Wing")]

rm(northdat, southdat)


# Making classification trees

fit <- ctree(Genotype.1~., data=geno1northdat)
geno1fun(fit)

fit <- ctree(Genotype.2~., data=geno2northdat)
geno2fun(fit)

fit <- ctree(Phenotype~., data=phenonorthdat)
phenofun(fit)


fit <- ctree(Genotype.1~., data=geno1southdat)
geno1fun2(fit)

fit <- ctree(Genotype.2~., data=geno2southdat)
geno2fun2(fit)

fit <- ctree(Phenotype~., data=phenosouthdat)
phenofun(fit)



performfuncG1(geno1northdat, 1000)

performfuncG2(geno2northdat, 1000)

performfuncP(phenonorthdat, 1000)


performfuncG1_2(geno1southdat, 1000)

performfuncG2_2(geno2southdat, 1000)

performfuncP(phenosouthdat, 1000)


####################
## 3. Banded Data ##
####################


banddat = fulldat[fulldat$study == "banding",]

geno1banddat = banddat[sample(1:nrow(banddat)), c("Genotype.1", "Sex", "Culmen", "Right.Wing",
                                                  "Total.Mass", "Head.x", "Tarsus", "Keel.x")]
geno2banddat = banddat[sample(1:nrow(banddat)), c("Genotype.2", "Sex", "Culmen", "Right.Wing",
                                                   "Total.Mass", "Head.x", "Tarsus", "Keel.x")]
phenobanddat = banddat[sample(1:nrow(banddat)), c("Phenotype", "Sex", "Culmen", "Right.Wing",
                                                    "Total.Mass", "Head.x", "Tarsus", "Keel.x")]


fit <- ctree(Genotype.1~., data=geno1banddat)
geno1fun(fit)

fit <- ctree(Genotype.2~., data=geno2banddat)
geno2fun(fit)

fit <- ctree(Phenotype~., data=phenobanddat)
phenofun(fit)



performfuncG1(geno1banddat, 1000)

performfuncG2(geno2banddat, 1000)

performfuncP(phenobanddat, 1000)


#####################
## 4. Hunting Data ##
#####################


huntdat = fulldat[fulldat$study == "phil",]

geno1huntdat = huntdat[sample(1:nrow(huntdat)), c("Genotype.1", "Sex", "Culmen", "Right.Wing",
                                                  "Total.Mass", "Tail.Length", "Tarsus", 
                                                  "Body.Length", "Nares", "Bill.Width")]
geno2huntdat = huntdat[sample(1:nrow(huntdat)), c("Genotype.2", "Sex", "Culmen", "Right.Wing",
                                                  "Total.Mass", "Tail.Length", "Tarsus", "Body.Length",
                                                  "Nares", "Bill.Width")]
phenohuntdat = huntdat[sample(1:nrow(huntdat)), c("Phenotype", "Sex", "Culmen", "Right.Wing",
                                                  "Total.Mass", "Tail.Length", "Tarsus", "Body.Length",
                                                  "Nares", "Bill.Width")]




fit <- ctree(Genotype.1~., data=geno1huntdat)
geno1fun2(fit)

fit <- ctree(Genotype.2~., data=geno2huntdat)
geno2fun2(fit)

fit <- ctree(Phenotype~., data=phenohuntdat)
phenofun(fit)


performfuncG1_2(geno1huntdat, 1000)

performfuncG2_2(geno2huntdat, 1000)

performfuncP(phenohuntdat, 1000)


geno2fun = function(x){
    ggparty(x, layout = data.frame(id = c(2), x = c(0.3), y = c(0.9))) +
        geom_edge() +
        geom_node_label(aes(label = paste0("n = ", nodesize)),
                        ids = "terminal", nudge_y = 0.017) +
        geom_edge_label() +
        geom_node_splitvar() +
        # pass list to gglist containing all ggplot components we want to plot for each
        # (default: terminal) node
        geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = Genotype.2),
                                              position = position_fill()),
                                     theme_bw(),
                                     theme(panel.grid = element_blank(),
                                           panel.border = element_blank(),
                                           axis.title.x=element_blank(),
                                           axis.text.x=element_blank(),
                                           axis.ticks.x=element_blank(),
                                           axis.title.y=element_blank(),
                                           axis.text.y=element_blank(),
                                           axis.ticks.y=element_blank())),
                       shared_axis_labels = F)
}





