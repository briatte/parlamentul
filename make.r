# hi Romania

library(ggplot2)
library(GGally)
library(grid)
library(network)
library(plyr)
library(rgexf)
library(sna)
library(stringr)
library(tnet)
library(XML)

root = "http://www.cdep.ro/pls/"
bills = "data/bills.csv"
sponsors = "data/sponsors.csv"

plot = TRUE
gexf = TRUE

dir.create("data", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)
dir.create("photos", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

colors = c(
  "VERZII" = "#4DAF4A", # Partidul Verde, green
  "PER" = "#4DAF4A", # Partidul Ecologist Român, green
  "PSD" = "#E41A1C", # Partidul Social Democrat, red
  "UNPR" = "#FB8072", # Uniunea Naţională pentru Progresul României (PSD/PNL splitters), light red
  "PNL" = "#FFFF33", # Partidul Național Liberal, yellow
  "PD-L" = "#FF7F00", # Partidul Democrat-Liberal, liberal-conservative, orange (ex-PD; includes FC)
  "FSN" = "#FF7F00", # Frontul Salvării Naționale - now absorbed in PD-L, orange
  "UDMR" = "#1B9E77", # Uniunea Democrată Maghiară din România, liberal-conservative, dark green/teal
  "PNTCD" = "#FFFFB3", # Partidul Naţional Ţaranesc Creştin-Democrat, agrarian, light yellow
  "PDAR" = "#000000", # Partidul Democrat Agrar din România, light brown (only one MP from 1992, not used)
  "PC" = "#80B1D3", # Partidul Conservator (ex-PUR-SL), light blue
  "PUNR" = "#80B1D3", # Partidul Unității Națiunii Române - now absorbed in PC, light blue
  "PP-DD" = "#984EA3", # Partidul Poporului - Dan Diaconescu, populist, purple
  "PRM" = "#A65628", # Partidul România Mare, extreme-right, brown
  "Minoritatilor" = "#444444", # minorities, dark grey
  "Independent" = "#AAAAAA"    # independents, light grey
)
order = names(colors)

source("data.r")
# source("build.r")

b$au_type = NA
b$au_type[ grepl("cam=1", b$authors) & !grepl("cam=2", b$authors) ] = "Senate"
b$au_type[ grepl("cam=2", b$authors) & !grepl("cam=1", b$authors) ] = "Chamber"
b$au_type[ grepl("cam=1", b$authors) & grepl("cam=2", b$authors) ] = "both"

# a third of cosponsored bills are cosponsored by members of both chambers
table(b$au_type, b$n_au > 1)

b$year = str_sub(b$ref, start = -4)
b$legislature = NA
b$legislature[ b$year %in% 1997:2000 ] = "1996-2000"
b$legislature[ b$year %in% 2001:2004 ] = "2000-2004"
b$legislature[ b$year %in% 2005:2008 ] = "2004-2008"
b$legislature[ b$year %in% 2009:2012 ] = "2008-2012"
b$legislature[ b$year %in% 2013:2014 ] = "2012-2016"
table(b$legislature, exclude = NULL) # excluding a handful of old bills

# subset to recent cosponsored bills
b = subset(b, !is.na(legislature) & !is.na(authors) & authors != "")
b$authors = gsub("/pls/", "", b$authors)
table(grepl(";", b$authors), b$legislature)

# sponsor identification through unique URLs
rownames(s) = s$url

# between 42% and 70% of bills are cosponsored (increases through time)
prop.table(table(grepl(";", b$authors), b$legislature), 2)

for(jj in c("ca", "se")) {
  
  for(ii in unique(b$legislature)) {
    
    cat(jj, ii)
    leg = substr(ii, 1, 4)
    
    # subset to cosponsored bills
    bb = subset(b, legislature == ii & n_au > 1)
    
    # further subset to specific chamber
    bb$authors = sapply(bb$authors, function(x) {
      x = unlist(strsplit(x, ";"))
      x = x[ grepl(paste0("cam=", ifelse(jj == "ca", 2, 1)), x) ]
      return(paste0(x, collapse = ";"))
    })
    
    # subset to chamber-specific cosponsored bills
    bb$n_au = 1 + str_count(bb$authors, ";")
    data = subset(bb, n_au > 1)
    
    cat(":", nrow(data), "cosponsored bills, ")
    
    edges = rbind.fill(lapply(data$authors, function(d) {
      
      w = unlist(strsplit(d, ";"))
      d = s$url[ s$url %in% w & s$legislature == leg ] # use unique URLs from the same legislature
      
      d = subset(expand.grid(d, d), Var1 != Var2)
      d = unique(apply(d, 1, function(x) paste0(sort(x), collapse = "_")))
      
      if(length(d))
        return(data.frame(d, w = length(w) - 1)) # number of cosponsors
      else
        return(data.frame())
      
    }))
    
    # raw edge counts
    count = table(edges$d)
    
    # Newman-Fowler weights (weighted quantity of bills cosponsored)
    edges = aggregate(w ~ d, function(x) sum(1 / x), data = edges)
    
    # raw counts
    edges$count = as.vector(count[ edges$d ])
    
    edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$d),
                       j = gsub("(.*)_(.*)", "\\2", edges$d),
                       w = edges$w, n = edges[, 3])
    
    cat(nrow(edges), "edges, ")
    
    # network
    
    n = network(edges[, 1:2 ], directed = FALSE)
    n %n% "title" = paste("Parlamentul", paste0(range(unique(data$year)), collapse = " to "))
    n %n% "n_bills" = nrow(data)
    n %n% "n_sponsors" = table(bb$n_au) # chamber-specific dataset
    
    cat(network.size(n), "nodes")
    
    n %v% "url" = as.character(s[ network.vertex.names(n), "url" ])
    n %v% "sex" = as.character(s[ network.vertex.names(n), "sex" ])
    n %v% "born" = as.numeric(s[ network.vertex.names(n), "born" ]) # already years, no need to substr
    n %v% "party" = s[ network.vertex.names(n), "party" ]
    n %v% "nyears" = s[ network.vertex.names(n), "nyears" ]
    n %v% "constituency" = s[ network.vertex.names(n), "constituency" ]
    n %v% "photo" = as.character(s[ network.vertex.names(n), "photo" ])
    n %v% "type" = as.character(s[ network.vertex.names(n), "type" ])
    
    network::set.edge.attribute(n, "source", as.character(edges[, 1]))
    network::set.edge.attribute(n, "target", as.character(edges[, 2]))
    
    network::set.edge.attribute(n, "weight", edges[, 3])
    network::set.edge.attribute(n, "count", edges[, 4])
    network::set.edge.attribute(n, "alpha",
                                as.numeric(cut(n %e% "count", c(1:4, Inf),
                                               include.lowest = TRUE)) / 5)
    
    # modularity
    
    nn = graph.edgelist(as.matrix(edges[, 1:2 ]), directed = FALSE)
    E(nn)$weight = edges[, 3]
    
    i = s[ V(nn)$name, "party" ]
    # ignoring: independents, minorities
    i[ i %in% c("Independent", "Minorităților") ] = NA
    
    nn = nn - which(is.na(i))
    i = as.numeric(factor(i[ !is.na(i) ]))
    
    n %n% "modularity" = modularity(nn, membership = i, weights = E(nn)$weight)
    cat("\nModularity:", round(n %n% "modularity", 2))
    
    walktrap = lapply(1:50, function(x) walktrap.community(nn, steps = x))
    
    # max. partition
    maxwalks = order(sapply(walktrap, modularity), decreasing = TRUE)[1]
    walktrap = walktrap[[ maxwalks ]]
    
    n %n% "modularity_walktrap" = modularity(walktrap)
    cat(" Walktrap:", round(n %n% "modularity_walktrap", 2))
    
    louvain = multilevel.community(nn)
    
    n %n% "modularity_louvain" = modularity(louvain)
    cat(" Louvain:", round(n %n% "modularity_louvain", 2))
    
    # weighted adjacency matrix to tnet
    tnet = as.tnet(as.sociomatrix(n, attrname = "weight"), type = "weighted one-mode tnet")
    
    # weighted degree and distance
    wdeg = as.data.frame(degree_w(tnet, measure = "degree"))
    dist = distance_w(tnet)
    wdeg$distance = NA
    wdeg[ attr(dist, "nodes"), ]$distance = colMeans(dist, na.rm = TRUE)
    wdeg = cbind(wdeg, clustering_local_w(tnet)[, 2])
    names(wdeg) = c("node", "degree", "distance", "clustering")
    
    n %v% "degree" = wdeg$degree
    n %n% "degree" = mean(wdeg$degree, na.rm = TRUE)
    cat("\nDegree:", round(n %n% "degree", 2))
    
    n %v% "distance" = wdeg$distance
    n %n% "distance" = mean(wdeg$distance, na.rm = TRUE)
    cat(" Distance:", round(n %n% "distance", 2))
    
    n %v% "clustering" = wdeg$clustering    # local
    n %n% "clustering" = clustering_w(tnet) # global
    cat(" Clustering:", round(n %n% "clustering", 2))
    
    i = colors[ s[ n %e% "source", "party" ] ]
    j = colors[ s[ n %e% "target", "party" ] ]
    
    party = as.vector(i)
    party[ i != j ] = "#AAAAAA"
    
    print(table(n %v% "party", exclude = NULL))
    
    # number of bills cosponsored
    nb = unlist(strsplit(data$authors, ";"))
    nb = sapply(n %v% "url", function(x) {
      sum(nb == x) # ids are unique URLs
    })
    n %v% "n_bills" = as.vector(nb)
    
    if(plot) {
      
      q = unique(quantile(n %v% "degree")) # safer
      n %v% "size" = as.numeric(cut(n %v% "degree", q, include.lowest = TRUE))
      
      g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2, # mode = "kamadakawai",
                                 segment.color = party) +
                             geom_point(alpha = 1/3, aes(size = n %v% "size", color = n %v% "party")) +
                             geom_point(alpha = 1/2, aes(size = min(n %v% "size"), color = n %v% "party")) +
                             scale_size_continuous(range = c(6, 12)) +
                             scale_color_manual("", values = colors, breaks = order) +
                             theme(legend.key.size = unit(1, "cm"),
                                   legend.text = element_text(size = 16)) +
                             guides(size = FALSE, color = guide_legend(override.aes = list(alpha = 1/3, size = 6))))
      
      print(g)
      
      ggsave(paste0("plots/net_ro_", jj, ii, ".pdf"), 
             g + theme(legend.key = element_blank()), width = 10, height = 9)
      ggsave(paste0("plots/net_ro_", jj, ii, ".jpg"),
             g + theme(legend.position = "none"), width = 9, height = 9)
      
    }
    
    # replace unique URLs with names in vertex names and edge attributes
    network.vertex.names(n) = as.character(s[ network.vertex.names(n), "name" ])
    n %e% "source" = s[ n %e% "source", "name" ]
    n %e% "target" = s[ n %e% "target", "name" ]
    
    assign(paste0("net_ro_", jj, leg), n)
    assign(paste0("edges_ro_", jj, leg), edges)
    assign(paste0("bills_ro_", jj, leg), data)
    
    # gexf
    if(gexf) {
      
      rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
      mode = "fruchtermanreingold"
      meta = list(creator = "rgexf",
                  description = paste(mode, "placement", nrow(data), "bills"),
                  keywords = "parliament, romania")
      
      node.att = data.frame(url = gsub("parlam/structura.mp?idm=", "", gsub("&", "&amp;", n %v% "url")), # protected
                            party = n %v% "party",
                            bills = n %v% "n_bills",
                            # extra attributes
                            # type = n %v% "type",
                            constituency = n %v% "constituency",
                            distance = round(n %v% "distance", 1),
                            photo = gsub("photos/", "", n %v% "photo"),
                            stringsAsFactors = FALSE)
      
      people = data.frame(id = as.numeric(factor(network.vertex.names(n))),
                          label = network.vertex.names(n),
                          stringsAsFactors = FALSE)
      
      relations = data.frame(
        source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
        target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
        weight = round(n %e% "weight", 3), count = n %e% "count")
      relations = na.omit(relations)
      
      # check all weights are positive after rounding
      stopifnot(all(relations$weight > 0))
      
      nodecolors = lapply(n %v% "party", function(x)
        data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .5))
      nodecolors = as.matrix(rbind.fill(nodecolors))
      
      # node placement
      position = do.call(paste0("gplot.layout.", mode),
                         list(as.matrix.network.adjacency(n), NULL))
      position = as.matrix(cbind(round(position, 1), 1))
      colnames(position) = c("x", "y", "z")
      
      write.gexf(nodes = people, nodesAtt = node.att,
                 edges = relations[, 1:2 ], edgesWeight = relations[, 3],
                 nodesVizAtt = list(position = position, color = nodecolors,
                                    size = round(n %v% "degree", 1)),
                 # edgesVizAtt = list(size = relations[, 4]),
                 defaultedgetype = "undirected", meta = meta,
                 output = paste0("net_ro_", jj, leg, ".gexf"))
      
    }
    
  }
  
  if(gexf)
    zip(paste0("net_ro_", jj, ".zip"), dir(pattern = paste0("^net_ro_", jj, "\\d{4}\\.gexf$")))
  
}

save(list = ls(pattern = "^(net|edges|bills)_ro_(ca|se)\\d{4}$"),
     file = "data/net_ro.rda")

# kthxbye
