for(jj in c("ca", "se")) {
  
  for(ii in unique(b$legislature)) {
    
    cat("\n", meta[ jj ], ii)
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
    sp = s[ s$legislature == leg, ]
    
    cat(":", nrow(data), "cosponsored documents, ")
    
    #
    # directed edge list
    #
    
    edges = lapply(data$authors, function(d) {
      
      w = unlist(strsplit(d, ";"))
      
      # avoid adding sponsors from previous legislatures
      d = expand.grid(i = sp$url[ sp$url %in% w ],
                      j = sp$url[ sp$url == w[1]],
                      stringsAsFactors = FALSE)
      
      if(nrow(d) > 0)
        return(data.frame(d, w = length(w) - 1)) # number of cosponsors
      
    }) %>% bind_rows
    
    #
    # edge weights
    #
    
    # first author self-loops, with counts of cosponsors
    self = subset(edges, i == j)
    
    # count number of bills per first author
    n_au = table(self$j)
    
    # remove self-loops from directed edge list
    edges = subset(edges, i != j)
    
    # count number of bills cosponsored per sponsor
    n_co = table(edges$i)
    
    # identify directed ties
    edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
    
    # raw edge counts
    raw = table(edges$ij)
    
    # Newman-Fowler weights (weighted quantity of bills cosponsored)
    edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
    
    # expand to edge list
    edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                       j = gsub("(.*)///(.*)", "\\2", edges$ij),
                       raw = as.vector(raw[ edges$ij ]), # raw edge counts
                       nfw = edges$w, stringsAsFactors = FALSE)
    
    # Gross-Shalizi weights (weighted propensity to cosponsor)
    edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
    edges$gsw = edges$nfw / edges$w
    
    # sanity check
    stopifnot(edges$gsw <= 1)
    
    # final edge set: cosponsor, first author, weights
    edges = select(edges, i, j, raw, nfw, gsw)
    
    cat(nrow(edges), "edges, ")
    
    #
    # directed network
    #
    
    n = network(edges[, 1:2 ], directed = TRUE)
    
    n %n% "country" = meta[ "cty" ]
    n %n% "title" = paste(meta[ jj ], paste0(range(unique(substr(data$date, 1, 4))),
                                             collapse = " to "))
    
    n %n% "n_bills" = nrow(data)
    n %n% "n_sponsors" = table(bb$n_au) # chamber-specific dataset
    
    n_au = as.vector(n_au[ network.vertex.names(n) ])
    n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
    
    n_co = as.vector(n_co[ network.vertex.names(n) ])
    n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
    
    n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
    
    cat(network.size(n), "nodes\n")
        
    n %v% "url" = as.character(gsub("parlam/structura.mp?idm=", "", 
                                    gsub("&", "&amp;", s[ network.vertex.names(n), "url" ])))
    n %v% "sex" = as.character(s[ network.vertex.names(n), "sex" ])
    n %v% "born" = as.numeric(s[ network.vertex.names(n), "born" ]) # already years, no need to substr
    n %v% "party" = as.character(s[ network.vertex.names(n), "party" ])
    n %v% "partyname" = as.character(parties[ n %v% "party" ])
    n %v% "lr" = as.numeric(scores[ n %v% "party" ])
    n %v% "nyears" = as.numeric(s[ network.vertex.names(n), "nyears" ])
    n %v% "constituency" = as.character(s[ network.vertex.names(n), "constituency" ])
    # do not remove .jpg, a few photos are .gif
    n %v% "photo" = as.character(gsub("photos/", "", s[ network.vertex.names(n), "photo" ]))
    
    # unweighted degree
    n %v% "degree" = degree(n)
    q = n %v% "degree"
    q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
    
    # check all sponsors come from the right chamber
    # print(table(s[ network.vertex.names(n), "type" ]))
    
    set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
    set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
    
    set.edge.attribute(n, "raw", edges$raw) # raw edge counts
    set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
    set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
    
    #
    # network plot
    #
    
    if(plot) {
      
      save_plot(n, file = paste0("plots/net_ro_", jj, ii),
                 i = colors[ s[ n %e% "source", "party" ] ],
                 j = colors[ s[ n %e% "target", "party" ] ],
                 q, colors, order)
      
    }
    
    #
    # save objects
    #
    
    # replace unique URLs with names in vertex names and edge attributes
    network.vertex.names(n) = as.character(s[ network.vertex.names(n), "name" ])
    set.edge.attribute(n, "source", s[ n %e% "source", "name" ])
    set.edge.attribute(n, "target", s[ n %e% "target", "name" ])
    
    assign(paste0("net_ro_", jj, leg), n)
    assign(paste0("edges_ro_", jj, leg), edges)
    assign(paste0("bills_ro_", jj, leg), data)
    
    # gexf
    if(gexf)
      save_gexf(paste0("net_ro_", jj, leg), n,
                meta = c(meta[ "cty" ], meta[ jj ]), mode, colors,
                extra = "constituency")
    
  }
  
  if(gexf)
    zip(paste0("net_ro_", jj, ".zip"), dir(pattern = paste0("^net_ro_", jj, "\\d{4}\\.gexf$")))
  
}

save(list = ls(pattern = "^(net|edges|bills)_ro_(ca|se)\\d{4}$"),
     file = "data/net_ro.rda")
