for (jj in c("ca", "se")) {
  
  for (ii in unique(b$legislature) %>% sort) {
    
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
    
    # ==========================================================================
    # DIRECTED EDGE LIST
    # ==========================================================================
    
    edges = lapply(data$authors, function(d) {
      
      w = unlist(strsplit(d, ";"))
      w = paste0(root, w)
      
      # avoid adding sponsors from previous legislatures
      d = expand.grid(i = sp$url[ sp$url %in% w ],
                      j = sp$url[ sp$url == w[1]],
                      stringsAsFactors = FALSE)
      
      if (nrow(d) > 0)
        return(data.frame(d, w = length(w) - 1)) # number of cosponsors
      
    }) %>% bind_rows
    
    # ==========================================================================
    # EDGE WEIGHTS
    # ==========================================================================
    
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
    edges = data_frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                       j = gsub("(.*)///(.*)", "\\2", edges$ij),
                       raw = as.vector(raw[ edges$ij ]), # raw edge counts
                       nfw = edges$w)
    
    # Gross-Shalizi weights (weighted propensity to cosponsor)
    edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
    edges$gsw = edges$nfw / edges$w
    
    # sanity check
    stopifnot(edges$gsw <= 1)
    
    # final edge set: cosponsor, first author, weights
    edges = select(edges, i, j, raw, nfw, gsw)
    
    cat(nrow(edges), "edges, ")
    
    # ==========================================================================
    # DIRECTED NETWORK
    # ==========================================================================
    
    n = network(edges[, 1:2 ], directed = TRUE)
    
    n %n% "country" = meta[ "cty" ] %>% as.character
    n %n% "lang" = meta[ "lang" ] %>% as.character
    n %n% "years" = ii %>% as.character
    n %n% "legislature" = NA_character_
    n %n% "chamber" = meta[ jj ] %>% as.character
    n %n% "type" = meta[ paste0("type-", jj) ] %>% as.character
    n %n% "ipu" = meta[ paste0("ipu-", jj) ] %>% as.integer
    n %n% "seats" = meta[ paste0("seats-", jj) ] %>% as.integer
    
    n %n% "n_cosponsored" = nrow(data)
    n %n% "n_sponsors" = table(bb$n_au) # chamber-specific dataset

    # ==========================================================================
    # VERTEX-LEVEL ATTRIBUTES
    # ==========================================================================

    n_au = as.vector(n_au[ network.vertex.names(n) ])
    n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
    
    n_co = as.vector(n_co[ network.vertex.names(n) ])
    n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
    
    n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
    
    cat(network.size(n), "nodes\n")
        
    n %v% "url" = s[ network.vertex.names(n), "url" ]
    n %v% "sex" = s[ network.vertex.names(n), "sex" ]
    n %v% "born" = s[ network.vertex.names(n), "born" ]
    n %v% "party" = s[ network.vertex.names(n), "party" ]
    n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
    n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
    n %v% "nyears" = s[ network.vertex.names(n), "nyears" ]
    n %v% "constituency" = s[ network.vertex.names(n), "constituency" ]
    # do not remove .jpg, a few photos are .gif
    n %v% "photo" = s[ network.vertex.names(n), "photo" ]
    
    # check all sponsors come from the right chamber
    # print(table(s[ network.vertex.names(n), "type" ]))
    
    set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
    set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
    
    set.edge.attribute(n, "raw", edges$raw) # raw edge counts
    set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
    set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
    
    # ==========================================================================
    # SAVE PLOTS
    # ==========================================================================
    
    if (plot) {
      
      save_plot(n, paste0("plots/net_ro_", jj, ii),
                i = colors[ s[ n %e% "source", "party" ] ],
                j = colors[ s[ n %e% "target", "party" ] ],
                mode, colors)
      
    }
    
    # ==========================================================================
    # SAVE OBJECTS
    # ==========================================================================
    
    # replace unique URLs with names in vertex names and edge attributes
    network.vertex.names(n) = as.character(s[ network.vertex.names(n), "name" ])
    set.edge.attribute(n, "source", s[ n %e% "source", "name" ])
    set.edge.attribute(n, "target", s[ n %e% "target", "name" ])
    
    assign(paste0("net_ro_", jj, leg), n)
    assign(paste0("edges_ro_", jj, leg), edges)
    assign(paste0("bills_ro_", jj, leg), data)
    
    # gexf
    if (gexf)
      save_gexf(n, paste0("net_ro_", jj, ii), mode, colors)
    
  }
  
  if (gexf)
    zip(paste0("net_ro_", jj, ".zip"),
        dir(pattern = paste0("^net_ro_", jj, "\\d{4}-\\d{4}\\.gexf$")))
  
}
