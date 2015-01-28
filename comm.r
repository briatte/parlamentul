# add committee co-memberships

load("data/net_ro.rda")
raw = data.frame()
sponsors = dir("raw", pattern = "\\.html$", full.names = TRUE)

# find unique committees

cat("Parsing committees")
for(i in sponsors) {
  
  h = htmlParse(i)
  n = xpathSApply(h, "//a[contains(@href, 'idc=')]", xmlValue)
  l = xpathSApply(h, "//a[contains(@href, 'idc=')]/@href")
  if(length(l))
    raw = rbind(raw, unique(data.frame(i, n, l, stringsAsFactors = FALSE)))
  
}

cat(":", nrow(unique(raw[, -1 ])), "unique categories\n")

# save flat list
write.csv(raw[, -1 ] %>%
            arrange(n, l) %>%
            group_by(n, l) %>%
            mutate(members = n()) %>%
            unique, "data/committees.csv", row.names = FALSE)

# unique committees, using URLs
comm = data.frame(l = unique(raw$l), stringsAsFactors = FALSE)

# add sponsor columns
for(i in sponsors)
  comm[, gsub("raw/|\\.html", "", i) ] = 0

raw$i = gsub("raw/|\\.html", "", raw$i)

for(i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$l %in% raw$l[ raw$i == i ])

# assign co-memberships to networks
for(i in ls(pattern = "^net_")) {
  
  n = get(i)
  cat(i, ":", network.size(n), "nodes")
  
  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  names(sp) = gsub("(.*)idm=(\\d+)&amp;cam=(\\d)&amp;leg=(\\d+)", "\\3_\\4_\\2", names(sp)) # URL to id
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ , names(sp) ]
  
  cat(" :", nrow(m), "committees", ncol(m), "MPs")
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  stopifnot(ncol(m) == network.size(n))
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data.frame(i = n %e% "source",
                 j = n %e% "target",
                 stringsAsFactors = FALSE)
  e$committee = NA
  
  for(j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]
  
  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")
  
  nn = network(e[, 1:2], directed = FALSE)
  nn %e% "committee" = e$committee
  
  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(!is.na(nn %e% "committee"))
  
  n %e% "committee" = e$committee
  assign(i, n)
  assign(paste0("co", i), nn)
  
}

save(list = ls(pattern = "^((co)?net|edges|bills)_ro_(ca|se)\\d{4}$"),
     file = "data/net_ro.rda")
