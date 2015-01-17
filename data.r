root = "http://www.cdep.ro/pls/"
bills = "data/bills.csv"
sponsors = "data/sponsors.csv"

# scrape bills and MP details from the Romanian Parliament, 1996-2014
# covers approx. 45,000 bills, of which 13,000 by 2,600 parliamentarians
# pretty slow on MP details; re-run a few times to solve network issues

if(!file.exists(bills)) {
  
  b = data.frame()
  
  for(j in c(1, 2)) { # two chambers
    
    h = htmlParse(paste0("http://www.cdep.ro/pls/proiecte/upl_pck.home?cam=", j))
    h = xpathSApply(h, "//a[contains(@href, '&anp') or contains(@href, '&anb') or contains(@href, '&anl') or contains(@href, '&ans')]/@href")
    
    for(i in unique(h)) {
      
      cat(i)
      hh = htmlParse(paste0(root, "proiecte/", i))
      ref = xpathSApply(hh, "//a[contains(@href, 'idp=')]", xmlValue)
      if(length(ref))
        b = rbind(b,
                  data.frame(
                    page = i,
                    url = xpathSApply(hh, "//a[contains(@href, 'idp=')]/@href"), ref,
                    name = xpathSApply(hh, "//a[contains(@href, 'idp=')]/../following-sibling::td[1]", xmlValue),
                    status = xpathSApply(hh, "//a[contains(@href, 'idp=')]/../following-sibling::td[2]", xmlValue),
                    stringsAsFactors = FALSE))
      cat(":", sprintf("%5.0f", nrow(b)), "bills\n")
      
    }
    
  }
  
  b$authors = NA
  write.csv(b, bills, row.names = FALSE)
  
}

b = read.csv(bills, stringsAsFactors = FALSE)
cat("Loaded:", nrow(b), "bills\n")

# scrape only links to bills that have no authors (including government bills)
l = unique(b$url[ is.na(b$authors) ])

if(length(l)) {
  
  for(i in rev(l)) {
    
    cat(sprintf("%5.0f", which(l == i)), str_pad(i, 32, "right"))
    hh = try(htmlParse(paste0(root, "proiecte/", i)), silent = TRUE)
    
    if("try-error" %in% class(hh)) {
      
      cat(" : failed\n") # will be scraped again at re-run
      
    } else {
      
      hh = xpathSApply(hh, "//a[contains(@href, 'structura.mp?idm=')]/@href")
      if(length(hh)) {
        
        cat(" :", length(hh), "sponsor(s)\n")
        b$authors[ b$url == i ] = paste0(hh, collapse = ";")
        
      } else {
        
        cat(" : no sponsors\n") # government or parliamentary commissions
        b$authors[ b$url == i ] = ""
        
      }
      
    }
    
  }
  
  # make all URLs formed the same way: id, chamber, legislature
  j = !is.na(b$authors) & b$authors != ""
  b$authors[ j ] = sapply(b$authors[ j ], function(x) {
    j = unlist(strsplit(x, ";"))
    j = paste0("/pls/parlam/structura.mp?", 
               str_extract(j, "idm=[0-9]+"), "&", 
               str_extract(j, "cam=[0-9]"), "&", 
               str_extract(j, "leg=[0-9]+"))
    return(paste0(unique(j), collapse = ";"))
  })
  
  b$n_au = 1 + str_count(b$authors, ";")
  b$n_au[ is.na(b$authors) ] = NA
  b$n_au[ b$authors == "" ] = 0
  
  print(table(b$n_au, exclude = NULL))
  print(table(b$n_au > 1, exclude = NULL))
  print(table(b$n_au > 2, exclude = NULL))
  
  write.csv(b, bills, row.names = FALSE)
  
}

if(!file.exists(sponsors)) {
  
  a = unique(gsub("^/pls/", "", unlist(strsplit(b$authors, ";"))))
  s = data.frame()
  
  for(i in rev(a)) {
    
    # filename: chamber_legislature_id.html
    # ids are not attached to persons: id 3 in 1992 is not the same person as id 3 in 2004
    j = gsub("(.*)idm=(\\d+)&cam=(\\d)&leg=(\\d+)", "raw/\\3_\\4_\\2.html", i)
    cat(sprintf("%5.0f", which(a == i)), str_pad(i, 45, "right"))
    
    if(!file.exists(j)) {
      
      f = try(download.file(paste0(root, i), j, quiet = TRUE, mode = "wb"), silent = TRUE)
      
      if("try-error" %in% class(f) | !file.info(j)$size)
        file.remove(j)
      
    }
    
    if(file.exists(j)) {
      
      l = gsub("(.*)&leg=(\\d+)", "\\2", i) # legislature start
      h = htmlParse(j)
      nfo = xpathSApply(h, "//td[@class='menuoff']", xmlValue)
      nfo = gsub("\\si(a|u)n\\.\\s", " ", nfo)
      
      name = gsub("(.*)n\\.(.*)", "\\1", nfo)
      name = gsub("([0-9]{4})-([0-9]{4})", "", name) # remove dates for deceased persons
      
      # born = gsub("(.*)n\\.(.*)(\\d{4})", "\\3", nfo)
      born = str_extract(gsub("(.*)n\\.(.*)", "\\2", nfo), "[0-9]{4}")
      
      # nb. mandates (counts both MP and senator mandates; many have both)
#       mdts = xpathSApply(h, "//b[contains(text(), 'dep.') or contains(text(), 'sen.')]", xmlValue)
#       mdts = sum(str_extract(mdts, "[0-9]{4}") <= l)
      
      mdts_ca = xpathSApply(h, "//b[contains(text(), 'dep.')]", xmlValue)
      if(length(mdts_ca))
        mdts_ca = sum(str_extract(mdts_ca, "[0-9]{4}") < l)
      else
        mdts_ca = 0
      
      mdts_se = xpathSApply(h, "//b[contains(text(), 'sen.')]", xmlValue)
      if(length(mdts_se))
        mdts_se = sum(str_extract(mdts_se, "[0-9]{4}") < l)
      else
        mdts_se = 0

      sex = xpathSApply(h, "//a[contains(@href, 'structura.ce') or contains(@href, 'structura.gp')][1]/..", xmlValue)
      sex[ grepl("^aleasă\\s", sex) ] = "F"
      sex[ grepl("^ales\\s", sex) ] = "M"
      sex[ !sex %in% c("F", "M") ] = NA # none missing
      
      circo = xpathSApply(h, "//a[contains(@href, 'structura.ce') or contains(@href, 'structura.gp')][1]", xmlValue)
      # circo = grps[1] # constituency
      # grps = grps[-1] # parliamentary group(s) -- without start-end dates
      circo = paste0(substr(circo, 1, 1), tolower(substring(circo, 2))) # capitalize
      
      # full party info
      m = xpathSApply(h, "//a[contains(@href, 'structura.fp')]/../..", xmlValue)
      # full party names
      n = xpathSApply(h, "//a[contains(@href, 'structura.fp')]", xmlValue)
      
      if(length(n) > 1) {
        
        # years in each party; simplifies some common transitions:
        # - independent just before elections
        # - PD to PDL (Partidul Democrat Liberal)
        # - PUR-SL to PC (Partidul Conservator)
        m = sapply(m, str_extract_all, "[0-9]{4}")
        
        if(length(m[[1]]) == 1) # până în (first)
          m[[1]] = c(l, m[[1]])
        if(length(m[[ length(m) ]]) == 1) # până în (last)
          m[[ length(m) ]] = c(m[[ length(m) ]], as.numeric(l) + 4)
        
        # length of stay in each party, in approximate years
        m = lapply(m, as.numeric)
        m = sapply(m, function(x) max(x) - min(x)) # throws warning if no date
        
        # approx. 148 ambiguous cases out of 2612 (5%)
        o = which(m == max(m))
        if(length(o) == 1) {
          n = n[ which(m == max(m)) ]
          o = 0
        } else {
          n = n[ length(n) ] # if equal lengths, use last party
          o = 1
        }
        
      }
      
      photo = xpathSApply(h, "//img[contains(@src, 'parlamentari')]/@src")
      if(!length(photo))
        p = NA
      else {
        p = gsub("(.*)/(.*)", "photos/\\2", photo)
        if(!file.exists(p))
          try(download.file(gsub("^/", "http://www.cdep.ro/", photo), p, mode = "wb", quiet = TRUE),
              silent = TRUE)
        if(!file.info(p)$size) {
          file.remove(p)
          p = NA
        }
      }
      
      s = rbind(s, data.frame(legislature = l, url = i, name, sex, born,
                              party = n, party_dummy = o, mdts_ca, mdts_se,
                              constituency = circo, photo = p, stringsAsFactors = FALSE))
      
    }
    
    cat(name, "\n")
    
  }

  # constituencies
  s$constituency = paste0(gsub("\\s", "_", s$constituency), "_County")
  s$constituency = gsub("ţ", "ț", s$constituency)
  s$constituency = gsub("ş", "ș", s$constituency)
  s$constituency[ s$constituency == "Caraș-severin_County" ] = "Caraș-Severin_County"
  s$constituency[ s$constituency == "Bistrița-năsăud_County" ] = "Bistrița-Năsăud_County"
  s$constituency[ s$constituency == "Satu-mare_County" ] = "Satu_Mare_County"
  s$constituency[ s$constituency == "Satu-mare_County" ] = "Satu_Mare_County"
  s$constituency[ s$constituency == "București_County" ] = "Bucharest"
  s$constituency[ s$constituency == "Diaspora_County" ] = "Romanian_diaspora"
  s$constituency[ s$constituency == "la_nivel_național_County" ] = "Romanian_ethnic_minorities_parties"

  s$type = ifelse(grepl("cam=1", s$url), "Senator", "Deputat")
  # print(table(s$party_dummy))
  # s$photo[ !sapply(s$photo, file.exists) ] = NA
  
  write.csv(s, sponsors, row.names = FALSE)

}

s = read.csv(sponsors, stringsAsFactors = FALSE)
cat("Loaded:", nrow(s), "sponsors",
    sum(s$type == "Senator"), "senators",
    sum(s$type == "Deputat"), "MPs\n")

# mandates in the same chamber only (for comparability)
s$nyears = ifelse(s$type == "Deputat", 4 * s$mdts_ca, 4 * s$mdts_se)

# duplicate names
s$name[ s$url == "parlam/structura.mp?idm=164&cam=2&leg=1996" ] = "Gheorghe Ana-1" # oldest
s$name[ s$url == "parlam/structura.mp?idm=333&cam=2&leg=2012" ] = "Ovidiu Ioan Silaghi-1" # as PNL, 2012-2013
s$name[ s$url == "parlam/structura.mp?idm=416&cam=2&leg=2012" ] = "Ovidiu Ioan Silaghi-2" # as independent, 2014-

# national / linguistic / religious (FER) / ethnic minority (Minorităților) communities, except UDMR
s$party[ grepl("^(Asociaţia|Comunitatea|Uniunea)", s$party) ] = "MIN"
s$party[ grepl("Evreieşti|Germanilor|Albanezilor|Romilor", s$party) ] = "MIN"
s$party[ s$party == "independent" ] = "IND"

# absorptions and renamings
s$party[ s$party == "PNL-CD" ] = "PNL" # Partidul Naţional Liberal - Convenţia Democrată (coalition)
s$party[ s$party %in% c("PDSR", "PSDR", "PSM") ] = "PSD" # absorbed FDSN
s$party[ s$party %in% c("PD", "PDL", "FC") ] = "PD-L" # PD renamed to PD-L; FC (only 4 MPs) absorbed in 2014
s$party[ s$party == "PUR-SL" ] = "PC" # renamed (Conservatives, allied to PSD then PNL)
s$party[ s$party %in% c("PAR", "PAC") ] = "PNL" # small parties, absorbed

cbind(table(s$party, s$legislature), table(s$party))
cbind(table(s$type, s$legislature), table(s$type))

# finalize bills data
b$au_type = NA
b$au_type[ grepl("cam=1", b$authors) & !grepl("cam=2", b$authors) ] = "Senate"
b$au_type[ grepl("cam=2", b$authors) & !grepl("cam=1", b$authors) ] = "Chamber"
b$au_type[ grepl("cam=1", b$authors) & grepl("cam=2", b$authors) ] = "both"

# a third of cosponsored bills are cosponsored by members of both chambers
table(b$au_type, b$n_au > 1)

b$date = gsub("(.*)/(.*)", "\\2", b$ref)
b$date = as.Date(strptime(b$date, "%d.%m.%Y"))
b$legislature = NA
b$legislature[ is.na(b$legislature) & b$date >= as.Date("2012-12-09") ] = "2012-2016"
b$legislature[ is.na(b$legislature) & b$date >= as.Date("2008-11-30") ] = "2008-2012"
b$legislature[ is.na(b$legislature) & b$date >= as.Date("2004-11-28") ] = "2004-2008"
b$legislature[ is.na(b$legislature) & b$date >= as.Date("2000-11-26") ] = "2000-2004"
b$legislature[ is.na(b$legislature) & b$date >= as.Date("1996-11-03") ] = "1996-2000"
b$legislature[ b$date < as.Date("1996-11-03")  ] = NA

# # a few bills do not have complete dates
# b$year = str_sub(b$ref, start = -4)
# b$legislature[ is.na(b$legislature) & b$year %in% 1997:2000 ] = "1996-2000"
# b$legislature[ is.na(b$legislature) & b$year %in% 2001:2004 ] = "2000-2004"
# b$legislature[ is.na(b$legislature) & b$year %in% 2005:2008 ] = "2004-2008"
# b$legislature[ is.na(b$legislature) & b$year %in% 2009:2012 ] = "2008-2012"
# b$legislature[ is.na(b$legislature) & b$year %in% 2013:2014 ] = "2012-2016"

table(b$legislature, exclude = NULL) # excluding a handful of old bills

# subset to recent cosponsored bills
b = subset(b, !is.na(legislature) & !is.na(authors) & authors != "")
b$authors = gsub("/pls/", "", b$authors)
table(grepl(";", b$authors), b$legislature)

# between 42% and 70% of bills are cosponsored (increases through time)
prop.table(table(grepl(";", b$authors), b$legislature), 2)

# sponsor identification through unique URLs
rownames(s) = s$url

# kthxbye
