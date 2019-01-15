library('pdftools')
library(magick)
library(stringr)
library(tesseract)
library(httr)
library(xml2)
library(plyr)

#This function downloads all of the X-17A-5 pdf reports from the SEC Edgar database
#Creates necessary directories starting from this file location, unless working directory is manually set
downloadpdfs <- function() {
  tickers <- read.csv("x17a5tickers.csv", header = FALSE, stringsAsFactors=FALSE)
  enddate = format(Sys.Date(), format="%Y%m%d")

  for(each in 1:length(tickers$V2)) { #Iterates through each cik in tickers.csv
    comp = tickers$V1[each]
    cik = str_pad(tickers$V2[each], 10, pad="0")

    base_url = "http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany"
    base_url = paste0(base_url, "&CIK=", cik)
    base_url = paste0(base_url, "&type=x-17a-5")
    base_url = paste0(base_url, "&dateb=", enddate)
    base_url = paste0(base_url, "&owner=exclude&output=xml&count=100")
    
    r = httr::GET(base_url)
    data = suppressWarnings(httr::content(r,as="text"))
    x = xml2::read_xml(data)
    href = xml2::xml_find_all(x, ".//filingHREF") #Selects all filing links from given company
    
    linklist = c()
    for(each in 1:length(href)) {
      URL = xml2::xml_text(href[each])
      splitURL = str_split(URL,"\\.")
      if(length(splitURL) > 0) {
        ext = splitURL[[1]][length(splitURL[[1]])]
        if(ext == "htm") {
          URL = paste0(URL,"l")
          linklist = c(linklist,URL)
        }
      }
    }
    if(length(linklist) > 0) {
      doclist = c()
      docnamelist = c()
      for(each in 1:length(linklist)) {
        r = httr::GET(linklist[each])
        data = suppressWarnings(httr::content(r,as="text"))
        x = xml2::read_html(data)
        divs = xml2::xml_find_all(x,".//div")
        classes = xml2::xml_attr(divs,"class")
        first = match("formGrouping",classes)
        slice = classes[first+1:length(classes)]
        group = divs[first + match("formGrouping",slice)]
        children = xml2::xml_children(group)[2]
        date = xml2::xml_text(children) #Gets the filing date from the html response
        
        a = xml2::xml_find_all(x,".//a")
        url = grep("/Archives/edgar/.+\\.pdf",xml2::xml_attr(a,"href"),value=TRUE)[1]
        link = paste0("https://www.sec.gov",url)
        filename = paste(comp,date,"x17a5.pdf",sep = "_")
        
        doclist = c(doclist,link)
        docnamelist = c(docnamelist,filename)
      }
      
      dir.create(paste0("SEC-Edgar-data/",comp,"/x17a5/"))
      for(doc in 1:length(doclist)) {
        r = httr::GET(doclist[doc])
        writeBin(suppressWarnings(httr::content(r)), paste0("SEC-Edgar-data/",comp,"/x17a5/",docnamelist[doc]))
      }
    }
  }
}

#This function converts all of the pdf files in the directory into text, using Tesseract OCR if necessary
#main_dir: directory containing pdf files
x17a5totext <- function(main_dir) {
  unlink(paste0(main_dir,"/txt"))
  dir.create(paste0(main_dir,"/txt"))
  files <- list.files(main_dir,pattern = "*.pdf")
  if(length(files) >= 1) {
    for(filenum in 1:length(files)) {
      currfile = files[filenum]
      filename = substr(currfile, 1, nchar(currfile)-4)
      text <- pdftools::pdf_text(paste0(main_dir,"/",currfile))
      if(text == "") {
        text <- betterocr(paste0(main_dir,"/",currfile))
        txtfile = file(paste0(main_dir,"/txt/",filename,".txt"),"wb")
        writeLines(text, txtfile)
        close(txtfile)
      }
      else {
        txtfile = file(paste0(main_dir,"/txt/",filename,".txt"),"wb")
        writeBin(paste0(text,"\n"), txtfile)
        close(txtfile)
      }
    }
  }
}

#This function parses the text reports into a csv containing the cash, total assets, and equity values
#main_dir: directory containing pdf files
#reg: dataframe of regex strings
parsex17a5 <- function(main_dir,reg) {
  txtpath = paste0(main_dir,"/txt/")
  
  textfiles <- list.files(txtpath, pattern = "*.txt")
  
  #Initialize empty lists for each field of the data frame
  yearlist = c()
  cashlist = c()
  assetslist = c()
  equitylist = c()
  levratiolist = c()
  cashratiolist = c()
  
  if(length(textfiles) >= 1) {
    #Iterate through text files
    dir_split = strsplit(main_dir,"/")
    comp = dir_split[[1]][2]
    columnlist = c("Name","Field")
    
    for(fileindex in 1:length(textfiles)) {
      currentfile = textfiles[fileindex]
      
      date = strsplit(currentfile, "_")[[1]][2]
      date = format(as.Date(date, format = "%Y-%m-%d"))
      year = strsplit(date, "-")[[1]][1]
      
      text <- readLines(paste0(txtpath,currentfile))
      
      for(line in 1:length(text)) {
        text[line] = gsub("\\s+"," ",text[line]) #remove extra whitespace from text
        text[line] = gsub(") ,","1,",text[line])
        text[line] = gsub(", ","",text[line]) #remove ", " from text
        text[line] = gsub(",","",text[line])
        text[line] = gsub(">","",text[line])
        text[line] = gsub("_"," ",text[line])
        text[line] = gsub("=","",text[line])
        text[line] = gsub("\\.","",text[line])
        text[line] = gsub("-","",text[line])
        text[line] = gsub("\\|","",text[line])
        text[line] = gsub(":","",text[line])
      }
      
      cashline = grep(reg$cash, text)[1]
      assetsline = grep(reg$assets, text)[1]
      equityline = grep(reg$equity, text, perl=TRUE)[1]
      
      if(!is.na(cashline)) {
        bottom = max(1,cashline-50)
        subtext = text[bottom:(cashline+50)]
      }
      else {
        subtext = text
      }
      thousands = grep("[Ii]n [Tt]housands",subtext)
      millions = grep("[Ii]n [Mm]illions",subtext)
      
      cash = text[cashline]
      cash = substr(cash,regexpr(reg$cash,cash),nchar(cash))
      cash = substr(cash,regexpr("\\$",cash),nchar(cash))
      cash = substr(cash,regexpr(")",cash),nchar(cash))
      
      assets = text[assetsline]
      assets = substr(assets,regexpr(reg$assets,assets),nchar(assets))
      assets = substr(assets,regexpr("\\$",assets),nchar(assets))
      assets = substr(assets,regexpr(")",assets),nchar(assets))
      
      equity = text[equityline]
      equity = substr(equity,regexpr(reg$equity,equity),nchar(equity))
      equity = substr(equity,regexpr("\\$",equity),nchar(equity))
      equity = substr(equity,regexpr(")",equity),nchar(equity))
      equity = substr(equity,regexpr(" 8 ",equity),nchar(equity))
      equity = gsub(" 8 "," ",equity)
  
      cashvallist = stringr::str_extract_all(cash, "[0-9]+(?!\\s*\\w)")
      cashval = cashvallist[[1]][1]
      
      assetvallist = stringr::str_extract_all(assets, "[0-9]+")
      assetval = assetvallist[[1]][1]
      
      equityvallist = stringr::str_extract_all(gsub("\\d \\d","",equity), "[0-9]+")
      equityval = equityvallist[[1]][1]
      
      #If cashval returns no number, check next and previous lines
      if(is.na(cashval) || identical(cashval[[1]],character(0))) {
        cashval = stringr::str_extract(text[cashline-1],"[0-9]+")
        if(is.na(cashval) || identical(cashval,character(0))) {
          cashval = stringr::str_extract(text[cashline+1],"[0-9]+")
          if(is.na(cashval) || identical(cashval,character(0))) {
            cashval = NA
          }
        }
      }
      #If assetval returns no number, check next and previous lines
      if(is.na(assetval) || identical(assetval[[1]],character(0))) {
        assetval = stringr::str_extract(text[assetsline+1],"[0-9]+")
        if(is.na(assetval) || identical(assetval,character(0))) {
          assetval = stringr::str_extract(text[assetsline-1],"[0-9]+")
          if(is.na(assetval) || identical(assetval,character(0))) {
            assetval = NA
          }
        }
      }
      
      #If equityval returns no number, check next and previous lines
      if(is.na(equityval) || identical(equityval[[1]],character(0))) {
        if(reg$nextfirst) {
          equityval = stringr::str_extract(gsub(" ","",text[equityline+1]),"[0-9]+")
        } else {
          equityval = stringr::str_extract(gsub(" ","",text[equityline-1]),"[0-9]+")
        }
        if(is.na(equityval) || identical(equityval,character(0))) {
          if(reg$nextfirst) {
            equityval = stringr::str_extract(gsub(" ","",text[equityline-1]),"[0-9]+")
          } else {
            equityval = stringr::str_extract(gsub(" ","",text[equityline+1]),"[0-9]+")
          }
          if(is.na(equityval) || identical(equityval,character(0))) {
            equityval = NA
          }
        }
      }
      lev = round(as.numeric(assetval)/as.numeric(equityval), digits = 6)
      cashratio = round(as.numeric(cashval)/as.numeric(assetval), digits = 6)
      
      if(is.na(cashval) || is.na(assetval) || is.na(equityval) || lev == 1 || cashval == "0" || assetval == "0" || equityval == "0") {
        print(paste("reprocess",comp,date))
        
        text <- betterocr(paste0(main_dir,"/",comp,"_",date,"_x17a5.pdf"))
        
        for(line in 1:length(text)) {
          text[line] = gsub("\\s+"," ",text[line]) #remove extra whitespace from text
          text[line] = gsub(", ","",text[line]) #remove ", " from text
          text[line] = gsub(",","",text[line])
          text[line] = gsub(">","",text[line])
          text[line] = gsub("_"," ",text[line])
          text[line] = gsub("=","",text[line])
          text[line] = gsub("\\.","",text[line])
        }
        
        cashline = grep(reg$cash, text)[1]
        assetsline = grep(reg$assets, text)[1]
        equityline = grep(reg$equity, text, perl=TRUE)[1]
        
        if(!is.na(cashline)) {
          bottom = max(1,cashline-50)
          subtext = text[bottom:(cashline+50)]
        }
        else {
          subtext = text
        }
        thousands = grep("[Ii]n [Tt]housands",subtext)
        millions = grep("[Ii]n [Mm]illions",subtext)
        
        cash = text[cashline]
        cash = substr(cash,regexpr(reg$cash,cash),nchar(cash))
        assets = text[assetsline]
        assets = substr(assets,regexpr(reg$assets,assets),nchar(assets))
        equity = text[equityline]
        equity = substr(equity,regexpr(reg$equity,equity),nchar(equity))
        
        cashvallist = stringr::str_extract_all(cash, "[0-9]+")
        cashval = cashvallist[[1]][1]
        
        assetvallist = stringr::str_extract_all(assets, "[0-9]+")
        assetval = assetvallist[[1]][1]
        
        equityvallist = stringr::str_extract_all(gsub(" ","",equity), "[0-9]+")
        equityval = equityvallist[[1]][1]
        
        #If cashval returns no number, check next and previous lines
        if(is.na(cashval) || identical(cashval[[1]],character(0))) {
          cashval = stringr::str_extract(text[cashline-1],"[0-9]+")
          if(is.na(cashval) || identical(cashval,character(0))) {
            cashval = stringr::str_extract(text[cashline+1],"[0-9]+")
            if(is.na(cashval) || identical(cashval,character(0))) {
              cashval = NA
            }
          }
        }
        #If assetval returns no number, check next and previous lines
        if(is.na(assetval) || identical(assetval[[1]],character(0))) {
          assetval = stringr::str_extract(text[assetsline+1],"[0-9]+")
          if(is.na(assetval) || identical(assetval,character(0))) {
            assetval = stringr::str_extract(text[assetsline-1],"[0-9]+")
            if(is.na(assetval) || identical(assetval,character(0))) {
              assetval = NA
            }
          }
        }
        
        #If equityval returns no number, check next and previous lines
        if(is.na(equityval) || identical(equityval[[1]],character(0))) {
          if(reg$nextfirst) {
            equityval = stringr::str_extract(gsub(" ","",text[equityline+1]),"[0-9]+")
          } else {
            equityval = stringr::str_extract(gsub(" ","",text[equityline-1]),"[0-9]+")
          }
          if(is.na(equityval) || identical(equityval,character(0))) {
            if(reg$nextfirst) {
              equityval = stringr::str_extract(gsub(" ","",text[equityline-1]),"[0-9]+")
            } else {
              equityval = stringr::str_extract(gsub(" ","",text[equityline+1]),"[0-9]+")
            }
            if(is.na(equityval) || identical(equityval,character(0))) {
              equityval = NA
            }
          }
        }
        lev = round(as.numeric(assetval)/as.numeric(equityval), digits = 6)
        cashratio = round(as.numeric(cashval)/as.numeric(assetval), digits = 6)
      }
      
      if(!identical(thousands,integer(0))) {
        cashval = paste0(cashval,"000")
        assetval = paste0(assetval,"000")
        equityval = paste0(equityval,"000")
      }
      else if(!identical(millions,integer(0))) {
        cashval = paste0(cashval,"000000")
        assetval = paste0(assetval,"000000")
        equityval = paste0(equityval,"000000")
      }
      
      yearlist = c(yearlist, date)
      cashlist = c(cashlist, as.numeric(cashval))
      assetslist = c(assetslist, as.numeric(assetval))
      equitylist = c(equitylist, as.numeric(equityval))
      levratiolist = c(levratiolist, lev)
      cashratiolist = c(cashratiolist, cashratio)
      columnlist = c(columnlist,year)
    }
    
    cashlist = c(comp,"Cash",cashlist)
    assetslist = c(comp,"Total Assets",assetslist)
    equitylist = c(comp,"Member's Equity",equitylist)
    levratiolist = c(comp,"Leverage Ratio",levratiolist)
    cashratiolist = c(comp,"Cash/Assets",cashratiolist)
    
    cashframe = as.data.frame(t(cashlist))
    colnames(cashframe) = columnlist
    assetframe = as.data.frame(t(assetslist))
    colnames(assetframe) = columnlist
    equityframe = as.data.frame(t(equitylist))
    colnames(equityframe) = columnlist
    levframe = as.data.frame(t(levratiolist))
    colnames(levframe) = columnlist
    crframe = as.data.frame(t(cashratiolist))
    colnames(crframe) = columnlist
    
    
    fullframe = rbind(cashframe,assetframe)
    fullframe = rbind(fullframe,equityframe)
    fullframe = rbind(fullframe,levframe)
    fullframe = rbind(fullframe,crframe)
    
    fullframe = fullframe[, !duplicated(colnames(fullframe))]
    write.csv(fullframe, paste0("CSV Output","/",comp,".csv"))
    return(fullframe)
  }
}

#This function iterates through all of the companies to convert the pdf files into text
alltotext <- function() {
  companies <- list.dirs("SEC-Edgar-data", recursive = FALSE)
  for(each in 1:length(companies)) {
    print(paste0(companies[each],"/x17a5"))
    x17a5totext(paste0(companies[each],"/x17a5"))
  }
}

#This function iterates through all of the companies to parse the text reports using the specified regex strings
parseall <- function() {
  companies <- list.dirs("SEC-Edgar-data", recursive = FALSE)
  regexdf <- read.csv("Regex.csv")
  alldf = data.frame()
  for(each in 1:length(companies)) {
    print(paste0(companies[each],"/x17a5"))
    comp = strsplit(companies[each], "/")
    ind <- which(regexdf$name == comp[[1]][2])
    if(length(ind) > 0) {
      reg = regexdf[ind,]
    }
    else {
      reg = data.frame(
        name="default",
        cash="^\\s*Cash$|(Cash\\s*\\d+)|(Cash and)",
        assets="(Total assets)|(Total\\s+[Ll]iabilities\\s+and)|(Total\\s*\\d+)",
        equity="([Mm]ember.?|Owner.?|[Ss]tockholder.?)[s*'*]*\\s+((equi[tl]y\\s*\\d*)|(Equity\\s*\\d+)|(equity . \\d+))",
        nextfirst=TRUE)
    }
    compdf = parsex17a5(paste0(companies[each],"/x17a5"),reg)
  }
  for(each in 1:length(companies)) {
    comp = strsplit(companies[each], "/")[[1]][2]
    compdf = tryCatch(read.csv(paste0("CSV Output/",comp,".csv")),
                      error=function(err) {
                        return(NULL)
                      })
    alldf = rbind.fill(alldf,compdf)
  }
  alldf = alldf[,c(colnames(alldf)[1:2],sort((colnames(alldf)[3:length(alldf)])))]
  alldf = subset(alldf,select=-c(X))
  write.csv(alldf,"CSV Output/all.csv")
}

#This function performs some preprocessing on the pdf files before they are read by tesseract
betterocr <- function(pdf) {
  ##PDF to pngs
  #pages = pdftools::pdf_info(pdf)$pages
  images = pdftools::pdf_convert(pdf, format="png",dpi=400)
  ##pngs to text
  text = c()
  for(each in 1:length(images)) {
    img_file = magick::image_read(images[each])
    img_file = magick::image_scale(img_file,"x2000") #Upscales the image to increase pixel height of each character
    img_file = magick::image_convert(img_file,colorspace="gray") #Converts the image into grayscale to improve contrast slightly and remove colors
    img_file = magick::image_trim(img_file) #Trims margins from image (removes extra area matching the document background color)
    img_file = magick::image_noise(img_file) #Reduces image noise
    img_file = magick::image_enhance(img_file) #Alternative method to reduce image noise
    img_file = magick::image_contrast(img_file,sharpen=1) #Increases the contrast of the image to distinguish characters from background

    text = c(text,tesseract::ocr(img_file))
  }
  fulltext = strsplit(paste(text,collapse = "\n"),split = "\n")[[1]]
  pngs <- dir(pattern = "*.png")
  file.remove(pngs)
  return(fulltext)
}

#main_dir: directory containing x17a5tickers.csv and Regex.csv
main <- function(main_dir) {
  setwd(main_dir)
  downloadpdfs() #download pdf reports
  alltotext() #convert pdfs to text
  parseall() #convert text to csv
}
