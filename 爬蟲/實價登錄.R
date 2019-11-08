library('dplyr')
library('data.table')


#實價登錄 爬全部資料
yearST <- 101
yearEN <-  Sys.Date() %>% format(.,"%Y") %>% as.integer() - 1911 
seasons <- c("S1", "S2", "S3", "S4")
path <- "D:\\HousePrice\\"

for(year in yearST:yearEN){
  for(season in seasons){
    fileName <- paste0(year, season, "_HousePrice.rar")
    filePath <- paste0(path, fileName)
    downloadFile <- paste0("https://plvr.land.moi.gov.tw//DownloadSeason?season=", year, season, "&type=zip&fileName=lvr_landcsv.zip")
    download.file(downloadFile, destfile = filePath, mode = "wb")
  }
}




#實價登錄 資料整合
yearST <- 101
yearEN <-  Sys.Date() %>% format(.,"%Y") %>% as.integer() - 1911 
seasons <- c("S1", "S2", "S3", "S4")
csvName <- "_lvr_land_a.csv"
csvPath <- "D:\\HousePrice\\"
# header <- readLines("D:\\HousePrice\\header.txt", encoding = 'UTF-8') %>% strsplit(.,',') %>% unlist
header <- c('鄉鎮市區', '交易標的', '土地區段位置/建物區段門牌', '土地移轉總面積平方公尺', '都市土地使用分區',
            '非都市土地使用分區', '非都市土地使用編定', '交易年月日', '交易筆棟數', '移轉層次', '總樓層數',
            '建物型態', '主要用途', '主要建材', '建築完成年月', '建物移轉總面積平方公尺', '建物現況格局-房',
            '建物現況格局-廳', '建物現況格局-衛', '建物現況格局-隔間', '有無管理組織', '總價元',
            '單價元/平方公尺', '車位類別', '車位移轉總面積平方公尺', '車位總價元', '備註', '編號')


for(letter in letters){
  temp <- NULL
  for(year in yearST:yearEN){
    for(season in seasons){
      filePath <- paste0(csvPath, year, season, "_HousePrice\\", letter, csvName)
      tryCatch({
        data <- fread(filePath, skip = 2, encoding = 'UTF-8', header = F)
        temp <- bind_rows(temp, data)
      }, error = function(x){
        print(paste0(year, season,"_", letter, csvName, '  not exist'))
      })
    }
  }
  finalPath <- paste0(csvPath, "total_", letter, "_land.csv")
  if(!is.null(temp)){
    colnames(temp) = header
    write.csv(temp,file = finalPath, row.names = F)
  }
}
