
#------------------------------------------------------------
# объявление функции

ch.gen <- function(file.src, FA.df, all){
  # константы
  
  # 0xC  -- number of measurement records(int32)
  # 0x30 -- time stamp (character) in format '%d-%m-%y%H:M.%S'
  # 0x88 -- begin of measurement records (32 signed ints)
  
  # вообще-то непонятно, надо так делать или не надо так делать
  Sys.setenv(TZ="Europe/Minsk")
  
  # для колдовства нужен дата фрейм - FA.df
  
  # даем новое имя файлу
  file.new.name <- sprintf("%s.new.dat", strsplit(file.src, "\\.")[[1]][1])
  file.copy(file.src, file.new.name, overwrite=T)
  
  file.src <- file(file.new.name,'r+b')
  
  file.dst <- file.src
  
  # padding from beginning 
  #seek(file.src,where=0x0,origin='start')
  #writeBin(readBin(file.src,raw(),0xb),file.dst)
  
  # number of records
  seek(file.src,where=0xC,origin='start')
  num.of.records <- readBin(file.src,integer(),1)
  #writeBin(num.of.records,file.dst)
  
  # timestamp
  seek(file.src,where=0x30,origin='start')
  date <- strptime(readChar(file.src,nchars=16),format='%d-%m-%y%H:%M.%S')
  date <- date + as.integer(runif(1,max=100))
  
  seek(file.src,where=0x30,origin='start',rw='write')
  writeChar(strftime(date,format='%d-%m-%y%H:%M.%S'),file.dst)
  
  # second padding
  #seek(file.src,0x30+16+1,origin='start')
  #writeBin(readBin(file.src,raw(),0x88-(0x30+16+1)),file.dst)
  
  # measurements data
  seek(file.src,0x88,origin='start')
  mes.data <- readBin(file.src,integer(),num.of.records)/0.4
  #std <- sd(mes.data[mes.data <= median(mes.data)])
  #mes.data <- mes.data + (std - runif(num.of.records,max=1.96*std))
  
  # изменяем данные
  for(i in 1:nrow(FA.df)) {
    start = FA.df$start[i]/all*length(mes.data)
    end   = FA.df$end[i]/all*length(mes.data)
    
    mes.data.target = mes.data[start:end]
    x = seq(0, pi, by = pi/(length(mes.data.target)-1))
    p = sin(x)*FA.df$m[i]+1; #plot(p, type = "l")
    
    mes.data <- c(mes.data[0:(start-1)],
                  mes.data.target*p,
                  mes.data[(end+1):length(mes.data)])
  }
  
  k = seq(1, length(mes.data))/(tail(mes.data,1)*0.25)
  writeBin(as.integer(mes.data*0.4+k), file.dst)
  plot(mes.data, type = "l", ylim = c(0, mean(mes.data)/2))
  # final padding
  #seek(file.src,0x88 + num.of.records * 4,origin='start')
  close(file.src)
  #return(mes.data)
}


a = data.frame(start = 12.7200, end = 13.2900, m = 2)  #C12
a = rbind(a, c(start = 17.0353, end = 17.5520, m = 10))#C14
a = rbind(a, c(start = 21.7200, end = 22.2500, m = 10))#C16
a = rbind(a, c(start = 17.0353, end = 17.5520, m = 10))#C18:2n6t
a = rbind(a, c(start = 17.0353, end = 17.5520, m = 10))#C18:2n6c
a = rbind(a, c(start = 17.0353, end = 17.5520, m = 10))#C18:3n6
ch.gen("D:/V106.dat", a, 46.598)
