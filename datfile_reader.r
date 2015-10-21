#setwd("E:/work/Projects/FocusGC/")
setwd("E:/work/FocusGC/")

#------------------------------------------------------------
# объявление функции

ch.update <- function(file.src, t.s = 1, t.e = 1, t.a = 1, m = 1, overwrite = F){
  # константы
  
  # 0xC  -- number of measurement records(int32)
  # 0x30 -- time stamp (character) in format '%d-%m-%y%H:M.%S'
  # 0x88 -- begin of measurement records (32 signed ints)
  
  # вообще-то непонятно, надо так делать или не надо так делать
  Sys.setenv(TZ="Europe/Minsk")
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
  
  # парсим диапазон пика
  start = t.s/t.a*length(mes.data)
  end = t.e/t.a*length(mes.data)
  # изменяем данные
  if(overwrite == T) {
    mes.data.target = mes.data[start:end]
    
    x = seq(0, pi, by = pi/(length(mes.data.target)-1))
    p = sin(x)*m+1; #plot(p, type = "l")
    
    mes.data <- c(mes.data[0:(start-1)],
                mes.data.target*p,
                mes.data[(end+1):length(mes.data)])
    k = seq(1, length(mes.data))/(tail(mes.data,1)*0.25)
    writeBin(as.integer(mes.data*0.4+k),file.dst)
    
  }
  #plot(mes.data, type = "l", ylim = c(0, mean(mes.data)/2))
  # final padding
  #seek(file.src,0x88 + num.of.records * 4,origin='start')
  close(file.src)
  return(mes.data)
}

#------------------------------------------------------------
# обработка данных

a <- ch.update("D:/H19.dat")

library(ptw)
plot(a, type = "l", ylim = c(0, 25000))
a.blc = baseline.corr(a)
lines(a.blc, col = 2)
plot(a.blc, type = "l", ylim = c(0, 25000))
a <- ch.update("D:/V70.dat", 11.8970, 12.2007, 46.666, m = 12.431, T)



