#------------------------------------------------------------
# константы

# 0xC  -- number of measurement records(int32)
# 0x30 -- time stamp (character) in format '%d-%m-%y%H:M.%S'
# 0x88 -- begin of measurement records (32 signed ints)
setwd("E:/Work/Projects/FocusGC/")

#------------------------------------------------------------
# функция для работы с файлами

ch.update <- function(file.src, start, end, m = 1, overwrite = F){
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
  
  # изменяем данные
  if(overwrite == T) {
    mes.data.target = mes.data[start:end]
    
    x = seq(0, pi, by = pi/(length(mes.data.target)-1))
    p = sin(x)*m+1; #plot(p, type = "l")
    
    mes.data <- c(mes.data[0:(start-1)],
                mes.data.target*p,
                mes.data[(end+1):length(mes.data)])
    
    writeBin(as.integer(mes.data*0.4),file.dst)
    
  }
  plot(mes.data, type = "l", ylim = c(0, mean(mes.data)/2))
  # final padding
  #seek(file.src,0x88 + num.of.records * 4,origin='start')
  close(file.src)
  #return(mes.data)
}

#------------------------------------------------------------
# обработка данных a

a <- ch.update("2.dat")
plot(a, type = "l", ylim = c(0, mean(a)/2))

a.target = a[35001:45000]

m = 1.0
x = seq(0, pi, by = pi/(length(a.target)-1))
p = sin(x)*m+1; #plot(p, type = "l")

b <- c(a[0:35000],
       a.target*p,
       a[45001:69954])
plot(b, type = "l", ylim = c(0, mean(a)/2))

