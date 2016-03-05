
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
  file.new.name <- sprintf("%s%s.dat", strsplit(file.src, "\\.")[[1]][1], sample(100, 1))
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
    x = seq(0, pi, length.out = length(mes.data.target))
    p = sin(x)*FA.df$m[i]+1; #plot(p, type = "l")
    
    mes.data <- c(mes.data[0:(start-1)],
                  mes.data.target*p,
                  mes.data[(end+1):length(mes.data)])
  }
  sim.k.start = sample(length(mes.data)/2,1)+length(mes.data)/4
  k = rep(0,length(mes.data))
  k[sim.k.start:length(k)] = seq(1, sample(10,1)/10*median(mes.data),
                                 length.out = length(mes.data)-sim.k.start)
  mes.data = mes.data+k
  mes.data = mes.data[-c(1:sample(1000,1))]
  writeBin(as.integer(mes.data*0.4), file.dst)
  plot(mes.data, type = "l", ylim = c(0, mean(mes.data)/2))
  # final padding
  #seek(file.src,0x88 + num.of.records * 4,origin='start')
  close(file.src)
  #return(mes.data)
}


a = data.frame(start = 12.8200, end = 13.5667, m = (sample(500, 1)-250)/1001)  #C12
a = rbind(a, c(start = 17.1833, end = 17.4733, m = sample(100, 1)/120))#C14
a = rbind(a, c(start = 21.7800, end = 22.5833, m = sample(120, 1)/120))#C16
a = rbind(a, c(start = 22.7900, end = 23.1233, m = sample(100, 1)/100))#C16:1
a = rbind(a, c(start = 26.3233, end = 26.7967, m = sample(140, 1)/100))#C18:2n6t
a = rbind(a, c(start = 27.0900, end = 27.9167, m = sample(150, 1)/100))#C18:2n6c
a = rbind(a, c(start = 28.5033, end = 29.2667, m = sample(160, 1)/100))#C18:3n6
a = rbind(a, c(start = 30.1767, end = 30.5033, m = sample(120, 1)/100))#C20:1
a = rbind(a, c(start = 32.7420, end = 33.1900, m = sample(120, 1)/100))#C20:3n6
a = rbind(a, c(start = 33.5693, end = 33.9140, m = sample(120, 1)/120))#C22:0
a = rbind(a, c(start = 34.1633, end = 34.5267, m = sample(120, 1)/120))#C20:5n3
a = rbind(a, c(start = 40.2207, end = 40.6347, m = sample(120, 1)/120))#C22:6n3
ch.gen("D:/V.dat", a, 46.666)
