library(gtools)

x5 = permutations(13,5)
ans1 = list()
for (i in 1:nrow(x5)){
  a = x5[i, ]
  
  
  p1 = a[1] / (a[2] + a[3]);
  p2 = (a[2] + a[3]) / (a[3] + a[4] + a[5]);
  if(p1 >= 1 || p1 != p2)
    next
  
  s = length(ans1)
  ans1[[s+1]] = a
  
}



x6 = permutations(13,6)
ans2 = list()
for(i in 1:nrow(x6)){
  a = x6[i, ]
  i5 = 1
  i6 = 2
  i7 = 3
  i8 = 4
  i9 = 5
  i10 = 6
  
  p3 = (a[i6]) / (a[i6] + a[i7]);
  p4 = (a[i6] + a[i7]) / (a[i7] + a[i8] + a[i9]);
  
  if(p3 >= 1 || p3 != p4)
    next()
  
  p5 = (a[i7] + a[i8] + a[i9]) / (a[i5] + a[i9] + a[i10]);
  if(p3 != p5)
    next
  

  s = length(ans2)
  ans2[[s+1]] = a
}

length(ans2)


x4 = permutations(13,4)
ans3 = list()
for(i in 1:nrow(x4)){
  a = x4[i,]
  
  i10 = 1
  i11 = 2
  i12 = 3
  i13 = 4
  
  p6 = (a[i11] + a[i12]) / (a[i12] + a[i13]);
  p7 = (a[i12] + a[i13]) / (a[i13] + a[i10]);
  
  if(p6 >= 1 || p6 != p7)
    next
  
  s = length(ans3)
  ans3[[s+1]] = a
}


length(ans3)


res = 13:1
for (i in ans1){
  for(j in ans2){
    for (k in ans3){

      if(i[5] != j[1] | j[6] != k[1])
        next
      jj = j[2:6]
      kk = k[2:4]
      tmp = c(i, jj, kk)
      found = TRUE
      for (t0 in 1:13){
        for(t1 in 1:13){
          if(t0 == t1)
            next
          if(tmp[t0] == tmp[t1])
            found = FALSE
          if(!found)
            break
          
        }
        if(!found)
          break
      }
      if(found)
        res = tmp

    }

  }

}



print(res)