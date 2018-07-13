n = 8L

a = integer(n)

#1 liar 
#2 truthful

found = TRUE
for (i0 in 1:n){
  for (i1 in 1:n){
    if(i0 == i1)
      next
    for(i2 in 1:n){
      if(i2 == i1 || i2 == i0)
        next
      for(i3 in 1:n){
        if(i3 == i2 || i3 == i1 || i3 == i0)
          next
        for(k in 0:15){
          
          
          a[1] = as.integer(k %% 2) + 1
          a[2] = (as.integer(k / 2) %%2) + 1
          a[3] = (as.integer(k / 4) %%2) + 1
          a[4] = (as.integer(k / 8) %%2) + 1
          a[5] = (as.integer(k / 16) %%2) + 1
          a[6] = (as.integer(k / 32) %%2) + 1
          a[7] = (as.integer(k / 64) %%2) + 1
          a[8] = (as.integer(k / 128) %%2) + 1
          
          found = TRUE
          
         
          for (q in 2:7){
            
            
            if(q == i0 || q == i1 || q == i2 || q == i3){
              if(a[q] == 1){
                if(a[q-1] == 1 && a[q+1] == 1)
                  found = FALSE
              }
              else{
                if(a[q-1] != 1 || a[q+1] != 1)
                  found = FALSE
              }
            }
            else{
              if(a[q] == 2){
                if(a[q-1] == a[q + 1])
                  found = FALSE
              }
              else{
                if(a[q-1] != a[q+1])
                  found = FALSE
              }
            }
          }
          
          if(i0 == 1 || i1 == 1 || i2 == 1 || i3 == 1){
            if(a[1] == 1){
              if(a[n] == 1 && a[2] == 1)
                found = FALSE
            }
            else{
              if(a[n] != 1 || a[2] != 1)
                found = FALSE
            }
          }
          else{
            if(a[1] == 2){
              if(a[n] == a[2])
                found = FALSE
            }
            else{
              if(a[n] != a[2])
                found = FALSE
            }
          }
          
          
          
          if(n == i0 || n == i1 || n == i2 || n == i3){
            if(a[n] == 1){
              if(a[n-1] == 1 && a[1] == 1)
                found = FALSE
            }
            else{
              if(a[n-1] != 1 || a[1] != 1)
                found = FALSE
            }
          }
          else{
            if(a[n] == 2){
              if(a[n-1] == a[1])
                found = FALSE
            }
            else{
              if(a[n-1] != a[1])
                found = FALSE
            }
          }
          
          if(found){
            print("their sides are both liar:")
            cat(i0, i1, i2, i3 , "\n")
            break
          }
        }
        if(found)
          break
        
      }
      if(found)
        break
    }
    if(found)
      break
  }
  if(found)
    break
}

ans = 0
for (i in a){
  if(i == 1){
    ans = ans + 1
    print("liar")
  }
  else
    print("truthful")
}

print(ans)