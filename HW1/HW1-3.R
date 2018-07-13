n = 16L

a = integer(n)

#1 liar 
#2 truthful

for (k in 0:3){
  a[1] = as.integer(k %% 2) + 1
  a[2] = as.integer(k / 2) + 1
  
  found = TRUE
  
  for (i in 2:16){

    if(a[i - 1] == 1L & a[i] != 2L){
      found = FALSE
      break
    }
    if(a[i - 1] == 2L & a[i] != 1L){
      found = FALSE
      break
    }
    a[i + 1] = a[i - 1]
    
  }
  if (found)
    break
}

ans = 0
for (i in a){
  if(i == 1)
    ans = ans + 1
}

print(ans)