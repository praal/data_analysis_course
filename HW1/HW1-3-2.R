n = 12L

a = integer(n)

#1 liar 
#2 truthful

found = FALSE
for (k in 0:3){
  a[1] = as.integer(k %% 2) + 1
  a[2] = as.integer(k / 2) + 1
  

  
  for (i in 2:(n-1)){
    if(a[i] == 2)
      a[i + 1] = 3 - a[i - 1]
    else
      a[i + 1] = a[i-1]
  }
  
  if(a[n] == 2){
    if(a[1] != a[n-1])
      found = TRUE
  }
  else{
    if(a[1] == a[n-1])
      found = TRUE
  }
}

#print(found)
ans = 0
for (i in a){
  if(i == 1)
    ans = ans + 1

}

print(ans)