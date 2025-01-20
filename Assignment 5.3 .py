n=int(input("Enter the number till which you want to print prime numbers"))
def isPrime(x):
    if(x<2):
     return False ;
    
    for i in range(2,int(x/2)+1):
     if(x%i==0):
      return False ; 
    
    return True ; 
            
        
            
       
sum=0
for i in range(2,n+1):
    if(isPrime(i)):
        sum+=i

print(sum) ; 
