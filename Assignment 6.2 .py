n=int(input("Enter the number:")) ; 
def AddOdd(n):
    sum=0
    for i in range(1,n+1):
        if(i%2!=0):
         sum+=i
    return sum ; 

print(f"The sum of first {n} odd numbers {AddOdd(n)}") ; 
