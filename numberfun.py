
''' Probabilistically solve the congruence x**2 = a (mod p).
The probability that no solution is found after n iterations is given by 2**(-n).
The solution reported is always correct. If the algorithm is certain that no solution exists, 0 is reported.
'''

def modsqroot(a,p,n=100):
	if p%4==2 or p%4==0: return 0
	if p%4==3:
		b=pow(a,(p+1)//4,p)
		#Euler's criterion says this is a solution if and only if a solution exists
		if pow(b,2,p)==a:
			return b
		else:
			return a
	
