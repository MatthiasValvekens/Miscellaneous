import random
'''
Compute the largest number m such that p**m | n.
If res is true, the n/2**m is returned as well.
'''
def pord(n,p,res=False):
	m=0
	while n%p==0:
		n//=p
		m+=1
	if res:
		return m,n
	else:
		return m
'''
Compute the jacobi symbol of two odd coprime numbers m and n, satisfying n>m>1.

(extra: if n%m==0, the method returns zero)
'''
def jacobi(m,n):
	if n%m==0:
		return 0
	jac=1
	while m>1:
		#switch
		t=n
		n=m
		m=t
		
		jac*=(-1)**(((n-1)//2)*((m-1)//2))
		m%=n
		t,m=pord(m,2,True)
		jac*=(-1)**(t*(n**2-1)//8)
	return jac
