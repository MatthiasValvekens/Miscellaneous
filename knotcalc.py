from copy import deepcopy
from ast import literal_eval
from knotdraw import save_link
def lget(list,index,default=0):
	if index<0 or index>=len(list): return default
	else: return list[index]
class LaurentPolynomial(object):
	def __init__(self,coeff): #coeff centered around 0
		self.coeff=coeff	
	def trim(self):
		for i in range(len(self.coeff)//2):
			if(self.coeff[i]!=0 or self.coeff[len(self.coeff)-1-i]!=0):
				break
		return LaurentPolynomial(self.coeff[i:len(self.coeff)-i])
	def __str__(self):
		extr=len(self.coeff)//2
		res=''
		for i in range(-extr,extr+1):
			c=self.coeff[i+extr]
			part=''
			cstr='+'
			if c==-1 and i!=0:
				cstr='-'
			if abs(c)!=1 or i==0:
				cstr=str(c)
			expstr=str(i)
			if i!=-extr and (c>1 or i==0):
				cstr='+'+cstr
			if i<0:
				expstr='('+expstr+')'
			if i==0:
				part=cstr
			elif i==1:
				part=cstr+'A'
			else: part=cstr+'A^'+expstr
			if c!=0: res+=part
		return res

	def __add__(self,other):
		extr1=len(self.coeff)//2
		extr2=len(other.coeff)//2
		extrres=max(extr1,extr2)
		res=LaurentPolynomial([0]*(2*extrres+1))
		for i in range(-extrres,extrres+1):
			res.coeff[i+extrres]=lget(self.coeff,i+extr1)+lget(other.coeff,i+extr2)
		return res.trim()
	#multiply by a**p
	def amul(self,p):
		extr=len(self.coeff)//2
		nextr=extr+abs(p)
		res=LaurentPolynomial([0]*(2*nextr+1))
		for i in range(-extr,extr+1):
			res.coeff[i+p+nextr]=self.coeff[i+extr]
		return res.trim()
	def __neg__(self):
		res=LaurentPolynomial([0]*len(self.coeff))
		for i in range(len(self.coeff)):
			res.coeff[i]=-self.coeff[i]
		return res

	def __sub__(self,other):
		return self.__add__(other.__neg__())
class Link(object):
	#0  1
	# 
	#2  3
	#len(joins)==len(crossings)
	#array of four-tuples that records where strands are attached
	#joins(5)=((3,3),(2,0),(4,1),(6,0)) means crossing 5 is attached to crossings 3,2,4,5, at the strands specified
	#basic non-binding consistency check implemented
	def __init__(self,crossings,joins,name="nameless"):
		self.crossings=crossings #crossings: -1 (right strand up) or 1 (left strand up) (0 means a crossing has been deleted and can safely be ignored)
		self.joins=joins
		self.name=name
		self.is_consistent()
	def is_consistent(self):
		if len(self.joins) != len(self.crossings):
			print("Lengths of joins and crossings do not match in knot "+self.name)
		for c,j in enumerate(self.joins):
			if j!=0:
				for i in range(4):
					otherjoin=self.joins[j[i][0]]
					if otherjoin==0:
						print("Null reference at "+str((c,i))+" in knot "+self.name)
						return False
					if otherjoin[j[i][1]][0]!=c or otherjoin[j[i][1]][1]!=i:
						print("Inconsistency at "+str((c,i))+" in knot "+self.name)
						return False
		return True
	#perform a skein fork at crossing c and return two child knots
	#The fork that should be multiplied by A in the Kauffman bracket calculation is returned first
	def skein_fork(self,c,crosspos=None,canvlen=10):
		if sum(abs(i) for i in self.crossings)==1: return Link([],[],"unknot"),Link([],[],"unknot")
		
		
		#We generate two forks, order is determined by whether the crossing is over/under
		#F1: connects strands 1-3 and 0-2
		#F2: connects strands 0-1 and 2-3
		#sign 1: F1,F2 returned
		#sign -1: F2,F1 returned
		j=self.joins[c]
		if j==0:
			return self,self
		cross0=j[0][0]
		cross1=j[1][0]
		cross2=j[2][0]
		cross3=j[3][0]
		othstr0=j[0][1]
		othstr1=j[1][1]
		othstr2=j[2][1]
		othstr3=j[3][1]
		F1=Link(deepcopy(self.crossings),deepcopy(self.joins),self.name+'1')
		F1.crossings[c]=0
		if self.iseight(c):
			F1.joins[c]=0
			return F1,None
		F2=Link(deepcopy(self.crossings),deepcopy(self.joins),self.name+'2')
		F2.crossings[c]=0
		
		#print('DebugF1:',F1.joins,cross3,cross1,c,self.crossings,j)
		loop=self.isloop(c)
		
			 
		#connect 1 and 3
		
		F1.joins[cross1][othstr1]=j[3]
		F1.joins[cross3][othstr3]=j[1]
		#connect 0 and 2
	
		
		F1.joins[cross0][othstr0]=j[2]
		F1.joins[cross2][othstr2]=j[0]
	#print('DebugF2:',F2.joins,cross2,cross3,c,self.crossings,j)
		
		#connect 0 and 1
		
		F2.joins[cross1][othstr1]=j[0]
		F2.joins[cross0][othstr0]=j[1]
		#connect 2 and 3
		F2.joins[cross3][othstr3]=j[2]
		F2.joins[cross2][othstr2]=j[3]
		if loop is not None:
			
			if loop==(0,2):
				F2.joins[cross1][othstr1]=j[3]
				F2.joins[cross3][othstr3]=j[1]
			if loop==(1,3):
				F2.joins[cross0][othstr0]=j[2]
				F2.joins[cross2][othstr2]=j[0]
			if loop==(2,3):
				F1.joins[cross0][othstr0]=j[1]
				F1.joins[cross1][othstr1]=j[0]
			if loop==(0,1):
				F1.joins[cross2][othstr2]=j[3]
				F1.joins[cross3][othstr3]=j[2]
		F1.joins[c]=0
		F2.joins[c]=0
		pfork,nfork=((F1,F2) if self.crossings[c]==1 else (F2,F1))
		if crosspos is not None:
			ltext=' loop untwisted' if loop is not None else ''
			save_link(pfork,crosspos,canvlen,'A-branch'+ltext)
			save_link(nfork,crosspos,canvlen,'A^(-1)-branch'+ltext)
		return pfork,nfork
	def isloop(self,c): #make this method return the looped strands
		for i in range(len(self.joins[c])):
			k=self.joins[c][i]
			if k[0]==c:
				return (i,k[1]) if i<=k[1] else (k[1],i)
		return None
	def iseight(self,c):
		j=self.joins[c]
		return j==[(c,1),(c,0),(c,3),(c,2)] or j==[(c,2),(c,3),(c,0),(c,1)]
	def __str__(self):
		return str(self.crossings)+'\n'+str(self.joins)
	def kauffman(self,crosspos=None,canvlen=10):
		#use skein relations to compute the Kauffman bracket.
		res=LaurentPolynomial([1])
		nonzero=[i for i in range(len(self.crossings)) if self.crossings[i]!=0]
		if len(nonzero)!=0:
			c=nonzero[0]
			if self.crossings[c]!=0:
				#detect unknot on branch
				
				#BUG: figure eight-shaped unknot is not processed correctly
				loop=self.isloop(c)

				pfork,nfork=self.skein_fork(c,crosspos,canvlen)
				pkauf=pfork.kauffman(crosspos,canvlen)
				nkauf=nfork.kauffman(crosspos,canvlen)
				#print(str(self))
				# if self.iseight(c) and len(nonzero)>1:
					# sn=-1
					# if (loop==(0,1) and self.crossings[c]==-1) or (loop==(0,2) and self.crossings[c]==1):
						# sn=1
					# return (pkauf.amul(-2+sn*3)+pkauf.amul(2+sn*3))
					
				
				if loop is not None:
					#overtwist: -A**3
					#undertwist: -A**(-3)
					if (self.crossings[c]==-1 and (loop==(2,3) or loop==(0,1))) or (self.crossings[c]==1 and (loop==(0,2) or loop==(1,3))):
						#print(self.name+'\t'+str(-pkauf.amul(3)))
						return -pkauf.amul(3)
					else:
						#print(self.name+'\t'+str(-pkauf.amul(-3)))
						return -pkauf.amul(-3)
				else:
					#print(self.name+'\t'+str(pkauf.amul(1)+nkauf.amul(-1)))
					return (pkauf.amul(1)+nkauf.amul(-1))
		return res
def read_link(fname):
	joins=[]
	crossings=[]
	with open(fname,'r') as infile:
		for line in infile:
			if ':' not in line or '[' not in line: continue
			spl=line.split(':')
			crossings.append(-1 if ('-' in spl[0]) else 1)
			joins.append(literal_eval(spl[1].strip()))
	return crossings,joins