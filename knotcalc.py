from copy import deepcopy
from ast import literal_eval
from knotdraw import save_link
debug=False
inoutdic={0:3,3:0,1:2,2:1}
seendic={}
def joinstotuples(l):
	return tuple((0 if i==0 else tuple(i)) for i in l)
def joinstolists(t):
	return list((0 if i==0 else list(i)) for i in t)
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
		if p==0: return self
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
		self.crossings=tuple(crossings) #crossings: -1 (right strand up) or 1 (left strand up) (0 means a crossing has been deleted and can safely be ignored)
		self.joins=joinstotuples(joins)
		self.name=name
		self.is_consistent()
	def __key__(self):
		return self.crossings,self.joins
	def __hash__(self):
		return hash(self.__key__())
	def __eq__(self,other):
		return self.__key__()==self.__key__()
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
	def skein_fork(self,c,crosspos=None,canvlen=10,cnumber=False):
		if sum(abs(i) for i in self.crossings)==1: return Link([],[],"unknot"),Link([],[],"unknot")
		
		
		#We generate two forks, order is determined by whether the crossing is over/under
		#F1: connects strands 1-3 and 0-2
		#F2: connects strands 0-1 and 2-3
		#sign 1: F1,F2 returned
		#sign -1: F2,F1 returned
		j=self.joins[c]
		if j==0:
			print("You messed up, this should not happen!")
			return self,self
		cross0=j[0][0]
		cross1=j[1][0]
		cross2=j[2][0]
		cross3=j[3][0]
		othstr0=j[0][1]
		othstr1=j[1][1]
		othstr2=j[2][1]
		othstr3=j[3][1]
		f1crossings=list(self.crossings)
		f2crossings=list(self.crossings)
		f1joins=joinstolists(self.joins)
		f2joins=joinstolists(self.joins)
		f1crossings[c]=0
		if self.iseight(c):
			f1joins[c]=0
			return Link(f1crossings,f1joins,self.name+'1'),None
		f2crossings[c]=0
		
		loop=self.isloop(c)
		
			 
		#connect 1 and 3
		
		f1joins[cross1][othstr1]=j[3]
		f1joins[cross3][othstr3]=j[1]
		#connect 0 and 2
	
		
		f1joins[cross0][othstr0]=j[2]
		f1joins[cross2][othstr2]=j[0]
		
		#connect 0 and 1
		
		f2joins[cross1][othstr1]=j[0]
		f2joins[cross0][othstr0]=j[1]
		#connect 2 and 3
		f2joins[cross3][othstr3]=j[2]
		f2joins[cross2][othstr2]=j[3]
		if loop is not None:
			
			if loop==(0,2):
				f2joins[cross1][othstr1]=j[3]
				f2joins[cross3][othstr3]=j[1]
			if loop==(1,3):
				f2joins[cross0][othstr0]=j[2]
				f2joins[cross2][othstr2]=j[0]
			if loop==(2,3):
				f1joins[cross0][othstr0]=j[1]
				f1joins[cross1][othstr1]=j[0]
			if loop==(0,1):
				f1joins[cross2][othstr2]=j[3]
				f1joins[cross3][othstr3]=j[2]
		f1joins[c]=0
		f2joins[c]=0
		F1=Link(f1crossings,f1joins,self.name+'1')
		F2=Link(f2crossings,f2joins,self.name+'2')
		pfork,nfork=((F1,F2) if self.crossings[c]==1 else (F2,F1))
		if crosspos is not None:
			ltext=' loop untwisted' if loop is not None else ''
			save_link(pfork,crosspos,canvlen,'A-branch'+ltext,cnumber)
			save_link(nfork,crosspos,canvlen,'A^(-1)-branch'+ltext,cnumber)
		return pfork,nfork
	def isloop(self,c):
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
	def kauffman(self,crosspos=None,canvlen=10,cnumber=False):
		#use skein relations to compute the Kauffman bracket.
		#check if we know this one already
		res=seendic.get(self)
		if res is not None:
			return res
		res=LaurentPolynomial([1])
		nonzero=[i for i in range(len(self.crossings)) if self.crossings[i]!=0]
		if len(nonzero)!=0:
			c=nonzero[0]
			if self.crossings[c]!=0:
				#detect unknot on branch
				loop=self.isloop(c)

				pfork,nfork=self.skein_fork(c,crosspos,canvlen,cnumber)
				pkauf=pfork.kauffman(crosspos,canvlen,cnumber)
				nkauf=nfork.kauffman(crosspos,canvlen,cnumber)
				if loop is not None:
					#overtwist: -A**3
					#undertwist: -A**(-3)
					if (self.crossings[c]==-1 and (loop==(2,3) or loop==(0,1))) or (self.crossings[c]==1 and (loop==(0,2) or loop==(1,3))):
						if debug: print(self.name+'\t'+str(-pkauf.amul(3)))
						res=-pkauf.amul(3)
					else:
						if debug: print(self.name+'\t'+str(-pkauf.amul(-3)))
						res=-pkauf.amul(-3)
				else:
					if debug: print(self.name+'\t'+str(pkauf.amul(1)+nkauf.amul(-1)))
					res=(pkauf.amul(1)+nkauf.amul(-1))
		seendic[self]=res
		return res
	def writhe(self):
		#This routine doesn't work for links
		if sum(abs(j) for j in self.crossings)==0: return 0
		
		outputs=[[-1,-1] for i in range(len(self.crossings))]
		outputs[0][0]=0
		curcrossing=self.joins[0][0][0]
		instrand=self.joins[0][0][1]
		while curcrossing!=0 or instrand!=3:
			
			outstrand=inoutdic[instrand]
			#print(curcrossing,instrand,outstrand)
			outputs[curcrossing][0 if outputs[curcrossing][0]==-1 else 1]=outstrand
			instrand=self.joins[curcrossing][outstrand][1]
			curcrossing=self.joins[curcrossing][outstrand][0]
			
		signs=[0]*len(self.crossings)
		for c,o in enumerate(outputs):
			o.sort()
			if o==[0,1] or o==[2,3]:
				signs[c]=self.crossings[c]
			else:
				signs[c]=-self.crossings[c]
		return sum(signs)
	def xlpol(self):
		kauff=self.kauffman()
		wr=self.writhe()
		correct_exp=-3*wr
		res=kauff.amul(correct_exp)
		return res if wr%2==0 else -res
			
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
if __name__=='__main__':
	righttrefoil=Link(*read_link('rtrefoil.txt'),name='rtrefoil')
	print('right trefoil:',righttrefoil.xlpol())
	kinotera=Link(*read_link('kinotera.txt'),name='Kinoshita-Terasaka')
	print('Kinoshita-Terasaka: ',kinotera.xlpol())
	conway=Link(*read_link('conway.txt'), name='Conway')
	print('Conway: ',conway.xlpol())