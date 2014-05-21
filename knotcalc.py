from ast import literal_eval

#Connection mappings
inoutdic={0:3,3:0,1:2,2:1}
f1conn={1:3,3:1,0:2,2:0}
f2conn={0:1,1:0,3:2,2:3}
loopdic={(0,2):(1,3),(1,3):(0,2),(0,1):(2,3),(2,3):(0,1)}

seendic={}
def joinstotuples(l):
	return tuple((0 if i==0 else tuple(i)) for i in l)
def joinstolists(t):
	return list((0 if i==0 else list(i)) for i in t)
def lget(list,index,default=0):
	if index<0 or index>=len(list): return default
	else: return list[index]
def connect(targlst,srclst,strand,strandb):
	targlst[srclst[strand][0]]\
	       [srclst[strand][1]]=srclst[strandb]
def connect_all(targlst,srclst,mapping):
	for a,b in mapping.items():
		connect(targlst,srclst,a,b)
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

class LaurentPolynomial(object):
	def __init__(self,coeff):
		self.coeff=coeff
	def trim(self):
		for i in range(len(self.coeff)//2):
			if(self.coeff[i]!=0 or \
				self.coeff[len(self.coeff)-1-i]!=0):
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
			res.coeff[i+extrres]=\
				lget(self.coeff,i+extr1)+lget(other.coeff,i+extr2)
		return res.trim()
	#Multiply by a**p
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
	def __init__(self,crossings,joins):
		self.crossings=tuple(crossings)
		self.joins=joinstotuples(joins)
	def __key__(self):
		return self.crossings,self.joins
	def __hash__(self):
		return hash(self.__key__())
	def __eq__(self,other):
		return self.__key__()==self.__key__()
	#Perform a skein fork at crossing c 
	#and return two child knots.
	#The fork that should be multiplied by A
	#in the Kauffman bracket calculation is returned first
	def skein_fork(self,c):
		if sum(abs(i) for i in self.crossings)==1:
			return Link([],[]),Link([],[])
		
		#We generate two branches, order is 
		#determined by whether the crossing is over/under
		#F1: connects strands 1-3 and 0-2
		#F2: connects strands 0-1 and 2-3
		#overcrossing: F1,F2 returned
		#undercrossing: F2,F1 returned
		
		j=self.joins[c]
		f1crossings=list(self.crossings)
		f2crossings=list(self.crossings)
		f1joins=joinstolists(self.joins)
		f2joins=joinstolists(self.joins)
		f1crossings[c]=0
		f2crossings[c]=0
		if self.iseight(c):
			f1joins[c]=0
			return Link(f1crossings,f1joins),None
		loop=self.isloop(c)
		if loop is not None:
			#Single-looped crossings leave only one option
			#since we ignore unknot diagrams
			toconnect=loopdic[loop]
			connect(f1joins,j,toconnect[0],toconnect[1])
			connect(f1joins,j,toconnect[1],toconnect[0])
			f1joins[c]=0
			res=Link(f1crossings,f1joins)
			return res,None
		#Dissolve the crossing and 
		#reconnect broken strands for both branches
		connect_all(f1joins,j,f1conn)
		connect_all(f2joins,j,f2conn)
		f1joins[c]=0
		f2joins[c]=0
		F1=Link(f1crossings,f1joins)
		F2=Link(f2crossings,f2joins)
		pfork,nfork=((F1,F2) if self.crossings[c]==1 else (F2,F1))
		return pfork,nfork
	def isloop(self,c):
		for i in range(len(self.joins[c])):
			k=self.joins[c][i]
			if k[0]==c:
				return (i,k[1]) if i<=k[1] else (k[1],i)
		return None
	#Determine whether crossing has two handles
	def iseight(self,c):
		j=self.joins[c]
		return j==[(c,1),(c,0),(c,3),(c,2)] or\
				j==[(c,2),(c,3),(c,0),(c,1)]
	def __str__(self):
		return str(self.crossings)+'\n'+str(self.joins)
	def kauffman(self):
		#Keep track of sub-polynomials we already computed.
		res=seendic.get(self)
		if res is not None:
			return res
		res=LaurentPolynomial([1])
		nonzero=[i for i in range(len(self.crossings))\
				if self.crossings[i]!=0]
		if len(nonzero)!=0:
			c=nonzero[0]
			if self.crossings[c]!=0:
				loop=self.isloop(c)

				pfork,nfork=self.skein_fork(c)
				pkauf=pfork.kauffman()
				if loop is not None:
					#Account for R1-moves
					#overtwist: -A**3
					#undertwist: -A**(-3)
					if  (self.crossings[c]==-1 and \
							(loop==(2,3) or loop==(0,1))) \
						or \
						(self.crossings[c]==1 and \
								(loop==(0,2) or loop==(1,3))):
						res=-pkauf.amul(3)
					else:
						res=-pkauf.amul(-3)
				else:
					nkauf=nfork.kauffman()
					res=(pkauf.amul(1)+nkauf.amul(-1))
		seendic[self]=res
		return res
	def writhe(self):
		#Assume we are operating on a knot
		if sum(abs(j) for j in self.crossings)==0: return 0
		
		outputs=[[-1,-1] for i in range(len(self.crossings))]
		outputs[0][0]=0
		curcrossing=self.joins[0][0][0]
		instrand=self.joins[0][0][1]
		while curcrossing!=0 or instrand!=3:
			
			outstrand=inoutdic[instrand]
			outputs[curcrossing][
					0 if outputs[curcrossing][0]==-1 else 1
					]=outstrand
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
