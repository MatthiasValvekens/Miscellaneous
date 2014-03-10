from matplotlib import pyplot as plt
from ast import literal_eval
def cell2axes(cx,cy,canvlen,strand=2):
	cellen=1/canvlen
	if strand==3 or strand==1:
		cx+=1
	if strand==0 or strand==1:
		cy+=1
	return cellen*cx,cellen*cy

def drawcrossing(ax,cx,cy,canvlen,updown,clabel=None):
	cellen=1/canvlen
	if updown==1:
		ax.plot([cx*cellen,(cx+1)*cellen],[cy*cellen,(cy+1)*cellen],'k-',lw=2,transform=ax.transAxes)
		ax.plot([cx*cellen,(cx+0.4)*cellen],[(cy+1)*cellen,(cy+0.6)*cellen],'k-',lw=2,transform=ax.transAxes)
		ax.plot([(cx+0.6)*cellen,(cx+1)*cellen],[(cy+0.4)*cellen,cy*cellen],'k-',lw=2,transform=ax.transAxes)
	elif updown==-1:
		ax.plot([cx*cellen,(cx+1)*cellen],[(cy+1)*cellen,cy*cellen],'k-',lw=2,transform=ax.transAxes)
		ax.plot([cx*cellen,(cx+0.4)*cellen],[cy*cellen,(cy+0.4)*cellen],'k-',lw=2,transform=ax.transAxes)
		ax.plot([(cx+0.6)*cellen,(cx+1)*cellen],[(cy+0.6)*cellen,(cy+1)*cellen],'k-',lw=2,transform=ax.transAxes)
	if clabel is not None:
		ax.text(cx*cellen,(cy+0.5)*cellen,clabel,transform=ax.transAxes)
def drawlink(ax,crossings,joins,crosspos,canvlen=10,cnumber=False):
	for c,pos in zip(enumerate(crossings),crosspos):
		if cnumber:
			drawcrossing(ax,pos[0],pos[1],canvlen,c[1],str(c[0]))
		else:
			drawcrossing(ax,pos[0],pos[1],canvlen,c[1])
	#we're drawing some joins twice, but who cares
	for c,j in enumerate(joins):
		if j!=0:
			for strand in range(4):
				startx,starty=cell2axes(crosspos[c][0],crosspos[c][1],canvlen,strand)
				endx,endy=cell2axes(crosspos[j[strand][0]][0],crosspos[j[strand][0]][1],canvlen,j[strand][1])
				ax.plot([startx,endx],[starty,endy],'r-',lw=2,transform=ax.transAxes)
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
def save_link(link,crosspos,canvlen=10,comment=None,cnumber=False):
	fig=plt.figure()
	ax=fig.add_subplot(111,adjustable='box',aspect=1.0)
	ax.set_axis_off()
	drawlink(ax,link.crossings,link.joins,crosspos,canvlen,cnumber)
	if comment is not None:
		ax.text(0,1,comment,transform=ax.transAxes)
	plt.savefig(link.name+'.png',bbox_inches='tight')
	fig.clf()
	plt.close()
if __name__=='__main__':
	fig=plt.figure()
	ax=fig.add_subplot(111,adjustable='box',aspect=1.0)
	kinoterac,kinoteraj=read_link('kinotera.txt')
	kinoterap=[(1,14),(6,13),(13,13),(3,12),(9,10),(3,10),(5,7),(3,4),(10,4),(2,2),(10,2)]
	drawlink(ax,kinoterac,kinoteraj,kinoterap,canvlen=16)
	plt.show()
