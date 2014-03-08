
from matplotlib import pyplot as plt
from ast import literal_eval
def cell2axes(cx,cy,canvlen,strand=2):
	cellen=1/canvlen
	if strand==3 or strand==1:
		cx+=1
	if strand==0 or strand==1:
		cy+=1
	return cellen*cx,cellen*cy

def drawcrossing(ax,cx,cy,canvlen,updown):
	cellen=1/canvlen
	if updown==1:
		ax.plot([cx*cellen,(cx+1)*cellen],[cy*cellen,(cy+1)*cellen],'k-',lw=2,transform=ax.transAxes)
		ax.plot([cx*cellen,(cx+0.4)*cellen],[(cy+1)*cellen,(cy+0.6)*cellen],'k-',lw=2,transform=ax.transAxes)
		ax.plot([(cx+0.6)*cellen,(cx+1)*cellen],[(cy+0.4)*cellen,cy*cellen],'k-',lw=2,transform=ax.transAxes)
		return
	elif updown==-1:
		ax.plot([cx*cellen,(cx+1)*cellen],[(cy+1)*cellen,cy*cellen],'k-',lw=2,transform=ax.transAxes)
		ax.plot([cx*cellen,(cx+0.4)*cellen],[cy*cellen,(cy+0.4)*cellen],'k-',lw=2,transform=ax.transAxes)
		ax.plot([(cx+0.6)*cellen,(cx+1)*cellen],[(cy+0.6)*cellen,(cy+1)*cellen],'k-',lw=2,transform=ax.transAxes)
		return
def drawlink(ax,crossings,joins,crosspos,canvlen=10):
	for c,pos in zip(crossings,crosspos):
		drawcrossing(ax,pos[0],pos[1],canvlen,c)
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
def save_link(link,crosspos,canvlen=10,comment=None):
	fig=plt.figure()
	ax=fig.add_subplot(111,adjustable='box',aspect=1.0)
	ax.set_axis_off()
	drawlink(ax,link.crossings,link.joins,crosspos,canvlen)
	if comment is not None:
		ax.text(0,1,comment,transform=ax.transAxes)
	plt.savefig(link.name+'.png',bbox_inches='tight')
if __name__=='__main__':
	fig=plt.figure()
	ax=fig.add_subplot(111,adjustable='box',aspect=1.0)
	cinqc,cinqj=read_link('cinquefoil.txt')
	cinqp=[(3,7),(5,7),(1,4),(7,4),(4,2)]
	drawlink(ax,cinqc,cinqj,cinqp)
	plt.show()
