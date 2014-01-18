import sys
from tkinter import Tk

#Convert tenji (Japanese braille-like script) to romaji (mostly) in accordance with the Nihonshiki standard
#I'm not too big a fan of Nihonshiki, but the logic behind it lines up well with the tenji system,
#making the task of implementing a transcriber considerably simpler and more elegant.

#Present input in following format: (example: shi shi)
#._._
#....
#_._.
#Underscores may be freely replaced by spaces.
#Input is taken from clipboard if no input file is specified.
#Input is read in chuncks of 3 lines each. One chunk corresponds to one line in tenji.

updict={
    '.__':'a',
    '._.':'i',
    '.._':'u',
    '...':'e',
    '_..':'o',
    '_._':'y',
    '___':'w'
}
downdict={ #normal, dakuten, handakuten
    '___':('','',''),
    '__.':('k','g','g'),
    '._.':('s','z','z'),
    '.._':('t','d','d'),
    '_._':('n','n','n'),
    '_..':('h','b','p'),
    '...':('m','m','m'),
    '.__':('r','r','r')
}
wydict={ #wouldn't work for wi and we. Assuming those are never encountered.
    '_._':'a',
    '_..':'u',
    '.._':'o'
}

def chunks(l, n):
    return [l[i:i+n] for i in range(0, len(l), n)]

def readline(symbols):
    mod=0
    times=1
    res=''
    infix=''
    for s in symbols:
        up=s[:3]
        down=s[3:]
        try:
            if s=='___.__':
                mod=1
            elif s=='_____.':
                mod=2
            elif s=='__.___':
                times=2
            elif s=='__..__':
                res+='-'
            elif s=='___...':
                res+='n'
            elif s=='_.____':
                infix='y'
            elif updict[up]=='y' or updict[up]=='w':
                res+=updict[up]+wydict[s[3:]]
            else:
                res+=times*downdict[down][mod]+infix+updict[up]
                times=1
                mod=0
                infix=''
        except KeyError:
            res+='?'
            times=1
            infix=''
            mod=0
    return res
txt=''
if len(sys.argv)<2:
    r= Tk()
    r.withdraw()
    txt=r.clipboard_get()
else:
    with open(sys.argv[1],'r') as f:
        txt=f.read()
lines=txt.split('\n')
for ls in chunks(lines,3):
    print(readline([(a+b+c).replace(' ','_') for a,b,c in zip(chunks(ls[0],2),chunks(ls[1],2),chunks(ls[2],2))]))
