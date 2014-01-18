import sys
from tkinter import *

class ClipCycle(Frame):
    def firstentry(self):
        self.index=-1
        self.nextentry()
    def loadstuff(self):
        content=self.master.selection_get(selection = "CLIPBOARD")
        
        if not content in self.sarray: #to prevent accidental clicks
            self.sarray=content.split('\t')
            self.firstentry()
    def nextentry(self):
        self.index+=1
        if self.index==len(self.sarray): self.index=0
        if(self.skipempty.get()):
            while not self.sarray[self.index].strip():
                self.index+=1
                if self.index==len(self.sarray): self.index=0
        self.lbl.set(str(self.index)+': '+self.sarray[self.index])
        self.master.clipboard_clear()
        self.master.clipboard_append(self.sarray[self.index])

    def createWidgets(self):
        self.sarray=['']
        self.index=-1
        self.lbl=StringVar()
        self.lbl.set('')
        self.entrylabel=Entry(self,textvariable=self.lbl)
        self.entrylabel.config(state=DISABLED)
        self.entrylabel.pack()
        self.skipempty=BooleanVar()
        self.skipempty.set(True)
        Button(self,text='Load from clipboard',command=self.loadstuff).pack()
        Button(self,text='Cycle through entries',command=self.nextentry).pack()
        Button(self,text='Jump to start of cycle',command=self.firstentry).pack()
        Checkbutton(self,text='Skip empty cells',variable=self.skipempty).pack()
    def __init__(self,master=None):
        Frame.__init__(self,master)
        self.pack()
        self.createWidgets()


root = Tk()
root.wm_title('Clipboard cycler')
root.wm_resizable(width=False, height=False) 
root.wm_attributes("-topmost", 1)
app = ClipCycle(master=root)
app.mainloop()
