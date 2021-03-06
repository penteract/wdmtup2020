import sys
from tkinter import *

colourdict = \
  { '0' : "white"
  , '1' : "black"
  , '2' : "red"
  , '3' : "green"
  , '4' : "blue"
  , '5' : "cyan"
  , '6' : "yellow"
  , '7' : "magenta"
  }

def getColour(c='0'):
  return colourdict.get(c, colourdict['0'])

colourMap = dict()
annotations = []
grid = None

def get_colour(x, y):
  z = colourMap.get((x,y), None)
  return getColour(z) if z is not None else getColour()

class Cell():
  FILLED_COLOR_BORDER = "green"
  EMPTY_COLOR_BORDER = "black"

  def __init__(self, master, x, y, size, colour):
     """ Constructor of the object called by Cell(...) """
     self.master = master
     self.abs = x
     self.ord = y
     self.size= size
     self.fill= colour

  def draw(self):
      """ order to the cell to draw its representation on the canvas """
      if self.master != None :
        outline = Cell.FILLED_COLOR_BORDER if self.fill else Cell.EMPTY_COLOR_BORDER

        xmin = self.abs * self.size
        xmax = xmin + self.size
        ymin = self.ord * self.size
        ymax = ymin + self.size

        self.master.create_rectangle(xmin, ymin, xmax, ymax, fill = self.fill, outline = outline)

class CellGrid(Canvas):
    def __init__(self, master, xmin, xmax, ymin, ymax, cellSize, xscrollbar, yscrollbar, *args, **kwargs):
      self.xmin = xmin
      self.ymin = ymin
      cellsHigh = ymax - ymin + 1
      cellsWide = xmax - xmin + 1
      Canvas.__init__(self, master, width = cellSize * cellsWide , height = cellSize * cellsHigh, xscrollcommand = xscrollbar.set, yscrollcommand = yscrollbar.set, *args, **kwargs)
      xscrollbar.config( command = self.xview )
      yscrollbar.config( command = self.yview )

      self.cellSize = cellSize

      self.grid = []
      for row in range(ymin,ymax+1):
        line = []
        for column in range(xmin,xmax+1):
          line.append(Cell(self, column - self.xmin, row - self.ymin, cellSize, get_colour(column, row)))
        self.grid.append(line)

      self.bind("<Button-1>", self.handleMouseClick)
      self.bind("<Motion>", self.drawAnnotation)
      
      self.draw()



    def draw(self):
      for row in self.grid:
        for cell in row:
          cell.draw()

    def _eventCoords(self, event):
      row = int(event.y / self.cellSize) + self.ymin
      column = int(event.x / self.cellSize) + self.xmin
      return row, column

    def handleMouseClick(self, event):
      row, column = self._eventCoords(event)
      print(column, row, flush=True)
      refresh()
    
    def drawAnnotation(self, event):
      row, column = self._eventCoords(event)
      for [x,y,w,h,s] in annotations:
        if x <= column and x+w > column and y <= row and y+h > row:
          #self.create_text(event.x, event.y, text=s)
          #print(s, file=sys.stderr, flush=True)
          numberOutput.set(s)
          break
      else:
        numberOutput.set("")

def command(text):
  def cmd():
    print(text, flush=True)
    refresh()
  return cmd

def refresh():
    global annotations
    global colourMap
    global grid
    annotations = []
    colourMap = dict()
    xmin = ymin = float('inf')
    xmax = ymax = float('-inf')
    for line in sys.stdin:
      xs = line.strip().split()
      if len(xs) == 0:
        break
      elif len(xs) == 5:
        x,y,w,h,s = xs
        annotations.append([int(x), int(y), int(w), int(h), s])
      else:
        x,y,z = xs
        x,y = int(x), int(y)
        xmin, xmax = min(x,xmin), max(x,xmax)
        ymin, ymax = min(y,ymin), max(y,ymax)
        colourMap[(x,y)] = z
    
    if xmin == float('inf'): #There are no points
      print("No data received in UI process. Terminating.")
      exit()

    oldGrid = grid
    grid = CellGrid(app, xmin, xmax, ymin, ymax, max(1,min(int(1800/(xmax-xmin)),int(1000/(ymax-ymin)))), xscrollbar, yscrollbar)
    if oldGrid:
      oldGrid.destroy()
    grid.pack()


if __name__ == "__main__" :
    app = Tk()
    
    xscrollbar = Scrollbar(app , orient = HORIZONTAL)
    xscrollbar.pack( side = BOTTOM, fill = X )
    yscrollbar = Scrollbar(app)
    yscrollbar.pack( side = RIGHT, fill = Y )

    f = Frame(app)
    f.pack(side=LEFT)
    Button(f, text="Back", command = command("b")).pack(side = TOP)
    Button(f, text="Reset", command = command("g")).pack(side = TOP)
    Button(f, text="Text UI", command = command("t")).pack(side = TOP)
    numberOutput = StringVar()
    Label(app, textvariable=numberOutput).pack(side = TOP)
    
    refresh()

    app.mainloop()
