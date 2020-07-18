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
    def __init__(self, master, xmin, xmax, ymin, ymax, cellSize, *args, **kwargs):
      self.xmin = xmin
      self.ymin = ymin
      cellsHigh = ymax - ymin + 1
      cellsWide = xmax - xmin + 1
      Canvas.__init__(self, master, width = cellSize * cellsWide , height = cellSize * cellsHigh, *args, **kwargs)
    
      self.cellSize = cellSize
    
      self.grid = []
      for row in range(ymin,ymax+1):
      
      line = []
      for column in range(xmin,xmax+1):
        line.append(Cell(self, column - self.xmin, row - self.ymin, cellSize, get_colour(column, row)))
        
      self.grid.append(line)
    
      self.bind("<Button-1>", self.handleMouseClick)  
    
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
      print(column, row)
      exit()
      
      
if __name__ == "__main__" :
    app = Tk()

    xmin = ymin = float('inf')
    xmax = ymax = float('-inf')
    for line in sys.stdin:
      x,y,z = line.strip().split()
      x,y = int(x), int(y)
      xmin, xmax = min(x,xmin), max(x,xmax)
      ymin, ymax = min(y,ymin), max(y,ymax)
      colourMap[(x,y)] = z
  
    grid = CellGrid(app, xmin, xmax, ymin, ymax, 7)
    grid.pack()
  
    app.mainloop()
