"""Trying to learn python."""
import py5

def setup() -> None:
 """Set basic properties."""
 py5.size(1000, 1000, py5.P2D)
 py5.smooth(8)
 py5.color_mode(py5.HSB, 360, 100, 100)
 py5.frame_rate(144)

def left_edge() -> int:
 """Lowest visible width value if x=0, y=0 is center."""
 return (0 - py5.width) // 2

def bottom_edge() -> int:
 """Lowest visible height value if x=0, y=0 is center."""
 return (0 - py5.height) // 2
STEP: float = 50

def draw() -> None:
 """Draw loop."""
 width_center = py5.width // 2
 print(width_center)
 py5.background(360, 0, 100)
 py5.translate(py5.width / 2, py5.height / 2)
 py5.scale(1.0, -1.0)
 py5.stroke_weight(2)
 for x in range(left_edge(), (py5.width // 2) + STEP, STEP):
  for y in range(bottom_edge(), (py5.height // 2) + STEP, STEP):
   py5.line(0, y, x, -py5.mouse_y - bottom_edge())

# py5.push_matrix()
# py5.pop_matrix()

py5.run_sketch()
