"""Polygon splitting."""

from typing import List
import py5

def setup() -> None:
  """Set basic properties."""
  py5.size(1000, 1000, py5.P2D)
  py5.smooth(8)
  py5.color_mode(py5.HSB, 360, 100, 100)
  py5.rect_mode(py5.CENTER)
  py5.no_stroke()
  py5.frame_rate(144)
  py5.stroke_weight(2)
  global width_center
  global height_center
  global rect_width
  global rect_height
  global rect_edges
  global rect_midpoint
  global recursions
  global variance

  # Variables.
  height_center = py5.height / 2
  width_center = py5.width / 2
  rect_width = 100
  rect_height = 100
  rect_edges = 4
  rect_midpoint = 100 / 2
  recursions = 4
  variance = 50

class Point:
  x: int
  y: int

  def __init__(self, x, y):
    self.x = x
    self.y = y

  def get_cords(self):
    """Returns a tuple containing the coordinates of all triangle points."""
    return (self.x, self.y)

class Triangle:
  peak_a: Point
  base_b: Point
  base_c: Point

  def __init__(self, base_b, base_c, peak_a):
    self.base_b = base_b
    self.base_c = base_c
    self.peak_a = peak_a

  def get_vertices(self):
    """Returns a tuple containing the coordinates of all triangle points."""
    return (
        self.base_b.x,
        self.base_b.y,
        self.base_c.x,
        self.base_c.y,
        self.peak_a.x,
        self.peak_a.y,
    )

def split_recursion(
    base_b: Point,
    base_c: Point,
    length: int,
    variability_range: int,
    depth=0,
    max_depth=3,
) -> List[Triangle]:
  """
    This function recursively splits a triangle and returns a list of all triangles formed.
    Args:
        base_b: The first base point of the triangle.
        base_c: The second base point of the triangle.
        length: The length to add to the peak_a point during splitting.
        variability: The amount of y offset of the triangle peak. From 0 to passed value.
        depth: Current depth of recursion (internal use).
        max_depth: Maximum depth of recursion (optional).
    Returns:
        A list of all triangles formed by splitting the original triangle recursively.
    """
  if depth >= max_depth:
    return [Triangle(
        base_b,
        base_c,
        Point(((base_b.x + base_c.x) / 2), (base_b.y + base_c.y) / 2),
    )]
  # TODO: Im moving the x by the length, but on a triangle that is no longer perpendicular movement.
  # This causes the entire thing to not "grow"
  peak_a = Point(
      ((base_b.x + base_c.x) / 2) + length,
      ((base_b.y + base_c.y) / 2),
  )
  triangles = []
  triangles.append(Triangle(base_b, base_c, peak_a))  # Add the original triangle
  triangles.extend(
      split_recursion(base_b, peak_a, length, variability_range, depth + 1, max_depth)
  )  # Recursively split left child
  triangles.extend(
      split_recursion(peak_a, base_c, length, variability_range, depth + 1, max_depth)
  )  # Recursively split right child
  return triangles

def draw() -> None:
  py5.background(0)
  py5.translate(width_center, height_center)
  variance = py5.mouse_y
  for rect_edge in range(360//10):
    py5.rotate(py5.radians(10))
    triangles = split_recursion(
        Point(rect_midpoint, rect_midpoint),
        Point(rect_midpoint, -rect_midpoint),
        py5.mouse_x,
        variance,
        0,
        recursions,
    )
    i = 0
    for triangle in triangles:
      py5.fill(i, 100, 100, 90)
      py5.triangle(*triangle.get_vertices())
      i += 360 // len(triangles)

py5.run_sketch()
