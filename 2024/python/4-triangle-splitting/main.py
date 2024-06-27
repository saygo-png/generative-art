"""Polygon splitting."""

from typing import List

import numpy as np
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
  global seed_triangle
  global recursions
  global variance
  global peak_length

  # Variables.
  height_center = py5.height // 2
  width_center = py5.width // 2
  seed_triangle = Triangle(
    Point(-160, 100),
    Point(160, 100),
    Point(0, -170),
  )
  peak_length = 100
  recursions = 5
  variance = 0

class Point:
  x: int
  y: int

  def __init__(self, x: int, y: int) -> None:
    self.x = x
    self.y = y

  def get(self) -> tuple[int, int]:
    """Returns a tuple containing the coordinates of all triangle points."""
    return (self.x, self.y)

class Line:
  a: Point
  b: Point

  def __init__(self, a: Point, b: Point) -> None:
    self.a = a
    self.b = b

  def get(self) -> tuple[int, int, int, int]:
    """Returns two Points containing the coordinates of a line."""
    return (self.a.x, self.a.y, self.b.x, self.b.y)

class Triangle:
  base_b: Point
  base_c: Point
  peak_a: Point

  def __init__(self, base_b: Point, base_c: Point, peak_a: Point) -> None:
    self.base_b = base_b
    self.base_c = base_c
    self.peak_a = peak_a

  def get(self) -> tuple[int, int, int, int, int, int]:
    """Returns a tuple containing the coordinates of all triangle points."""
    return (
      self.base_b.x,
      self.base_b.y,
      self.base_c.x,
      self.base_c.y,
      self.peak_a.x,
      self.peak_a.y,
    )

def midpoint(a: Point, b: Point) -> Point:
  return Point((a.x + b.x) // 2, (a.y + b.y) // 2)

def normal(a: Point, b: Point) -> Point:
  ortho = np.array((b.y - a.y, a.x - b.x))
  return Point(*(ortho // np.linalg.norm(ortho)))

def perpendicular_line(a: Point, b: Point, length: float) -> Line:
  midpoint_ = midpoint(a, b)
  normal_ = normal(a, b)
  scaled_norm = Point(normal_.x * length, normal_.y * length)
  endpoint = Point(midpoint_.x + scaled_norm.x, midpoint_.y + scaled_norm.y)
  return Line(midpoint_, endpoint)

def split_recursion(
  a: Point,
  b: Point,
  length: int,
  variability_range: int,
  depth: int = 0,
  max_depth: int = 3,
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

  line = perpendicular_line(a, b, length)
  midpoint = line.b

  if depth >= max_depth:
    return [Triangle(
      a,
      b,
      midpoint,
    )]

  triangles = []
  triangles.append(Triangle(a, b, midpoint))  # Add the original triangle

  triangles.extend(
    split_recursion(a, midpoint, length, variability_range, depth + 1, max_depth)
  )  # Recursively split left child
  triangles.extend(
    split_recursion(midpoint, b, length, variability_range, depth + 1, max_depth)
  )  # Recursively split right child

  return triangles

def draw() -> None:
  peak_length = py5.mouse_y / 6
  py5.background(20)
  py5.translate(width_center, height_center)

  # Split each edge, put in into one list
  left_edge = split_recursion(seed_triangle.base_b, seed_triangle.peak_a, peak_length, variance, 0, recursions)
  right_edge = split_recursion(seed_triangle.peak_a, seed_triangle.base_c, peak_length, variance, 0, recursions)
  bottom_edge = split_recursion(seed_triangle.base_c, seed_triangle.base_b, peak_length, variance, 0, recursions)
  triangles = left_edge + right_edge + bottom_edge

  # Draw each triangle
  i = 0
  for triangle in triangles:
    py5.fill(i, 80, 100, 70)
    py5.stroke(i, 80, 80)
    py5.triangle(*triangle.get())
    i += 1
    if i > 181:
      i = 0
    print(i)

  py5.fill(255)
  # py5.triangle(*seed_triangle.get())

py5.run_sketch()
