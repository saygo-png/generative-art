"""Polygon splitting with vectors."""

from cmath import sqrt
import numpy as np
import numpy.typing as npt

import py5

def setup() -> None:
  """Set basic properties."""
  py5.size(1000, 1000, py5.P2D)
  py5.smooth(8)
  py5.stroke_weight(2)  # Stroke thickness
  py5.no_fill()  # Fill of the shapes, like the triangle
  py5.text_size(16)

  # Variables used for grid.
  global width_center
  global height_center
  height_center = py5.height / 2
  width_center = py5.width / 2

# Displays text on vector location
def show_vector(vector: npt.NDArray, name: str) -> None:
  py5.text(name, vector[0], vector[1])

# Calculates the length of a given ab line segment
def line_length(a: npt.NDArray, b: npt.NDArray) -> float:
  return sqrt((a[0] - b[0])**2 + (a[1] - b[1])**2).real
  # ^Sqrt here returns an imaginary number, .real brings it back to reality.

# Finds the midpoint of a line
#def line_midpoint(a: npt.NDArray, b: npt.NDArray) -> npt.NDArray:
  #return npt.NDArray((a[0] + b[0]) / 2, (a[1] + b[1]) / 2)

# Finds the slope of a line
#def line_slope(a: npt.NDArray, b: npt.NDArray) -> npt.NDArray:
  #return (a[1] - b[1]) / (a[0] - b[0])

# Finds the perpendicular point of a line
#def line_magic(midpoint: npt.NDArray, slope: float) -> npt.NDArray:
#  neg_slope = -1 / slope
#  return npt.NDArray(neg_slope, midpoint[1] - neg_slope * midpoint[0])

def normal(a, b):
  ortho = np.array((b[0] - a[0], a[0] - b[0]))
  return ortho / np.linalg.norm(ortho)

#def perpendicular_line(a: npt.NDArray, b: npt.NDArray, midpoint: npt.NDArray) -> npt.NDArray:
#  slope = line_slope(a, b)
#  magic = line_magic(midpoint, slope)
#  py5.stroke(255, 0, 255, 100)
#  py5.line(*midpoint.get(), *magic.get())
#  py5.stroke(255, 0, 0)  # change color to red
#  return magic

#def extend_vector(vector: npt.NDArray, origin: npt.NDArray, length: int) -> npt.NDArray:
#  origin_vector = npt.NDArray((vector[0] - origin[0]) * length, (vector[1] - origin[1]) * length)
#  normalized_vector = origin_vector.normalized()
#  scaled_vector = origin + npt.NDArray(length * normalized_vector[0], length * normalized_vector[1])
#
#  # scaled_vector = npt.NDArray((vector[0] - origin[0]) * length, (vector[1] - origin[1]) * length)
#  return scaled_vector

def perpendicular_bisector(a: npt.NDArray, b: npt.NDArray, length: int) -> npt.NDArray:
  """
    Splits an ab segment with a perpendicular line of given length
    Args:
        a: First vector of the line.
        b: Second vector of the line.
        length: Length of the perpendicular bisector.
    Returns:
        Perpendicular bisector vector.
    """
  midpoint = line_midpoint(a, b)
  perpendicular: npt.NDArray = perpendicular_line(a, b, midpoint)
  return extend_vector(perpendicular, midpoint, length)

def draw() -> None:
  # draw grid.
  # py5.translate(width_center, height_center)
  py5.background(20)
  py5.stroke(50, 50, 50)  # gray color
  py5.line(-width_center, 0, py5.width, 0)
  py5.line(0, height_center, 0, -py5.height)

  vector_a = np.array((50, 250))
  vector_b = np.array((200, 300))
  vector_c = np.array((100, 100))

  py5.stroke(255)  # change to white color
  py5.triangle(*vector_a, *vector_b, *vector_c)
  py5.stroke(0, 255, 0)  # change to green color
  py5.line(*vector_a, *vector_b)  # Green line on ab

  # draw the extended, perpendicular vector
  normal_ = normal(vector_b, vector_a)
  midpoint_ = (vector_a + vector_b) / 2
  py5.ellipse(*midpoint_, 10, 10)
  py5.stroke(255, 0, 255)  # change color to purple
  py5.line(*midpoint_, *(midpoint_ + normal_ * 200))

  #perpendicular_bisec = perpendicular_bisector(vector_c, vector_b, -100)
  #midpoint = line_midpoint(vector_c, vector_b)
  #py5.ellipse(*midpoint.get(), 10, 10)
  #py5.stroke(255, 0, 255)  # change color to purple
  #py5.line(*midpoint.get(), *perpendicular_bisec.get())

  show_vector(vector_a, "vector_a")
  show_vector(vector_b, "vector_b")
  show_vector(vector_c, "vector_c")

py5.run_sketch()
