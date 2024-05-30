"""Polygon splitting with vectors."""

from typing import Any

import numpy as np
import numpy.typing as npt
import py5

def setup() -> None:
  """Set basic properties."""
  py5.size(1000, 1000, py5.P2D)
  py5.smooth(8)
  py5.stroke_weight(3)
  py5.no_fill()
  py5.text_size(18)

  # Variables used for grid.
  global width_center
  global height_center
  height_center = py5.height / 2
  width_center = py5.width / 2

def show_vector(vector: npt.NDArray, name: str) -> None:
  py5.text(name, vector[0], vector[1])

def midpoint(a: npt.NDArray, b: npt.NDArray) -> float:
  return (a+b) / 2

def normal(a: npt.NDArray, b: npt.NDArray) -> npt.NDArray:
  ortho = np.array((b[1] - a[1], a[0] - b[0]))
  return ortho / np.linalg.norm(ortho)

def perpendicular_line(a: npt.NDArray, b: npt.NDArray, length: float) -> npt.NDArray:
  midpoint_ = midpoint(a, b)
  normal_ = normal(a, b)
  return np.array((*midpoint_, *(midpoint_ + normal_*100)))

def draw() -> None:
  # Draw grid and center.
  py5.translate(width_center, height_center)
  py5.background(20)
  py5.stroke(50, 50, 50)
  py5.line(-width_center, 0, py5.width, 0)
  py5.line(0, height_center, 0, -py5.height)

  # Assing vectors
  vector_a: npt.NDArray = np.array((-100, 100))
  vector_b: npt.NDArray = np.array((200, 200))
  vector_c: npt.NDArray = np.array((100, -300))

  # Draw triangle
  py5.stroke(255)
  py5.triangle(*vector_a, *vector_b, *vector_c)

  # Create and draw line
  line = np.concatenate((vector_a, vector_c))
  py5.stroke(0, 255, 0)
  py5.line(*line)

  # Draw the extended, perpendicular vector.
  # normal_ = normal(vector_a, vector_c)
  # midpoint = (vector_a+vector_c) / 2
  perpendicular_line_ = perpendicular_line(vector_a, vector_c, 100)
  print(*perpendicular_line_)
  py5.stroke(255, 0, 255)
  py5.ellipse(perpendicular_line_[0], perpendicular_line_[1], 10, 10)
  py5.line(*perpendicular_line_)

  show_vector(vector_a, "vector_a")
  show_vector(vector_b, "vector_b")
  show_vector(vector_c, "vector_c")

py5.run_sketch()
