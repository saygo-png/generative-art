// GLOBAL VARS & TYPES
let recursions_slider: p5.Element;

class Point {
  constructor(
    public x: number,
    public y: number,
  ) {}
  get(): [number, number] {
    return [this.x, this.y];
  }
}
class Triangle {
  constructor(
    public p1: Point,
    public p2: Point,
    public p3: Point,
  ) {}
  get(): [number, number, number, number, number, number] {
    return [this.p1.x, this.p1.y, this.p2.x, this.p2.y, this.p3.x, this.p3.y];
  }
}

const unp = Object.values;

// SETTINGS
// eslint-disable-next-line @typescript-eslint/no-unused-vars
function setup() {
  createCanvas(1000, 1000);
  colorMode(HSB, 360, 100, 100, 100);
  stroke(40);
  strokeWeight(0);
  rectMode(CENTER);
  recursions_slider = createSlider(1, 15, 5, 1)
    .position(10, 10)
    .style("background", "green");
}
const seedTriangle = new Triangle(
  new Point(-160, 100),
  new Point(160, 100),
  new Point(0, -170),
);

const midpoint = (p1: Point, p2: Point): Point =>
  new Point((p1.x + p2.x) / 2, (p1.y + p2.y) / 2);

function myNormal(p1: Point, p2: Point): Point {
  const ortho = new Point(p2.y - p1.y, p1.x - p2.x);
  const norm = Math.hypot(...ortho.get());
  return new Point(ortho.x / norm, ortho.y / norm);
}

function endpoint(p1: Point, p2: Point, length: number): Point {
  const midpoint_ = midpoint(p1, p2);
  const unitVec = myNormal(p1, p2);
  const scaledUnitVec = new Point(unitVec.x * length, unitVec.y * length);
  return new Point(
    midpoint_.x + scaledUnitVec.x,
    midpoint_.y + scaledUnitVec.y,
  );
}

function splitRecursively(
  p1: Point,
  p2: Point,
  length: number,
  depth: number,
): Triangle[] {
  const endpoint_ = endpoint(p1, p2, length);
  const baseCaseTriangle = new Triangle(p1, p2, endpoint_);
  if (depth <= 0) {
    return [baseCaseTriangle];
  }

  return [
    baseCaseTriangle,
    ...splitRecursively(p1, endpoint_, length, depth - 1),
    ...splitRecursively(endpoint_, p2, length, depth - 1),
  ];
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function draw() {
  background(0, 0, 16);
  translate(width / 2, height / 2);

  const recursions = ((value: string | number) => {
    if (typeof value === "string") {
      throw new TypeError("This is not a number, imbecile");
    }
    return value;
  })(recursions_slider.value());

  const edges = [
    [seedTriangle.p1, seedTriangle.p3],
    [seedTriangle.p3, seedTriangle.p2],
    [seedTriangle.p2, seedTriangle.p1],
  ];
  const mouse_len = (height - mouseY) / 8
  const triangles = edges.flatMap(([p1, p2]: [Point, Point]) =>
    splitRecursively(p1, p2, mouse_len, recursions),
  );

  const hueStep = 360 / triangles.length;
  triangles.forEach((triangle_, index, _) => {
    const hue = (index * hueStep) / 2;
    fill(hue, 100, 100, 50);
    triangle(...triangle_.get());
  });
}
