// eslint-disable-next-line @typescript-eslint/no-unused-vars
function setup() {
  createCanvas(1000, 1000);
  stroke(0);
  strokeWeight(5);
  rectMode(CENTER);
}

const recursions = 5;

const seedTriangle: Triangle = {
  p1: {x: -160, y: 100},
  p2: {x: 160, y: 100},
  p3: {x: 0, y: -170},
};

type Point = {
  x: number;
  y: number;
};

type Triangle = {
  p1: Point;
  p2: Point;
  p3: Point;
};

const unp = Object.values;

const midpoint = (p1: Point, p2: Point): Point => ({
  x: (p1.x + p2.x) / 2,
  y: (p1.y + p2.y) / 2,
});

function myNormal(p1: Point, p2: Point): Point {
  const ortho = {x: p2.y - p1.y, y: p1.x - p2.x};
  const norm = math.number(math.norm(unp(ortho)));
  return {x: ortho.x / norm, y: ortho.y / norm};
}

function endpoint(p1: Point, p2: Point, length: number): Point {
  const midpoint_ = midpoint(p1, p2);
  const unitVec = myNormal(p1, p2);
  const scaledUnitVec = {x: unitVec.x * length, y: unitVec.y * length};
  return {x: midpoint_.x + scaledUnitVec.x, y: midpoint_.y + scaledUnitVec.y};
}

function splitRecursively(
  p1: Point,
  p2: Point,
  length: number,
  maxDepth: number,
  depth = 0
): Triangle[] {
  const endpoint_ = endpoint(p1, p2, length);
  const baseCaseTriangle: Triangle = {p1, p2, p3: endpoint_};

  if (depth >= maxDepth) {
    return [baseCaseTriangle];
  }

  const triangles: Triangle[] = [];
  triangles.push(baseCaseTriangle);

  triangles.push(
    ...splitRecursively(p1, endpoint_, length, maxDepth, depth + 1)
  );
  triangles.push(
    ...splitRecursively(endpoint_, p2, length, maxDepth, depth + 1)
  );

  return triangles;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function draw() {
  background(255);
  translate(width/2, height/2)
  let i = 0
  const leftEdge = splitRecursively (seedTriangle.p1, seedTriangle.p3, mouseY/8, recursions, 0)
  const rightEdge = splitRecursively (seedTriangle.p3, seedTriangle.p2, mouseY/8, recursions, 0)
  const bottomEdge = splitRecursively (seedTriangle.p2, seedTriangle.p1, mouseY/8, recursions, 0)
  const triangles = [...leftEdge, ...rightEdge, ...bottomEdge]
  console.log(triangles)
  for (const triangle_ of triangles) {
    fill(i, 100, 100, 90);
    triangle(
      triangle_.p1.x,
      triangle_.p1.y,

      triangle_.p2.x,
      triangle_.p2.y,

      triangle_.p3.x,
      triangle_.p3.y,
    );
    i += 360 / triangles.length;
  }
  fill(255)
}
