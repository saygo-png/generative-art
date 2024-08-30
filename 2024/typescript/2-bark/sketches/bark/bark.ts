// GLOBAL VARS & TYPES {{{
let recursions_slider: p5.Element;
let hue_slider: p5.Element;

class Point {
  constructor(
    public x: number,
    public y: number,
  ) {}
  get(): [number, number] {
    return [this.x, this.y];
  }
}
class Quad {
  constructor(
    public p1: Point,
    public p2: Point,
    public p3: Point,
    public p4: Point,
  ) {}
  get(): [number, number, number, number, number, number, number, number] {
    return [
      this.p1.x,
      this.p1.y,
      this.p2.x,
      this.p2.y,
      this.p3.x,
      this.p3.y,
      this.p4.x,
      this.p4.y,
    ];
  }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function setup() {
  createCanvas(1000, 1000);
  colorMode(HSB, 360, 100, 100, 100);
  stroke(40);
  fill(255);
  strokeWeight(0);
  rectMode(CENTER);
  randomSeed(1);
  noLoop();
}
// }}}
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

function makeQuad(size: number, center: Point): Quad {
  const halfSize = size / 2;
  const ratioX = 0.5;
  const bottomRight = new Point(
    center.x + halfSize * ratioX,
    center.y + halfSize,
  );
  const bottomLeft = new Point(
    center.x - halfSize * ratioX,
    center.y + halfSize,
  );
  const topLeft = new Point(center.x - halfSize * ratioX, center.y - halfSize);
  const topRight = new Point(center.x + halfSize * ratioX, center.y - halfSize);
  return new Quad(topLeft, topRight, bottomRight, bottomLeft);
}

function generateRandomQuads(amount: number): Quad[] {
  const randomPositionX = random(0, width);
  const randomPositionY = random(0, height);
  const randomSize = random(100, 300);
  const baseCaseQuad = makeQuad(
    randomSize,
    new Point(randomPositionX, randomPositionY),
  );
  if (amount <= 0) {
    return [baseCaseQuad];
  }
  return [baseCaseQuad, ...generateRandomQuads(amount - 1)];
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function draw() {
  background(0, 0, 16);
  // translate(width / 2, height / 2);
  const randomQuads = generateRandomQuads(20);
  randomQuads.forEach((quad_, index) => quad(...quad_.get()));
}
