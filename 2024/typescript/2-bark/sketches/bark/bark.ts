// GLOBAL VARS & TYPES {{{
let recursions_slider: p5.Element;
let hue_slider: p5.Element;
let randomQuads: Quad[];

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
// }}}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function setup() {
  createCanvas(1000, 1000);
  colorMode(HSB, 360, 100, 100, 100);
  stroke(40);
  frameRate(1);
  fill(255);
  strokeWeight(0);
  rectMode(CENTER);
  randomSeed(1);

  randomQuads = generateRandomQuads(30, 10);
}

function getWidth(p1: Point, p2: Point): number {
  return Math.abs(p2.x - p1.x);
}

function getHeight(p1: Point, p3: Point): number {
  return Math.abs(p3.y - p1.y);
}

function doesOverlap(
  checkRect: Quad,
  rectangles: Quad[],
  border: number,
): boolean {
  return rectangles.some((otherRect: Quad) => {
    const checkRectWidth = getWidth(checkRect.p1, checkRect.p2);
    const otherRectWidth = getWidth(otherRect.p1, otherRect.p2);
    const checkRectHeight = getHeight(checkRect.p1, checkRect.p3);
    const otherRectHeight = getHeight(otherRect.p1, otherRect.p3);
    return (
      checkRect.p1.x < otherRect.p1.x + otherRectWidth + border &&
      checkRect.p1.x + checkRectWidth + border > otherRect.p1.x &&
      checkRect.p1.y < otherRect.p1.y + otherRectHeight + border &&
      checkRect.p1.y + checkRectHeight + border > otherRect.p1.y
    );
  });
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

function generateRandomQuads(
  amount: number,
  border: number,
  oldQuads: Quad[] = [],
): Quad[] {
  if (amount <= 0) {
    return oldQuads;
  }
  const randomPositionX = random(0, width);
  const randomPositionY = random(0, height);
  const randomSize = random(100, 300);
  const newQuad = makeQuad(
    randomSize,
    new Point(randomPositionX, randomPositionY),
  );
  if (doesOverlap(newQuad, oldQuads, border)) {
    return generateRandomQuads(amount, border, oldQuads);
  } else {
    return generateRandomQuads(amount - 1, border, [...oldQuads, newQuad]);
  }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function draw() {
  background(0, 0, 16);
  fill(255);
  randomQuads.forEach((quad_, index) => quad(...quad_.get()));

  const randomPosX = random(0, width);
  const randomPosY = random(0, height);
  const testQuad = makeQuad(100, new Point(randomPosX, randomPosY));

  fill(260, 100, 50, 100);
  quad(...testQuad.get());
  fill(255);

  if (doesOverlap(testQuad, randomQuads, 30)) {
    fill(360, 100, 50, 100);
    quad(...testQuad.get());
  }
}
