# p5 with typescript

```
npm install
```
to run a sketch, ls /sketches/
and then run
```
npm run sketch <directory-in-sketches>
```

### IMPORTANT: DO NOT USE IMPORTS

It makes tsc exclude that file from the build for some insane reason.
Instead, modify `tsconfig.json` next to the sketch file to include whatever you need.
