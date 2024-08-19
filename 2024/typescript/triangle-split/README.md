# p5 with typescript

```
npm install
```

```
npm run sketch helloworld
```

To add a new sketch, just clone that directory.

### IMPORTANT: DO NOT USE IMPORTS

It makes tsc exclude that file from the build for some insane reason.
Instead, modify `tsconfig.json` next to the sketch file to include whatever you need.
