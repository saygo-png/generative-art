// helper script to run a cross-platform sketch with args
const exec = require("child_process").exec;
const arg = process.argv[2] || "helloworld"; // default to the hello world sketch

const compileProcess = exec("npx tsc --watch --p ./sketches/"+arg) // stdin, out, err
compileProcess.stdout.pipe(process.stdout);

exec("npx browser-sync start --server -w") // stdin, out, err
