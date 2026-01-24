// Minimal reproduction of forward function reference bug
function makeInit() {
  function init() {
    console.log("init called, _ is:", typeof _);
    if (_ === undefined) {
      throw new Error("_ is undefined - closure bug!");
    }
    return _.prototype;
  }

  function _(def) {
    return { value: def };
  }

  _.init = init;
  return _;
}

const Constructor = makeInit();
console.log("Constructor:", typeof Constructor);
console.log("Calling Constructor.init()...");
const result = Constructor.init();
console.log("Result:", result);
console.log("PASS");
