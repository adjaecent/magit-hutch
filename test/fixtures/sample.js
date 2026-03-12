const CONFIG = { port: 3000 };

function greet(name) {
  return `Hello, ${name}!`;
}

class Router {
  constructor(app) {
    this.app = app;
  }

  addRoute(path, handler) {
    this.app.get(path, handler);
  }
}

const multiply = (a, b) => {
  return a * b;
};

function nested() {
  function inner() {
    return 42;
  }
  return inner();
}
