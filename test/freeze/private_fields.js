// Test private field opcodes for freeze system

// Test 1: Basic private field
print("=== Test 1: Basic private field ===");
class Counter {
    #count = 0;

    increment() {
        this.#count++;
        return this.#count;
    }

    getCount() {
        return this.#count;
    }
}

const counter = new Counter();
print("Initial count: " + counter.getCount());  // Expected: 0
print("After increment: " + counter.increment());  // Expected: 1
print("After increment: " + counter.increment());  // Expected: 2
print("Final count: " + counter.getCount());  // Expected: 2

// Test 2: Private field with initial value
print("\n=== Test 2: Private field with initial value ===");
class Person {
    #name;
    #age = 0;

    constructor(name, age) {
        this.#name = name;
        this.#age = age;
    }

    info() {
        return this.#name + " is " + this.#age + " years old";
    }
}

const person = new Person("Alice", 30);
print(person.info());  // Expected: Alice is 30 years old

// Test 3: Private field in check (#field in obj)
print("\n=== Test 3: Private field 'in' check ===");
class HasPrivate {
    #secret = 42;

    static hasSecret(obj) {
        return #secret in obj;
    }

    getSecret() {
        return this.#secret;
    }
}

const hp = new HasPrivate();
print("hp has #secret: " + HasPrivate.hasSecret(hp));  // Expected: true
print("plain obj has #secret: " + HasPrivate.hasSecret({}));  // Expected: false
print("secret value: " + hp.getSecret());  // Expected: 42

// Test 4: Multiple private fields
print("\n=== Test 4: Multiple private fields ===");
class Point {
    #x;
    #y;

    constructor(x, y) {
        this.#x = x;
        this.#y = y;
    }

    distanceFromOrigin() {
        return Math.sqrt(this.#x * this.#x + this.#y * this.#y);
    }

    toString() {
        return "(" + this.#x + ", " + this.#y + ")";
    }
}

const p = new Point(3, 4);
print("Point: " + p.toString());  // Expected: (3, 4)
print("Distance from origin: " + p.distanceFromOrigin());  // Expected: 5

// Test 5: Private methods (if supported)
print("\n=== Test 5: Private methods ===");
class Calculator {
    #validate(n) {
        if (typeof n !== 'number') {
            throw new Error('Must be a number');
        }
        return n;
    }

    double(n) {
        return this.#validate(n) * 2;
    }
}

const calc = new Calculator();
print("double(5): " + calc.double(5));  // Expected: 10
print("double(7): " + calc.double(7));  // Expected: 14

print("\n=== All tests passed! ===");
