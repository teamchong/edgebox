// Test global variable opcodes for freeze system

// Test 1: Simple global variable read/write (get_var, put_var)
print("=== Test 1: Global variable read/write ===");
globalVar = 42;
print("globalVar = " + globalVar);  // Expected: 42

// Test 2: Check if variable exists (check_var)
print("\n=== Test 2: Check variable existence ===");
existsTest = 100;
print("'existsTest' in globalThis: " + ('existsTest' in globalThis));  // Expected: true
print("'nonExistent' in globalThis: " + ('nonExistent' in globalThis));  // Expected: false

// Test 3: Delete global variable (delete_var)
print("\n=== Test 3: Delete global variable ===");
toDelete = "will be deleted";
print("Before delete: toDelete = " + toDelete);  // Expected: will be deleted
print("delete toDelete: " + (delete toDelete));  // Expected: true
print("'toDelete' in globalThis: " + ('toDelete' in globalThis));  // Expected: false

// Test 4: Delete property from object (delete)
print("\n=== Test 4: Delete object property ===");
var obj = { a: 1, b: 2, c: 3 };
print("Before delete: obj.b = " + obj.b);  // Expected: 2
print("delete obj.b: " + (delete obj.b));  // Expected: true
print("After delete: obj.b = " + obj.b);  // Expected: undefined
print("'b' in obj: " + ('b' in obj));  // Expected: false

// Test 5: Delete non-existent property
print("\n=== Test 5: Delete non-existent property ===");
print("delete obj.nonExistent: " + (delete obj.nonExistent));  // Expected: true (always true for non-existent)

// Test 6: Multiple global variables
print("\n=== Test 6: Multiple global variables ===");
x = 1;
y = 2;
z = x + y;
print("x + y = z: " + x + " + " + y + " = " + z);  // Expected: 1 + 2 = 3

// Test 7: Shadowing and reassignment
print("\n=== Test 7: Reassignment ===");
counter = 0;
counter = counter + 1;
counter = counter + 1;
counter = counter + 1;
print("counter after 3 increments: " + counter);  // Expected: 3

print("\n=== All tests passed! ===");
