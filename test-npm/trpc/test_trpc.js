const { initTRPC, TRPCError } = require('@trpc/server');
const { z } = require('zod');

// Test utilities
let passed = 0;
let failed = 0;

function test(name, fn) {
  try {
    const result = fn();
    if (result && typeof result.then === 'function') {
      return result
        .then(() => {
          console.log(`PASS: ${name}`);
          passed++;
        })
        .catch((err) => {
          console.log(`FAIL: ${name}`);
          console.log(`  Error: ${err.message}`);
          failed++;
        });
    }
    console.log(`PASS: ${name}`);
    passed++;
  } catch (err) {
    console.log(`FAIL: ${name}`);
    console.log(`  Error: ${err.message}`);
    failed++;
  }
}

async function testAsync(name, fn) {
  try {
    await fn();
    console.log(`PASS: ${name}`);
    passed++;
  } catch (err) {
    console.log(`FAIL: ${name}`);
    console.log(`  Error: ${err.message}`);
    failed++;
  }
}

function assertEqual(actual, expected, msg) {
  if (actual !== expected) {
    throw new Error(`${msg || 'Assertion failed'}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}

function assertDeepEqual(actual, expected, msg) {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`${msg || 'Assertion failed'}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}

// Initialize tRPC
const t = initTRPC.context().create();
const router = t.router;
const publicProcedure = t.procedure;

// Create nested router for testing router composition
const userRouter = router({
  getById: publicProcedure
    .input(z.object({ id: z.string() }))
    .query(({ input }) => ({ id: input.id, name: 'User ' + input.id })),

  list: publicProcedure.query(() => [
    { id: '1', name: 'Alice' },
    { id: '2', name: 'Bob' },
  ]),
});

// Create main router with various procedures
const appRouter = router({
  // Simple query - no input
  hello: publicProcedure.query(() => 'Hello World'),

  // Query with simple input
  greet: publicProcedure
    .input(z.object({ name: z.string() }))
    .query(({ input }) => `Hello ${input.name}`),

  // Query with optional input
  greetOptional: publicProcedure
    .input(z.object({ name: z.string().optional() }))
    .query(({ input }) => `Hello ${input.name || 'Anonymous'}`),

  // Query with complex validation
  validateEmail: publicProcedure
    .input(z.object({ email: z.string().email() }))
    .query(({ input }) => ({ valid: true, email: input.email })),

  // Mutation - create pattern
  createUser: publicProcedure
    .input(z.object({
      name: z.string().min(2),
      email: z.string().email(),
      age: z.number().min(0).optional(),
    }))
    .mutation(({ input }) => ({
      id: 'user_' + Date.now(),
      ...input,
      createdAt: new Date().toISOString(),
    })),

  // Mutation - update pattern
  updateUser: publicProcedure
    .input(z.object({
      id: z.string(),
      name: z.string().optional(),
      email: z.string().email().optional(),
    }))
    .mutation(({ input }) => ({
      ...input,
      updatedAt: new Date().toISOString(),
    })),

  // Context access
  getUser: publicProcedure
    .query(({ ctx }) => ctx.user),

  // Custom error
  throwError: publicProcedure
    .input(z.object({ code: z.string() }))
    .query(({ input }) => {
      throw new TRPCError({
        code: input.code,
        message: 'Custom error message',
      });
    }),

  // Array input/output
  sum: publicProcedure
    .input(z.array(z.number()))
    .query(({ input }) => input.reduce((a, b) => a + b, 0)),

  // Nested router
  user: userRouter,
});

// Run all tests
async function runTests() {
  console.log('=== tRPC Runtime Tests ===\n');

  // Create caller with context
  const caller = appRouter.createCaller({
    user: { id: 'ctx_user_1', name: 'Context User' }
  });

  // Test 1: Basic Setup - tRPC initializes correctly
  test('tRPC initializes correctly', () => {
    assertEqual(typeof t.router, 'function', 'router should be a function');
    assertEqual(typeof t.procedure, 'object', 'procedure should be an object');
  });

  // Test 2: Simple query - no input
  await testAsync('Simple query returns correct value', async () => {
    const result = await caller.hello();
    assertEqual(result, 'Hello World');
  });

  // Test 3: Query with input
  await testAsync('Query with input works', async () => {
    const result = await caller.greet({ name: 'EdgeBox' });
    assertEqual(result, 'Hello EdgeBox');
  });

  // Test 4: Query with optional input - provided
  await testAsync('Optional input - provided', async () => {
    const result = await caller.greetOptional({ name: 'Test' });
    assertEqual(result, 'Hello Test');
  });

  // Test 5: Query with optional input - not provided
  await testAsync('Optional input - not provided', async () => {
    const result = await caller.greetOptional({});
    assertEqual(result, 'Hello Anonymous');
  });

  // Test 6: Input validation - valid email
  await testAsync('Valid email passes validation', async () => {
    const result = await caller.validateEmail({ email: 'test@example.com' });
    assertEqual(result.valid, true);
    assertEqual(result.email, 'test@example.com');
  });

  // Test 7: Input validation - invalid email throws
  await testAsync('Invalid email throws validation error', async () => {
    let threw = false;
    try {
      await caller.validateEmail({ email: 'not-an-email' });
    } catch (err) {
      threw = true;
      // Should be a TRPCError with BAD_REQUEST code
      assertEqual(err.code, 'BAD_REQUEST', 'Should throw BAD_REQUEST');
    }
    if (!threw) {
      throw new Error('Expected validation error');
    }
  });

  // Test 8: Mutation - create user
  await testAsync('Mutation creates user correctly', async () => {
    const result = await caller.createUser({
      name: 'John Doe',
      email: 'john@example.com',
      age: 30,
    });
    assertEqual(result.name, 'John Doe');
    assertEqual(result.email, 'john@example.com');
    assertEqual(result.age, 30);
    assertEqual(typeof result.id, 'string');
    assertEqual(typeof result.createdAt, 'string');
  });

  // Test 9: Mutation validation - name too short
  await testAsync('Mutation validates minimum string length', async () => {
    let threw = false;
    try {
      await caller.createUser({
        name: 'J', // Too short - min 2
        email: 'j@example.com',
      });
    } catch (err) {
      threw = true;
      assertEqual(err.code, 'BAD_REQUEST');
    }
    if (!threw) {
      throw new Error('Expected validation error for short name');
    }
  });

  // Test 10: Mutation - update user
  await testAsync('Mutation updates user correctly', async () => {
    const result = await caller.updateUser({
      id: 'user_123',
      name: 'Jane Doe',
    });
    assertEqual(result.id, 'user_123');
    assertEqual(result.name, 'Jane Doe');
    assertEqual(typeof result.updatedAt, 'string');
  });

  // Test 11: Context access
  await testAsync('Context is accessible in procedure', async () => {
    const result = await caller.getUser();
    assertEqual(result.id, 'ctx_user_1');
    assertEqual(result.name, 'Context User');
  });

  // Test 12: Custom TRPCError
  await testAsync('Custom TRPCError is thrown correctly', async () => {
    let threw = false;
    try {
      await caller.throwError({ code: 'NOT_FOUND' });
    } catch (err) {
      threw = true;
      assertEqual(err.code, 'NOT_FOUND');
      assertEqual(err.message, 'Custom error message');
    }
    if (!threw) {
      throw new Error('Expected TRPCError');
    }
  });

  // Test 13: Array input
  await testAsync('Array input works correctly', async () => {
    const result = await caller.sum([1, 2, 3, 4, 5]);
    assertEqual(result, 15);
  });

  // Test 14: Empty array input
  await testAsync('Empty array input returns zero', async () => {
    const result = await caller.sum([]);
    assertEqual(result, 0);
  });

  // Test 15: Nested router - getById
  await testAsync('Nested router query with input works', async () => {
    const result = await caller.user.getById({ id: '42' });
    assertEqual(result.id, '42');
    assertEqual(result.name, 'User 42');
  });

  // Test 16: Nested router - list
  await testAsync('Nested router query returns list', async () => {
    const result = await caller.user.list();
    assertEqual(Array.isArray(result), true);
    assertEqual(result.length, 2);
    assertEqual(result[0].name, 'Alice');
    assertEqual(result[1].name, 'Bob');
  });

  // Test 17: Different caller contexts
  await testAsync('Different contexts work independently', async () => {
    const caller2 = appRouter.createCaller({
      user: { id: 'other_user', name: 'Other User' },
    });
    const result = await caller2.getUser();
    assertEqual(result.id, 'other_user');
    assertEqual(result.name, 'Other User');
  });

  // Test 18: Missing required input throws
  await testAsync('Missing required input throws error', async () => {
    let threw = false;
    try {
      await caller.greet({}); // Missing 'name'
    } catch (err) {
      threw = true;
      assertEqual(err.code, 'BAD_REQUEST');
    }
    if (!threw) {
      throw new Error('Expected error for missing required input');
    }
  });

  // Test 19: Wrong input type throws
  await testAsync('Wrong input type throws error', async () => {
    let threw = false;
    try {
      await caller.greet({ name: 123 }); // Should be string
    } catch (err) {
      threw = true;
      assertEqual(err.code, 'BAD_REQUEST');
    }
    if (!threw) {
      throw new Error('Expected error for wrong input type');
    }
  });

  // Test 20: Negative number validation
  await testAsync('Negative age validation works', async () => {
    let threw = false;
    try {
      await caller.createUser({
        name: 'Test',
        email: 'test@example.com',
        age: -5, // Should be >= 0
      });
    } catch (err) {
      threw = true;
      assertEqual(err.code, 'BAD_REQUEST');
    }
    if (!threw) {
      throw new Error('Expected error for negative age');
    }
  });

  // Summary
  console.log('\n=== Test Summary ===');
  console.log(`Passed: ${passed}`);
  console.log(`Failed: ${failed}`);
  console.log(`Total: ${passed + failed}`);

  if (failed === 0) {
    console.log('\nALL PASS');
  } else {
    console.log('\nSOME TESTS FAILED');
    process.exit(1);
  }
}

// Run tests
runTests().catch((err) => {
  console.error('Fatal error:', err);
  process.exit(1);
});
