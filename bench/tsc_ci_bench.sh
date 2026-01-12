#!/bin/bash
# CI TSC Benchmark - Standalone benchmark for CI pipelines
# Tests TypeScript compilation with real-world popular projects
#
# Usage: ./bench/tsc_ci_bench.sh [--json] [--quick]

set -e
BENCH_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$BENCH_DIR")"
cd "$BENCH_DIR"

# Parse args
JSON_OUTPUT=false
QUICK_MODE=false
for arg in "$@"; do
  case $arg in
    --json) JSON_OUTPUT=true ;;
    --quick) QUICK_MODE=true ;;
  esac
done

# Test file sources (popular TypeScript patterns)
TEST_DIR="/tmp/tsc_ci_bench_$$"
mkdir -p "$TEST_DIR"
trap "rm -rf $TEST_DIR" EXIT

# ============================================================================
# Test 1: Zod-like Schema Validation Library
# ============================================================================
cat > "$TEST_DIR/zod_like.ts" << 'TYPESCRIPT'
// Zod-like schema validation library pattern
type ZodType<T> = { _output: T; parse(data: unknown): T; safeParse(data: unknown): { success: boolean; data?: T; error?: Error } };

const z = {
  string: () => ({
    _output: "" as string,
    min: (n: number) => z.string(),
    max: (n: number) => z.string(),
    email: () => z.string(),
    url: () => z.string(),
    parse: (data: unknown): string => String(data),
    safeParse: (data: unknown) => ({ success: true, data: String(data) })
  }),
  number: () => ({
    _output: 0 as number,
    min: (n: number) => z.number(),
    max: (n: number) => z.number(),
    int: () => z.number(),
    positive: () => z.number(),
    parse: (data: unknown): number => Number(data),
    safeParse: (data: unknown) => ({ success: true, data: Number(data) })
  }),
  boolean: () => ({
    _output: false as boolean,
    parse: (data: unknown): boolean => Boolean(data),
    safeParse: (data: unknown) => ({ success: true, data: Boolean(data) })
  }),
  object: <T extends Record<string, ZodType<any>>>(shape: T) => ({
    _output: {} as { [K in keyof T]: T[K]["_output"] },
    parse: (data: unknown) => data as { [K in keyof T]: T[K]["_output"] },
    safeParse: (data: unknown) => ({ success: true, data: data as { [K in keyof T]: T[K]["_output"] } }),
    extend: <E extends Record<string, ZodType<any>>>(ext: E) => z.object({ ...shape, ...ext }),
    pick: <K extends keyof T>(...keys: K[]) => z.object(Object.fromEntries(keys.map(k => [k, shape[k]])) as Pick<T, K>),
    omit: <K extends keyof T>(...keys: K[]) => z.object(Object.fromEntries(Object.entries(shape).filter(([k]) => !keys.includes(k as K))) as Omit<T, K>)
  }),
  array: <T extends ZodType<any>>(item: T) => ({
    _output: [] as T["_output"][],
    min: (n: number) => z.array(item),
    max: (n: number) => z.array(item),
    parse: (data: unknown) => data as T["_output"][],
    safeParse: (data: unknown) => ({ success: true, data: data as T["_output"][] })
  }),
  union: <T extends ZodType<any>[]>(...types: T) => ({
    _output: undefined as T[number]["_output"],
    parse: (data: unknown) => data as T[number]["_output"],
    safeParse: (data: unknown) => ({ success: true, data: data as T[number]["_output"] })
  }),
  optional: <T extends ZodType<any>>(type: T) => ({
    _output: undefined as T["_output"] | undefined,
    parse: (data: unknown) => data as T["_output"] | undefined,
    safeParse: (data: unknown) => ({ success: true, data: data as T["_output"] | undefined })
  })
};

// Usage example - User schema
const UserSchema = z.object({
  id: z.string(),
  email: z.string().email(),
  name: z.string().min(1).max(100),
  age: z.number().int().positive(),
  isActive: z.boolean(),
  roles: z.array(z.string()),
  metadata: z.optional(z.object({
    createdAt: z.string(),
    updatedAt: z.string()
  }))
});

type User = typeof UserSchema._output;

console.log("Zod-like schema compiled successfully");
TYPESCRIPT

# ============================================================================
# Test 2: tRPC-like API Router Pattern
# ============================================================================
cat > "$TEST_DIR/trpc_like.ts" << 'TYPESCRIPT'
// tRPC-like type-safe API router pattern
type Procedure<TInput, TOutput> = {
  _input: TInput;
  _output: TOutput;
  query: (input: TInput) => Promise<TOutput>;
  mutation: (input: TInput) => Promise<TOutput>;
};

type Router<T extends Record<string, Procedure<any, any> | Router<any>>> = {
  [K in keyof T]: T[K];
};

const t = {
  procedure: <TInput = void>() => ({
    input: <I>(validator: { parse: (d: unknown) => I }) => t.procedure<I>(),
    query: <TOutput>(fn: (opts: { input: TInput }) => Promise<TOutput>) => ({
      _input: undefined as unknown as TInput,
      _output: undefined as unknown as TOutput,
      query: async (input: TInput) => fn({ input }),
      mutation: async (input: TInput) => fn({ input })
    }),
    mutation: <TOutput>(fn: (opts: { input: TInput }) => Promise<TOutput>) => ({
      _input: undefined as unknown as TInput,
      _output: undefined as unknown as TOutput,
      query: async (input: TInput) => fn({ input }),
      mutation: async (input: TInput) => fn({ input })
    })
  }),
  router: <T extends Record<string, any>>(routes: T): Router<T> => routes
};

// Define API router
const appRouter = t.router({
  user: t.router({
    getById: t.procedure<{ id: string }>()
      .query(async ({ input }) => ({ id: input.id, name: "John" })),
    create: t.procedure<{ name: string; email: string }>()
      .mutation(async ({ input }) => ({ id: "1", ...input })),
    list: t.procedure()
      .query(async () => [{ id: "1", name: "John" }])
  }),
  post: t.router({
    getById: t.procedure<{ id: string }>()
      .query(async ({ input }) => ({ id: input.id, title: "Hello" })),
    create: t.procedure<{ title: string; content: string }>()
      .mutation(async ({ input }) => ({ id: "1", ...input }))
  })
});

type AppRouter = typeof appRouter;

console.log("tRPC-like router compiled successfully");
TYPESCRIPT

# ============================================================================
# Test 3: React-like Component Types
# ============================================================================
cat > "$TEST_DIR/react_like.ts" << 'TYPESCRIPT'
// React-like component type patterns
type ReactNode = string | number | boolean | null | undefined | ReactElement<any>;
type ReactElement<P = any> = { type: string | FC<P>; props: P; key: string | null };
type FC<P = {}> = (props: P) => ReactNode;
type PropsWithChildren<P = {}> = P & { children?: ReactNode };

type ComponentProps<T> = T extends FC<infer P> ? P : never;
type InferProps<T> = T extends { props: infer P } ? P : never;

// Hook types
type Dispatch<A> = (action: A) => void;
type SetStateAction<S> = S | ((prevState: S) => S);
type Reducer<S, A> = (state: S, action: A) => S;

const useState = <S>(initial: S | (() => S)): [S, Dispatch<SetStateAction<S>>] => {
  const state = typeof initial === 'function' ? (initial as () => S)() : initial;
  return [state, () => {}];
};

const useReducer = <S, A>(reducer: Reducer<S, A>, initial: S): [S, Dispatch<A>] => {
  return [initial, () => {}];
};

const useEffect = (effect: () => void | (() => void), deps?: readonly any[]): void => {};
const useMemo = <T>(factory: () => T, deps: readonly any[]): T => factory();
const useCallback = <T extends (...args: any[]) => any>(callback: T, deps: readonly any[]): T => callback;
const useRef = <T>(initial: T): { current: T } => ({ current: initial });

// Component examples
interface ButtonProps {
  onClick: () => void;
  disabled?: boolean;
  variant?: 'primary' | 'secondary' | 'danger';
  size?: 'sm' | 'md' | 'lg';
  children: ReactNode;
}

const Button: FC<ButtonProps> = ({ onClick, disabled, variant = 'primary', size = 'md', children }) => {
  return { type: 'button', props: { onClick, disabled, className: `btn-${variant} btn-${size}` }, key: null };
};

interface InputProps<T = string> {
  value: T;
  onChange: (value: T) => void;
  placeholder?: string;
  type?: 'text' | 'email' | 'password' | 'number';
}

function Input<T extends string | number>({ value, onChange, placeholder, type = 'text' }: InputProps<T>): ReactElement {
  return { type: 'input', props: { value, onChange, placeholder, type }, key: null };
}

// Form with generics
interface FormProps<T extends Record<string, any>> {
  initialValues: T;
  onSubmit: (values: T) => void | Promise<void>;
  children: (form: { values: T; setValue: <K extends keyof T>(key: K, value: T[K]) => void }) => ReactNode;
}

function Form<T extends Record<string, any>>({ initialValues, onSubmit, children }: FormProps<T>): ReactElement {
  const [values, setValues] = useState(initialValues);
  const setValue = <K extends keyof T>(key: K, value: T[K]) => {
    setValues(prev => ({ ...prev, [key]: value }));
  };
  return { type: 'form', props: { onSubmit: () => onSubmit(values) }, key: null };
}

console.log("React-like types compiled successfully");
TYPESCRIPT

# ============================================================================
# Test 4: Prisma-like ORM Types
# ============================================================================
cat > "$TEST_DIR/prisma_like.ts" << 'TYPESCRIPT'
// Prisma-like ORM type patterns
type PrismaClient<Models extends Record<string, ModelDefinition>> = {
  [K in keyof Models]: ModelDelegate<Models[K]>;
} & { $connect: () => Promise<void>; $disconnect: () => Promise<void> };

type ModelDefinition = {
  fields: Record<string, FieldDefinition>;
  relations?: Record<string, RelationDefinition>;
};

type FieldDefinition = {
  type: 'string' | 'number' | 'boolean' | 'datetime' | 'json';
  optional?: boolean;
  unique?: boolean;
  default?: any;
};

type RelationDefinition = {
  model: string;
  type: 'one' | 'many';
  foreignKey: string;
};

type ModelDelegate<M extends ModelDefinition> = {
  findUnique: (args: { where: UniqueWhere<M> }) => Promise<ModelOutput<M> | null>;
  findMany: (args?: { where?: Where<M>; orderBy?: OrderBy<M>; take?: number; skip?: number }) => Promise<ModelOutput<M>[]>;
  create: (args: { data: CreateInput<M> }) => Promise<ModelOutput<M>>;
  update: (args: { where: UniqueWhere<M>; data: UpdateInput<M> }) => Promise<ModelOutput<M>>;
  delete: (args: { where: UniqueWhere<M> }) => Promise<ModelOutput<M>>;
  count: (args?: { where?: Where<M> }) => Promise<number>;
};

type UniqueWhere<M extends ModelDefinition> = {
  [K in keyof M['fields']]?: FieldOutput<M['fields'][K]>;
};

type Where<M extends ModelDefinition> = {
  [K in keyof M['fields']]?: FieldOutput<M['fields'][K]> | { equals?: FieldOutput<M['fields'][K]>; not?: FieldOutput<M['fields'][K]>; in?: FieldOutput<M['fields'][K]>[] };
} & { AND?: Where<M>[]; OR?: Where<M>[]; NOT?: Where<M> };

type OrderBy<M extends ModelDefinition> = {
  [K in keyof M['fields']]?: 'asc' | 'desc';
};

type FieldOutput<F extends FieldDefinition> =
  F['type'] extends 'string' ? string :
  F['type'] extends 'number' ? number :
  F['type'] extends 'boolean' ? boolean :
  F['type'] extends 'datetime' ? Date :
  F['type'] extends 'json' ? Record<string, any> :
  never;

type ModelOutput<M extends ModelDefinition> = {
  [K in keyof M['fields']]: M['fields'][K]['optional'] extends true
    ? FieldOutput<M['fields'][K]> | null
    : FieldOutput<M['fields'][K]>;
};

type CreateInput<M extends ModelDefinition> = {
  [K in keyof M['fields'] as M['fields'][K]['optional'] extends true ? never : K]: FieldOutput<M['fields'][K]>;
} & {
  [K in keyof M['fields'] as M['fields'][K]['optional'] extends true ? K : never]?: FieldOutput<M['fields'][K]>;
};

type UpdateInput<M extends ModelDefinition> = Partial<CreateInput<M>>;

// Define schema
type UserModel = {
  fields: {
    id: { type: 'string'; unique: true };
    email: { type: 'string'; unique: true };
    name: { type: 'string' };
    createdAt: { type: 'datetime'; default: 'now' };
    updatedAt: { type: 'datetime'; optional: true };
  };
  relations: {
    posts: { model: 'Post'; type: 'many'; foreignKey: 'authorId' };
  };
};

type PostModel = {
  fields: {
    id: { type: 'string'; unique: true };
    title: { type: 'string' };
    content: { type: 'string'; optional: true };
    published: { type: 'boolean'; default: false };
    authorId: { type: 'string' };
  };
};

type Schema = { User: UserModel; Post: PostModel };

console.log("Prisma-like ORM types compiled successfully");
TYPESCRIPT

# ============================================================================
# Benchmark Runner
# ============================================================================

benchmark_file() {
  local name="$1"
  local file="$2"
  local runs="${3:-5}"
  local warmup="${4:-2}"

  if [ "$QUICK_MODE" = true ]; then
    runs=3
    warmup=1
  fi

  local node_times=()
  local bun_times=()

  # Node.js - Warmup
  for i in $(seq 1 $warmup); do
    node typescript/bin/tsc "$file" --noEmit --skipLibCheck > /dev/null 2>&1
  done

  # Node.js - Timed runs
  for i in $(seq 1 $runs); do
    local start=$(python3 -c 'import time; print(int(time.time() * 1000))')
    node typescript/bin/tsc "$file" --noEmit --skipLibCheck > /dev/null 2>&1
    local end=$(python3 -c 'import time; print(int(time.time() * 1000))')
    node_times+=($((end - start)))
  done

  # Bun - Warmup
  for i in $(seq 1 $warmup); do
    bun typescript/bin/tsc "$file" --noEmit --skipLibCheck > /dev/null 2>&1
  done

  # Bun - Timed runs
  for i in $(seq 1 $runs); do
    local start=$(python3 -c 'import time; print(int(time.time() * 1000))')
    bun typescript/bin/tsc "$file" --noEmit --skipLibCheck > /dev/null 2>&1
    local end=$(python3 -c 'import time; print(int(time.time() * 1000))')
    bun_times+=($((end - start)))
  done

  # Calculate averages
  local node_sum=0 bun_sum=0
  for t in "${node_times[@]}"; do node_sum=$((node_sum + t)); done
  for t in "${bun_times[@]}"; do bun_sum=$((bun_sum + t)); done
  local node_avg=$((node_sum / runs))
  local bun_avg=$((bun_sum / runs))

  if [ "$JSON_OUTPUT" = true ]; then
    echo "{\"test\":\"$name\",\"node_ms\":$node_avg,\"bun_ms\":$bun_avg}"
  else
    echo "  $name:"
    echo "    Node.js: ${node_avg}ms (${node_times[*]})"
    echo "    Bun:     ${bun_avg}ms (${bun_times[*]})"

    # Winner
    if [ $node_avg -lt $bun_avg ]; then
      local diff=$((bun_avg - node_avg))
      local pct=$((diff * 100 / bun_avg))
      echo "    Winner: Node.js (+${pct}% faster)"
    else
      local diff=$((node_avg - bun_avg))
      local pct=$((diff * 100 / node_avg))
      echo "    Winner: Bun (+${pct}% faster)"
    fi
    echo ""
  fi
}

# Run benchmarks
if [ "$JSON_OUTPUT" = true ]; then
  echo "["
  benchmark_file "zod_like" "$TEST_DIR/zod_like.ts"
  echo ","
  benchmark_file "trpc_like" "$TEST_DIR/trpc_like.ts"
  echo ","
  benchmark_file "react_like" "$TEST_DIR/react_like.ts"
  echo ","
  benchmark_file "prisma_like" "$TEST_DIR/prisma_like.ts"
  echo "]"
else
  echo "╔════════════════════════════════════════════════════════════╗"
  echo "║           CI TSC Benchmark - Popular TS Patterns           ║"
  echo "║   Testing: zod, tRPC, React, Prisma type patterns         ║"
  echo "╚════════════════════════════════════════════════════════════╝"
  echo ""
  echo "Runtime versions:"
  echo "  Node.js: $(node --version)"
  echo "  Bun: $(bun --version)"
  echo ""
  echo "═══════════════════════════════════════════════════════════════"
  echo "Benchmarks (lower is better):"
  echo "═══════════════════════════════════════════════════════════════"
  echo ""

  benchmark_file "zod_like" "$TEST_DIR/zod_like.ts"
  benchmark_file "trpc_like" "$TEST_DIR/trpc_like.ts"
  benchmark_file "react_like" "$TEST_DIR/react_like.ts"
  benchmark_file "prisma_like" "$TEST_DIR/prisma_like.ts"

  echo "═══════════════════════════════════════════════════════════════"
  echo "Complete"
  echo "═══════════════════════════════════════════════════════════════"
fi
