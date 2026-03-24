# Functional Programming in JavaScript with Ramda

## Core FP Concepts in JavaScript

### 1. Immutability
```javascript
// JavaScript doesn't enforce immutability, but you can use conventions

// Use const for bindings (not deep immutability)
const numbers = [1, 2, 3, 4, 5];
// numbers = [6, 7, 8];  // Error! Can't reassign

// But arrays/objects are still mutable
numbers.push(6);  // This works! Avoid this.

// Object.freeze for shallow immutability
const frozen = Object.freeze({ a: 1, b: 2 });
// frozen.a = 10;  // Silently fails (or throws in strict mode)

// Spread operator for immutable updates
const original = [1, 2, 3];
const modified = [...original, 4];  // New array

const obj = { a: 1, b: 2 };
const updated = { ...obj, b: 3 };  // { a: 1, b: 3 }

// Ramda functions never mutate - they return new data
const R = require('ramda');
const appended = R.append(4, [1, 2, 3]);  // [1, 2, 3, 4]
```

### 2. Pure Functions
```javascript
// Pure function: same input → same output, no side effects
const square = x => x * x;

// Not pure (has side effects)
let globalCounter = 0;
const impureSquare = x => {
    globalCounter++;  // Side effect!
    return x * x;
};

// Not pure (depends on external state)
const impureRandom = x => x + Math.random();  // Non-deterministic!

// Not pure (mutates input)
const impureDouble = arr => {
    arr.push(arr[0] * 2);  // Mutates!
    return arr;
};

// Pure version
const pureDouble = arr => [...arr, arr[0] * 2];
```

### 3. Higher-Order Functions (Functions as First-Class Objects)
```javascript
// Function taking another function as parameter
const applyToAll = (arr, func) => arr.map(func);

// Arrow functions (anonymous functions)
const square = x => x * x;
const add = (a, b) => a + b;

// Using higher-order functions
const numbers = [1, 2, 3, 4, 5];
const squares = applyToAll(numbers, square);  // [1, 4, 9, 16, 25]

// Functions returning functions
const multiplier = factor => x => x * factor;
const double = multiplier(2);
const triple = multiplier(3);
console.log(double(5));  // 10
console.log(triple(5));  // 15
```

---

## Ramda Basics

```javascript
const R = require('ramda');
// or
import * as R from 'ramda';
```

### Key Principles
1. **Data-last**: Functions take data as the last argument (enables currying)
2. **Auto-curried**: All functions are automatically curried
3. **Immutable**: Never mutates input data

```javascript
// Data-last enables partial application
const add10 = R.add(10);
console.log(add10(5));  // 15

// Compare to native (data-first)
[1, 2, 3].map(x => x * 2);  // Native
R.map(x => x * 2, [1, 2, 3]);  // Ramda
R.map(x => x * 2)([1, 2, 3]);  // Curried
```

---

## Essential Ramda Functions

### R.map
```javascript
const numbers = [1, 2, 3, 4, 5];

// Basic map
const squares = R.map(x => x * x, numbers);  // [1, 4, 9, 16, 25]

// Curried version
const double = R.map(x => x * 2);
console.log(double(numbers));  // [2, 4, 6, 8, 10]

// Map over object values
const obj = { a: 1, b: 2, c: 3 };
const doubled = R.map(x => x * 2, obj);  // { a: 2, b: 4, c: 6 }

// With R.prop for accessing properties
const users = [
    { name: 'Alice', age: 30 },
    { name: 'Bob', age: 25 }
];
const names = R.map(R.prop('name'), users);  // ['Alice', 'Bob']
```

### R.filter
```javascript
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Basic filter
const evens = R.filter(x => x % 2 === 0, numbers);  // [2, 4, 6, 8, 10]

// Curried
const getPositive = R.filter(x => x > 0);
console.log(getPositive([-1, 0, 1, 2]));  // [1, 2]

// Filter objects
const users = [
    { name: 'Alice', age: 30, active: true },
    { name: 'Bob', age: 25, active: false },
    { name: 'Charlie', age: 35, active: true }
];
const activeUsers = R.filter(R.prop('active'), users);
// [{ name: 'Alice', ... }, { name: 'Charlie', ... }]

// R.reject - opposite of filter
const odds = R.reject(x => x % 2 === 0, numbers);  // [1, 3, 5, 7, 9]
```

### R.reduce
```javascript
const numbers = [1, 2, 3, 4, 5];

// Sum
const sum = R.reduce(R.add, 0, numbers);  // 15

// Product
const product = R.reduce(R.multiply, 1, numbers);  // 120

// Custom reducer
const concat = R.reduce(
    (acc, word) => acc === '' ? word : `${acc} ${word}`,
    '',
    ['hello', 'world']
);  // 'hello world'

// Build object from array
const pairs = [['a', 1], ['b', 2], ['c', 3]];
const obj = R.reduce(
    (acc, [key, val]) => R.assoc(key, val, acc),
    {},
    pairs
);  // { a: 1, b: 2, c: 3 }

// Or use R.fromPairs
const obj2 = R.fromPairs(pairs);  // { a: 1, b: 2, c: 3 }
```

### R.chain (flatMap)
```javascript
const numbers = [1, 2, 3];

// Expand each element
const expanded = R.chain(x => [x, x * 10], numbers);
// [1, 10, 2, 20, 3, 30]

// Flatten nested arrays while transforming
const nested = [[1, 2], [3, 4], [5, 6]];
const doubled = R.chain(R.map(x => x * 2), nested);
// [2, 4, 6, 8, 10, 12]

// Extract from objects
const users = [
    { name: 'Alice', tags: ['admin', 'user'] },
    { name: 'Bob', tags: ['user'] }
];
const allTags = R.chain(R.prop('tags'), users);
// ['admin', 'user', 'user']
```

### R.flatten
```javascript
const nested = [[1, 2], [3, [4, 5]], 6];

// Flatten one level
const flat = R.unnest(nested);  // [1, 2, 3, [4, 5], 6]

// Deep flatten
const deepFlat = R.flatten(nested);  // [1, 2, 3, 4, 5, 6]
```

### R.take / R.drop
```javascript
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

const firstThree = R.take(3, numbers);  // [1, 2, 3]
const afterThree = R.drop(3, numbers);  // [4, 5, 6, 7, 8, 9, 10]

// Last elements
const lastTwo = R.takeLast(2, numbers);  // [9, 10]
const dropLast = R.dropLast(2, numbers);  // [1, 2, 3, 4, 5, 6, 7, 8]

// Take/drop while
const ascending = R.takeWhile(x => x < 5, numbers);  // [1, 2, 3, 4]
const afterThreshold = R.dropWhile(x => x < 5, numbers);  // [5, 6, 7, 8, 9, 10]
```

### R.reverse
```javascript
const numbers = [1, 2, 3, 4, 5];
const reversed = R.reverse(numbers);  // [5, 4, 3, 2, 1]

// Works on strings too
const str = R.reverse('hello');  // 'olleh'
```

### R.zip
```javascript
const nums = [1, 2, 3];
const letters = ['a', 'b', 'c'];

const zipped = R.zip(nums, letters);
// [[1, 'a'], [2, 'b'], [3, 'c']]

// Zip with function
const added = R.zipWith(R.add, [1, 2, 3], [10, 20, 30]);
// [11, 22, 33]

// Zip to object
const obj = R.zipObj(['a', 'b', 'c'], [1, 2, 3]);
// { a: 1, b: 2, c: 3 }
```

### R.concat
```javascript
const a = [1, 2, 3];
const b = [4, 5, 6];

const combined = R.concat(a, b);  // [1, 2, 3, 4, 5, 6]

// Works on strings
const str = R.concat('hello', ' world');  // 'hello world'
```

### R.sort
```javascript
const numbers = [3, 1, 4, 1, 5, 9, 2, 6];

// Ascending
const sorted = R.sort(R.subtract, numbers);  // [1, 1, 2, 3, 4, 5, 6, 9]

// Descending
const desc = R.sort((a, b) => b - a, numbers);  // [9, 6, 5, 4, 3, 2, 1, 1]

// Sort by property
const users = [
    { name: 'Charlie', age: 35 },
    { name: 'Alice', age: 30 },
    { name: 'Bob', age: 25 }
];

const byAge = R.sortBy(R.prop('age'), users);
// [{ name: 'Bob', age: 25 }, { name: 'Alice', age: 30 }, { name: 'Charlie', age: 35 }]

const byName = R.sortBy(R.prop('name'), users);
// [{ name: 'Alice', ... }, { name: 'Bob', ... }, { name: 'Charlie', ... }]

// Descending sort
const byAgeDesc = R.sort(R.descend(R.prop('age')), users);
```

### R.uniq
```javascript
const numbers = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4];

const unique = R.uniq(numbers);  // [1, 2, 3, 4]

// Unique by property
const users = [
    { id: 1, name: 'Alice' },
    { id: 2, name: 'Bob' },
    { id: 1, name: 'Alice Clone' }
];
const uniqueById = R.uniqBy(R.prop('id'), users);
// [{ id: 1, name: 'Alice' }, { id: 2, name: 'Bob' }]

// Unique with custom equality
const uniqByLength = R.uniqWith((a, b) => a.length === b.length, ['a', 'bb', 'cc', 'ddd']);
// ['a', 'bb', 'ddd']
```

---

## Predicates and Logic

### R.any / R.all
```javascript
const numbers = [1, 2, 3, 4, 5];

const hasEven = R.any(x => x % 2 === 0, numbers);  // true
const allPositive = R.all(x => x > 0, numbers);    // true
const allEven = R.all(x => x % 2 === 0, numbers);  // false

// R.none - opposite of any
const noNegatives = R.none(x => x < 0, numbers);   // true
```

### R.find / R.findIndex
```javascript
const users = [
    { id: 1, name: 'Alice' },
    { id: 2, name: 'Bob' },
    { id: 3, name: 'Charlie' }
];

const bob = R.find(R.propEq('name', 'Bob'), users);
// { id: 2, name: 'Bob' }

const bobIndex = R.findIndex(R.propEq('name', 'Bob'), users);  // 1

const notFound = R.find(R.propEq('name', 'Dave'), users);  // undefined
```

### R.count / R.length
```javascript
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

const total = R.length(numbers);  // 10

// Count elements matching a predicate
const evenCount = R.count(x => x % 2 === 0, numbers);  // 5

// Equivalent using filter + length
const evenCount2 = R.pipe(
    R.filter(x => x % 2 === 0),
    R.length
)(numbers);  // 5
```

### R.min / R.max / R.sum
```javascript
const numbers = [3, 1, 4, 1, 5, 9, 2, 6];

const min = R.reduce(R.min, Infinity, numbers);  // 1
const max = R.reduce(R.max, -Infinity, numbers); // 9
const sum = R.sum(numbers);  // 31
const product = R.product(numbers);  // 2160

// Min/max by property
const users = [
    { name: 'Alice', age: 30 },
    { name: 'Bob', age: 25 },
    { name: 'Charlie', age: 35 }
];

const youngest = R.reduce(R.minBy(R.prop('age')), { age: Infinity }, users);
const oldest = R.reduce(R.maxBy(R.prop('age')), { age: -Infinity }, users);

// Mean/median
const mean = R.mean(numbers);  // 3.875
const median = R.median(numbers);  // 3.5
```

---

## Composition

### R.compose (right to left)
```javascript
// compose applies functions right to left
const processString = R.compose(
    R.toUpper,
    R.trim,
    R.replace(/_/g, ' ')
);

console.log(processString('  hello_world  '));  // 'HELLO WORLD'

// With more complex transformations
const getActiveUserNames = R.compose(
    R.map(R.prop('name')),
    R.filter(R.prop('active'))
);

const users = [
    { name: 'Alice', active: true },
    { name: 'Bob', active: false },
    { name: 'Charlie', active: true }
];

console.log(getActiveUserNames(users));  // ['Alice', 'Charlie']
```

### R.pipe (left to right)
```javascript
// pipe applies functions left to right (more readable)
const processString = R.pipe(
    R.replace(/_/g, ' '),
    R.trim,
    R.toUpper
);

console.log(processString('  hello_world  '));  // 'HELLO WORLD'

// Complex data pipeline
const getTopSalaries = R.pipe(
    R.filter(R.propSatisfies(x => x > 50000, 'salary')),
    R.sortBy(R.prop('salary')),
    R.reverse,
    R.take(3),
    R.map(R.prop('name'))
);

const employees = [
    { name: 'Alice', salary: 60000 },
    { name: 'Bob', salary: 45000 },
    { name: 'Charlie', salary: 80000 },
    { name: 'Diana', salary: 55000 },
    { name: 'Eve', salary: 70000 }
];

console.log(getTopSalaries(employees));  // ['Charlie', 'Eve', 'Alice']
```

---

## Currying and Partial Application

### R.curry
```javascript
// All Ramda functions are auto-curried, but you can curry your own
const add = R.curry((a, b, c) => a + b + c);

console.log(add(1, 2, 3));    // 6
console.log(add(1)(2)(3));    // 6
console.log(add(1, 2)(3));    // 6
console.log(add(1)(2, 3));    // 6

// Practical use
const greet = R.curry((greeting, name) => `${greeting}, ${name}!`);
const sayHello = greet('Hello');
const sayGoodbye = greet('Goodbye');

console.log(sayHello('Alice'));    // 'Hello, Alice!'
console.log(sayGoodbye('Bob'));    // 'Goodbye, Bob!'
```

### R.partial / R.partialRight
```javascript
const greet = (greeting, name, punctuation) => 
    `${greeting}, ${name}${punctuation}`;

// Partial from left
const sayHello = R.partial(greet, ['Hello']);
console.log(sayHello('Alice', '!'));  // 'Hello, Alice!'

// Partial from right
const exclaim = R.partialRight(greet, ['!']);
console.log(exclaim('Hello', 'Alice'));  // 'Hello, Alice!'
```

### R.__ (Placeholder)
```javascript
// Use R.__ as a placeholder for partial application
const divide = R.curry((a, b) => a / b);

const divideBy2 = divide(R.__, 2);  // _ / 2
console.log(divideBy2(10));  // 5

const divide10By = divide(10);  // 10 / _
console.log(divide10By(2));  // 5

// Useful for data-first style
const getEvens = R.filter(R.__, [1, 2, 3, 4, 5, 6]);
console.log(getEvens(x => x % 2 === 0));  // [2, 4, 6]
```

---

## Working with Objects

### R.prop / R.path
```javascript
const user = {
    name: 'Alice',
    address: {
        city: 'Wonderland',
        zip: '12345'
    },
    tags: ['admin', 'user']
};

// Get property
const name = R.prop('name', user);  // 'Alice'

// Get nested property
const city = R.path(['address', 'city'], user);  // 'Wonderland'

// Safe access (returns undefined if not found)
const missing = R.path(['address', 'country'], user);  // undefined

// With default value
const country = R.pathOr('Unknown', ['address', 'country'], user);  // 'Unknown'
```

### R.assoc / R.assocPath
```javascript
const user = { name: 'Alice', age: 30 };

// Set property (returns new object)
const updated = R.assoc('age', 31, user);
// { name: 'Alice', age: 31 }

// Set nested property
const withCity = R.assocPath(['address', 'city'], 'Wonderland', user);
// { name: 'Alice', age: 30, address: { city: 'Wonderland' } }
```

### R.evolve
```javascript
const user = { name: 'alice', age: 30, visits: 5 };

// Transform multiple properties
const transformed = R.evolve({
    name: R.toUpper,
    age: R.inc,
    visits: R.multiply(2)
}, user);
// { name: 'ALICE', age: 31, visits: 10 }
```

### R.pick / R.omit
```javascript
const user = { id: 1, name: 'Alice', password: 'secret', email: 'alice@example.com' };

// Pick specific keys
const public = R.pick(['id', 'name', 'email'], user);
// { id: 1, name: 'Alice', email: 'alice@example.com' }

// Omit specific keys
const safe = R.omit(['password'], user);
// { id: 1, name: 'Alice', email: 'alice@example.com' }
```

### R.merge / R.mergeDeep
```javascript
const defaults = { theme: 'light', language: 'en', notifications: true };
const userPrefs = { theme: 'dark' };

// Shallow merge (right wins)
const merged = R.merge(defaults, userPrefs);
// { theme: 'dark', language: 'en', notifications: true }

// Deep merge
const config1 = { db: { host: 'localhost', port: 5432 } };
const config2 = { db: { port: 3306 }, cache: true };
const deepMerged = R.mergeDeepRight(config1, config2);
// { db: { host: 'localhost', port: 3306 }, cache: true }
```

### R.keys / R.values / R.toPairs
```javascript
const obj = { a: 1, b: 2, c: 3 };

const keys = R.keys(obj);      // ['a', 'b', 'c']
const values = R.values(obj);  // [1, 2, 3]
const pairs = R.toPairs(obj);  // [['a', 1], ['b', 2], ['c', 3]]

// Convert back
const fromPairs = R.fromPairs([['x', 10], ['y', 20]]);  // { x: 10, y: 20 }
```

---

## Grouping and Indexing

### R.groupBy
```javascript
const users = [
    { name: 'Alice', role: 'admin' },
    { name: 'Bob', role: 'user' },
    { name: 'Charlie', role: 'admin' },
    { name: 'Diana', role: 'user' }
];

const byRole = R.groupBy(R.prop('role'), users);
// {
//   admin: [{ name: 'Alice', role: 'admin' }, { name: 'Charlie', role: 'admin' }],
//   user: [{ name: 'Bob', role: 'user' }, { name: 'Diana', role: 'user' }]
// }

// Group numbers by even/odd
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const byParity = R.groupBy(x => x % 2 === 0 ? 'even' : 'odd', numbers);
// { odd: [1, 3, 5, 7, 9], even: [2, 4, 6, 8, 10] }
```

### R.indexBy
```javascript
const users = [
    { id: 1, name: 'Alice' },
    { id: 2, name: 'Bob' },
    { id: 3, name: 'Charlie' }
];

const byId = R.indexBy(R.prop('id'), users);
// {
//   1: { id: 1, name: 'Alice' },
//   2: { id: 2, name: 'Bob' },
//   3: { id: 3, name: 'Charlie' }
// }
```

### R.partition
```javascript
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

const [evens, odds] = R.partition(x => x % 2 === 0, numbers);
// evens: [2, 4, 6, 8, 10]
// odds: [1, 3, 5, 7, 9]

// Partition users
const users = [
    { name: 'Alice', active: true },
    { name: 'Bob', active: false },
    { name: 'Charlie', active: true }
];

const [active, inactive] = R.partition(R.prop('active'), users);
```

---

## Lenses (Immutable Updates)

```javascript
// Lenses provide a way to focus on parts of data structures
const user = {
    name: 'Alice',
    address: {
        city: 'Wonderland',
        zip: '12345'
    }
};

// Create a lens for 'name'
const nameLens = R.lensProp('name');

// View through lens
R.view(nameLens, user);  // 'Alice'

// Set through lens (returns new object)
R.set(nameLens, 'Bob', user);  // { name: 'Bob', address: { ... } }

// Transform through lens
R.over(nameLens, R.toUpper, user);  // { name: 'ALICE', address: { ... } }

// Nested lens
const cityLens = R.lensPath(['address', 'city']);
R.view(cityLens, user);  // 'Wonderland'
R.set(cityLens, 'New York', user);  // { name: 'Alice', address: { city: 'New York', zip: '12345' } }
```

---

## Practical Examples

### Example 1: Data Processing Pipeline
```javascript
const R = require('ramda');

const employees = [
    { name: 'Alice',   department: 'Engineering', salary: 80000, active: true  },
    { name: 'Bob',     department: 'Sales',       salary: 60000, active: false },
    { name: 'Charlie', department: 'Engineering', salary: 90000, active: true  },
    { name: 'Diana',   department: 'Marketing',   salary: 55000, active: true  },
    { name: 'Eve',     department: 'Engineering', salary: 75000, active: true  },
];

// 1. Names of active Engineering employees, sorted
const engNames = R.pipe(
    R.filter(R.prop('active')),
    R.filter(R.propEq('department', 'Engineering')),
    R.map(R.prop('name')),
    R.sort(R.comparator(R.lt))
)(employees);
console.log(engNames);  // ['Alice', 'Charlie', 'Eve']

// 2. Average salary of active Engineering employees
const avgEngSalary = R.pipe(
    R.filter(R.prop('active')),
    R.filter(R.propEq('department', 'Engineering')),
    R.map(R.prop('salary')),
    R.mean
)(employees);
console.log(avgEngSalary);  // 81666.67

// 3. Department headcount and total salary (active only)
const deptSummary = R.pipe(
    R.filter(R.prop('active')),
    R.groupBy(R.prop('department')),
    R.map(emps => ({
        count: emps.length,
        total: R.sum(R.map(R.prop('salary'), emps)),
    }))
)(employees);
console.log(deptSummary);
// {
//   Engineering: { count: 3, total: 245000 },
//   Marketing:   { count: 1, total: 55000  },
// }
```

### Example 2: String Processing
```javascript
const R = require('ramda');

const words = ['hello', 'world', 'functional', 'programming'];

// Transform pipeline
const process = R.pipe(
    R.filter(w => w.length > 5),
    R.map(R.toUpper),
    R.sortBy(R.identity)
);
console.log(process(words));  // ['FUNCTIONAL', 'PROGRAMMING']

// Word frequency
const text = 'the quick brown fox jumps over the lazy dog the fox';
const wordFrequency = R.pipe(
    R.split(' '),
    R.groupBy(R.identity),
    R.map(R.length)
)(text);
console.log(wordFrequency);
// { the: 3, quick: 1, brown: 1, fox: 2, jumps: 1, over: 1, lazy: 1, dog: 1 }

// Top N words
const topWords = R.pipe(
    R.toPairs,
    R.sortBy(R.pipe(R.nth(1), R.negate)),
    R.take(3),
    R.fromPairs
)(wordFrequency);
console.log(topWords);  // { the: 3, fox: 2, quick: 1 }
```

### Example 3: Transforming Nested Data
```javascript
const R = require('ramda');

const orders = [
    {
        id: 1,
        customer: 'Alice',
        items: [
            { product: 'Widget', price: 10, quantity: 2 },
            { product: 'Gadget', price: 25, quantity: 1 }
        ]
    },
    {
        id: 2,
        customer: 'Bob',
        items: [
            { product: 'Widget', price: 10, quantity: 5 }
        ]
    }
];

// Calculate order totals
const orderTotals = R.map(order => ({
    ...order,
    total: R.pipe(
        R.prop('items'),
        R.map(item => item.price * item.quantity),
        R.sum
    )(order)
}), orders);
console.log(orderTotals);
// [{ id: 1, customer: 'Alice', items: [...], total: 45 }, ...]

// Get all products across orders
const allProducts = R.pipe(
    R.chain(R.prop('items')),
    R.map(R.prop('product')),
    R.uniq
)(orders);
console.log(allProducts);  // ['Widget', 'Gadget']
```

### Example 4: Validation Pipeline
```javascript
const R = require('ramda');

// Validators return { valid: boolean, errors: string[] }
const validate = validators => data =>
    R.reduce(
        (acc, validator) => {
            const result = validator(data);
            return {
                valid: acc.valid && result.valid,
                errors: R.concat(acc.errors, result.errors)
            };
        },
        { valid: true, errors: [] },
        validators
    );

const required = field => data => ({
    valid: R.has(field, data) && !R.isEmpty(R.prop(field, data)),
    errors: R.has(field, data) && !R.isEmpty(R.prop(field, data)) 
        ? [] 
        : [`${field} is required`]
});

const minLength = (field, min) => data => ({
    valid: !R.has(field, data) || R.prop(field, data).length >= min,
    errors: !R.has(field, data) || R.prop(field, data).length >= min
        ? []
        : [`${field} must be at least ${min} characters`]
});

const validateUser = validate([
    required('name'),
    required('email'),
    minLength('name', 2),
    minLength('email', 5)
]);

console.log(validateUser({ name: 'A', email: 'a@b' }));
// { valid: false, errors: ['name must be at least 2 characters', 'email must be at least 5 characters'] }

console.log(validateUser({ name: 'Alice', email: 'alice@example.com' }));
// { valid: true, errors: [] }
```

### Example 5: Point-Free Style
```javascript
const R = require('ramda');

// Point-free: functions defined without mentioning arguments
const users = [
    { name: 'Alice', age: 30, premium: true },
    { name: 'Bob', age: 25, premium: false },
    { name: 'Charlie', age: 35, premium: true }
];

// Instead of: users.filter(u => u.premium)
const getPremiumUsers = R.filter(R.prop('premium'));

// Instead of: users.map(u => u.name)
const getNames = R.map(R.prop('name'));

// Instead of: users.filter(u => u.age > 25)
const getOlderThan25 = R.filter(R.propSatisfies(R.gt(R.__, 25), 'age'));

// Compose point-free
const getPremiumNames = R.pipe(
    R.filter(R.prop('premium')),
    R.map(R.prop('name'))
);

console.log(getPremiumNames(users));  // ['Alice', 'Charlie']

// More point-free examples
const isAdult = R.propSatisfies(R.gte(R.__, 18), 'age');
const hasName = R.propSatisfies(R.complement(R.isEmpty), 'name');
const isValidUser = R.allPass([isAdult, hasName]);

console.log(isValidUser({ name: 'Alice', age: 30 }));  // true
console.log(isValidUser({ name: '', age: 30 }));       // false
```

### Example 6: Working with Maybe/Either Pattern
```javascript
const R = require('ramda');

// Simple Maybe implementation
const Maybe = {
    of: x => ({ value: x, isNothing: false }),
    nothing: () => ({ value: null, isNothing: true }),
    fromNullable: x => x == null ? Maybe.nothing() : Maybe.of(x)
};

// Safe property access
const safeProp = R.curry((key, obj) => 
    Maybe.fromNullable(R.prop(key, obj))
);

const safeHead = arr => 
    arr.length === 0 ? Maybe.nothing() : Maybe.of(arr[0]);

// Chain operations
const user = {
    name: 'Alice',
    address: {
        city: 'Wonderland'
    }
};

const getCity = R.pipe(
    safeProp('address'),
    maybe => maybe.isNothing ? maybe : safeProp('city', maybe.value)
);

console.log(getCity(user));  // { value: 'Wonderland', isNothing: false }
console.log(getCity({}));    // { value: null, isNothing: true }

// Using R.pathOr for safe nested access (simpler approach)
const city = R.pathOr('Unknown', ['address', 'city'], user);
console.log(city);  // 'Wonderland'
```

---

## Useful Ramda Functions Reference

```javascript
// Transformation
R.map, R.filter, R.reduce, R.chain, R.flatten, R.unnest

// Composition
R.compose, R.pipe, R.tap, R.identity

// Currying
R.curry, R.partial, R.partialRight, R.__

// Predicates
R.equals, R.gt, R.lt, R.gte, R.lte, R.isEmpty, R.isNil

// Logic
R.and, R.or, R.not, R.both, R.either, R.allPass, R.anyPass

// Object
R.prop, R.path, R.assoc, R.assocPath, R.evolve, R.pick, R.omit, R.merge

// Array
R.head, R.tail, R.last, R.init, R.take, R.drop, R.nth, R.slice

// Grouping
R.groupBy, R.indexBy, R.partition, R.splitEvery

// Math
R.add, R.subtract, R.multiply, R.divide, R.sum, R.product, R.mean, R.median

// String
R.toUpper, R.toLower, R.trim, R.split, R.replace, R.match

// Type
R.is, R.type, R.isNil, R.isEmpty
```

---

## Key Takeaways

1. **Data-Last**: Ramda functions take data as the last argument for easy currying
2. **Auto-Curried**: All functions are curried automatically
3. **Immutable**: Never mutates input - always returns new data
4. **Composable**: Use `R.pipe` (left-to-right) or `R.compose` (right-to-left)
5. **Point-Free**: Functions can be defined without mentioning arguments
6. **Lenses**: For immutable updates to nested structures
7. **Predicates**: Rich set of functions for filtering and testing
