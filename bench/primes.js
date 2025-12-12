// Prime counting benchmark - Loop-heavy, non-recursive
// Tests general-purpose frozen function optimization

function countPrimes(max) {
    let count = 0;
    for (let i = 2; i < max; i++) {
        let isPrime = true;
        for (let j = 2; j * j <= i; j++) {
            if (i % j === 0) {
                isPrime = false;
                break;
            }
        }
        if (isPrime) count++;
    }
    return count;
}

// Count primes up to 100000
const EXPECTED = 9592; // Number of primes below 100000

const start = Date.now();
const result = countPrimes(100000);
const elapsed = Date.now() - start;

if (result !== EXPECTED) {
    console.log(`FAIL: countPrimes(100000) = ${result}, expected ${EXPECTED}`);
    if (typeof process !== 'undefined' && process.exit) process.exit(1);
} else {
    console.log(`OK: countPrimes(100000) = ${result} (${elapsed}ms)`);
}
