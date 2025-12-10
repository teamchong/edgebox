// Test if QuickJS os.Worker is available

console.log('Testing os.Worker availability...');

if (typeof os !== 'undefined') {
    console.log('os module available');
    console.log('os.Worker:', typeof os.Worker);
    if (typeof os.Worker === 'function') {
        console.log('os.Worker is available!');
    } else {
        console.log('os.Worker not available');
    }
} else {
    console.log('os module not available');
}

// Try std module
if (typeof std !== 'undefined') {
    console.log('std module available');
} else {
    console.log('std module not available');
}

// Check _os (QuickJS internal)
if (typeof _os !== 'undefined') {
    console.log('_os module available');
    console.log('_os.Worker:', typeof _os.Worker);
} else {
    console.log('_os module not available');
}
