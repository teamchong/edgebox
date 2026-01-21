// EdgeBox Runtime: crypto (getRandomValues, randomUUID)

globalThis.crypto = globalThis.crypto || {
    getRandomValues(array) {
        for (let i = 0; i < array.length; i++) {
            if (array instanceof Uint8Array) array[i] = Math.floor(Math.random() * 256);
            else if (array instanceof Uint16Array) array[i] = Math.floor(Math.random() * 65536);
            else if (array instanceof Uint32Array) array[i] = Math.floor(Math.random() * 4294967296);
            else array[i] = Math.floor(Math.random() * 256);
        }
        return array;
    },
    randomUUID() {
        const bytes = new Uint8Array(16);
        crypto.getRandomValues(bytes);
        bytes[6] = (bytes[6] & 0x0f) | 0x40;
        bytes[8] = (bytes[8] & 0x3f) | 0x80;
        const hex = Array.from(bytes, b => b.toString(16).padStart(2, '0')).join('');
        return hex.slice(0,8) + '-' + hex.slice(8,12) + '-' + hex.slice(12,16) + '-' + hex.slice(16,20) + '-' + hex.slice(20);
    },
};
