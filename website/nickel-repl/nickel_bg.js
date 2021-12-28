import * as wasm from './nickel_bg.wasm';

const lTextDecoder = typeof TextDecoder === 'undefined' ? (0, module.require)('util').TextDecoder : TextDecoder;

let cachedTextDecoder = new lTextDecoder('utf-8', { ignoreBOM: true, fatal: true });

cachedTextDecoder.decode();

let cachegetUint8Memory0 = null;
function getUint8Memory0() {
    if (cachegetUint8Memory0 === null || cachegetUint8Memory0.buffer !== wasm.memory.buffer) {
        cachegetUint8Memory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachegetUint8Memory0;
}

function getStringFromWasm0(ptr, len) {
    return cachedTextDecoder.decode(getUint8Memory0().subarray(ptr, ptr + len));
}

const heap = new Array(32).fill(undefined);

heap.push(undefined, null, true, false);

let heap_next = heap.length;

function addHeapObject(obj) {
    if (heap_next === heap.length) heap.push(heap.length + 1);
    const idx = heap_next;
    heap_next = heap[idx];

    heap[idx] = obj;
    return idx;
}

function getObject(idx) { return heap[idx]; }

function dropObject(idx) {
    if (idx < 36) return;
    heap[idx] = heap_next;
    heap_next = idx;
}

function takeObject(idx) {
    const ret = getObject(idx);
    dropObject(idx);
    return ret;
}

let WASM_VECTOR_LEN = 0;

const lTextEncoder = typeof TextEncoder === 'undefined' ? (0, module.require)('util').TextEncoder : TextEncoder;

let cachedTextEncoder = new lTextEncoder('utf-8');

const encodeString = (typeof cachedTextEncoder.encodeInto === 'function'
    ? function (arg, view) {
    return cachedTextEncoder.encodeInto(arg, view);
}
    : function (arg, view) {
    const buf = cachedTextEncoder.encode(arg);
    view.set(buf);
    return {
        read: arg.length,
        written: buf.length
    };
});

function passStringToWasm0(arg, malloc, realloc) {

    if (realloc === undefined) {
        const buf = cachedTextEncoder.encode(arg);
        const ptr = malloc(buf.length);
        getUint8Memory0().subarray(ptr, ptr + buf.length).set(buf);
        WASM_VECTOR_LEN = buf.length;
        return ptr;
    }

    let len = arg.length;
    let ptr = malloc(len);

    const mem = getUint8Memory0();

    let offset = 0;

    for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset);
        if (code > 0x7F) break;
        mem[ptr + offset] = code;
    }

    if (offset !== len) {
        if (offset !== 0) {
            arg = arg.slice(offset);
        }
        ptr = realloc(ptr, len, len = offset + arg.length * 3);
        const view = getUint8Memory0().subarray(ptr + offset, ptr + len);
        const ret = encodeString(arg, view);

        offset += ret.written;
    }

    WASM_VECTOR_LEN = offset;
    return ptr;
}

function isLikeNone(x) {
    return x === undefined || x === null;
}

let cachegetInt32Memory0 = null;
function getInt32Memory0() {
    if (cachegetInt32Memory0 === null || cachegetInt32Memory0.buffer !== wasm.memory.buffer) {
        cachegetInt32Memory0 = new Int32Array(wasm.memory.buffer);
    }
    return cachegetInt32Memory0;
}
/**
* Return a new instance of the WASM REPL, with the standard library loaded.
* @returns {WASMInitResult}
*/
export function repl_init() {
    var ret = wasm.repl_init();
    return WASMInitResult.__wrap(ret);
}

function _assertClass(instance, klass) {
    if (!(instance instanceof klass)) {
        throw new Error(`expected instance of ${klass.name}`);
    }
    return instance.ptr;
}
/**
* Evaluate an input in the WASM REPL.
* @param {ReplState} state
* @param {string} line
* @returns {WASMInputResult}
*/
export function repl_input(state, line) {
    _assertClass(state, ReplState);
    var ptr0 = passStringToWasm0(line, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    var len0 = WASM_VECTOR_LEN;
    var ret = wasm.repl_input(state.ptr, ptr0, len0);
    return WASMInputResult.__wrap(ret);
}

/**
* Evaluate an input in the WASM REPL and serialize it.
* @param {ReplState} state
* @param {any} format
* @param {string} line
* @returns {WASMInputResult}
*/
export function repl_serialize(state, format, line) {
    _assertClass(state, ReplState);
    var ptr0 = passStringToWasm0(line, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    var len0 = WASM_VECTOR_LEN;
    var ret = wasm.repl_serialize(state.ptr, addHeapObject(format), ptr0, len0);
    return WASMInputResult.__wrap(ret);
}

/**
* Return codes of the WASM REPL.
*
* wasm-bindgen doesn't support exporting arbitrary enumeration. Thus we have to encode these
* enums as structures with a tag and values. The values that are actually set depend on the
* tag.
*/
export const WASMResultTag = Object.freeze({ Success:0,"0":"Success",Blank:1,"1":"Blank",Partial:2,"2":"Partial",Error:3,"3":"Error", });
/**
* WASM-compatible wrapper around `ReplImpl`.
*/
export class ReplState {

    static __wrap(ptr) {
        const obj = Object.create(ReplState.prototype);
        obj.ptr = ptr;

        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.ptr;
        this.ptr = 0;

        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_replstate_free(ptr);
    }
}
/**
* WASM wrapper for the result type of the initialization of the REPL.
*/
export class WASMInitResult {

    static __wrap(ptr) {
        const obj = Object.create(WASMInitResult.prototype);
        obj.ptr = ptr;

        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.ptr;
        this.ptr = 0;

        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_wasminitresult_free(ptr);
    }
    /**
    * @returns {number}
    */
    get tag() {
        var ret = wasm.__wbg_get_wasminitresult_tag(this.ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set tag(arg0) {
        wasm.__wbg_set_wasminitresult_tag(this.ptr, arg0);
    }
    /**
    * @returns {string}
    */
    get msg() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.wasminitresult_msg(retptr, this.ptr);
            var r0 = getInt32Memory0()[retptr / 4 + 0];
            var r1 = getInt32Memory0()[retptr / 4 + 1];
            return getStringFromWasm0(r0, r1);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
            wasm.__wbindgen_free(r0, r1);
        }
    }
    /**
    * @returns {ReplState}
    */
    repl() {
        const ptr = this.__destroy_into_raw();
        var ret = wasm.wasminitresult_repl(ptr);
        return ReplState.__wrap(ret);
    }
}
/**
* WASM wrapper for the result type of an execution of the REPL.
*/
export class WASMInputResult {

    static __wrap(ptr) {
        const obj = Object.create(WASMInputResult.prototype);
        obj.ptr = ptr;

        return obj;
    }

    __destroy_into_raw() {
        const ptr = this.ptr;
        this.ptr = 0;

        return ptr;
    }

    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_wasminputresult_free(ptr);
    }
    /**
    * @returns {number}
    */
    get tag() {
        var ret = wasm.__wbg_get_wasminputresult_tag(this.ptr);
        return ret >>> 0;
    }
    /**
    * @param {number} arg0
    */
    set tag(arg0) {
        wasm.__wbg_set_wasminputresult_tag(this.ptr, arg0);
    }
    /**
    * @returns {string}
    */
    get msg() {
        try {
            const retptr = wasm.__wbindgen_add_to_stack_pointer(-16);
            wasm.wasminputresult_msg(retptr, this.ptr);
            var r0 = getInt32Memory0()[retptr / 4 + 0];
            var r1 = getInt32Memory0()[retptr / 4 + 1];
            return getStringFromWasm0(r0, r1);
        } finally {
            wasm.__wbindgen_add_to_stack_pointer(16);
            wasm.__wbindgen_free(r0, r1);
        }
    }
    /**
    * @returns {any}
    */
    get errors() {
        var ret = wasm.wasminputresult_errors(this.ptr);
        return takeObject(ret);
    }
}

export function __wbindgen_json_parse(arg0, arg1) {
    var ret = JSON.parse(getStringFromWasm0(arg0, arg1));
    return addHeapObject(ret);
};

export function __wbindgen_object_drop_ref(arg0) {
    takeObject(arg0);
};

export function __wbindgen_object_clone_ref(arg0) {
    var ret = getObject(arg0);
    return addHeapObject(ret);
};

export function __wbindgen_string_get(arg0, arg1) {
    const obj = getObject(arg1);
    var ret = typeof(obj) === 'string' ? obj : undefined;
    var ptr0 = isLikeNone(ret) ? 0 : passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
    var len0 = WASM_VECTOR_LEN;
    getInt32Memory0()[arg0 / 4 + 1] = len0;
    getInt32Memory0()[arg0 / 4 + 0] = ptr0;
};

export function __wbindgen_throw(arg0, arg1) {
    throw new Error(getStringFromWasm0(arg0, arg1));
};

