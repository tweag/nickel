/* tslint:disable */
/* eslint-disable */
/**
* Return a new instance of the WASM REPL, with the standard library loaded.
* @returns {WasmInitResult}
*/
export function repl_init(): WasmInitResult;
/**
* Evaluate an input in the WASM REPL.
* @param {ReplState} state
* @param {string} line
* @returns {WasmInputResult}
*/
export function repl_input(state: ReplState, line: string): WasmInputResult;
/**
* Evaluate an input in the WASM REPL and serialize it.
* @param {ReplState} state
* @param {any} format
* @param {string} line
* @returns {WasmInputResult}
*/
export function repl_serialize(state: ReplState, format: any, line: string): WasmInputResult;
/**
* Return codes of the WASM REPL.
*
* wasm-bindgen doesn't support exporting arbitrary enumeration. Thus we have to encode these
* enums as structures with a tag and values. The values that are actually set depend on the
* tag.
*/
export enum WasmResultTag {
  Success,
  Blank,
  Partial,
  Error,
}
/**
* WASM-compatible wrapper around `ReplImpl`.
*/
export class ReplState {
  free(): void;
}
/**
* WASM wrapper for the result type of the initialization of the REPL.
*/
export class WasmInitResult {
  free(): void;
/**
* @returns {ReplState}
*/
  repl(): ReplState;
/**
* @returns {string}
*/
  readonly msg: string;
/**
* @returns {number}
*/
  tag: number;
}
/**
* WASM wrapper for the result type of an execution of the REPL.
*/
export class WasmInputResult {
  free(): void;
/**
* @returns {any}
*/
  readonly errors: any;
/**
* @returns {string}
*/
  readonly msg: string;
/**
* @returns {number}
*/
  tag: number;
}
