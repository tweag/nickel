/* tslint:disable */
/* eslint-disable */
/**
* Return a new instance of the WASM REPL, with the standard library loaded.
* @returns {WASMInitResult}
*/
export function repl_init(): WASMInitResult;
/**
* Evaluate an input in the WASM REPL.
* @param {REPLState} state
* @param {string} line
* @returns {WASMInputResult}
*/
export function repl_input(state: REPLState, line: string): WASMInputResult;
/**
* Return codes of the WASM REPL.
*
* wasm-bindgen doesn't support exporting arbitrary enumeration. Thus we have to encode these
* enums as structures with a tag and values. The values that are actually set depend on the
* tag.
*/
export enum WASMResultTag {
  Success,
  Blank,
  Partial,
  Error,
}
/**
* WASM-compatible wrapper around `REPLImpl`.
*/
export class REPLState {
  free(): void;
}
/**
* WASM wrapper for the result type of the initialization of the REPL.
*/
export class WASMInitResult {
  free(): void;
/**
* @returns {REPLState}
*/
  repl(): REPLState;
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
export class WASMInputResult {
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
