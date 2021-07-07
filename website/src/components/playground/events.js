/**
 * The user request the execution of the current snippet (by clicking the "Run" button, or using the corresponding shortcut).
 * @type {string}
 */
const PLAYGROUND_SEND_EVENT = 'playground:send';
/**
 * The editor is sending a snippet for execution (by clicking the "Run" button, or usign the corresponding shortcut). Editor listens to `PLAYGROUND_SEND_EVENT`,
 * and dispatch `EDITOR_SEND_ENVENT`. The payload (field `detail` of the event) contains the input as a string.
 * @type {string}
 */
const EDITOR_SEND_EVENT = 'nickel-repl:send';
/**
 * The REPL has run a snippet and returned. The `details`
 * @type {string}
 */
const REPL_RUN_EVENT = 'nickel-repl:run';

export {PLAYGROUND_SEND_EVENT, EDITOR_SEND_EVENT, REPL_RUN_EVENT};