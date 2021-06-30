/**
 * Codes returned by the Nickel WASM evaluator.
 * @type {{result: {BLANK: number, SUCCESS: number, PARTIAL: number, ERROR: number}, error: {severity: {HELP: number, BUG: number, NOTE: number, ERROR: number, WARNING: number}, label: {SECONDARY: number, PRIMARY: number}}}}
 */
export default {
    result: {
        SUCCESS: 0,
        BLANK: 1,
        PARTIAL: 2,
        ERROR: 3,
    },
    error: {
        severity: {
            HELP: 1,
            NOTE: 2,
            WARNING: 3,
            ERROR: 4,
            BUG: 5,
        },
        label: {
            PRIMARY: 0,
            SECONDARY: 1,
        }
    }
};