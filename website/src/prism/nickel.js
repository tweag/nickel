/**
 * Simple language definition for Nickel to use with the Prism highlighting library.
 * @type {{number: RegExp, string: RegExp[], builtin: RegExp, punctuation: RegExp[], comment: RegExp, keyword: RegExp[], operator: RegExp[]}}
 */
const nickel = {
    comment: /\/\/.+/,
    string: [
        /m(#)+".*"\1m/,
        /".+"/,
    ],
    operator: [
        />/, />=/, /</, /<=/, /&/, /==/, /&&/, /\|\|/, /!/, /\+/, /@/, /-/, /\+\+/,
    ],
    keyword: [
        /let/,
        /in/,
        /fun/,
        /switch/,
        /forall/,
    ],
    punctuation: [
        /:/, /,/, /;/, /\{/, /}/, /\(/, /\)/, /=/, /\|/, /#/,
    ],
    number: /[0-9]*\.?[0-9]+/,
    builtin: /((?:Dyn)|(?:Num)|(?:Bool)|(?:Str)|(?:List:(?:[a-zA-Z0-9_]*)?))/,
};

export default nickel;