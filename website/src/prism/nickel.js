export default {
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