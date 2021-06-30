import * as React from 'react';
import {repl_init, repl_input} from "nickel-repl";
import Ansi from "ansi-to-react";
import {EDITOR_SEND_EVENT, REPL_RUN_EVENT} from "./events";
import modes from './modes';

/**
 * An REPL. This component can run Nickel programs or REPL commands and display a stylized output.
 */
export default class Repl extends React.Component {
    constructor(props, context) {
        super(props, context);
        this.state = {
            lastInput: '',
            // Output displayed in REPL mode. It is appended to at each run.
            output_repl: "Nickel Online REPL | Welcome to the Nickel online REPL.\n"
                + "See the output of your snippets here.\n\n",
            // Output displayed in serialize mode. Cleared at each new run.
            output_serialize: '',
        };
        this.endRef = React.createRef();
    }

    static defaultProps = {
        mode: modes.REPL
    };

    /**
     * Return the current REPL output as an array of lines.
     * @returns {string[]}
     */
    lines() {
        return this.state.output_repl.split(/\r?\n/g);
    }

    componentDidMount() {
        const result = repl_init();

        if (result.tag === nickelCodes.result.ERROR) {
            this.write(`Initialization error: ${result.msg}\n`);
        } else {
            // /!\ WARNING: result is moved by the Rust code when calling to the repl() method. Do not use or copy result after this call to repl().
            this.repl = result.repl();
            this.prompt();
        }
        document.addEventListener(EDITOR_SEND_EVENT, this.onSend.bind(this))
    }

    /**
     * Write text. Newlines and ANSI escape codes are converted to HTML before rendering.
     * In serialize mode, the new output erase the old content. In REPL mode, the new output is appended to.
     * Because state updates are asynchronous, this returns a Promise that resolves when everything is up to date.
     * @param data String
     * @returns {Promise<unknown>}
     */
    write(data) {
        return new Promise(resolve => {
                if (this.props.mode === modes.REPL) {
                    this.setState({output_repl: this.state.output_repl + data}, resolve);
                } else {
                    this.setState({output_serialize: data}, resolve);
                }
            }
        );
    }

    /**
     * Clear the output.
     * @returns {Promise<unknown>}
     */
    clear() {
        return new Promise(resolve => {
                if (this.props.mode === modes.REPL) {
                    this.setState({output_repl: ''}, resolve);
                } else {
                    this.setState({output_serialize: ''}, resolve);
                }
            }
        ).then(() => this.prompt());
    }

    /**
     * Write a new line followed by a prompt, if in REPL mode. Do nothing otherwise.
     * @returns {Promise<unknown>}
     */
    prompt = () => {
        if (this.props.mode === modes.REPL) {
            return this.write('\n\u001b[32mnickel>\u001b[0m ');
        }
    };

    /**
     * Run it. In REPL mode, the input is also appended to the output.
     * @param input String
     */
    onSend = ({detail: input}) => {
        if (this.props.mode === modes.REPL) {
            return this.write(input).then(() => this.run(input));
        } else {
            return this.run(input);
        }
    };

    /**
     * Unescape serialized output_repl. To serialize an input, the REPL component wraps the program in a call to `builtins.serialize`.
     * The returned result is a Nickel string with escaped characters. This function unescapes them.
     * @param result String
     * @returns String
     */
    unescape = (result) => {
        if (result.charAt(0) === '"' && result.slice(result.length - 2, result.length) === '"\n') {
            return result.slice(1, result.length - 2).replaceAll('\\"', '"').replaceAll('\\\\', '\\')
        } else {
            return result;
        }
    };

    /**
     * Run an input and write the result in the output.
     * @param input String
     * @returns {Promise<number>} A promise resolving to the return code of the execution of the Nickel REPL, or -1 if the REPL wasn't loaded.
     */
    run = (input) => {
        if (this.repl === null) {
            console.error("Terminal: REPL is not loaded (this.repl === null)");
            return new Promise(resolve => resolve(-1));
        }

        this.setState({lastInput: input});

        if (this.props.mode !== modes.REPL) {
            const format = this.props.mode.charAt(0).toUpperCase() + this.props.mode.slice(1);
            input = `builtins.serialize \`${format} (${input})`;
        }

        let result = repl_input(this.repl, input);
        let task;

        if (this.props.mode === modes.REPL) {
            task = this.write("\n" + result.msg).then(() => this.prompt());
        } else {
            //If there's an error, we run the original snippet in order to have a better error message.
            if (result.tag === nickelCodes.result.ERROR) {
                const resultAlone = repl_input(this.repl, this.state.lastInput);

                // If there's no error for the original snippet alone, this may be a NonSerializable error. In this case,
                // we keep the first error message.
                if (resultAlone.tag === nickelCodes.result.ERROR) {
                    result = resultAlone;
                }

                task = this.write(result.msg);
            } else {
                task = this.write(this.unescape(result.msg));
            }
        }

        // Dispatch the result as an event, so that the editor or other components can react to the outcome of the last input
        const event = new CustomEvent(REPL_RUN_EVENT, {detail: result});
        document.dispatchEvent(event);

        return task.then(() => result.tag);
    };

    componentDidUpdate = (prevProps) => {
        // If we switched mode to a serialization mode and there is a last input, we re-run the last input
        if (this.props.mode !== prevProps.mode && this.props.mode !== modes.REPL && this.state.lastInput !== '') {
            this.run(this.state.lastInput);
        }

        // Scroll to the last message
        const terminalContainer = document.getElementById(this.props.containerId);
        terminalContainer.scrollTop = terminalContainer.scrollHeight;
    };

    render() {
        let content;

        if (this.props.mode === modes.REPL) {
            content = this.lines().map((line, index) => <div key={index}><Ansi useClasses>{line.toString()}</Ansi><br/>
            </div>);
        } else {
            content = <Ansi useClasses>{this.state.output_serialize}</Ansi>;
        }

        return <div style={{whiteSpace: 'pre'}}>
            {content}
            <div ref={this.endRef}/>
        </div>
    }
}