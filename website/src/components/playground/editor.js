import * as React from 'react';
import AceEditor from 'react-ace';
import nickelCodes from './nickel-codes'
import {PLAYGROUND_SEND_EVENT, EDITOR_SEND_EVENT, REPL_RUN_EVENT} from "./events";

import "ace-builds/src-noconflict/theme-solarized_dark";
import "../../ace-nickel-mode/ace-nickel-mode";
import ReactDOMServer from "react-dom/server";
import {wrapPageElement} from "gatsby/dist/utils/api-browser-docs";
import modes from "./modes";

/**
 * Nickel code editor component, based on the Ace editor.
 */
export default class Editor extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
            value: this.props.value,
            name: this.props.name,
            placeholder: 'Write your code. Press Ctrl+Enter (Cmd+Enter) to run it',
            theme: "solarized_dark",
            mode: "nickel",
            height: "100%",
            width: "100%",
            enableBasicAutocompletion: false,
            enableLiveAutocompletion: false,
            fontSize: 14,
            showGutter: true,
            showPrintMargin: true,
            highlightActiveLine: true,
            enableSnippets: false,
            showLineNumbers: true,
            annotations: [],
            wrapEnabled: true,
        };

        if(this.props.fit && this.props.fit === 'code') {
            // Taking more lines to account for potential wrapping
            const lines = this.props.value.split(/\r?\n/g).length+2;
            this.state.maxLines = lines;
            this.state.minLines = lines;
        }
        else if(this.props.fit && this.props.fit === 'lines' && this.props.lines) {
            this.state.maxLines = this.props.lines;
            this.state.minLines = this.props.lines;
        }

        this.onChange = this.onChange.bind(this);
        this.onREPLRun = this.onREPLRun.bind(this);
        this.send = this.send.bind(this);
        this.aceEditorRef = React.createRef();
    }

    static defaultProps = {
        value: `let data = {value = "Hello," ++ " world!"} in data.value`,
        name: 'nickel-repl-input',
    };

    componentDidMount() {
        // Listen to the REPL's execution events, in order to update potential error messages.
        document.addEventListener(REPL_RUN_EVENT, this.onREPLRun);
        document.addEventListener(PLAYGROUND_SEND_EVENT, this.send);

        if(this.props.onResize) {
            const ro = new ResizeObserver(entries => {
                for(let entry of entries) {
                    this.props.onResize(entry.target.clientHeight);
                }
            });
            ro.observe(document.getElementById(this.state.name));
        }
     }

    onChange(newValue) {
        this.setState({
            value: newValue
        });
    }

    getHeight() {
        return document.getElementById(this.state.name).clientHeight;
    }

    /**
     * Static component displaying a Nickel diagnostic error.
     * @param diagnostic
     * @param label
     * @returns {*}
     */
    annotationWidget(diagnostic, label) {
        const labelClass = label.style === nickelCodes.error.label.PRIMARY ? 'ansi-red-fg' : 'ansi-blue-fg';
        return (<div>
            <span className={"ansi-bright-red-fg"}>{diagnostic.msg}</span><br/>
            <span className={labelClass}>{label.msg}</span><br/>
            <ul>
                {diagnostic.notes.map(note => <li>{note}</li>)}
            </ul>
        </div>)
    }

    /**
     * Once the REPL has run, update the error messages.
     * @param result
     */
    onREPLRun({detail: result}) {
        if (result.tag === nickelCodes.result.ERROR) {
            const annotations = result.errors.filter(diagnostic => diagnostic.severity >= nickelCodes.error.severity.WARNING)
                .map(diagnostic => (
                    diagnostic.labels.map(label => ({
                        row: label.line_start,
                        column: label.col_start,
                        html: ReactDOMServer.renderToStaticMarkup(this.annotationWidget(diagnostic, label)),
                        type: label.style === nickelCodes.error.label.PRIMARY ? 'error' : 'warning',
                    }))
                )).flat();

            // In some obscure circumstances (annotation on the last line, and then insertion of a new line), annotations disappear, even if the user send the same input again.
            // To avoid this and make annotations reappear at least when sending an input, we clear the old one first, to triggers reactive updates.
            this.setState({annotations: []}, () => this.setState({annotations}));
        } else {
            this.setState({annotations: []});
        }
    }

    /**
     * Dispatch an EDITOR_SEND_EVENT with the current content as a payload.
     */
    send() {
        // Dispatch the result as an event, so that the editor or other components can react to the outcome of the last input
        const event = new CustomEvent(EDITOR_SEND_EVENT, {detail: this.state.value});
        document.dispatchEvent(event);
    }

    render() {
        const setEditorMargin = (editor) => {
            editor.renderer.setPadding(10);
            editor.renderer.setScrollMargin(10, 10, 0, 0);
        };

        return <AceEditor
            ref={this.aceEditorRef}
            placeholder={this.state.placeholder}
            mode={this.state.mode}
            theme={this.state.theme}
            name={this.state.name}
            height={this.state.height}
            width={this.state.width}
            minLines={this.state.minLines}
            maxLines={this.state.maxLines}
            onChange={this.onChange}
            onSelectionChange={this.onSelectionChange}
            onCursorChange={this.onCursorChange}
            onValidate={this.onValidate}
            value={this.state.value}
            annotations={this.state.annotations}
            fontSize={this.state.fontSize}
            showPrintMargin={this.state.showPrintMargin}
            showGutter={this.state.showGutter}
            highlightActiveLine={this.state.highlightActiveLine}
            wrapEnabled={this.state.wrapEnabled}
            onLoad={setEditorMargin}
            commands={[
                {
                    name: 'send-repl',
                    bindKey: {
                        win: 'Ctrl-enter',
                        mac: 'Cmd-enter',
                    },
                    exec: this.send,
                },
            ]}
            setOptions={{
                useWorker: false,
                enableBasicAutocompletion: this.state.enableBasicAutocompletion,
                enableLiveAutocompletion: this.state.enableLiveAutocompletion,
                enableSnippets: this.state.enableSnippets,
                showLineNumbers: this.state.showLineNumbers,
                tabSize: 2
            }}
        />;
    }
}