import * as React from 'react';
import AceEditor from 'react-ace';
import {nickelCodes, REPL_RUN_EVENT} from './repl'
import {PLAYGROUND_SEND_EVENT} from "./playground";

import "ace-builds/src-noconflict/theme-solarized_dark";
import "../ace-nickel-mode/ace-nickel-mode";
import ReactDOMServer from "react-dom/server";

const EDITOR_SEND_EVENT = 'nickel-repl:send';

/**
 * Nickel code editor component, based on the Ace editor.
 */
export default class Editor extends React.Component {
    constructor(props) {
        super(props);

        const value = this.props.value ? this.props.value : `let data = {value = "Hello," ++ " world!"} in data.value`;

        this.state = {
            value,
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
        };

        if(this.props.fit && this.props.fit === 'code') {
            const lines = value.split(/\r?\n/g).length;
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

    componentDidMount() {
        // Listen to the REPL's execution events, in order to update potential error messages.
        document.addEventListener(REPL_RUN_EVENT, this.onREPLRun);
        document.addEventListener(PLAYGROUND_SEND_EVENT, this.send);
    }

    onChange(newValue) {
        this.setState({
            value: newValue
        });
    }

    getHeight() {
        return this.aceEditorRef.current.editor.getSession().getScreenLength() * this.aceEditorRef.current.editor.renderer.lineHeight + this.aceEditorRef.current.editor.renderer.scrollBarH.height;
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
        return <AceEditor
            ref={this.aceEditorRef}
            placeholder={this.state.placeholder}
            mode={this.state.mode}
            theme={this.state.theme}
            name={"nickel-repl-input"}
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

export {Editor, EDITOR_SEND_EVENT};