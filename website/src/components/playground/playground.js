import * as React from 'react';

import Editor from "./editor";
import Repl from "./repl";
import modes from "./modes";
import {PLAYGROUND_SEND_EVENT} from "./events.js";
import {Command} from 'react-bootstrap-icons';

/**
 * Playground component, composed of both a code editor and a REPL component.
 */
export default class Playground extends React.Component {
    constructor(props) {
        super(props);

        this.state = {mode: this.props.mode};
        this.setMode = this.setMode.bind(this);
        this.dispatchSendEvent = this.dispatchSendEvent.bind(this);

        if(this.props.fit === 'code' || this.props.fit === 'lines') {
            // In fit-to-code mode, fix the height of the output (terminal) element to the height of the current code
            this.onEditorResize = (height) => {
                if(this.terminalContainer) {
                    this.terminalContainer.style.height = height + "px";
                }
                else {
                    setTimeout(this.onEditorResize, 100)
                }
            }
        }
    }

    componentDidMount() {
        // If a program was provided initially, run it.
        if(this.props.value) {
            this.editor.send();
        }
    }

    static defaultProps = {
        mode: modes.REPL,
        fit: 'page',
    };

    setTerminalContainer = element => this.terminalContainer = element;
    setEditor = editor => this.editor = editor;

    replTabStyle = (mode) => ('nav-link link-secondary playground-nav-item' + (this.state.mode === mode ? ' active' : ''));

    setMode = (mode) => {
        this.setState({mode});
    };

    dispatchSendEvent = () => {
        const event = new CustomEvent(PLAYGROUND_SEND_EVENT);
        document.dispatchEvent(event);
    };

    render() {
        return <React.Fragment>
            <div className={"row"}>
                <div className={"col-6 playground-tab d-flex align-items-center"}>
                    <div>
                        <button className={"btn btn-primary"} onClick={() => this.dispatchSendEvent()}>Run</button>
                        <span className={'ml-4'}> or press</span> <kbd>Ctrl</kbd>+<kbd>Enter</kbd> or <kbd>Cmd <Command/>
                    </kbd>+<kbd>Enter</kbd>
                    </div>
                </div>
                <ul className={"col-6 nav nav-pills playground-tab"}>
                    <li className="nav-item">
                        <a className={this.replTabStyle(modes.REPL)} onClick={() => this.setMode(modes.REPL)}>REPL</a>
                    </li>
                    <li className="nav-item">
                        <a className={this.replTabStyle(modes.JSON)} onClick={() => this.setMode(modes.JSON)}>JSON</a>
                    </li>
                    <li className="nav-item">
                        <a className={this.replTabStyle(modes.YAML)} onClick={() => this.setMode(modes.YAML)}>YAML</a>
                    </li>
                    <li className="nav-item">
                        <a className={this.replTabStyle(modes.TOML)} onClick={() => this.setMode(modes.TOML)}>TOML</a>
                    </li>
                </ul>
            </div>

            <section className={'row playground-container overflow-hidden flex-grow-1'}>
                <div className={'col-6'}>
                    <Editor ref={this.setEditor} fit={this.props.fit} lines={this.props.lines} value={this.props.value} onResize={this.onEditorResize}/>
                </div>
                <div id={"playground-terminal-container"}
                     ref={this.setTerminalContainer}
                     className={'col-6 ansi-monokai playground-terminal-container'}>
                    <Repl containerId={"playground-terminal-container"} className={'playground-terminal'}
                          mode={this.state.mode}/>
                </div>
            </section>
        </React.Fragment>
    }
}