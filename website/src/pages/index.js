import * as React from "react"
import Layout from "../components/layout"
import {StaticImage} from "gatsby-plugin-image";
import mergeImage from '../images/merge-2.png';
import validateImage from '../images/validate-2.png';
import reuseImage from '../images/reuse-2.png';
import PlaygroundComponent from "../components/playground";
import {modes} from "../components/repl";
import {Command} from "react-bootstrap-icons";

const codeExample = `let conf = {
  name = "NiCl",
  version  = "0.0.1$",
  description = "My cool app!"
} in

let SemanticVersion = fun label value =>
  let pattern = "^\\\\d{1,2}\\\\.\\\\d{1,2}(\\\\.\\\\d{1,2})?$" in
  if strings.isMatch value pattern then
    value
  else
    let msg = "invalid version number" in
    contracts.blame (contracts.tag msg label)
  in

let AppSchema = {
  name | Str,
  version | #SemanticVersion,
  description | Str,
} in

conf | #AppSchema`

const IndexPage = () => {
  return (
      <Layout>
        <main className="container main-container">
          <section className="row first-section-block">
            <div className="col-12 text-center">
              <h1 className="main-title mb-4"><StaticImage className={"logo"} src="../images/nickel-logo-2.svg" alt="logo"/><span className="nickel">Nickel</span></h1>
              <div className="main-subtitle mt-4 mb-4 title-font">Better configuration
                for less
              </div>

              <div className="mt-4 mb-4 main-text">
                Write complex configurations. Modular, correct and boilerplate-free.
              </div>
            </div>
          </section>

          <hr className={'horizontal-sep'}/>

          <section className={'row section-block'}>
            <div className="col-12 text-center">
              <h2 className="mb-4">Try it out. Find the error!</h2>
              <div className="mt-4 mb-4 main-text">
                This configuration contains an error. Fix it and type <kbd>Ctrl</kbd>+<kbd>Enter</kbd> (or <kbd>Cmd <Command/>
              </kbd>+<kbd>Enter</kbd>) or click <span className={'btn btn-primary disabled'}>Run</span> to try your solution.
              </div>
              <div className={'text-left landingpage-playground-wrapper'}>
                <PlaygroundComponent value={codeExample} fit={'code'} mode={modes.JSON}/>
              </div>
            </div>
          </section>

          <hr className={'horizontal-sep'}/>

          <section className="row last-section-block">
            <div className="col-4 main-text text-center landingpage-column">
              <img src={mergeImage} className="abstract-illustration" alt={""}/>
                <h3 className="mb-4 mt-4">Merge</h3>
                <div className="text-left mt-4">
                    Write simple, modular blocks. Merge them into a complex configuration.
                </div>
            </div>
            <div className="col-4 main-text text-center landingpage-column">
              <img src={validateImage} className="abstract-illustration" alt={""}/>
                <h3 className="mb-4 mt-4">Verify & Validate</h3>
              <div className="text-left mt-4">
                <p>Use (opt-in) static typing to verify functions, if you need to. Let
                type inference do the boring work.</p>

                <p>Use contracts
                  to validate your data and ensure they conform to a given schema.</p>
              </div>
            </div>
            <div className="col-4 main-text text-center landingpage-column">
              <img src={reuseImage} className="abstract-illustration" alt={""}/>
                <h3 className="mb-4 mt-4">Reuse</h3>
              <div className="text-left lt-4">
                Don't hack, don't reinvent the wheel: Nickel is a
                programming language. Factorize. Reuse the generic parts. Import external libraries.
              </div>
            </div>
          </section>
        </main>
      </Layout>
  )
};

export default IndexPage
