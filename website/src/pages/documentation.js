import * as React from "react"
import Layout from "../components/layout"
import { Link } from "gatsby"

const IndexPage = () => {
  return (
      <Layout>
        <main className="container main-container">
          <div className="row">
            <h1 className="main-title col-12">Documentation</h1>

            <div className="mt-4 col-12">
              <div className="list-group">
                <Link to="https://github.com/tweag/nickel/#readme" className="list-group-item list-group-item-action">The
                  Nickel README</Link>
                <Link to="https://github.com/tweag/nickel/blob/master/RATIONALE.md"
                      className="list-group-item list-group-item-action">Design rationale</Link>
                <Link to="#" className="list-group-item list-group-item-action
                disabled">Tutorials<span className={'text-primary'}> - coming soon</span></Link>
                <Link to="#" className="list-group-item list-group-item-action disabled">The Nickel Manual <span className={'text-primary'}> - coming soon</span></Link>
                <Link to="#" className="list-group-item list-group-item-action disabled">Language specification<span className={'text-primary'}> - coming soon</span></Link>
              </div>
            </div>
          </div>
        </main>
      </Layout>
  )
};

export default IndexPage
