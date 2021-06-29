import React from "react"
import {graphql, StaticQuery} from "gatsby"
import {Helmet} from "react-helmet"
import Header from "./header"
import Footer from "./footer"

export default function Layout({children}) {
    return (
        <StaticQuery
            query={graphql`
      query SiteTitleQuery {
        site {
          siteMetadata {
            title
             menuLinks {
               name
               link
             }
          }
        }
      }
    `}
            render={data => (
                <React.Fragment>
                    <Helmet
                        title={'Nickel'}
                        meta={[
                            {name: 'description', content: 'Sample'},
                            {name: 'keywords', content: 'sample, something'},
                        ]}
                    >
                    </Helmet>
                    <Header siteTitle={data.site.siteMetadata.title} menuLinks={data.site.siteMetadata.menuLinks}/>
                    <div>
                        {children}
                    </div>
                    <Footer/>
                </React.Fragment>
            )}
        />
    )
}