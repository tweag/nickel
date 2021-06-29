module.exports = {
    siteMetadata: {
        title: "Nickel",
        menuLinks: [
            {
                name: 'Getting started',
                link: '/getting-started'
            },
            // Disabling the documentation page for now.
            // There is just not enough interesting content to show.
            /*{
                name: 'Documentation',
                link: '/documentation'
            },*/
            {
                name: 'Playground',
                link: '/playground'
            },
        ]
    },
    plugins: [
        'gatsby-plugin-react-helmet',
        'gatsby-plugin-sharp',
        'gatsby-transformer-sharp',
        'gatsby-plugin-image',
        {
            resolve: `gatsby-source-filesystem`,
            options: {
                name: `markdown-pages`,
                path: `${__dirname}/src/markdown-pages`,
            },
        },
        `gatsby-plugin-sass`,
        {
            resolve: 'gatsby-transformer-remark',
            options: {
                plugins: [
                    {
                        resolve: `gatsby-remark-prismjs`,
                        options: {
                            // Class prefix for <pre> tags containing syntax highlighting;
                            // defaults to 'language-' (e.g. <pre class="language-js">).
                            // If your site loads Prism into the browser at runtime,
                            // (e.g. for use with libraries like react-live),
                            // you may use this to prevent Prism from re-processing syntax.
                            // This is an uncommon use-case though;
                            // If you're unsure, it's best to use the default value.
                            classPrefix: "language-",
                            // This is used to allow setting a language for inline code
                            // (i.e. single backticks) by creating a separator.
                            // This separator is a string and will do no white-space
                            // stripping.
                            // A suggested value for English speakers is the non-ascii
                            // character '›'.
                            inlineCodeMarker: null,
                            // Customize the prompt used in shell output
                            // Values below are default
                            prompt: {
                                user: "devops",
                                host: "nickel-lang",
                                global: false,
                            },
                            // By default the HTML entities <>&'" are escaped.
                            // Add additional HTML escapes by providing a mapping
                            // of HTML entities and their escape value IE: { '}': '&#123;' }
                            escapeEntities: {},
                            languageExtensions: [
                                {
                                    language: "nickel",
                                    definition: {
                                        comment: /\/\/.+/,
                                        let: /let/,
                                        in: /in/,
                                        fun: /fun/,
                                        switch: /switch/,
                                        numLiteral: /[0-9]*\.?[0-9]+/,
                                        baseTypes: /((?:Num)|(?:Bool)|(?:Str)|(?:List:(?:[a-zA-Z0-9_]*)?))/,
                                        stringSimple: /".+"/,
                                        stringMulti: /m(#)+".+"\1m/,
                                    },
                                },
                            ]
                        },
                    },
                ],
            },
        },
    ],
};
