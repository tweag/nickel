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
    ],
};
