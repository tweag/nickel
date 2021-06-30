exports.onCreateWebpackConfig = ({ stage, actions, loaders }) => {
    actions.setWebpackConfig({
        experiments: {
            // This was necessary to have the Nickel WASM REPL work with Webpack
            asyncWebAssembly: true,
        },
       /* module: {
            rules: [
                {
                    test: /react-ace/,
                    use: loaders.null(),
                },
                {
                    test: /ace-builds/,
                    use: loaders.null(),
                },
                {
                    test: /ace-nickel-mode.js/,
                    use: loaders.null(),
                }
            ],
        },*/
    })
};
