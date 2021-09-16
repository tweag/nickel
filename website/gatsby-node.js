exports.onCreateWebpackConfig = ({ _stage, actions, _loaders }) => {
    actions.setWebpackConfig({
        experiments: {
            // This was necessary to have the Nickel WASM REPL work with Webpack
            asyncWebAssembly: true,
        },
    })
};
