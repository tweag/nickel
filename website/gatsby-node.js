exports.onCreateWebpackConfig = ({ stage, actions }) => {
    actions.setWebpackConfig({
        experiments: {
            // This was necessary to have the Nickel WASM REPL work with Webpack
            syncWebAssembly: true,
        },
    })
};
