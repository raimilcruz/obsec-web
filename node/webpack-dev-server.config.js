const webpack = require('webpack');
const path = require('path');
const buildPath = path.resolve(__dirname, 'build');
const nodeModulesPath = path.resolve(__dirname, 'node_modules');
const TransferWebpackPlugin = require('transfer-webpack-plugin');

var reactDomLibPath = path.join(__dirname, "./node_modules/react-dom/lib");
var alias = {};
["EventPluginHub", "EventConstants", "EventPluginUtils", "EventPropagators",
    "SyntheticUIEvent", "CSSPropertyOperations", "ViewportMetrics"].forEach(function (filename) {
    alias["react/lib/" + filename] = path.join(__dirname, "./node_modules/react-dom/lib", filename);
});


const config = {
    //Entry points to the project
    entry: [
        'webpack/hot/dev-server',
        'webpack/hot/only-dev-server',
        path.join(__dirname, '/src/app/app.jsx'),
    ],
    //Config options on how to interpret requires imports
    resolve: {
        extensions: ["", ".js", ".jsx"],
        alias: alias
        //node_modules: ["web_modules", "node_modules"]  (Default Settings)
    },
    //Server Configuration options
    devServer: {
        contentBase: 'src/www',  //Relative directory for base of server
        devtool: 'eval',
        hot: true,        //Live-reload
        inline: true,
        port: 3000,        //Port Number
        host: '0.0.0.0',  //Change to '0.0.0.0' for external facing server
    },
    devtool: 'eval',
    output: {
        path: buildPath,    //Path of output file
        filename: 'app.js'
    },
    plugins: [
        //Enables Hot Modules Replacement
        new webpack.HotModuleReplacementPlugin(),
        //Allows error warnings but does not stop compiling. Will remove when eslint is added
        new webpack.NoErrorsPlugin(),
        //Moves files
        new TransferWebpackPlugin([
            {from: 'www'},
        ], path.resolve(__dirname, "src")),
    ],
    module: {
        loaders: [
            {
                //React-hot loader and
                test: /\.jsx?$/,  //All .js files
                loaders: ['react-hot', 'babel-loader'], //react-hot is like browser sync and babel loads jsx and es6-7
                exclude: [nodeModulesPath],
            },
            {
                test: /\.css$/,
                loaders: ['style', 'css'],
                include: [path.join(__dirname, 'src/www'), path.join(__dirname, 'node_modules/react-select/dist/react-select.css')
                    , path.join(__dirname, 'node_modules/highlight.js/styles/github.css')],
                exclude: /flexboxgrid/
            },
            {
                test: /\.css$/,
                loader: 'style!css?modules',
                include: /flexboxgrid/,
            },
        ],
    },
    //eslint config options. Part of the eslint-loader package
    eslint: {
        configFile: '.eslintrc',
    },
};

module.exports = config;


