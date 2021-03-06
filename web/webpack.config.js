const path = require("path")

module.exports = {
  entry: {
    index: "./index.js",
    redirect: "./redirect.js"
  },
  devtool: "source-map",
  stats: "minimal",
  output: {
    path: path.resolve(__dirname, "../public"),
    filename: "_[name].js"
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ["react", "env"]
          }
        }
      }
    ]
  }
}
