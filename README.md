# Rich Text Editor Toolkit
Create rich text editors in Elm.

Rich Text Editor Toolkit is an open source project to make cross platform editors on the web. This package treats contenteditable as an I/O device, and uses browser events and mutation observers to detect changes and update itself.  The editor's model is defined and validated by a programmable specification that allows you to create a custom tailored editor that fits your needs.

This project was heavily inspired by other browser rich text editor frameworks like ProseMirror, Trix, and DraftJS.  

## Getting started

TODO: add getting started

## Examples
Examples can be seen in the demo page.

TODO: add links to demo page. 

## Contributing

This package is open-source software, freely distributable under the terms of an [BSD-3 license](LICENSE). The [source code is hosted on GitHub](https://github.com/mweiss/elm-rte-toolkit).

Contributions in the form of bug reports, pull requests, or thoughtful discussions in the [GitHub issue tracker](https://github.com/mweiss/elm-rte-toolkit/issues) are welcome. Please see the [Code of Conduct](CODE_OF_CONDUCT.md) for our pledge to contributors.

### Running the demo

The demo was bootstrapped with [create-elm-app](https://github.com/halfzebra/create-elm-app).  See that repository for instructions of how to install the `elm-app` command.

To debug the demo locally, run the following from the repository's root directory:
```bash
cd demo
elm-app start
```

To build the demo, run the following from the repository's root directory:
```bash
cd demo
elm-app build
```

### Running tests

For now, because of mysterious package issues with elm-test I don't want to debug,
 tests for the package are in the demo app folder.  To run tests:

```bash
cd demo
elm-test
```