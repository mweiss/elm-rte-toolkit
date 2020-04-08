# Rich Text Editor Toolkit
Create rich text editors in Elm.

Rich Text Editor Toolkit is an open source project to make cross platform editors on the web. This package treats contenteditable as an I/O device, and uses browser events and mutation observers to detect changes and update itself.  The editor's model is defined and validated by a programmable specification that allows you to create a custom tailored editor that fits your needs.

This package was heavily inspired by other rich text editor frameworks like ProseMirror, Trix, and DraftJS.  

## Resources
- Elm Package: https://package.elm-lang.org/packages/mweiss/elm-rte-toolkit/latest/ 
- Demo page: https://mweiss.github.io/elm-rte-toolkit (source code is in the [demo](demo) directory)
- Wiki: https://github.com/mweiss/elm-rte-toolkit/wiki

## Getting started

This package requires some webcomponents to get started.

If you can support ES6, you can include [js/elmEditor.js](js/elmEditor.js) in your project and import it
along with your favorite webcomponent polyfill.

```js
import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'
import 'elmEditor.js'
```

The demo in this repository does it that way.

However, if you want to use a bundler and polyfill, you can import your favorite polyfill and
import the npm package that has this repository's js compiled to es5 with npm, e.g:

```bash
npm install --save @webcomponents/webcomponentsjs
npm install --save elm-rte-toolkit
```

And in your javascript, you can import it like so:

```js
import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'
import '@webcomponents/webcomponentsjs/custom-elements-es5-adapter.js'
import 'elm-rte-toolkit';
```

### Starting CSS

You can use whatever styles you want for the editor, but you may want to use the following as
a jumping off point.  Most importantly, you'll probably want `white-space: pre-wrap;` to distinguish
between multiple spaces:

```css
.rte-main {
    text-align: left;
    outline: none;
    user-select: text;
    -webkit-user-select: text;
    white-space: pre-wrap;
    word-wrap: break-word;
}

.rte-hide-caret {
    caret-color: transparent;
}

```

## Contributing

This package is open-source software, freely distributable under the terms of an [BSD-3-Clause license](LICENSE). The [source code is hosted on GitHub](https://github.com/mweiss/elm-rte-toolkit).

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
./build.sh
```

The demo is hosted with gh-pages, so to update the demo, please update the gh-pages branch with the latest
build.

### Running tests

For now, because of mysterious package issues with elm-test I don't want to debug,
 tests for the package are in the demo app folder.  To run tests:

```bash
cd demo
elm-test
```


