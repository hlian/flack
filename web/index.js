'use strict'

// "fetch" global
import 'whatwg-fetch'
import './prelude'

import React from 'react'
// import only the render function of react-dom using ES2015 destructuring
import { render } from 'react-dom'
import Wall from './components/wall'

render(
  <Wall />,
  document.getElementById('js-content')
)
