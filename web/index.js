import React from 'react'
// import only the render function of react-dom using ES2015 destructuring
import { render } from 'react-dom'

import Auth from './components/auth'
import * as $ from './prelude'

console.log("hey", $.map((x) => { return x * 100 }, $.Just(5)))

render(
  <Auth />,
  _byID('component-auth')
)
