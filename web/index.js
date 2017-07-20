import './prelude'

import React from 'react'
// import only the render function of react-dom using ES2015 destructuring
import { render } from 'react-dom'
import Auth from './components/auth'

render(
  <Auth />,
  _byID('component-auth')
)
