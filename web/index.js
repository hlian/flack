import React from 'react'
import { render } from 'react-dom'

import Auth from './components/auth'
import Files from './components/files'
import * as $ from './prelude'

render(
  <Auth />,
  _byID('component-auth')
)

render(
  <Files />,
  _byID('component-files')
)
