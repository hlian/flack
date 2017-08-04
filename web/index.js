import React from 'react'
import { render } from 'react-dom'

import Auth from './components/auth'
import Archive from './components/archive'
import Files from './components/files'
import * as $ from './prelude'

render(
  <Auth />,
  _byID('component-auth')
)

render(
  <Files>
    {{archiveFactory: (id) => <Archive fileID={id} />}}
  </Files>,
  _byID('component-files')
)
