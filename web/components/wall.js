'use strict'

import '../prelude'
import React, { Component, PropTypes } from 'react';

export default class Wall extends Component {
  constructor(props) {
    super(props)
    this.state = { loading: true }
  }

  toString() {
    return `[Wall]`
  }

  componentDidMount() {
    _fetchJSON("http://localhost:3000/api/")
      .then(data => {
        console.log("did mount", data)
        this.setState({
          loading: false,
          avatarURL: _a(data.avatarURL),
          posts: _a(data.posts),
          name: _a(data.name)
        })
      })
  }

  render() {
    const state = _a(this.state);
    if (state.loading) {
      return (
        <p id="wall-loading">reticulating splines...</p>
      )
    } else {
      return (
        <div id="wall-done">
          <p className="name">{state.name}</p>
          <p>
            <img src={state.avatarURL} />
          </p>
        </div>
      )
    }
  }
}
