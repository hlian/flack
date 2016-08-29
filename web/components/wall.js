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
    if (state.loading) return this._renderLoading();
    else return this._renderLoaded();
  }

  _renderLoading() {
    return (
      <p id="wall-loading">reticulating splines...</p>
    )
  }

  _renderLoaded() {
    const state = _a(this.state);
    return (
      <div id="wall-done">
        <h1 className="name">{state.name}</h1>
        <p>
          <img src={state.avatarURL} />
        </p>
        <form method="post" action="http://localhost:3000/api/posts">
          <p>
            <textarea placeholder={"post something to " + state.name + "'s wall"} name="textarea" rows="7" cols="40" className="css-textarea"></textarea>
          </p>
          <p>
            <input type="submit" value="post to wall"></input>
          </p>
        </form>
      </div>
    )
  }
}
