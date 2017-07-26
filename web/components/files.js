import '../prelude'
import React, { Component } from 'react'

const Status = Object.freeze({
  NEW: Symbol("new"),
  AUTH: Symbol("auth")
})

export default class Files extends Component {
  constructor(props) {
    super(props)
    this.state = { status: Status.NEW }
  }

  toString() {
    return `[Files ${JSON.stringify(this.state)}]`
  }

  componentDidMount() {
    const that = this
    window._listen(sessionID => {
      that.setState({status: Status.AUTH, auth: sessionID})
    })
  }

  render() {
    const state = _a(this.state)
    if (state.status === Status.NEW) return this._renderNew()
    if (state.status === Status.AUTH) return this._renderAuth()
    console.trace("render(): partial")
    return null
  }

  _renderNew() {
    return (
      <div>
        <h2>[files]</h2>
        <p>You must authenticate first.</p>
      </div>
    )
  }

  _renderAuth() {
    return (
      <div>
        <h2>[files]</h2>
        <p>Authed! {this.state.auth}</p>
      </div>
    )
  }
}
