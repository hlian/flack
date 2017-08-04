import '../prelude'
import React, { Component } from 'react'

const Status = Object.freeze({
  NEW: Symbol("new"),
  LOADING: Symbol("loading"),
  GOOD: Symbol("good"),
  BAD: Symbol("bad")
})

export default class Archive extends Component {
  constructor(props) {
    super(props)
    this.state = { status: Status.NEW }
  }

  toString() {
    return `[Archive ${JSON.stringify(this.state)}]`
  }

  componentDidMount() {
  }

  render() {
    const state = _a(this.state)
    if (state.status === Status.NEW) return this._renderNew()
    console.trace("render(): partial")
    return null
  }

  _renderNew() {
    return (
      <span>
        <button onClick={(e) => this._delete(e)}>Delete</button>
        <button onClick={(e) => this._archive(e)}>Archive</button>
      </span>
    )
  }

  _archive(e) {
    alert("hey")
  }

  _delete(e) {
    alert("hey")
  }
}
