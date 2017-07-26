import '../prelude'
import React, { Component } from 'react'
import update from "immutability-helper"

const Status = Object.freeze({
  NEW: Symbol("new"),
  SOON: Symbol("soon"),
  LOADING: Symbol("loading"),
  POPUP: Symbol("popup"),
  BAD: Symbol("bad"),
  GOOD: Symbol("good")
})

export default class Auth extends Component {
  constructor(props) {
    super(props)
    this.state = { status: Status.NEW }
  }

  toString() {
    return `[Auth ${JSON.stringify(this.state)}]`
  }

  componentDidMount() {
    _fetchJSON("/api/oauth").then(data => {
      if (data.id !== null && data.id !== "") {
        this.setState({status: Status.GOOD, goodID: _a(data.id)})
        window._broadcast(data.id)
      } else {
        this.setState({status: Status.SOON, soonURL: _a(data.slack)})
      }
    }).catch(reason => {
      this.setState({status: Status.BAD, response: reason.response})
    })
  }

  render() {
    const state = _a(this.state)
    if (state.status === Status.NEW) return this._renderNew()
    if (state.status === Status.SOON) return this._renderSoon()
    if (state.status === Status.LOADING) return this._renderLoading()
    if (state.status === Status.POPUP) return this._renderPopup()
    if (state.status === Status.GOOD) return this._renderGood()
    if (state.status === Status.BAD) return this._renderBad()
    console.trace("render(): partial")
    return null
  }

  _renderLoading() {
    return (
      <div>
        <h2>[authenticate]</h2>
        <p>Authenticating... [spinner]</p>
      </div>
    )
  }

  _renderNew() {
    return (
      <div>
        <h2>[authenticate]</h2>
        <p>Reticulating splines...</p>
      </div>
    )
  }

  _renderSoon() {
    return (
      <div>
        <h2>[authenticate]</h2>
        <p>
          Before doing any work, Flack needs to authenticate as you with Slack.
          <button onClick={(e) => this._renderNewClick(e)}>Drink Me</button>
        </p>
      </div>
    )
  }

  _renderGood() {
    return (
      <div>
        <h2>[authenticate]</h2>
        <p>You are authenticated! Your session ID is <code>{this.state.goodID}</code>.</p>
        <p className="css-sub">Other websites won't show you your session ID. This one will, and does. Your session will end as soon as you quit your browser.</p>
      </div>
    )
  }

  _renderNewClick(e) {
    window.open(_a(this.state.soonURL), "[flack:authenticate]", "height=600;width=800")
    this.setState(update(this.state, {status: {$set: Status.POPUP}}))
  }

  _renderPopup() {
    return (
      <div>
        <h2>[authenticate]</h2>
        <p>There should be a popup open now, requesting permissions from Slack. Flack will never share your private information, nor will it add files under your name. It will only ever delete the files you ask it to delete, which is in its own way beautiful.</p>
      </div>
    )
  }

  _renderBad() {
    const e = _a(this.state.response)
    return (
      <div>
        <h2>[authenticate]</h2>
        <p className="css-bad">Unexpected error while talking to {e.url}: HTTP {e.status} {e.statusText}.</p>
      </div>
    )
  }
}
