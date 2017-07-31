import Query from "query-string"
import React, { Component } from "react"

import "../prelude"

const Status = Object.freeze({
  NEW: Symbol("new"),
  NOCODE: Symbol("nocode"),
  BAD: Symbol("bad"),
  GOOD: Symbol("good")
})

export default class Redirect extends Component {
  constructor(props) {
    super(props)
    this.state = { status: Status.NEW }
  }

  toString() {
    return `[Redirect ${JSON.stringify(this.state)}]`
  }

  componentDidMount() {
    const query = Query.parse(window.location.search)
    if (query.code !== null && query.code !== "") {
      const opts = {method: "POST"}
      _fetchJSON("/api/oauth/redirect?code=" + encodeURIComponent(query.code), opts).then(data => {
        this.setState({status: Status.GOOD})
        setTimeout(() => window.close(), 5000)
      }).catch(reason => {
        reason.response.text().then(text => {
          this.setState({status: Status.BAD, response: reason.response, text: text})
        })
      })
    } else {
      this.setState({status: Status.NOCODE})
    }
  }

  render() {
    const state = _a(this.state)
    if (state.status === Status.NEW) return this._renderNew()
    if (state.status === Status.NOCODE) return this._renderNoCode()
    if (state.status === Status.GOOD) return this._renderGood()
    if (state.status === Status.BAD) return this._renderBad()
    console.trace("render(): partial")
    return null
  }

  _renderNew() {
    return (
      <div>
        <h2>[redirect]</h2>
        <p>Keep this window open. Almost done authenticating...</p>
      </div>
    )
  }

  _renderGood() {
    return (
      <div>
        <h2>[redirect]</h2>
        <p>All done! You can close this window now.</p>
      </div>
    )
  }

  _renderNoCode() {
    return (
      <div>
        <h2>[redirect]</h2>
        <p className="css-bad">We were expecting a <code>?code=</code> from Slack but could not find one. Contact Hao.</p>
      </div>
    )
  }

  _renderBad() {
    const e = _a(this.state.response)
    const text = _a(this.state.text)
    return (
      <div>
        <h2>[redirect]</h2>
        <p className="css-bad">Unexpected error ({e.status}) while talking to <code>{e.url}</code>: <code>{text}</code>. Contact Hao.</p>
      </div>
    )
  }
}
