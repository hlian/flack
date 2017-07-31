import '../prelude'
import React, { Component } from 'react'

const Status = Object.freeze({
  NEW: Symbol("new"),
  LOADING: Symbol("loading"),
  GOOD: Symbol("good"),
  BAD: Symbol("bad")
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
      that.setState({status: Status.LOADING, loading: sessionID})
      _fetchJSON("/api/files").then(data => {
        that.setState({status: Status.GOOD, data: data})
      }).catch(reason => {
        that.setState({status: Status.BAD, response: reason.response})
      })
    })
  }

  render() {
    const state = _a(this.state)
    if (state.status === Status.NEW) return this._renderNew()
    if (state.status === Status.LOADING) return this._renderLoading()
    if (state.status === Status.GOOD) return this._renderGood()
    if (state.status === Status.BAD) return this._renderBad()
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

  _renderLoading() {
    return (
      <div>
        <h2>[files]</h2>
        <p>Loading...</p>
      </div>
    )
  }

  _renderGood() {
    if (this.state.data.length === 0) {
      return (
        <div>
          <h2>[files]</h2>
          <p>You have no files! Thank you for visiting <b>[flack]</b>. Now you are free to go.</p>
        </div>
      )
    }

    const pretty = (x) => {
      if (x > 1024 * 1024 * 1024) {
        return (x / 1024 / 1024 / 1024).toFixed(1) + " GB"
      }
      if (x > 1024 * 1024) {
        return (x / 1024 / 1024).toFixed(1) + " MB"
      }
      if (x > 1024) {
        return (x / 1024).toFixed(1) + " KB"
      }
      return x + " B"
    }
    const toRow = (blob) => {
      return <tr key={blob.id}>
        <td><a href={blob.url}>{blob.id}</a></td>
        <td>{pretty(blob.size)}</td>
        <td><em>{blob.name}</em></td>
        <td>{JSON.stringify(blob.channels)}</td>
      </tr>
    }
    return (
      <div>
        <h2>[files]</h2>
        <table>
          <thead><tr>
            <th>ID & URL</th>
            <th>Size</th>
            <th>Name</th>
            <th>Channels</th>
          </tr></thead>
          <tbody>
            {this.state.data.map(toRow)}
          </tbody>
        </table>
      </div>
    )
  }

  _renderBad() {
    const e = _a(this.state.response)
    return (
      <div>
        <h2>[files]</h2>
        <p className="css-bad">Unexpected error while talking to <code>{e.url}</code>: HTTP {e.status} {e.statusText}.</p>
      </div>
    )
  }
}
