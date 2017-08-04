import 'whatwg-fetch'

window._j = JSON.stringify

window._a = (o) => {
  if (o === null || o === undefined) {
    throw new Error("unexpected null")
  }
  return o
}

class FetchError extends Error {
  constructor(response) {
    super("http mistake")
    this.response = response
    if (typeof Error.captureStackTrace === 'function') {
      Error.captureStackTrace(this, this.constructor)
    } else {
      this.stack = (new Error("")).stack
    }
  }
}

window._fetchJSON = (url, opts = {}) => {
  const copy = Object.assign({}, opts)
  copy.headers = new window.Headers({
    "Accept": "application/json",
    "Content-type": "application/json"
  })
  copy.credentials = "same-origin"
  return window.fetch(url, copy).then(x => {
    if (x.ok) return x.json()
    throw new FetchError(x)
  })
}

window._byID = (id) => {
  const o = document.getElementById(id)
  if (o === null) {
    throw new Error("_byID: could not find #" + id)
  }
  return o
}


window._broadcast = (id) => {
  for (const handler of window._handlers) {
    handler(id)
  }
}

window._listen = (handler) => {
  window._handlers.push(handler)
}

window._handlers = []

export * from "./prelude/haskell"
