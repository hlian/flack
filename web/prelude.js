import 'whatwg-fetch'

window._j = JSON.stringify

window._a = (o) => {
  if (o === null) {
    throw new Error("unexpected null")
  }
  return o
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
    throw new Error(x)
  })
}

window._byID = (id) => {
  const o = document.getElementById(id)
  if (o === null) {
    throw new Error("_byID: could not find #" + id)
  }
  return o
}

export * from "./prelude/haskell"
