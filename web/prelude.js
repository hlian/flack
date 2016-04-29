'use strict'

window._j = JSON.stringify

window._a = (o) => {
  if (o === null) {
    throw new Error("unexpected null");
  }
  return o;
}

window._fetchJSON = (url, opts = {}) => {
  Object.assign(opts, {
    headers: {
      "Accept": "application/json",
      "Content-type": "application/json"
    }
  })
  return fetch(url, opts).then(x => x.json())
}
