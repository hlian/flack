export const Nothing = {
  _type: "Maybe"
}

export const Just = (x) => {
  return { _just: x, _type: "Maybe" }
}

export const map = (a2b, fa) => {
  if (fa._type === undefined) {
    throw new Error("unknown type: " + fa)
  }

  if (!_functor.has(fa._type)) {
    throw new Error("unknown functor: " + fa._type)
  }

  return _functor.get(fa._type).map(a2b, fa)
}

const _functor = new Map(
  [["Maybe", {
    "map": (a2b, maybe) => {
      if (maybe.hasOwnProperty("_just")) {
        return Just(a2b(maybe._just))
      } else {
        return Nothing
      }
    }
  }]]
)
