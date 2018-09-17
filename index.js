const R = require('ramda')

const ONE_OF = 'ONE_OF'
const E = 'Empty'
const S = 1
const A = 2
const O = 3
const P = 4

// is(E|S|A|O|P) :: T -> Boolean
const isOneOf = t => t === ONE_OF
const isNotOneOf = t => t !== ONE_OF
const isNotEmptyType = t => t !== E

const isE = R.allPass([
  t => t === E,
  isNotOneOf
])
const isS = R.allPass([
  R.pipe(
    t => typeof t,
    R.equals('string')
  ),
  t => t !== E,
  isNotOneOf,
  isNotEmptyType
])

const isA = R.allPass([
  Array.isArray,
  R.anyPass([R.isEmpty, R.pipe(R.head, isNotOneOf)]),
  isNotOneOf,
  isNotEmptyType
])
const isO = R.allPass([
  t => typeof t === 'object',
  R.complement(Array.isArray),
  isNotOneOf,
  isNotEmptyType
])
const isP = R.allPass([
  Array.isArray,
  R.pipe(R.length, optionsAmount => optionsAmount >= 3),
  R.pipe(R.head, isOneOf),
  isNotEmptyType
])


// SETOID ---------------------------------------------------------------------

const containsSameValues = R.compose(R.isEmpty, R.symmetricDifference)
const containsSameValuesWith = R.compose(R.isEmpty, R.symmetricDifferenceWith)

// equalsS :: S -> S -> Boolean
const equalsS = R.equals

// equalsA :: A -> A -> Boolean
const equalsA = (a1, a2) => containsSameValuesWith(equals, a1, a2)

// equalsP :: P -> P -> Boolean
const equalsP = R.curry((p1, p2) => {
  const variants1 = p1.slice(1)
  const variants2 = p2.slice(1)
  return containsSameValuesWith(equals, variants1, variants2)
})

// equalsO :: O -> O -> Boolean
const equalsO = R.curry((obj1, obj2) => {
  const keys1 = Object.keys(obj1)
  const keys2 = Object.keys(obj2)
  const hasSameKeys = containsSameValues(keys1, keys2)
  const hasSameTypesOfKeys = keys1.every(key => equals(obj1[key], obj2[key]))
  return hasSameKeys && hasSameTypesOfKeys
})

const equalsE = R.always(true)

var equals = R.curry((t1, t2) => {
  if (isS(t1) && isS(t2)) return equalsS(t1, t2)
  if (isA(t1) && isA(t2)) return equalsA(t1, t2)
  if (isP(t1) && isP(t2)) return equalsP(t1, t2)
  if (isO(t1) && isO(t2)) return equalsO(t1, t2)
  if (isE(t1) && isE(t2)) return equalsE(t1, t2)
  return false
})

// ORD ------------------------------------------------------------------------

// getSortedArrayOfTypes :: [T] -> [T]
const getSortedArrayOfTypes = R.pipe(
  R.clone,
  R.sort((t1, t2) => lte(t1, t2) ? -1 : 1)
)

// lteSortedTypesArray [T] -> [T] -> Boolean
const lteSortedTypesArray = R.curry((a1, a2) => {
  if (R.isEmpty(a1) && R.isEmpty(a2)) return true

  for (let i = 0; i < a1.length; i++) {
    const first = a1[i]
    const second = a2[i]
    if (equals(first, second)) {
      continue
    }
    if (lte(first, second)) {
      return true
    }
    if (lte(second, first)) {
      return false
    }
  }
  return true
})

// lteE :: E -> E -> Boolean
const lteE = R.always(true)

// lteS :: S -> S -> Boolean
const lteS = R.curry((t1, t2) => t1 <= t2)

// lteA :: A -> A -> Boolean
const lteA = R.curry((a1, a2) => {
  if (a1.length < a2.length) return true
  if (a1.length > a2.length) return false
  const sorted1 = getSortedArrayOfTypes(a1)
  const sorted2 = getSortedArrayOfTypes(a2)
  return lteSortedTypesArray(sorted1, sorted2)
})

// lteP :: P -> P -> Boolean
const lteP = R.curry((p1, p2) => {
  if (p1.length < p2.length) return true
  if (p1.length > p2.length) return false
  const types1 = p1.slice(1)
  const types2 = p2.slice(1)
  const sortedTypes1 = getSortedArrayOfTypes(types1)
  const sortedTypes2 = getSortedArrayOfTypes(types2)
  return lteSortedTypesArray(sortedTypes1, sortedTypes2)
})

// lteO :: O -> O -> Boolean
const lteO = R.curry((obj1, obj2) => {
  const keys1 = Object.keys(obj1)
  const keys2 = Object.keys(obj2)
  if (keys1.length < keys2.length) return true
  if (keys1.length > keys2.length) return false
  const sKeys1 = keys1.sort()
  const sKeys2 = keys2.sort()
  const joinedKeys1 = sKeys1.join('')
  const joinedKeys2 = sKeys2.join('')
  if (joinedKeys1 < joinedKeys2) return true
  if (joinedKeys1 > joinedKeys2) return false
  const types1 = sKeys1.map(k => obj1[k])
  const types2 = sKeys2.map(k => obj2[k])
  return lteSortedTypesArray(types1, types2)
})

// lte :: T -> T -> Boolean
var lte = R.curry((t1, t2) => {
  switch (true) {
    case equals(t1, t2):
      return true
    case isE(t1) && isE(t2):
      return lteE(t1, t2)
    case isS(t1) && isS(t2):
      return lteS(t1, t2)
    case isA(t1) && isA(t2):
      return lteA(t1, t2)
    case isO(t1) && isO(t2):
      return lteO(t1, t2)
    case isP(t1) && isP(t2):
      return lteP(t1, t2)
    case isE(t1):
      return true
    case isS(t1):
      return [isA, isO, isP].some(f => f(t2))
    case isA(t1):
      return [isO, isP].some(f => f(t2))
    case isO(t1):
      return isP(t2)
    case isP(t1):
      return false
  }
  return false
})

// MONOID ---------------------------------------------------------------------

const empty = () => E

// concatS :: S -> S -> P
const concatS = R.curry((s1, s2) => [ONE_OF, s1, s2])

// concatA :: A -> A -> P
const concatA = R.curry((a1, a2) => [ONE_OF, a1, a2])

// concatSameStructureO :: O -> O -> O
const concatSameStructureO = R.curry((obj1, obj2) => {
  const keys = Object.keys(obj1)
  const mergeReducer = (resObj, key) => {
    const t1 = obj1[key]
    const t2 = obj2[key]
    const newType = concat(t1, t2)
    return R.assoc(key, newType, resObj)
  }
  return keys.reduce(mergeReducer, {})
})

// hasSameStructureO :: O -> O -> Boolean
const hasSameStructureO = R.curry((obj1, obj2) => {
  const keys1 = Object.keys(obj1)
  const keys2 = Object.keys(obj2)
  return containsSameValues(keys1, keys2)
})


// concatS :: S -> S -> T
const concatO = R.curry((obj1, obj2) => {
  return hasSameStructureO(obj1, obj2) ?
    concatSameStructureO(obj1, obj2) : [ONE_OF, obj1, obj2]
})

const minimifyArrayType = arr => {
  const sTypes = R.filter(isS, arr)
  const aTypes = R.filter(isA, arr)
  const oTypes = R.filter(isO, arr)
  const pTypes = R.filter(isP, arr)
  const objTypesReducer = (resOTypes, obj) => {
    const ind = resOTypes.findIndex(t => hasSameStructureO(t, obj))
    if (ind < 0) return [...resOTypes, obj]
    const objToBeMergedLens = R.lensIndex(ind)
    return R.over(objToBeMergedLens, concatSameStructureO(obj), resOTypes)
  }

  const concatenatedOTypes = oTypes.reduce(objTypesReducer, [])
  const resTypes = [...sTypes, ...aTypes, ...concatenatedOTypes, ...pTypes]
  return resTypes
}

// concatDifferent :: T -> T -> T
const concatDifferent = R.curry((t1, t2) => {
  const types1 = isP(t1) ? t1.slice(1) : [t1]
  const types2 = isP(t2) ? t2.slice(1) : [t2]
  const allTypes = R.uniqWith(equals, [...types1, ...types2])
  const resTypes = minimifyArrayType(allTypes)
  switch (resTypes.length) {
    case 0:
      return empty()
    case 1:
      return resTypes[0]
    default:
      return [ONE_OF, ...resTypes]
  }
})

// concatP :: P -> P -> T
const concatP = concatDifferent

// concat :: T -> T -> T
var concat = R.curry((t1, t2) => {
  switch (true) {
    case isE(t1):
      return t2
    case isE(t2):
      return t1
    case equals(t1, t2):
      return t1
    case isS(t1) && isS(t2):
      return concatS(t1, t2)
    case isA(t1) && isA(t2):
      return concatA(t1, t2)
    case isO(t1) && isO(t2):
      return concatO(t1, t2)
    case isP(t1) && isP(t2):
      return concatP(t1, t2)
  }

  return concatDifferent(t1, t2)
})

var concatAll = R.reduce((res, t) => concat(res, t), E)


/**
 * @callback GetType
 * @param {*} any value of javascript
 * @param {object.<alias, type>} typesAliases
 * @returns {Type} type of value of javascript
 */
/**
 * @type GetType
 */
const getType = function (value, typesAliases = {}) {
  const _getType = (value, path, pathNames) => {
    const type = R.type(value)
    if (R.contains(value, path)) {
      const index = path.findIndex(e => e === value)
      return `${type}[${pathNames.slice(0, index+1).join(', ')}]`
    }
    switch (true) {
      case R.contains(type, ['Number', 'Boolean', 'String', 'Null', 'RegExp', 'Undefined']):
        return type
        break;
      case value instanceof Date:
        return `Date`
        break;
      case value instanceof Promise:
        return 'Promise'
        break;
      case type === 'Function':
        return value.length ?
          `Function (${value.length})` :
          `Function (void)`
        break;
      case type === 'Array':
        return value.length ?
          minimifyArrayType(R.pipe(
            arr => arr.map((e, i, arr) => _getType(e, R.append(value, path), R.append(i, pathNames))),
            R.uniqWith(equals))(value)) : []
        break;
      case type === 'Object':
        const keys = Object.keys(value)
        return keys.reduce((res, key) => {
          return R.assoc(key, _getType(value[key], R.append(value, path), R.append(key, pathNames)), res)
        }, {})
        break;
      default:
        return type
    }
  }
  return simplifyFromDict(_getType(value, [], ['value']), typesAliases)
}

const simplifyFromDict = R.curry((t, dict) => {
  const pairs = R.toPairs(dict)
  if (R.isEmpty(pairs)) return t
  const _simplifyFromDict = (t) => {
    const [alias] = pairs.find(([currentAlias, type]) => equals(type, t)) || []
    if (alias) return alias
    if (isS(t) || isE(t)) return t
    if (isA(t)) {
      return t.map(innerT => _simplifyFromDict(innerT))
    }
    if (isP(t)) {
      return [ONE_OF, ...t.slice(1).map(innerT => _simplifyFromDict(innerT))]
    }
    if (typeof t !== 'object') return t
    const getReplaced = R.pipe(
      R.toPairs,
      ([propName, innerT]) => [propName, _simplifyFromDict(innerT)],
      R.fromPairs
    )
    return getReplaced(t)
  }
  return _simplifyFromDict(t)
})

const toJSDocTypeA = (t, level, TAB, NEW_LINE) => {
  if (R.isEmpty(t)) return '[]'

  let arrItemType = t.map(t => toJSDocType(t, level, TAB, NEW_LINE)).join("|")

  if (t.length > 1) arrItemType = `(${arrItemType})`

  return `${arrItemType}[]`
}

const toJSDocTypeP = (t, level, TAB, NEW_LINE) => {
  const options = t.slice(1)
  const optionsJSDoc = options.map(opt => toJSDocType(opt, level, TAB, NEW_LINE))
  const optionsText = optionsJSDoc.join('|')
  return `(${optionsText})`
}

const toJSDocTypeES = t => `${t}`

const toJSDocTypeO = (t, level, TAB, NEW_LINE) => {
  const pairToJsDocObj = ([propName, type]) => ({
    propName,
    jsdocType: toJSDocType(type, level + 1, TAB, NEW_LINE)
  })

  const jsDocObjToPropDesc = ({
    propName,
    jsdocType
  }, i, a) => `${propName}: ${jsdocType}${i === a.length - 1 ? '' : ','}`
  const propDescToPropDescsWithTabs = propDesc => `${TAB.repeat(level + 1)}${propDesc}`
  const propDescWithTabsToMultiline = propDescsWithTabs => propDescsWithTabs.join(NEW_LINE)
  const addCurlyBraces = str => `{${NEW_LINE}${str}${NEW_LINE}${TAB.repeat(level)}}`

  const getObjectTypeJSDoc = R.pipe(
    R.toPairs,
    R.map(pairToJsDocObj),
    jsDocObjs => jsDocObjs.map(jsDocObjToPropDesc),
    R.map(propDescToPropDescsWithTabs),
    propDescWithTabsToMultiline,
    addCurlyBraces
  )
  return getObjectTypeJSDoc(t)
}

const toJSDocType = (t, level = 0, TAB = '  ', NEW_LINE = '\n') => {
  if (isA(t)) {
    return toJSDocTypeA(t, level, TAB, NEW_LINE)
  }
  if (isP(t)) {
    return toJSDocTypeP(t, level, TAB, NEW_LINE)
  }
  if (!isO(t)) {
    return toJSDocTypeES(t, level, TAB, NEW_LINE)
  }
  return toJSDocTypeO(t, level, TAB, NEW_LINE)
};


module.exports = {
  ONE_OF,
  E,
  S,
  A,
  O,
  P,
  isOneOf,
  isNotOneOf,
  isE,
  isS,
  isA,
  isO,
  isP,
  containsSameValues,
  containsSameValuesWith,
  equalsS,
  equalsA,
  equalsP,
  equalsO,
  equalsE,
  equals,
  getSortedArrayOfTypes,
  lteSortedTypesArray,
  lteE,
  lteS,
  lteA,
  lteP,
  lteO,
  lte,
  empty,
  concatS,
  concatA,
  concatSameStructureO,
  hasSameStructureO,
  concatO,
  minimifyArrayType,
  concatDifferent,
  concatP,
  concat,
  concatAll,
  getType,
  simplifyFromDict,
  toJSDocType
}