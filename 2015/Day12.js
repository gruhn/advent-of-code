const input = require('./input/12.json')

const add = (a,b) => a+b

const traverse = item => {
    if (typeof item === 'number') {
        return item 
    } else if (Array.isArray(item)) {
        return item.map(traverse).reduce(add)
    } else if (typeof item === 'object') {
        return Object.values(item).map(traverse).reduce(add)
    } else {
        return 0
    }
}

const mapObject = (f, object) => {
    return Object.entries(object).map(
        ([key,value]) => [key,f(value)]
    )
}

const rejectRed = item => {
    if (Array.isArray(item)) {
        return item.map(rejectRed)
    } else if (typeof item === 'object') {
        if (Object.values(item).includes("red")) {
            return 0
        } else {
            return mapObject(rejectRed, item)
        }
    } else {
        return item
    }
}

console.log("Part 1: " + traverse(input))
console.log("Part 2: " + traverse(rejectRed(input)))