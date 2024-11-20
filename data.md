* started working on 1.8 Motion 101 (Velocity and Constant Acceleration)
* scraped all example number and title from website using following snippet. And stored it noc-data.json.

```js
console.clear()

// $$("main .not-prose > div > div:nth-child(1)").map(e => e.textContent)
// $$("main .not-prose > a > div:nth-child(2)").map(e => e.textContent)

// $$("main .not-prose").map(e => $("a > div:nth-child(2)",e).textContent)
// $$("main .not-prose").map(e => $(":scope > div > div",e))[0].textContent

function foo(e) {
    return [
        $(":scope > a > div:nth-child(2)", e).textContent,
        $(":scope > div > div", e).textContent
    ]
}

data = $$("main .not-prose").map(foo)

console.dir(JSON.stringify(data))
```
