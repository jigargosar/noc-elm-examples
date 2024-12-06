# ch 03: 11 examples, start: 4th, target: 14th Dec
we have time till 8th, either we move forward with 3rd chapter, or work on generating index page. With simple styling. Let's give it some thought, and come up with a simple plan. Get a mockup ui ready, perhaps cloning the examples page of NOC.
4th Dec:
starting with chapter 3 oscillation,
it has 11 examples.
so in 11 days ( 11 + 3 ) 14th Dec we should finish this chapter.
and will also try to migrate examples in their own folder.
we also gained few days by completing before deadline.
Current Buffer: +6

## Log
* Day 1: 4th Dec : eg 3.1: angular motions.
* Day 2: 5th Dec : Example 3.2: Forces with (arbitrary) angular motion.
* Day 3: 6th Dec: Example 3.3: Pointing in the Direction of Motion

---

# Current Targets: DT 2nd Dec`24
* 5 examples remaining in Ch2: Forces. Target 6 days.
* Start: 2nd Dec`24
* Target: 8th Dec`24

Overall pending target is 9 chapters with (max) 15 examples each. At max the project should finish in 5 months, i.e. 150 days. This is month of december, so project should finish, at max, in April. Keeping current target to end of March.

December target is 25 examples.

## Checklist
* target date 8th
* 2.6 done on 2nd
* 2.7 started on 3rd
* 2.8
* 2.9 completed on 3rd


# Archived
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
